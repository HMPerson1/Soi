{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}

module Language.Soi.Internal.Codegen where

import           ClassyPrelude

import           Control.Lens                            (ASetter', Cons, Snoc,
                                                          at, makeLenses, use,
                                                          view)
import           Control.Lens.Cons                       (_Cons, _head, _tail)
import           Control.Lens.Fold                       (preuse)
import           Control.Monad.Except                    (ExceptT, MonadError,
                                                          catchError,
                                                          runExceptT,
                                                          throwError)
import           Control.Monad.Reader                    (local)
import           Control.Monad.State                     (MonadState, StateT,
                                                          execStateT)
import           Data.List                               (genericLength)
import qualified Data.Sequence                           as Seq

import qualified LLVM.General.AST.CallingConvention      as CC
import qualified LLVM.General.AST.Constant               as C
import qualified LLVM.General.AST.Float                  as SF
import           LLVM.General.AST.FloatingPointPredicate (FloatingPointPredicate)
import           LLVM.General.AST.IntegerPredicate       (IntegerPredicate)
import           LLVM.General.AST.Type                   (ptr)
import qualified LLVM.General.AST.Type                   as T

import           Control.Lens.Operators

import           LLVM.General.AST

--------------------------------------------------------------------------------
-- Codegen Monad
--------------------------------------------------------------------------------

-- TODO: enforce (im)mutability
data LocalBinding  = LocalBinding  Type Name deriving (Show, Eq)
data GlobalBinding = GlobalBinding Type Name deriving (Show, Eq)
data AnyBinding = AL LocalBinding | AG GlobalBinding deriving (Show, Eq)

type LocalSymbolTable  = HashMap Text LocalBinding
type GlobalSymbolTable = HashMap Text GlobalBinding

type NameSupply = HashMap (Maybe Text) Word

data BlockgenState = BlockgenState
  { _blockName :: Name
  , _instrs    :: Seq (Named Instruction)
  , _term      :: Maybe (Named Terminator)
  }
  deriving (Show, Eq)

makeLenses ''BlockgenState

data ScopeInfo = ScopeInfo
  { _symbolTable :: LocalSymbolTable
  }
  deriving (Show, Eq)

makeLenses ''ScopeInfo

data LoopInfo = LoopInfo
  { _lpLabel :: Maybe Text
  , _lpBody  :: Name
  , _lpExit  :: Name
  }
  deriving (Show, Eq)

makeLenses ''LoopInfo

data CodegenState = CodegenState
  { _currentBlock      :: BlockgenState
  , _blocks            :: Seq BlockgenState
  , _scopeStack        :: [ScopeInfo]
  , _loopStack         :: [LoopInfo]
  , _outOfScopeSymbols :: Seq LocalBinding
  , _nameSupply        :: NameSupply
  , _stringConstants   :: Seq (Name,ByteString)
  }
  deriving (Show, Eq)

makeLenses ''CodegenState

data CodegenEnv = CodegenEnv
  { _globalsTable   :: GlobalSymbolTable
  , _functionName   :: Text
  , _argumentsTable :: LocalSymbolTable
  }
  deriving (Show, Eq)

makeLenses ''CodegenEnv

type CodegenError = String

newtype Codegen a = Codegen
  { runCodegen :: ExceptT CodegenError (StateT CodegenState (Reader CodegenEnv)) a
  }
  deriving
    (Functor, Applicative, Monad,
     MonadState CodegenState, MonadReader CodegenEnv, MonadError CodegenError)

emptyBlock :: Name -> BlockgenState
emptyBlock n = BlockgenState n empty Nothing

execCodegen :: GlobalSymbolTable -> Text -> LocalSymbolTable -> Codegen a -> CodegenState
execCodegen gbls tn args m = runIdentity
                           . (`runReaderT` CodegenEnv gbls tn args)
                           . (`execStateT` emptyCodegenState)
                           . runExceptT
                           . (`catchError` error) -- TODO: better error reporting
                           . runCodegen
                           $ do newArgs <- stackifyArgs
                                local (argumentsTable .~ newArgs) m
  where
    emptyCodegenState = CodegenState entryBlk empty empty empty empty emptyNameSupply empty
    entryBlk          = emptyBlock (UnName 0)
    emptyNameSupply   = singletonMap Nothing 1

finalize :: CodegenState -> ([BasicBlock],[(Name,ByteString)])
finalize cs = (finalBlocks,toList(cs^.stringConstants))
  where
    finalBlocks = toList . map mkBlock $ bs
    bs = genAllocas (cs^.outOfScopeSymbols) $ cs^.blocks |> cs^.currentBlock
    mkBlock BlockgenState {..} =
      BasicBlock _blockName
                 (toList _instrs)
                 (fromMaybe (error ("no terminator for block: " ++ show _blockName)) _term)

--------------------------------------------------------------------------------
-- State Manipulation Helpers
--------------------------------------------------------------------------------

uniqueName :: Maybe Text -> Codegen Name
uniqueName maybeString = mkName <$> mkId
  where
    mkId :: Codegen Word
    mkId = (nameSupply . at maybeString <<%= Just . (+1) . fromMaybe 0) <&> fromMaybe 0

    mkName :: Word -> Name
    mkName w = case maybeString of
      Nothing -> UnName w
      Just s  -> Name (unpack s ++ "." ++ show w)

currentBlockName :: Codegen Name
currentBlockName = use (currentBlock . blockName)

setTerm :: Named Terminator -> Codegen ()
setTerm t = currentBlock . term ?= t

addNewBlock :: Name -> Codegen ()
addNewBlock blkName =
  do
    c <- use currentBlock
    blocks |>= c
    currentBlock .= emptyBlock blkName

--------------------------------------------------------------------------------
-- Instructions
--------------------------------------------------------------------------------

instr :: Type -> Instruction -> Codegen Operand
instr ty i =
  do
    n <- uniqueName Nothing
    currentBlock . instrs |>= (n := i)
    return (LocalReference ty n)

phi :: Type -> [(Operand, Name)] -> Codegen Operand
phi ty inc = instr ty (Phi ty inc [])

ineg, fneg, iinv, bnot :: Operand -> Codegen Operand
ineg = isub (iconst 0)
fneg = fsub (fconst 0)
iinv = ixor (iconst (-1))
bnot = bxor (bconst True)

iadd, isub, imul, idiv, irem :: Operand -> Operand -> Codegen Operand
iadd op1 op2 = instr I64 (Add  False False op1 op2 [])
isub op1 op2 = instr I64 (Sub  False False op1 op2 [])
imul op1 op2 = instr I64 (Mul  False False op1 op2 [])
idiv op1 op2 = instr I64 (SDiv False       op1 op2 [])
irem op1 op2 = instr I64 (SRem             op1 op2 [])

fadd, fsub, fmul, fdiv, frem :: Operand -> Operand -> Codegen Operand
fadd op1 op2 = instr F64 (FAdd NoFastMathFlags op1 op2 [])
fsub op1 op2 = instr F64 (FSub NoFastMathFlags op1 op2 [])
fmul op1 op2 = instr F64 (FMul NoFastMathFlags op1 op2 [])
fdiv op1 op2 = instr F64 (FDiv NoFastMathFlags op1 op2 [])
frem op1 op2 = instr F64 (FRem NoFastMathFlags op1 op2 [])

band, bor, bxor :: Operand -> Operand -> Codegen Operand
band op1 op2 = instr I1 (And op1 op2 [])
bor  op1 op2 = instr I1 (Or  op1 op2 [])
bxor op1 op2 = instr I1 (Xor op1 op2 [])

iand, ior, ixor :: Operand -> Operand -> Codegen Operand
iand op1 op2 = instr I64 (And op1 op2 [])
ior  op1 op2 = instr I64 (Or  op1 op2 [])
ixor op1 op2 = instr I64 (Xor op1 op2 [])

ishl, ishr :: Operand -> Operand -> Codegen Operand
ishl op1 op2 = instr I64 ( Shl False False op1 op2 [])
ishr op1 op2 = instr I64 (AShr False       op1 op2 [])

icmp :: IntegerPredicate -> Operand -> Operand -> Codegen Operand
icmp ip op1 op2 = instr I1 (ICmp ip op1 op2 [])

fcmp :: FloatingPointPredicate -> Operand -> Operand -> Codegen Operand
fcmp fp op1 op2 = instr I1 (FCmp fp op1 op2 [])

load :: Type -> Operand -> Codegen Operand
load ty pt = instr ty (Load False pt Nothing 0 [])

store :: Operand -> Operand -> Codegen ()
store pt val = currentBlock . instrs |>= Do (Store False pt val Nothing 0 [])

getelemptr :: Type -> Operand -> Integer -> Codegen Operand
getelemptr ptTy arr idx = instr ptTy (GetElementPtr True arr [iconst 0, iconst idx] [])

call :: Type -> Operand -> [Operand] -> Codegen Operand
call ty func args = instr ty Call
  { tailCallKind = Nothing
  , callingConvention = CC.C
  , returnAttributes = []
  , function = Right func
  , arguments = fmap (,[]) args
  , functionAttributes = []
  , metadata = []
  }

--------------------------------------------------------------------------------
-- Symbol Table Helpers
--------------------------------------------------------------------------------

newScope :: Codegen ()
newScope = scopeStack <|= ScopeInfo mempty

declScopedVar :: Type -> Text -> Codegen LocalBinding
declScopedVar ty var =
  do
    nm <- uniqueName (Just var)
    let bd = LocalBinding ty nm
    table <- maybeThrow "no scope" $ preuse (scopeStack . _head . symbolTable)
    case lookup var table of
      Nothing -> scopeStack . _head . symbolTable . at var <?= bd
      Just _  -> throwError ("variable/parameter defined twice: " ++ unpack var)

lookupVar :: Text -> Codegen AnyBinding
lookupVar var =
  do
    stack <- use scopeStack
    args <- view argumentsTable
    gbls <- view globalsTable
    case (headMay . mapMaybe (lookup var . view symbolTable) $ stack) of
      Just b  -> return (AL b)
      Nothing -> case (lookup var args) of
        Just b  -> return (AL b)
        Nothing -> case (lookup var gbls) of
          Just b  -> return (AG b)
          Nothing -> throwError ("use of undeclared variable: " ++ unpack var)

endScope :: Codegen ()
endScope =
  do
    (scope,restScope) <- maybeThrow "no scope to close" $ preuse (scopeStack . _Cons)
    outOfScopeSymbols <>= (fromList . map snd . mapToList . view symbolTable $ scope)
    scopeStack .= restScope

genAllocas :: Seq LocalBinding -> Seq BlockgenState -> Seq BlockgenState
genAllocas bds = _head . instrs %~ (map genAlloca bds <>)
  where
    genAlloca (LocalBinding ty nm) = nm := (Alloca ty Nothing 0 [])

stackifyArgs :: Codegen LocalSymbolTable
stackifyArgs = view argumentsTable >>= mapM sfyArg
  where
    sfyArg (LocalBinding ty nm) =
      do
        newNm <- uniqueName Nothing
        currentBlock . instrs |>= (newNm := (Alloca ty Nothing 0 []))
        store (LocalReference (ptr ty) newNm) (LocalReference ty nm)
        return (LocalBinding ty newNm)

--------------------------------------------------------------------------------
-- Loop Helpers
--------------------------------------------------------------------------------

newLoop :: LoopInfo -> Codegen ()
newLoop li = loopStack <|= li

findLoop :: Maybe Text -> Codegen LoopInfo
findLoop Nothing = maybeThrow "no enclosing loop" $ preuse (loopStack . _head)
findLoop (Just lbl) = maybeThrow ("no enclosing loop with label " ++ unpack lbl)
                      $ find ((== Just lbl) . view lpLabel) <$> use loopStack

endLoop :: Codegen ()
endLoop = loopStack <~ (maybeThrow "no enclosing loop" $ preuse (loopStack . _tail))

--------------------------------------------------------------------------------
-- LLVM Utils
--------------------------------------------------------------------------------

pattern F64 = FloatingPointType 64 IEEE
pattern I64 = IntegerType 64
pattern I8  = IntegerType 8
pattern I1  = IntegerType 1

bindToOp :: AnyBinding -> Operand
bindToOp (AL (LocalBinding ty nm)) = LocalReference (ptr ty) nm
bindToOp (AG (GlobalBinding ty nm)) = ConstantOperand (C.GlobalReference (ptr ty) nm)

bindType :: AnyBinding -> Type
bindType (AL (LocalBinding ty _)) = ty
bindType (AG (GlobalBinding ty _)) = ty

opType :: Operand -> Type
opType (LocalReference ty _) = ty
opType (ConstantOperand c) = constType c
opType _ = error "metadata operands have no type"

constType :: C.Constant -> Type
constType (C.Int bits _) = IntegerType bits
constType (C.Float (SF.Half _)) = T.half
constType (C.Float (SF.Single _)) = T.float
constType (C.Float (SF.Double _)) = T.double
constType (C.Float (SF.Quadruple _ _)) = T.fp128
constType (C.Float (SF.X86_FP80 _ _)) = T.x86_fp80
constType (C.Float (SF.PPC_FP128 _ _)) = T.ppc_fp128
constType (C.Null ty) = ty
constType (C.Struct _ p es) = StructureType p (map constType es)
constType (C.Array mty es) = ArrayType (genericLength es) mty
constType (C.Vector es) = VectorType (genericLength es) (constType (headEx es))
constType (C.Undef ty) = ty
constType (C.BlockAddress _ _) = error "type of BlockAddress"
constType (C.GlobalReference ty _) = ptr ty
constType (C.GetElementPtr _ _ _) = error "type of GetElementPtr too hard"
constType (C.Trunc _ ty) = ty
constType (C.ZExt _ ty) = ty
constType (C.SExt _ ty) = ty
constType (C.FPToUI _ ty) = ty
constType (C.FPToSI _ ty) = ty
constType (C.UIToFP _ ty) = ty
constType (C.SIToFP _ ty) = ty
constType (C.FPTrunc _ ty) = ty
constType (C.FPExt _ ty) = ty
constType (C.PtrToInt _ ty) = ty
constType (C.IntToPtr _ ty) = ty
constType (C.BitCast _ ty) = ty
constType (C.AddrSpaceCast _ ty) = ty
constType (C.Select _ tv _) = constType tv
constType (C.ExtractElement v _) = let (VectorType _ e) = constType v in e
constType (C.InsertElement v _ _) = constType v
constType (C.ShuffleVector v1 _ m) =
  let (VectorType _ e, VectorType n _) = (constType v1, constType m) in VectorType n e
constType (C.ExtractValue _ _) = error "type of ExtractValue too hard"
constType (C.InsertValue a _ _) = constType a
constType x = constType (C.operand0 x)

bconst :: Bool -> Operand
bconst b = ConstantOperand (C.Int 1 (if b then 1 else 0))

iconst :: Integer -> Operand
iconst i = ConstantOperand (C.Int 64 i)

fconst :: Double -> Operand
fconst f = ConstantOperand (C.Float (SF.Double f))

strconst :: Text -> Codegen Operand
strconst str =
  do
    nm <- use stringConstants
    fnNm <- view functionName
    let name = Name (unpack fnNm ++ ".str" ++ show (Seq.length nm))
        bytes = encodeUtf8 str `snoc` 0
    stringConstants |>= (name,bytes)
    let b = AG (GlobalBinding (ArrayType (fromIntegral (length bytes)) I8) name)
    getelemptr (ptr I8) (bindToOp b) 0

--------------------------------------------------------------------------------
-- Misc. Utils
--------------------------------------------------------------------------------

infixl 4 |>=

(|>=) :: (MonadState s m, Snoc a a e e) => ASetter' s a -> e -> m ()
l |>= e = l %= (|> e)

infixl 4 <|=

(<|=) :: (MonadState s m, Cons a a e e) => ASetter' s a -> e -> m ()
l <|= e = l %= (e <|)

maybeThrow :: MonadError e m => e -> m (Maybe b) -> m b
maybeThrow e = (>>= maybe (throwError e) return)
