{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}

module Language.Soi.Internal.Codegen where

import           ClassyPrelude

import qualified Data.Sequence                           as Seq

import           Control.Lens                            (ASetter', Cons, Snoc,
                                                          at, makeLenses, use,
                                                          view)
import           Control.Lens.Cons                       (_Cons, _head)
import           Control.Lens.Fold                       (pre)
import           Control.Lens.Operators
import           Control.Monad.Reader                    (local)
import           Control.Monad.State                     (MonadState, StateT,
                                                          execStateT)

import qualified LLVM.General.AST.CallingConvention      as CC
import qualified LLVM.General.AST.Constant               as C
import qualified LLVM.General.AST.Float                  as SF
import           LLVM.General.AST.FloatingPointPredicate (FloatingPointPredicate)
import           LLVM.General.AST.IntegerPredicate       (IntegerPredicate)
import           LLVM.General.AST.Type                   (double, i1, i64, i8,
                                                          ptr)

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

data CodegenState = CodegenState
  { _currentBlock      :: BlockgenState
  , _blocks            :: Seq BlockgenState
  , _symbolTableStack  :: [LocalSymbolTable]
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

newtype Codegen a = Codegen { runCodegen :: StateT CodegenState (Reader CodegenEnv) a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState, MonadReader CodegenEnv)

emptyBlock :: Name -> BlockgenState
emptyBlock n = BlockgenState n empty Nothing

emptyCodegenState :: CodegenState
emptyCodegenState = CodegenState entryBlk empty empty empty emptyNameSupply empty
  where
    entryBlk         = emptyBlock (UnName 0)
    emptyNameSupply  = singletonMap Nothing 1

execCodegen :: GlobalSymbolTable -> Text -> LocalSymbolTable -> Codegen a -> CodegenState
execCodegen gbls tn args m = runIdentity
                           . (`runReaderT` env)
                           . (`execStateT` state)
                           . runCodegen
                           $ do newArgs <- stackifyArgs
                                local (argumentsTable .~ newArgs) m
  where
    state = emptyCodegenState
    env = CodegenEnv gbls tn args

finalize :: CodegenState -> ([BasicBlock],[(Name,ByteString)])
finalize cs = (finalBlocks,toList(cs^.stringConstants))
  where
    finalBlocks = toList . map mkBlock $ bs
    bs = cs^.blocks |> cs^.currentBlock & _head . instrs %~ (genAllocas (cs^.outOfScopeSymbols) <>)
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

iadd :: Operand -> Operand -> Codegen Operand
iadd op1 op2 = instr i64 (Add False False op1 op2 [])

isub :: Operand -> Operand -> Codegen Operand
isub op1 op2 = instr i64 (Sub False False op1 op2 [])

imul :: Operand -> Operand -> Codegen Operand
imul op1 op2 = instr i64 (Mul False False op1 op2 [])

idiv :: Operand -> Operand -> Codegen Operand
idiv op1 op2 = instr i64 (SDiv False op1 op2 [])

irem :: Operand -> Operand -> Codegen Operand
irem op1 op2 = instr i64 (SRem op1 op2 [])

ineg :: Operand -> Codegen Operand
ineg = isub (iconst 0)

icmp :: IntegerPredicate -> Operand -> Operand -> Codegen Operand
icmp ip op1 op2 = instr i1 (ICmp ip op1 op2 [])

fadd :: Operand -> Operand -> Codegen Operand
fadd op1 op2 = instr f64 (Add False False op1 op2 [])

fsub :: Operand -> Operand -> Codegen Operand
fsub op1 op2 = instr f64 (Sub False False op1 op2 [])

fmul :: Operand -> Operand -> Codegen Operand
fmul op1 op2 = instr f64 (Mul False False op1 op2 [])

fdiv :: Operand -> Operand -> Codegen Operand
fdiv op1 op2 = instr f64 (FDiv NoFastMathFlags op1 op2 [])

frem :: Operand -> Operand -> Codegen Operand
frem op1 op2 = instr f64 (FRem NoFastMathFlags op1 op2 [])

fneg :: Operand -> Codegen Operand
fneg = fsub (fconst 0)

fcmp :: FloatingPointPredicate -> Operand -> Operand -> Codegen Operand
fcmp fp op1 op2 = instr i1 (FCmp fp op1 op2 [])

load :: Type -> Operand -> Codegen Operand
load ty pt = instr ty (Load False pt Nothing 0 [])

store :: Operand -> Operand -> Codegen ()
store pt val = currentBlock . instrs |>= Do (Store False pt val Nothing 0 [])

getelemptr :: Type -> Operand -> Integer -> Codegen Operand
getelemptr ty arr idx = instr (ptr ty) (GetElementPtr True arr [iconst 0, iconst idx] [])

call :: Type -> Operand -> [Operand] -> Codegen Operand
call ty func args = instr ty Call
  { tailCallKind = Nothing
  , callingConvention = CC.C
  , returnAttributes = []
  , function = Right func
  , arguments = fmap (\x->(x, [])) args
  , functionAttributes = []
  , metadata = []
  }

--------------------------------------------------------------------------------
-- Symbol Table Helpers
--------------------------------------------------------------------------------

newScope :: Codegen ()
newScope = symbolTableStack <|= mempty

declScopedVar :: Type -> Text -> Codegen LocalBinding
declScopedVar ty var =
  do
    nm <- uniqueName (Just var)
    let bd = LocalBinding ty nm
    table <- fromMaybe (error "no scope") <$> use (symbolTableStack . pre _head)
    case lookup var table of
      Nothing -> symbolTableStack . _head . at var <?= bd
      Just _  -> terror ("variable/parameter defined twice: " ++ var)

lookupVar :: Text -> Codegen AnyBinding
lookupVar var =
  do
    scopeStack <- use symbolTableStack
    args <- view argumentsTable
    gbls <- view globalsTable
    return $ case (headMay . mapMaybe (lookup var) $ scopeStack) of
      Just b  -> AL b
      Nothing -> case (lookup var args) of
        Just b  -> AL b
        Nothing -> case (lookup var gbls) of
          Just b  -> AG b
          Nothing -> die
  where
    die = error ("use of undeclared variable: " ++ unpack var)

endScope :: Codegen ()
endScope =
  do
    (symTab,restSymTab) <- fromMaybe die <$> use (symbolTableStack . pre _Cons)
    outOfScopeSymbols <>= (fromList . map snd . mapToList $ symTab)
    symbolTableStack .= restSymTab
  where
    die = error "no scope to close"

genAllocas :: Seq LocalBinding -> Seq (Named Instruction)
genAllocas = map (\(LocalBinding ty nm) -> (nm := (Alloca ty Nothing 0 [])))

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
-- String Constants
--------------------------------------------------------------------------------

strconst :: Text -> Codegen Operand
strconst str =
  do
    nm <- use stringConstants
    fnNm <- view functionName
    let name = Name (unpack fnNm ++ ".str" ++ show (Seq.length nm))
        bytes = encodeUtf8 str `snoc` 0
    stringConstants |>= (name,bytes)
    let b = AG (GlobalBinding (ArrayType (fromIntegral (length bytes)) i8) name)
    getelemptr (ptr i8) (bindToOp b) 0

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

f64 :: Type
f64 = double

bindToOp :: AnyBinding -> Operand
bindToOp (AL (LocalBinding ty nm)) = LocalReference (ptr ty) nm
bindToOp (AG (GlobalBinding ty nm)) = ConstantOperand (C.GlobalReference (ptr ty) nm)

bindType :: AnyBinding -> Type
bindType (AL (LocalBinding ty _)) = ty
bindType (AG (GlobalBinding ty _)) = ty

iconst :: Integer -> Operand
iconst i = ConstantOperand (C.Int 64 i)

fconst :: Double -> Operand
fconst f = ConstantOperand (C.Float (SF.Double f))

infixl 4 |>=

(|>=) :: (MonadState s m, Snoc a a e e) => ASetter' s a -> e -> m ()
l |>= e = l %= (|> e)

infixl 4 <|=

(<|=) :: (MonadState s m, Cons a a e e) => ASetter' s a -> e -> m ()
l <|= e = l %= (e <|)
