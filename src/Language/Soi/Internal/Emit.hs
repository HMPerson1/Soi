{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}

module Language.Soi.Internal.Emit (emit) where

import           ClassyPrelude

import           Control.Arrow                   ((+++))
import           Control.Monad.Except            (throwError)
import qualified Data.ByteString.Short           as SBS

import qualified LLVM.AST                        as L
import qualified LLVM.AST.Constant               as C
import qualified LLVM.AST.FloatingPointPredicate as FPP
import qualified LLVM.AST.Global                 as G
import qualified LLVM.AST.IntegerPredicate       as IP
import qualified LLVM.AST.Linkage                as L

import qualified Language.Soi.Internal.StdLib    as StdLib

import           LLVM.AST.Type                   (ptr)

import           Language.Soi.Ast
import           Language.Soi.Internal.Codegen
import           Language.Soi.Internal.Prescan

-- Block refers to Soi AST blocks
-- LLBlock refers to LLVM IR BasicBlocks

emit :: String -> File -> L.Module
emit modName file = L.defaultModule
  { L.moduleName = SBS.toShort (encodeUtf8 (pack modName))
  , L.moduleDefinitions = StdLib.headers
                          ++ (map (convData . snd) . mapToList $ fileData)
                          ++ (concatMap (emitBind gblSymTab) . mapToList $ info)
  }
  where
    gblSymTab = unionWithKey (terror . ("can't redefine " ++)) fileSymTab StdLib.symbolTable
    fileSymTab = map bindInfoToBinding info
    info = gblBindsInfo fileBind
    (FileInfo {..}) = fileInfo file

bindInfoToBinding :: BindInfo -> GlobalBinding
bindInfoToBinding (n, argInfo) = GlobalBinding ty n
  where
    ty = case argInfo of
      Left (ret, argTab, _) -> L.FunctionType ret (map (\(_,LocalBinding bty _) -> bty) argTab) False
      Right (aty, _) -> aty

emitBind :: GlobalSymbolTable -> (Text,BindInfo) -> [L.Definition]
emitBind gblTab (tn,(n, Left (ret, argTab, body))) = fnDef:strsDef
  where
    fnDef =
      L.GlobalDefinition $ L.functionDefaults
        { G.returnType = ret
        , G.name = n
        , G.parameters = (map bindToLLParam argTab,False)
        , G.basicBlocks = bbs
        }
    bindToLLParam (_,LocalBinding ty bn) = L.Parameter ty bn []

    strsDef = map mkStrGlobal strs
    (bbs,strs) = emitFn gblTab ret tn (mapFromList argTab) body
    mkStrGlobal :: (L.Name,ByteString) -> L.Definition
    mkStrGlobal (sn,bytes) =
      L.GlobalDefinition $ L.globalVariableDefaults
        { G.name = sn
        , G.linkage = L.Private
        , G.unnamedAddr = Just G.GlobalAddr
        , G.isConstant = True
        , G.type' = L.ArrayType (fromIntegral (length bytes)) I8
        , G.initializer = Just (C.Array I8 (map (C.Int 8 . fromIntegral) (unpack bytes)))
        }

emitBind gblTab (tn,(n, Right (ty, rv))) = singleton $
  L.GlobalDefinition $ L.globalVariableDefaults
    { G.name = n
    , G.type' = ty
    , G.initializer = Just (emitVar gblTab tn ty rv)
    }

emitVar :: GlobalSymbolTable -> Text -> L.Type -> RValue -> C.Constant
emitVar _ _ ty _ = C.Undef ty -- TODO: UNDEFINED
-- emitVar gblTab tn ty rv = error "var not implemented"

emitFn :: GlobalSymbolTable -> L.Type -> Text -> LocalSymbolTable -> FnBody -> ([L.BasicBlock],[(L.Name,ByteString)])
emitFn gblTab retTy tn argTab body = finalize . execCodegen gblTab tn argTab $ llBody
  where
    llBody = case body of
      Left x  -> emitRValueAndRet retTy x
      Right x -> checkType retTy L.VoidType >> emitBlockAndRet x

emitRValueAndRet :: L.Type -> RValue -> Codegen ()
emitRValueAndRet retTy rv =
  do
    llRv <- emitRValue rv
    checkType (opType llRv) retTy
    setTerm (L.Do (L.Ret (Just llRv) []))

emitBlockAndRet :: StmtBlock -> Codegen ()
emitBlockAndRet (StmtBlock ss) =
  do
    emitBlock ss
    setTerm (L.Do (L.Ret Nothing []))

emitBlock :: Seq Statement -> Codegen ()
emitBlock ss =
  do
    newScope
    mapM_ emitStmt ss
    endScope

--------------------------------------------------------------------------------
-- Statements
--------------------------------------------------------------------------------

emitStmt :: Statement -> Codegen ()
-- emitStmt s | traceShow s False = error "impossible"

emitStmt (StVaBind (VaBinding Var (IdVar v) Nothing rv)) =
  do
    rvOp <- emitRValue rv
    b <- declScopedVar (opType rvOp) v
    store (bindToOp (AL b)) rvOp

emitStmt (StAssign (LvVa (IdVar v)) rv) =
  do
    rvOp <- emitRValue rv
    b <- lookupVar v
    store (bindToOp b) rvOp

emitStmt (StArithAssign lv ao rv) = emitStmt (StAssign lv (RvBinOp (Bao ao) (RvLVal lv) rv))

emitStmt (StLoop ml stmt) =
  do
    bodyLLBlkNm <- uniqueName (Just ("loop"++lbl++".body"))
    endLLBlkNm <- uniqueName (Just ("loop"++lbl++".end"))
    setTerm (L.Do (L.Br bodyLLBlkNm []))
    addNewBlock bodyLLBlkNm
    newLoop (LoopInfo mlbl bodyLLBlkNm endLLBlkNm)
    emitStmt stmt
    endLoop
    setTerm (L.Do (L.Br bodyLLBlkNm []))
    addNewBlock endLLBlkNm
  where
    mlbl = map unLabel ml
    lbl = fromMaybe mempty . map (cons '.') $ mlbl

emitStmt (StBreak ml) =
  do
    LoopInfo _ _ le <- findLoop (map unLabel ml)
    setTerm (L.Do (L.Br le []))
    -- prevent anything else going in to the current block
    addNewBlock =<< uniqueName Nothing

emitStmt (StContinue ml) =
  do
    LoopInfo _ lb _ <- findLoop (map unLabel ml)
    setTerm (L.Do (L.Br lb []))
    -- prevent anything else going in to the current block
    addNewBlock =<< uniqueName Nothing

emitStmt (StIf (IfStmt {..})) =
  do
    thenLLBlkNm <- uniqueName (Just "ifs.then")
    elseLLBlkNm <- uniqueName (Just "ifs.else")
    endLLBlkNm <- uniqueName (Just "if.end")
    emitLLTermCondBr (emitRValue ifsCond) thenLLBlkNm elseLLBlkNm
    emitNewLLBlock_ thenLLBlkNm (emitStmt ifsThen) endLLBlkNm
    emitNewLLBlock_ elseLLBlkNm (maybe (return ()) emitStmt ifsElse) endLLBlkNm
    addNewBlock endLLBlkNm

emitStmt (StBlock (StmtBlock ss)) = emitBlock ss
emitStmt (StExpr rv) = void $ emitRValue rv

emitStmt x = throwError ("stmt "++show x++" not implemented")

--------------------------------------------------------------------------------
-- RValues
--------------------------------------------------------------------------------

emitRValue :: RValue -> Codegen L.Operand
-- emitRValue s | traceShow s False = error "impossible"

emitRValue (RvLVal (LvVa (IdVar v))) =
  do
    b <- lookupVar v
    load (bindType b) (bindToOp b)

emitRValue (RvBlock (ExprBlock {..})) =
  do
    emitBlock eblkStmts
    emitRValue eblkReturn

emitRValue (RvCall (CallNormal (RvLVal (LvVa (IdVar f))) params)) =
  do
    b <- lookupVar f
    let (L.FunctionType rty argTys _) = bindType b
    paramOps <- toList <$> mapM emitRValue params
    sequence_ (zipWith checkType (map opType paramOps) argTys)
    call rty (bindToOp b) paramOps

emitRValue (RvIf (IfExpr {..})) =
  do
    thenLLBlkNm <- uniqueName (Just "ife.then")
    elseLLBlkNm <- uniqueName (Just "ife.else")
    endLLBlkNm <- uniqueName (Just "ife.end")
    emitLLTermCondBr (emitRValue ifeCond) thenLLBlkNm elseLLBlkNm
    thenOp <- emitNewLLBlock thenLLBlkNm (emitRValue ifeThen) endLLBlkNm
    elseOp <- emitNewLLBlock elseLLBlkNm (emitRValue ifeElse) endLLBlkNm
    checkType (opType thenOp) (opType elseOp)
    addNewBlock endLLBlkNm
    phi (opType thenOp) [(thenOp,thenLLBlkNm),(elseOp,elseLLBlkNm)]

emitRValue (RvBinOp (Bao (Bano bano)) v1 v2) =
  do
    v1Op <- emitRValue v1
    v2Op <- emitRValue v2
    case (opType v1Op, opType v2Op) of
      (I64,I64) -> bano2llio bano v1Op v2Op
      (F64,F64) -> bano2llfo bano v1Op v2Op
      _         -> throwError "numerical operation on non-numerical type"

emitRValue (RvBinOp (Bao (Babo babo)) v1 v2) =
  do
    v1Op <- emitRValue v1
    v2Op <- emitRValue v2
    case (opType v1Op, opType v2Op) of
      (I1 ,I1 ) -> babo2llbo babo v1Op v2Op
      (I64,I64) -> babo2llio babo v1Op v2Op
      _         -> throwError "bitwise operation on non-bit-like type"

emitRValue (RvBinOp (Bao (Baso baso)) v1 v2) =
  do
    v1Op <- emitRValue v1
    v2Op <- emitRValue v2
    case (opType v1Op, opType v2Op) of
      (I64,I64) -> baso2llio baso v1Op v2Op
      _         -> throwError "bitwise operation on non-integral type"

emitRValue (RvBinOp (Blo BloAnd) v1 v2) =
  do
    curLLBlkNm <- currentBlockName
    altLLBlkNm <- uniqueName (Just "and.alt")
    endLLBlkNm <- uniqueName (Just "and.end")
    emitLLTermCondBr (emitRValue v1) altLLBlkNm endLLBlkNm
    v2Op <- emitNewLLBlock altLLBlkNm (emitRValue v2) endLLBlkNm
    checkType I1 (opType v2Op)
    addNewBlock endLLBlkNm
    phi I1 [((bconst False),curLLBlkNm),(v2Op,altLLBlkNm)]

emitRValue (RvBinOp (Blo BloOr) v1 v2) =
  do
    curLLBlkNm <- currentBlockName
    altLLBlkNm <- uniqueName (Just "or.alt")
    endLLBlkNm <- uniqueName (Just "or.end")
    emitLLTermCondBr (emitRValue v1) endLLBlkNm altLLBlkNm
    v2Op <- emitNewLLBlock altLLBlkNm (emitRValue v2) endLLBlkNm
    checkType I1 (opType v2Op)
    addNewBlock endLLBlkNm
    phi I1 [((bconst True),curLLBlkNm),(v2Op,altLLBlkNm)]

emitRValue (RvBinOp (Bco (Bceo bceo)) v1 v2) =
  do
    v1Op <- emitRValue v1
    v2Op <- emitRValue v2
    case (opType v1Op, opType v2Op) of
      (I1 ,I1 ) -> icmp (bceo2llip bceo) v1Op v2Op
      (I64,I64) -> icmp (bceo2llip bceo) v1Op v2Op
      (F64,F64) -> fcmp (bceo2llfp bceo) v1Op v2Op
      _         -> throwError "equality operation on non-primitive type"

emitRValue (RvBinOp (Bco (Bcoo bcoo)) v1 v2) =
  do
    v1Op <- emitRValue v1
    v2Op <- emitRValue v2
    case (opType v1Op, opType v2Op) of
      (I64,I64) -> icmp (bcoo2llip bcoo) v1Op v2Op
      (F64,F64) -> fcmp (bcoo2llfp bcoo) v1Op v2Op
      _         -> throwError "numerical operation on non-numerical type"

emitRValue (RvUnOp UoPos v) =
  do
    vOp <- emitRValue v
    case opType vOp of
      I64 -> return vOp
      F64 -> return vOp
      _   -> throwError "numerical operation on non-numerical type"

emitRValue (RvUnOp UoNeg v) =
  do
    vOp <- emitRValue v
    case opType vOp of
      I64 -> ineg vOp
      F64 -> fneg vOp
      _   -> throwError "numerical operation on non-numerical type"

emitRValue (RvUnOp UoInv v) =
  do
    vOp <- emitRValue v
    case opType vOp of
      I64 -> iinv vOp
      _   -> throwError "bitwise operation on non-integral type"

emitRValue (RvUnOp UoNot v) =
  do
    vOp <- emitRValue v
    case opType vOp of
      I1 -> bnot vOp
      _  -> throwError "boolean operation on non-boolean type"

emitRValue (RvLit l) =
  case l of
    LitBool b   -> return (bconst b)
    LitInt i    -> return (iconst i)
    LitDouble f -> return (fconst f)
    LitString t -> strconst t
    LitUnit     -> return (L.ConstantOperand (C.Undef L.VoidType))

emitRValue x = throwError ("rvalue "++show x++" not implemented")

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

bano2llio :: BinArithNumOp -> L.Operand -> L.Operand -> Codegen L.Operand
bano2llio BanoAdd = iadd
bano2llio BanoSub = isub
bano2llio BanoMul = imul
bano2llio BanoDiv = idiv
bano2llio BanoRem = irem

bano2llfo :: BinArithNumOp -> L.Operand -> L.Operand -> Codegen L.Operand
bano2llfo BanoAdd = fadd
bano2llfo BanoSub = fsub
bano2llfo BanoMul = fmul
bano2llfo BanoDiv = fdiv
bano2llfo BanoRem = frem

babo2llbo :: BinArithBitOp -> L.Operand -> L.Operand -> Codegen L.Operand
babo2llbo BaboAnd = band
babo2llbo BaboOr  = bor
babo2llbo BaboXor = bxor

babo2llio :: BinArithBitOp -> L.Operand -> L.Operand -> Codegen L.Operand
babo2llio BaboAnd = iand
babo2llio BaboOr  = ior
babo2llio BaboXor = ixor

baso2llio :: BinArithShiftOp -> L.Operand -> L.Operand -> Codegen L.Operand
baso2llio BasoShl = ishl
baso2llio BasoShr = ishr

bceo2llip :: BinCmpEqOp -> IP.IntegerPredicate
bceo2llip BceoEQ = IP.EQ
bceo2llip BceoNE = IP.NE

bceo2llfp :: BinCmpEqOp -> FPP.FloatingPointPredicate
bceo2llfp BceoEQ = FPP.OEQ
bceo2llfp BceoNE = FPP.ONE

bcoo2llip :: BinCmpOrdOp -> IP.IntegerPredicate
bcoo2llip BcooLT = IP.SLT
bcoo2llip BcooLE = IP.SLE
bcoo2llip BcooGT = IP.SGT
bcoo2llip BcooGE = IP.SGE

bcoo2llfp :: BinCmpOrdOp -> FPP.FloatingPointPredicate
bcoo2llfp BcooLT = FPP.OLT
bcoo2llfp BcooLE = FPP.OLE
bcoo2llfp BcooGT = FPP.OGT
bcoo2llfp BcooGE = FPP.OGE

emitLLTermCondBr :: Codegen L.Operand -> L.Name -> L.Name -> Codegen ()
emitLLTermCondBr cond trueBlkNm falseBlkNm =
  do
    condOp <- cond
    checkType I1 (opType condOp)
    setTerm (L.Do (L.CondBr condOp trueBlkNm falseBlkNm []))

emitNewLLBlock :: L.Name -> Codegen a -> L.Name -> Codegen a
emitNewLLBlock blockNm code nextBlkNm =
  do
    addNewBlock blockNm
    r <- code
    setTerm (L.Do (L.Br nextBlkNm []))
    return r

emitNewLLBlock_ :: L.Name -> Codegen a -> L.Name -> Codegen ()
emitNewLLBlock_ blockNm code nextBlkNm =
  do
    addNewBlock blockNm
    void code
    setTerm (L.Do (L.Br nextBlkNm []))

type BindInfo = (L.Name, Either (L.Type, [(Text,LocalBinding)], FnBody) (L.Type, RValue))
gblBindsInfo :: HashMap IdVar AstBind -> HashMap Text BindInfo
gblBindsInfo = mapFromList . map conv . mapToList
  where
    conv :: (IdVar, AstBind) -> (Text, BindInfo)
    conv = unIdVar . fst &&& (mkTextName . unIdVar *** (infoFn +++ infoDecl))

    infoDecl :: (VaDecl, RValue) -> (L.Type, RValue)
    infoDecl = first (dataToType . vdlType)

    infoFn :: (Function, Maybe IdData) -> (L.Type, [(Text,LocalBinding)], FnBody)
    infoFn (Function {..}, maybeImplName) =
      let
        FnParams {..} = fnParams
        selfParam = case fnpSelf of
          Nothing -> []
          Just _  -> case maybeImplName of
            Just implName -> [("self",(dataToType implName,L.Name "self"))]
            Nothing       ->
              error ("found `self` parameter outside of `impl` in: " ++ unpack (unIdVar fnName))
        params = map (second (uncurry LocalBinding))
                 . mappend selfParam
                 . map (unIdVar . vdlName
                        &&& ((dataToType . vdlType) &&& (mkTextName . unIdVar . vdlName)))
                 . toList
                 $ fnpRest
      in
        (dataToType fnReturn, params, fnBody)

convData :: Data -> L.Definition
convData (Data {..}) = L.TypeDefinition llDataName (Just (L.StructureType False llFields))
  where
    llDataName = mkTextName (unIdData dataName)
    llFields = map (dataToType . vdlType) .toList $ dataFields

dataToType :: IdData -> L.Type
dataToType (IdData {..}) =
  if
    | unIdData == "Bool"   -> I1
    | unIdData == "Int"    -> I64
    | unIdData == "Double" -> F64
    | unIdData == "String" -> ptr I8
    | unIdData == "Unit"   -> L.VoidType
    | otherwise            -> L.NamedTypeReference (mkTextName unIdData)

checkType :: L.Type -> L.Type -> Codegen ()
checkType ty1 ty2 = when (ty1 /= ty2) (throwError ("("++show ty1++") /= ("++show ty2++")"))
