{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}

module Language.Soi.Internal.Emit where

import           ClassyPrelude

import           Control.Arrow                           ((+++))
import           Control.Monad.Except                    (throwError)

import qualified LLVM.General.AST                        as L
import qualified LLVM.General.AST.Constant               as C
import qualified LLVM.General.AST.FloatingPointPredicate as FPP
import qualified LLVM.General.AST.Global                 as G
import qualified LLVM.General.AST.IntegerPredicate       as IP
import qualified LLVM.General.AST.Linkage                as L

import qualified Language.Soi.Internal.StdLib            as StdLib

import           LLVM.General.AST.Type                   hiding (void)

import           Language.Soi.Ast
import           Language.Soi.Internal.Codegen
import           Language.Soi.Internal.Prescan

emit :: String -> File -> L.Module
emit modName file = L.defaultModule
  { L.moduleName = modName
  , L.moduleDefinitions =
    StdLib.headers
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
        , G.hasUnnamedAddr = True
        , G.isConstant = True
        , G.type' = ArrayType (fromIntegral (length bytes)) i8
        , G.initializer = Just (C.Array i8 (map (C.Int 8 . fromIntegral) (unpack bytes)))
        }

emitBind gblTab (tn,(n, Right (ty, rv))) = singleton $
  L.GlobalDefinition $ L.globalVariableDefaults
    { G.name = n
    , G.type' = ty
    , G.initializer = Just (emitVar gblTab tn ty rv)
    }

emitFn :: GlobalSymbolTable -> L.Type -> Text -> LocalSymbolTable -> FnBody -> ([L.BasicBlock],[(L.Name,ByteString)])
emitFn gblTab retTy tn argTab body = finalize . execCodegen gblTab tn argTab $ llBody
  where
    llBody = case body of
      Left x -> emitRValueAndRet retTy x
      Right x -> checkType retTy VoidType >> emitBlockAndRet x

emitRValueAndRet :: L.Type -> RValue -> Codegen ()
emitRValueAndRet retTy rv =
  do
    llRv <- emitRValue rv
    checkType (opType llRv) retTy
    setTerm (L.Do (L.Ret (Just llRv) []))

emitBlockAndRet :: StmtBlock -> Codegen ()
emitBlockAndRet b =
  do
    emitBlock b
    setTerm (L.Do (L.Ret Nothing []))

emitBlock :: StmtBlock -> Codegen ()
emitBlock (StmtBlock ss) =
  do
    newScope
    mapM_ emitStmt ss
    endScope


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

emitStmt (StArithAssign lv ao rv) = emitStmt (StAssign lv (RvBinOp (BoAo ao) (RvLVal lv) rv))

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
    condLLBlkNm <- uniqueName (Just "if.cond")
    thenLLBlkNm <- uniqueName (Just "if.then")
    elseLLBlkNm <- uniqueName (Just "if.else")
    endLLBlkNm <- uniqueName (Just "if.end")
    emitCondLLBlock condLLBlkNm ifsCond thenLLBlkNm (maybe endLLBlkNm (const elseLLBlkNm) ifsElse)
    emitNewLLBlock thenLLBlkNm (emitStmt ifsThen) endLLBlkNm
    maybe (return ()) (\b -> emitNewLLBlock elseLLBlkNm (emitStmt b) endLLBlkNm) ifsElse
    addNewBlock endLLBlkNm

emitStmt (StBlock sb) = emitBlock sb
emitStmt (StExpr rv) = void $ emitRValue rv

emitStmt _ = error "stmt not implemented"

emitCondLLBlock :: L.Name -> RValue -> L.Name -> L.Name -> Codegen ()
emitCondLLBlock condBlkNm cond trueBlkNm falseBlkNm =
  do
    setTerm (L.Do (L.Br condBlkNm []))
    addNewBlock condBlkNm
    condOp <- emitRValue cond
    checkType i1 (opType condOp)
    setTerm (L.Do (L.CondBr condOp trueBlkNm falseBlkNm []))

emitNewLLBlock :: L.Name -> Codegen () -> L.Name -> Codegen ()
emitNewLLBlock blockNm stmts nextBlkNm =
  do
    addNewBlock blockNm
    stmts
    setTerm (L.Do (L.Br nextBlkNm []))


emitRValue :: RValue -> Codegen L.Operand
-- emitRValue s | traceShow s False = error "impossible"

emitRValue (RvLVal (LvVa (IdVar v))) =
  do
    b <- lookupVar v
    load (bindType b) (bindToOp b)

emitRValue (RvCall (CallNormal (RvLVal (LvVa (IdVar f))) params)) =
  do
    b <- lookupVar f
    let (FunctionType rty argTys _) = bindType b
    paramOps <- toList <$> mapM emitRValue params
    sequence_ (zipWith checkType (map opType paramOps) argTys)
    call rty (bindToOp b) paramOps

emitRValue (RvBinOp bo v1 v2) =
  do
    v1Op <- emitRValue v1
    v2Op <- emitRValue v2
    let llo = case (opType v1Op, opType v2Op) of
          (IntegerType 64, IntegerType 64) -> bo2illo
          (FloatingPointType 64 IEEE, FloatingPointType 64 IEEE) -> bo2fllo
          _ -> error "numerical operation on non-numerical type"
    llo bo v1Op v2Op

emitRValue (RvLit l) =
  case l of
    LitInt i    -> return (iconst i)
    LitDouble f -> return (fconst f)
    LitString t -> strconst t
    LitUnit     -> return (L.ConstantOperand (C.Undef VoidType))

emitRValue _ = error "rvalue not implemented"

bo2illo :: BinOp -> L.Operand -> L.Operand -> Codegen L.Operand
bo2illo (BoAo AoAdd) = iadd
bo2illo (BoAo AoSub) = isub
bo2illo (BoAo AoMul) = imul
bo2illo (BoAo AoDiv) = idiv
bo2illo (BoAo AoRem) = irem
bo2illo (BoCo co) = icmp ip
  where
    ip = case co of
      CoEQ -> IP.EQ
      CoNE -> IP.NE
      CoLT -> IP.SLT
      CoLE -> IP.SLE
      CoGT -> IP.SGT
      CoGE -> IP.SGE

bo2fllo :: BinOp -> L.Operand -> L.Operand -> Codegen L.Operand
bo2fllo (BoAo AoAdd) = fadd
bo2fllo (BoAo AoSub) = fsub
bo2fllo (BoAo AoMul) = fmul
bo2fllo (BoAo AoDiv) = fdiv
bo2fllo (BoAo AoRem) = frem
bo2fllo (BoCo co) = fcmp fp
  where
    fp = case co of
      CoEQ -> FPP.OEQ
      CoNE -> FPP.ONE
      CoLT -> FPP.OLT
      CoLE -> FPP.OLE
      CoGT -> FPP.OGT
      CoGE -> FPP.OGE

{-
  str <- strconst "hi"
  _ <- call VoidType StdLib.printStr [str]
  o <- call i64 StdLib.readInt []
  _ <- call VoidType StdLib.printStr [str]
  foo <- lookupVar "foo"
  a <- call i64 (bindToOp foo) [o]
  _ <- call VoidType StdLib.printInt [a]
  setTerm (L.Do (L.Ret (Just (iconst 0)) []))
  return ()
 -}

emitVar :: GlobalSymbolTable -> Text -> L.Type -> RValue -> C.Constant
emitVar _ _ ty _ = C.Undef ty -- TODO: UNDEFINED
-- emitVar gblTab tn ty rv = error "var not implemented"

type BindInfo = (L.Name, Either (L.Type, [(Text,LocalBinding)], FnBody) (L.Type, RValue))
gblBindsInfo :: HashMap IdVar AstBind -> HashMap Text BindInfo
gblBindsInfo = mapFromList . map conv . mapToList
  where
    conv :: (IdVar, AstBind) -> (Text, BindInfo)
    conv = unIdVar . fst &&& (L.Name . unpack . unIdVar *** (infoFn +++ infoDecl))

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
      in
        ( (dataToType fnReturn)
        , ( map (second (uncurry (LocalBinding)))
          . mappend selfParam
          . map (unIdVar . vdlName
                 &&& ((dataToType . vdlType) &&& (L.Name . unpack . unIdVar . vdlName)))
          . toList
          $ fnpRest)
        , fnBody
        )

convData :: Data -> L.Definition
convData (Data {..}) = L.TypeDefinition llDataName (Just (L.StructureType False llFields))
  where
    llDataName = L.Name (unpack (unIdData dataName))
    llFields = map (dataToType . vdlType) .toList $ dataFields

dataToType :: IdData -> L.Type
dataToType (IdData {..}) =
  if
    | unIdData == "Int"    -> i64
    | unIdData == "Double" -> f64
    | unIdData == "String" -> ptr i8
    | unIdData == "Unit"   -> VoidType
    | otherwise            -> NamedTypeReference (L.Name (unpack unIdData))

checkType :: L.Type -> L.Type -> Codegen ()
checkType ty1 ty2 = when (ty1 /= ty2) (throwError ("("++show ty1++") /= ("++show ty2++")"))
