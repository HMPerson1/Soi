{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}

module Language.Soi.Internal.Emit where

import           ClassyPrelude

import           Control.Arrow                     ((+++))

import qualified LLVM.General.AST                  as L
import qualified LLVM.General.AST.Constant         as C
import qualified LLVM.General.AST.Global           as G
import qualified LLVM.General.AST.IntegerPredicate as IP
import qualified LLVM.General.AST.Linkage          as L
import           LLVM.General.AST.Type             hiding (void)

import           Language.Soi.Ast
import           Language.Soi.Internal.Codegen
import           Language.Soi.Internal.Prescan
import qualified Language.Soi.Internal.StdLib      as StdLib

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
    llBody = either emitRValueAndRet emitBlockAndRet body

emitRValueAndRet :: RValue -> Codegen ()
emitRValueAndRet rv =
  do
    llRv <- emitRValue rv
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
--emitStmt s | traceShow s False = error "impossible"

emitStmt (StVaBind (VaBinding Var (IdVar v) Nothing rv)) =
  do
    rvOp@(L.LocalReference ty _) <- emitRValue rv
    b <- declScopedVar ty v
    store (bindToOp (AL b)) rvOp

emitStmt (StAssign (LvVa (IdVar v)) rv) =
  do
    rvOp <- emitRValue rv
    b <- lookupVar v
    store (bindToOp b) rvOp

emitStmt (StIf (IfStmt {..})) =
  do
    condBlkNm <- uniqueName (Just "if.cond")
    thenBlkNm <- uniqueName (Just "if.then")
    elseBlkNm <- uniqueName (Just "if.else")
    endBlkNm <- uniqueName (Just "if.end")
    emitCondBlk condBlkNm ifsCond thenBlkNm (maybe endBlkNm (const elseBlkNm) ifsElse)
    emitNewBlock thenBlkNm (emitStmt ifsThen) endBlkNm
    maybe (return ()) (\b -> emitNewBlock elseBlkNm (emitStmt b) endBlkNm) ifsElse
    addNewBlock endBlkNm

emitStmt (StBlock sb) = emitBlock sb
emitStmt (StExpr rv) = void $ emitRValue rv

emitStmt _ = error "stmt not implemented"

emitCondBlk :: L.Name -> RValue -> L.Name -> L.Name -> Codegen ()
emitCondBlk condBlkNm cond trueBlkNm falseBlkNm =
  do
    setTerm (L.Do (L.Br condBlkNm []))
    addNewBlock condBlkNm
    condOp <- emitRValue cond
    setTerm (L.Do (L.CondBr condOp trueBlkNm falseBlkNm []))

emitNewBlock :: L.Name -> Codegen () -> L.Name -> Codegen ()
emitNewBlock blockNm stmts nextBlkNm =
  do
    addNewBlock blockNm
    stmts
    setTerm (L.Do (L.Br nextBlkNm []))

emitRValue :: RValue -> Codegen L.Operand

emitRValue (RvLVal (LvVa (IdVar v))) =
  do
    b <- lookupVar v
    load (bindType b) (bindToOp b)

emitRValue (RvCall (CallNormal (RvLVal (LvVa (IdVar f))) params)) =
  do
    b <- lookupVar f
    let (FunctionType rty _ _) = bindType b
    paramOps <- mapM emitRValue params
    call rty (bindToOp b) (toList paramOps)

emitRValue (RvBinOp bo v1 v2) =
  do
    v1Op <- emitRValue v1
    v2Op <- emitRValue v2
    (bo2lo bo) v1Op v2Op

emitRValue (RvLit (LitInt i)) = return (iconst i)
emitRValue (RvLit (LitDouble f)) = return (fconst f)
emitRValue (RvLit (LitString t)) = strconst t

emitRValue _ = error "rvalue not implemented"

bo2lo :: BinOp -> L.Operand -> L.Operand -> Codegen L.Operand
bo2lo (BoAo AoAdd) = iadd
bo2lo (BoAo AoSub) = isub
bo2lo (BoAo AoMul) = imul
bo2lo (BoAo AoDiv) = idiv
bo2lo (BoAo AoRem) = irem
bo2lo (BoCo co) = icmp ip
  where
    ip = case co of
      CoEQ -> IP.EQ
      CoNE -> IP.NE
      CoLT -> IP.SLT
      CoLE -> IP.SLE
      CoGT -> IP.SGT
      CoGE -> IP.SGE

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
emitVar gblTab tn ty rv = C.Undef ty -- TODO: UNDEFINED

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
