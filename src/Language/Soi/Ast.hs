{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Soi.Ast where

import           ClassyPrelude

data File = File
  { fileDecls :: Seq TopLevelBinding
  }
  deriving (Show, Eq)

data TopLevelBinding
  = TlData   Data
  | TlImpl   Impl
  | TlFunc   Function
  | TlGlblVa VaDecl RValue
  deriving (Show, Eq)

data Data = Data
  { dataName   :: IdData
  , dataFields :: Seq VaDecl
  }
  deriving (Show, Eq)

data Impl = Impl
  { implName :: IdData
  , implFns  :: Seq Function
  }
  deriving (Show, Eq)

data Function = Function
  { fnName   :: IdVar
  , fnParams :: FnParams
  , fnReturn :: IdData
  , fnBody   :: FnBody
  }
  deriving (Show, Eq)
type FnBody = Either RValue StmtBlock

data FnParams = FnParams
  { fnpSelf :: Maybe Self
  , fnpRest :: Seq VaDecl
  }
  deriving (Show, Eq)

data VaDecl = VaDecl
  { vdlVOV  :: ValOrVar
  , vdlName :: IdVar
  , vdlType :: IdData
  }
  deriving (Show, Eq)

data RValue
  = RvLVal     LValue
  | RvBlock    ExprBlock
  | RvCall     Call
  | RvConstr   Constr
  | RvIf       IfExpr
  | RvBinOp    BinOp RValue RValue
  | RvUnOp     UnOp RValue
  | RvFieldAcc RValue IdVar
  | RvSelf     Self
  | RvLit      Literal
  deriving (Show, Eq)

data LValue
  = LvVa IdVar
  | LvFieldAcc (Either Self LValue) IdVar
  deriving (Show, Eq)

data ExprBlock = ExprBlock
  { eblkStmts  :: Seq Statement
  , eblkReturn :: RValue
  }
  deriving (Show, Eq)

data Statement
  = StVaBind      VaBinding
  | StAssign      LValue RValue
  | StArithAssign LValue ArithOp RValue
  | StLoop        (Maybe Label) Statement
  | StBreak       (Maybe Label)
  | StContinue    (Maybe Label)
  | StIf          IfStmt
  | StBlock       StmtBlock
  | StExpr        RValue
  deriving (Show, Eq)

data IfStmt = IfStmt
  { ifsCond :: RValue
  , ifsThen :: Statement
  , ifsElse :: Maybe Statement
  }
  deriving (Show, Eq)

data StmtBlock = StmtBlock
  { sblkStmts :: Seq Statement
  }
  deriving (Show, Eq)

data VaBinding = VaBinding
  { vbdVOV  :: ValOrVar
  , vbdName :: IdVar
  , vbdType :: Maybe IdData
  , vbdInit :: RValue
  }
  deriving (Show, Eq)

data Call
  = CallExplicit IdData IdVar (Seq RValue)
  | CallNormal RValue (Seq RValue)
  deriving (Show, Eq)

data Constr = Constr
  { ctrName   :: IdData
  , ctrFields :: Seq (IdVar,RValue)
  }
  deriving (Show, Eq)

data IfExpr = IfExpr
  { ifeCond :: RValue
  , ifeThen :: RValue
  , ifeElse :: RValue
  }
  deriving (Show, Eq)

data Literal
  = LitInt    Integer
  | LitDouble Double
  | LitString Text
  | LitUnit
  deriving (Show, Eq)

data UnOp
  = UoNeg
  | UoNot
  deriving (Show, Eq)

data BinOp
  = BoAo ArithOp
  | BoCo CmpOp
  deriving (Show, Eq)

data ArithOp
  = AoAdd
  | AoSub
  | AoMul
  | AoDiv
  | AoRem
  deriving (Show, Eq)

data CmpOp
  = CoEQ
  | CoNE
  | CoLT
  | CoLE
  | CoGT
  | CoGE
  deriving (Show, Eq)

data ValOrVar = Val | Var
  deriving (Show, Eq)

data Self = Self
  deriving (Show, Eq)

newtype Label = Label { unLabel :: Text }
  deriving (Show, Eq, Hashable)

newtype IdVar = IdVar { unIdVar :: Text }
  deriving (Show, Eq, Hashable)

newtype IdData = IdData { unIdData :: Text }
  deriving (Show, Eq, Hashable)
