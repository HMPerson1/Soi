{-# LANGUAGE OverloadedStrings #-}

module Language.Soi.Internal.StdLib where

import           ClassyPrelude

import           LLVM.AST
import qualified LLVM.AST.Constant     as C
import qualified LLVM.AST.Global       as G
import           LLVM.AST.Type

import           Language.Soi.Internal.Codegen

headers :: [Definition]
headers =
  [ GlobalDefinition functionDefaults
    { G.returnType = i64
    , G.name = Name "readInt"
    , G.parameters = ([],False)
    }
  ,  GlobalDefinition functionDefaults
    { G.returnType = VoidType
    , G.name = Name "printStr"
    , G.parameters = ([Parameter (ptr i8) (Name "str") []],False)
    }
  ,  GlobalDefinition functionDefaults
    { G.returnType = VoidType
    , G.name = Name "printInt"
    , G.parameters = ([Parameter i64 (Name "i") []],False)
    }
  ]

readInt,   printStr,   printInt   :: Operand
readIntTy, printStrTy, printIntTy :: Type

readInt = ConstantOperand (C.GlobalReference readIntTy (Name "readInt"))
readIntTy = FunctionType i64 [] False

printStr = ConstantOperand (C.GlobalReference printStrTy (Name "printStr"))
printStrTy = FunctionType VoidType [ptr i8] False

printInt = ConstantOperand (C.GlobalReference printIntTy (Name "printInt"))
printIntTy = FunctionType VoidType [i64] False

symbolTable :: GlobalSymbolTable
symbolTable = mapFromList
  [ ("readInt", GlobalBinding readIntTy (Name "readInt"))
  , ("printStr", GlobalBinding printStrTy (Name "printStr"))
  , ("printInt", GlobalBinding printIntTy (Name "printInt"))
  ]
