{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}

module Language.Soi.Internal.ParseMonad
    ( module Language.Soi.Ast
    , P
    , ParseError (..)
    , ParseState
    , getInput
    , setInput
    , getLexOpts
    , pushLexOpts
    , popLexOpts
    , AlexInput (..)
    , LexOpts (..)
    , SrcLoc (..)
    , runParser
    , lexFail
    , parseFail
    , advanceSrcLoc
    , Token (..)
    , TokenClass (..)
    ) where

import           ClassyPrelude

import           Data.Bits            (shiftL, shiftR)

import           Control.Monad.Except
import           Control.Monad.State

import           Language.Soi.Ast

newtype P a = P (StateT ParseState (Except ParseError) a)
  deriving (Functor, Applicative, Monad, MonadError ParseError, MonadState ParseState)

evalP :: P a -> ParseState -> Either ParseError a
evalP (P p) st = runExcept (evalStateT p st)

data ParseError = ParseError String
  deriving (Show)

data ParseState = ParseState
  { input       :: !AlexInput
  , lexOptStack :: ![LexOpts]
  }
  deriving (Show)

getInput :: P AlexInput
getInput = gets input

setInput :: AlexInput -> P ()
setInput inp = modify (\s->s{input=inp})

getLexOpts :: P LexOpts
getLexOpts = gets (traceShowId . maybe defaultLexOpts fst . uncons . lexOptStack)

pushLexOpts :: LexOpts -> P ()
pushLexOpts opts = modify (\(ParseState i optss) -> ParseState i (opts : optss))

popLexOpts :: P ()
popLexOpts = modify (\st@(ParseState i opts) ->
                       case opts of
                         (_:os) -> ParseState i os
                         []     -> st)

data AlexInput = AI
  { loc :: !SrcLoc
  , buf :: !LByteString
  , pos :: !Int64
  }
  deriving (Show)

data LexOpts = LexOpts
  { newlinesEnabled :: !Bool
  }
  deriving (Show)

defaultLexOpts :: LexOpts
defaultLexOpts = LexOpts False

data SrcLoc = SrcLoc
  { file :: !String
  , line :: !Int
  , col  :: !Int
  }
  deriving (Show, Eq)

runParser :: P a -> String -> LByteString -> Either ParseError a
runParser p name contents = evalP p startState
  where
    startState :: ParseState
    startState = ParseState startAi startLo
    startAi = AI (SrcLoc name 1 1) contents 0
    startLo = repeat (LexOpts False)

lexFail :: Maybe String -> P a
lexFail msg =
  do
    eloc <- gets (loc . input)
    throwError (ParseError
                (emsg eloc))
  where
    emsg (SrcLoc f l c) =
      "Lexical error at " ++ f ++ ":" ++ show l ++ ":" ++ show c ++ extra
    extra = maybe "" (": " ++) msg

parseFail :: Token -> P a
parseFail (Token (SrcLoc {..}) tok) =
  do
    throwError (ParseError
                ("Parse error near " ++
                 file ++ ":" ++ show line ++ ":" ++ show col ++
                 " at token: " ++ show tok))

-- copied from ghc (compiler/basicTypes/SrcLoc.hs)
-- note that this defines the tab size to be 8
advanceSrcLoc :: SrcLoc -> Char -> SrcLoc
advanceSrcLoc (SrcLoc f l _) '\n' = SrcLoc f (l + 1) 1
advanceSrcLoc (SrcLoc f l c) '\t' = SrcLoc f l (((((c-1) `shiftR` 3) + 1) `shiftL` 3) + 1)
advanceSrcLoc (SrcLoc f l c) _    = SrcLoc f l (c + 1)

data Token = Token SrcLoc TokenClass
  deriving (Show, Eq)

data TokenClass
  = TokData
  | TokImpl
  | TokDef
  | TokVal
  | TokVar
  | TokIf
  | TokThen
  | TokElse
  | TokLoop
  | TokBreak
  | TokContinue
  | TokSelf
  | TokSemi
  | TokDot
  | TokBang
  | TokAssign
  | TokBraceOpen
  | TokBraceClose
  | TokParenOpen
  | TokParenClose
  | TokComma
  | TokColon
  | TokDoubleColon
  | TokArithOp     ArithOp
  | TokArithAssign ArithOp
  | TokCmpOp       CmpOp
  | TokLitInt      Integer
  | TokLitDouble   Double
  | TokLitString   Text
  | TokIdData      IdData
  | TokIdVar       IdVar
  | TokLabel       Label
  | TokEof
  deriving (Show, Eq)
