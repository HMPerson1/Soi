module Main where

import           ClassyPrelude

import           Data.Either.Combinators          (mapLeft)
import           System.Exit                      (die)

import           Language.Soi.Internal.Lexer
import           Language.Soi.Internal.ParseMonad
import           Language.Soi.Parser

main :: IO ()
main =
  do
    input <- hGetContents stdin
    case parseFile "<stdin>" input of
      Right x -> print x
      Left  x -> die (show x)

lexTest :: LByteString -> IO ()
lexTest = putStrLn . unlines . either (:[]) (map tshow) . lexAll

lexAll :: LByteString -> Either Text [((Int,Int),TokenClass)]
lexAll txt = mapLeft (\(ParseError e) -> fromString e) $ runParser go "<stdin>" txt
  where
    go :: P [((Int,Int),TokenClass)]
    go =
      do
        Token (SrcLoc _ l c) tc <- lexToken
        case tc of
          TokEof -> return []
          _      -> fmap (((l,c),tc) :) go
