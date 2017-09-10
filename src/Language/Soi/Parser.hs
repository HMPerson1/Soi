module Language.Soi.Parser
    ( parseFile
    ) where

import           ClassyPrelude

import           Language.Soi.Internal.ParseMonad
import           Language.Soi.Internal.Parser

parseFile :: String -> ByteString -> Either ParseError File
parseFile = runParser parseFileP

