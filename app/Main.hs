{-# LANGUAGE OverloadedStrings #-}

module Main where

import           ClassyPrelude

import           Data.Either.Combinators    (unlessRight)
import           System.Directory           (removeFile)
import           System.Environment         (getProgName)
import           System.Exit                (die)
import           System.IO                  (openTempFile)
import           System.Process             (callCommand)

import qualified Control.Monad.LLVM         as L
import qualified LLVM.General.PassManager   as P
import qualified LLVM.General.Transforms    as T

import           Language.Soi.Internal.Emit
import           Language.Soi.Parser

main :: IO ()
main =
  do
    args <- getArgs
    (inFile,libFile) <- case args of
      [a]   -> return (unpack a, "lib-rt/soi_lib.ll")
      [a,b] -> return (unpack a, unpack b)
      _     -> do n <- getProgName; error ("usage: " ++ n ++ " FILE [LIB]")
    let root = case reverse inFile of
                     'i':'o':'s':'.':r -> reverse r
                     _                 -> error ("unknown file type: " ++ inFile)

    input <- readFile (unpack inFile)
    ast <- case parseFile inFile input of
      Right x -> return x
      Left  x -> die (show x)
    print ast
    let llvmIr = emit inFile ast
    -- print llvmIr
    (asmFile, asmH) <- openTempFile "" (root ++ ".s")
    hClose asmH

    res <- L.runL $ do
      c   <- L.context
      m   <- L.moduleFromAst c llvmIr
      lib <- L.moduleFromLLVMAssembly c (L.File libFile)
      tm  <- L.hostTargetMachine
      dl  <- L.getTargetMachineDataLayout tm
      tt  <- L.getDefaultTargetTriple
      li  <- L.getTargetLibraryInfo tt

      -- lift $ putStrLn "linking..."
      L.linkModules True m lib
      let
        ps1s = P.PassSetSpec
          { P.transforms = [T.InternalizeFunctions ["main"]]
          , P.dataLayout = Just dl
          , P.targetLibraryInfo = Just li
          , P.targetMachine = Just tm
          }
        ps2s = P.defaultCuratedPassSetSpec
          { P.optLevel = Just 3
          , P.useInlinerWithThreshold = Just 255
          , P.dataLayout = Just dl
          , P.targetLibraryInfo = Just li
          , P.targetMachine = Just tm
          }
      opt1r <- L.runPasses ps1s m
      opt2r <- L.runPasses ps2s m
      unless (opt1r && opt2r) . lift $ hPutStrLn stderr ("optimising failed"::Text)
      -- lift $ putStrLn "writing..."
      -- L.writeLLVMAssemblyToFile (L.File (root++".ll")) m
      L.writeTargetAssemblyToFile tm (L.File asmFile) m
    unlessRight res error

    callCommand ("cc " ++ asmFile ++ " -o " ++ root)
    removeFile asmFile

{-
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
-}
