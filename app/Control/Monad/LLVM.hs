module Control.Monad.LLVM
  ( L.File (..)
  , runL
  , context
  , moduleFromAst
  , moduleFromLLVMAssembly
  , hostTargetMachineWithPic
  , getTargetMachineDataLayout
  , getTargetLibraryInfo
  , getTargetMachineTriple
  , linkModules
  , runPasses
  , writeTargetAssemblyToFile
  , writeLLVMAssemblyToFile
  )
where

import           ClassyPrelude

import           Control.Monad.Cont
import           Data.ByteString.Short (ShortByteString)

import qualified LLVM.AST              as AST
import qualified LLVM.AST.DataLayout   as L
import qualified LLVM.Context          as L
import qualified LLVM.Module           as L
import qualified LLVM.PassManager      as L
import qualified LLVM.Target           as L

import qualified LLVM.CodeGenOpt       as CodeGenOpt
import qualified LLVM.CodeModel        as CodeModel
import qualified LLVM.Relocation       as Reloc

type L r a = ContT r IO a

runL :: L r r -> IO r
runL l = runContT l return

context :: L r L.Context
context = ContT L.withContext

moduleFromAst :: L.Context -> AST.Module -> ContT r IO L.Module
moduleFromAst c m = ContT $ L.withModuleFromAST c m

moduleFromLLVMAssembly :: L.Context -> L.File -> L r L.Module
moduleFromLLVMAssembly c f = ContT $ L.withModuleFromLLVMAssembly c f

-- copied from LLVM.Target.withHostTargetMachine
hostTargetMachineWithPic :: ContT a IO L.TargetMachine
hostTargetMachineWithPic = ContT $ \f -> do
  L.initializeAllTargets
  triple <- L.getProcessTargetTriple
  cpu <- L.getHostCPUName
  features <- L.getHostCPUFeatures
  (target, _) <- L.lookupTarget Nothing triple
  L.withTargetOptions $ \options ->
    L.withTargetMachine target triple cpu features options Reloc.PIC CodeModel.Default CodeGenOpt.Default f

getTargetMachineDataLayout :: L.TargetMachine -> L r L.DataLayout
getTargetMachineDataLayout tm = lift $ L.getTargetMachineDataLayout tm

getTargetLibraryInfo :: ShortByteString -> L r L.TargetLibraryInfo
getTargetLibraryInfo tt = ContT $ L.withTargetLibraryInfo tt

getTargetMachineTriple :: L.TargetMachine -> L r ShortByteString
getTargetMachineTriple = lift . L.getTargetMachineTriple

linkModules :: L.Module -> L.Module -> L () ()
linkModules m1 m2 = lift $ L.linkModules m1 m2

runPasses :: L.PassSetSpec -> L.Module -> L () Bool
runPasses pss m = lift $ L.withPassManager pss $ \pm -> L.runPassManager pm m

writeTargetAssemblyToFile :: L.TargetMachine -> L.File -> L.Module -> L () ()
writeTargetAssemblyToFile tm f m = lift $ L.writeTargetAssemblyToFile tm f m

writeLLVMAssemblyToFile :: L.File -> L.Module -> L () ()
writeLLVMAssemblyToFile f m = lift $ L.writeLLVMAssemblyToFile f m
