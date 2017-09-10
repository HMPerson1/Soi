{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Soi.Internal.Prescan where

import           ClassyPrelude

import           Language.Soi.Ast (IdData (..), IdVar (..))
import qualified Language.Soi.Ast as A

-- TODO: The HashMaps are to ensure uniqueness in the names, but we should probably
--       just use HashSets instead.
data FileInfo = FileInfo
  { fileData :: HashMap IdData A.Data
  , fileBind :: HashMap IdVar  AstBind
  }
  deriving (Show, Eq)

type AstBind = Either (A.Function, Maybe IdData) (A.VaDecl, A.RValue)

fileInfo :: A.File -> FileInfo
fileInfo (A.File {..}) =
  FileInfo (foldr extractDatas mempty fileDecls) (foldr extractBinds mempty fileDecls)

extractDatas :: A.TopLevelBinding -> HashMap IdData A.Data -> HashMap IdData A.Data
extractDatas (A.TlData d@(A.Data {..})) = checkInsertMap dataName d
extractDatas _ = id

extractBinds :: A.TopLevelBinding -> HashMap IdVar AstBind -> HashMap IdVar AstBind
extractBinds (A.TlGlblVa d@(A.VaDecl {..}) value) = checkInsertMap vdlName (Right (d,value))
extractBinds (A.TlFunc f@(A.Function {..}))      = checkInsertMap fnName (Left (f,Nothing))
extractBinds (A.TlImpl (A.Impl {..}))            = flip (foldr insertImplFn) implFns
  where
    insertImplFn :: A.Function -> HashMap IdVar AstBind -> HashMap IdVar AstBind
    insertImplFn f@(A.Function {..}) = checkInsertMap (prefixImpl fnName) (Left (f, Just implName))

    prefixImpl :: IdVar -> IdVar
    prefixImpl (IdVar name) = IdVar ("_" ++ unIdData implName ++ "_" ++ name)

extractBinds _ = id

checkInsertMap :: forall map.
                  (Show (ContainerKey map), IsMap map)
               => ContainerKey map -> MapValue map -> map -> map
checkInsertMap k v = alterMap f k
  where
    f :: Maybe (MapValue map) -> Maybe (MapValue map)
    f Nothing  = Just v
    f (Just _) = error ("duplicate entry: " ++ show k)
