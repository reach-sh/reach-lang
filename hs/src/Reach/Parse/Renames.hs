module Reach.Parse.Renames
  ( Renames
  , RenameInfo (..)
  , ParseRenameErr (..)
  , exportClauseToRenames
  , importsNamedToRenames
  )
where

import Control.Monad.Except
import qualified Data.Map.Strict as M
import Language.JavaScript.Parser.AST
import Reach.AST
import Reach.JSUtil
import Reach.Parse.Ident

-- src as dst
data RenameInfo = RenameInfo
  { ri_srcAt :: SrcLoc
  , ri_src :: String
  , ri_dstAt :: SrcLoc
  }

type Renames = M.Map String RenameInfo

data ParseRenameErr
  = Err_ParseRename_Duplicate String SrcLoc SrcLoc -- ident origAt dupeAt
  | Err_ParseRename_JSIdentNone

identErrToRenameErr :: ParseIdentErr -> ParseRenameErr
identErrToRenameErr = \case
  Err_ParseIdent_JSIdentNone -> Err_ParseRename_JSIdentNone

exportClauseToRenames :: JSExportClause -> Except ParseRenameErr Renames
exportClauseToRenames (JSExportClause _ jsclExportSpecs _) =
  mapPairsToRenames exportSpecToIdentPair jsclExportSpecs

exportSpecToIdentPair :: JSExportSpecifier -> (JSIdent, JSIdent)
exportSpecToIdentPair = \case
  JSExportSpecifier ident -> (ident, ident)
  JSExportSpecifierAs srcIdent _as dstIdent -> (srcIdent, dstIdent)

importsNamedToRenames :: JSImportsNamed -> Except ParseRenameErr Renames
importsNamedToRenames (JSImportsNamed _ jsclImportSpecs _) =
  mapPairsToRenames importSpecToIdentPair jsclImportSpecs

importSpecToIdentPair :: JSImportSpecifier -> (JSIdent, JSIdent)
importSpecToIdentPair = \case
  JSImportSpecifier ident -> (ident, ident)
  JSImportSpecifierAs srcIdent _as destIdent -> (srcIdent, destIdent)

mapPairsToRenames :: (a -> (JSIdent, JSIdent)) -> JSCommaList a -> Except ParseRenameErr Renames
mapPairsToRenames f cl = do
  let l = jscl_flatten cl
  renameInfoListToRenames =<< mapM (identPairToRenameInfo . f) l

identPairToRenameInfo :: (JSIdent, JSIdent) -> Except ParseRenameErr (String, RenameInfo)
identPairToRenameInfo (srcIdent, dstIdent) = withExcept identErrToRenameErr $ do
  (ri_srcAt, ri_src) <- parseIdent srcIdent
  (ri_dstAt, dst) <- parseIdent dstIdent
  pure (dst, RenameInfo {..})

renameInfoListToRenames :: [(String, RenameInfo)] -> Except ParseRenameErr Renames
renameInfoListToRenames = go M.empty
  where
    go !m [] = pure m
    go !m ((k, ri) : rs) = case M.lookup k m of
      Just ri0 ->
        throwError $
          Err_ParseRename_Duplicate k (ri_dstAt ri0) (ri_dstAt ri)
      Nothing ->
        go (M.insert k ri m) rs
