module Reach.Eval.Module (evalLibs) where

import Control.Monad.Reader
import Data.Foldable
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Language.JavaScript.Parser
import Language.JavaScript.Parser.AST
import Reach.AST.Base
import Reach.AST.SL
import Reach.Connector
import Reach.Eval.Core
import Reach.Eval.Error
import Reach.JSUtil
import Reach.Parser
import Reach.Util
import Reach.Version

lookupDep :: ReachSource -> SLLibs -> App SLEnv
lookupDep rs libm =
  case M.lookup rs libm of
    Just x -> return x
    Nothing -> impossible $ "dependency not found"

evalFromClause :: SLLibs -> JSFromClause -> App SLEnv
evalFromClause libm (JSFromClause _ _ libn) =
  lookupDep (ReachSourceFile libn) libm

evalImExportSpecifiers :: LookupCtx -> SLEnv -> (a -> (JSIdent, JSIdent)) -> (JSCommaList a) -> App SLEnv
evalImExportSpecifiers lookupCtx env go cl = do
  Env {..} <- ask
  let p' (_, f) (_, t) = (,) t <$> env_lookup lookupCtx f env
  let p f t = p' (parseIdent e_at f) (parseIdent e_at t)
  l <- mapM (uncurry p) $ map go $ jscl_flatten cl
  foldlM env_insertp mempty l

evalImportClause :: SLEnv -> JSImportClause -> App SLEnv
evalImportClause env im =
  case im of
    JSImportClauseNameSpace (JSImportNameSpace _ _ ji) -> do
      (at', ns) <- withAt $ flip parseIdent ji
      return $ M.singleton ns $ (SLSSVal at' Public $ SLV_Object at' (Just $ "module " <> ns) env)
    JSImportClauseNamed (JSImportsNamed _ iscl _) ->
      evalImExportSpecifiers (LC_RefFrom "import") env go iscl
      where
        go = \case
          JSImportSpecifier x -> (x, x)
          JSImportSpecifierAs x _ y -> (x, y)
    JSImportClauseDefault {} -> illegal_import
    JSImportClauseDefaultNameSpace {} -> illegal_import
    JSImportClauseDefaultNamed {} -> illegal_import
  where
    illegal_import = expect_ $ Err_Import_IllegalJS im

evalExportClause :: SLEnv -> JSExportClause -> App SLEnv
evalExportClause env (JSExportClause _ escl _) =
  evalImExportSpecifiers (LC_RefFrom "export") env go escl
  where
    go = \case
      JSExportSpecifier x -> (x, x)
      JSExportSpecifierAs x _ y -> (x, y)

evalTopBody :: SLLibs -> SLEnv -> SLEnv -> [JSModuleItem] -> App SLEnv
evalTopBody libm env exenv = \case
  [] -> return $ exenv
  mi : body' ->
    case mi of
      (JSModuleImportDeclaration a im) ->
        locAtf (srcloc_jsa "import" a) $
          case im of
            JSImportDeclarationBare _ libn _ -> do
              libex <- lookupDep (ReachSourceFile libn) libm
              env' <- env_merge env libex
              evalTopBody libm env' exenv body'
            JSImportDeclaration ic fc _ -> do
              ienv <- evalFromClause libm fc
              news <- evalImportClause ienv ic
              env' <- env_merge env news
              evalTopBody libm env' exenv body'
      (JSModuleExportDeclaration a ex) ->
        locAtf (srcloc_jsa "export" a) $
          case ex of
            JSExport s _ -> doStmt True s
            JSExportFrom ec fc _ -> go ec =<< evalFromClause libm fc
            JSExportLocals ec _ -> go ec env
            JSExportAllFrom {} -> impossible "XXX export *"
        where
          go ec eenv = do
            news <- evalExportClause eenv ec
            env' <- env_merge exenv news
            evalTopBody libm env env' body'
      (JSModuleStatementListItem s) -> doStmt False s
    where
      doStmt isExport sm = do
        let sco =
              (SLScope
                 { sco_ret = Nothing
                 , sco_must_ret = RS_CannotReturn
                 , sco_while_vars = Nothing
                 , sco_penvs = mempty
                 , sco_cenv = env
                 })
        smr <- locSco sco $ evalStmt [sm]
        case smr of
          SLStmtRes sco' [] -> do
            let env' = sco_cenv sco'
            exenv' <- case isExport of
              -- If this is an exporting statement, then add to the export
              -- environment everything that is new.
              True -> env_merge exenv (M.difference env' env)
              False -> return $ exenv
            evalTopBody libm env' exenv' body'
          _ -> expect_ $ Err_Module_Return

evalLib :: Connectors -> SLMod -> SLLibs -> App SLLibs
evalLib cns (src, body) libm = do
  let at = srcloc_src src
  let base_env' =
        M.union base_env $
          M.mapKeys T.unpack $
            M.mapWithKey
              (\k _ -> SLSSVal srcloc_builtin Public $ SLV_Connector k)
              cns
  let stdlib_env =
        case src of
          ReachStdLib -> base_env'
          ReachSourceFile _ -> M.union (libm M.! ReachStdLib) base_env'
  let (prev_at, body') =
        case body of
          ((JSModuleStatementListItem (JSExpressionStatement (JSStringLiteral a hs) sp)) : j)
            | (trimQuotes hs) == versionHeader ->
              ((srcloc_after_semi "header" a sp at), j)
          _ -> expect_thrown at (Err_NoHeader body)
  exenv <- locAt prev_at $ evalTopBody libm stdlib_env mt_env body'
  return $ M.insert src exenv libm

evalLibs :: Connectors -> [SLMod] -> App SLLibs
evalLibs cns mods = foldrM (evalLib cns) mempty mods
