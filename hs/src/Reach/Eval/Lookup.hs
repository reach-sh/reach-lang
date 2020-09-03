module Reach.Eval.Lookup
  ( SLLibs
  , lookupModuleEnv
  , lookupRenamesModuleEnv
  , lookupRenamesEnv
  , EvalLookupErr (..)
  )
where

import Control.Monad.Except
import qualified Data.Map.Strict as M
import Reach.AST
import Reach.Parse.Renames
import Prelude hiding (lookup)

type SLLibs = (M.Map ReachSource SLEnv)

data EvalLookupErr k
  = Err_EvalLookup_Missing String SrcLoc k [k] -- label, needle, haystack

lookup :: Ord k => String -> SrcLoc -> k -> M.Map k a -> Except (EvalLookupErr k) a
lookup label at k m = case M.lookup k m of
  Nothing -> throwError $ Err_EvalLookup_Missing label at k (M.keys m)
  Just a -> pure a

stringifyReachSourceErr :: EvalLookupErr ReachSource -> EvalLookupErr String
stringifyReachSourceErr = \case
  Err_EvalLookup_Missing label at k ks ->
    Err_EvalLookup_Missing label at (show k) (fmap show ks)

-- | Select down and rename identifiers from an SLEnv.
lookupRenamesEnv :: String -> Renames -> SLEnv -> Except (EvalLookupErr String) SLEnv
lookupRenamesEnv label renames env = traverse toSing renames
  where
    toSing ri = lookup label (ri_srcAt ri) (ri_src ri) env

-- | Get the SLEnv exported by a module
lookupModuleEnv :: SrcLoc -> ReachSource -> SLLibs -> Except (EvalLookupErr String) SLEnv
lookupModuleEnv at srcm libm = do
  withExcept stringifyReachSourceErr $ lookup "module" at srcm libm

-- | Get the SLEnv exported by a module,
-- with identifiers selected down and renamed.
lookupRenamesModuleEnv :: SrcLoc -> Renames -> ReachSource -> SLLibs -> Except (EvalLookupErr String) SLEnv
lookupRenamesModuleEnv at renames srcm libm = do
  libex <- lookupModuleEnv at srcm libm
  lookupRenamesEnv label renames libex
  where
    label = case srcm of
      ReachSourceFile srcStr -> "binding in module " <> srcStr
      ReachStdLib -> "reach standard library"
