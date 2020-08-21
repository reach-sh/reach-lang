module Reach.Eval_Util
  ( ctxt_alloc
  , ctxt_lift_expr
  , ctxt_local_name
  , zipEq
  , env_insert
  , env_insert_
  , env_insertp
  , env_insertp_
  , env_merge
  , env_merge_
  , env_lookup
  , EnvInsertMode (..)
  , enforcePrivateUnderscore
  )
where

import Control.Monad.ST
import Data.List (foldl')
import qualified Data.Map as M
import GHC.Stack (HasCallStack)
import Reach.AST
import Reach.Eval_Types
import Reach.STCounter
import Reach.Util

ctxt_alloc :: SLCtxt s -> SrcLoc -> ST s Int
ctxt_alloc ctxt _at = do
  let idr = case ctxt_id ctxt of
        Just x -> x
        Nothing -> impossible $ "attempt to lift without id"
  incSTCounter idr

ctxt_lift_expr :: SLCtxt s -> SrcLoc -> (Int -> DLVar) -> DLExpr -> ST s (DLVar, DLStmts)
ctxt_lift_expr ctxt at mk_var e = do
  x <- ctxt_alloc ctxt at
  let dv = mk_var x
  let s = DLS_Let at dv e
  return (dv, return s)

ctxt_local_name :: SLCtxt s -> SLVar -> SLVar
ctxt_local_name ctxt def =
  case ctxt_local_mname ctxt of
    Nothing -> def
    Just [x] -> x ++ as
    Just xs -> "one of " ++ show xs ++ as
  where
    as = " (as " ++ def ++ ")"

zipEq :: Show e => SrcLoc -> (Int -> Int -> e) -> [a] -> [b] -> [(a, b)]
zipEq at ce x y =
  if lx == ly
    then zip x y
    else expect_throw at (ce lx ly)
  where
    lx = length x
    ly = length y

-- | Certain idents are special and bypass the public/private
-- enforced naming convention.
isSpecialIdent :: SLVar -> Bool
isSpecialIdent "interact" = True
isSpecialIdent "__decode_testing__" = True
isSpecialIdent _ = False

-- | Secret idents start with _, but are not _.
isSecretIdent :: SLVar -> Bool
isSecretIdent ('_' : _ : _) = True
isSecretIdent _ = False

data EnvInsertMode = AllowShadowing | DisallowShadowing

-- | The "_" never actually gets bound;
-- it is therefore only ident that may be "shadowed".
-- Secret idents must start with _.
-- Public idents must not start with _.
-- Special idents "interact" and "__decode_testing__" skip these rules.
env_insert_ :: HasCallStack => EnvInsertMode -> SrcLoc -> SLVar -> SLSVal -> SLEnv -> SLEnv
env_insert_ _ _ "_" _ env = env
env_insert_ insMode at k v env = case insMode of
  DisallowShadowing ->
    case M.lookup k env of
      Nothing -> go
      Just _ -> expect_throw at (Err_Shadowed k)
  AllowShadowing -> go
  where
    go = case v of
      -- Note: secret ident enforcement is limited to doOnly
      (Public, _)
        | not (isSpecialIdent k) && isSecretIdent k ->
          expect_throw at (Err_Eval_NotPublicIdent k)
      _ -> M.insert k v env

env_insert :: HasCallStack => SrcLoc -> SLVar -> SLSVal -> SLEnv -> SLEnv
env_insert = env_insert_ DisallowShadowing

env_insertp_ :: HasCallStack => EnvInsertMode -> SrcLoc -> SLEnv -> (SLVar, SLSVal) -> SLEnv
env_insertp_ imode at = flip (uncurry (env_insert_ imode at))

env_insertp :: HasCallStack => SrcLoc -> SLEnv -> (SLVar, SLSVal) -> SLEnv
env_insertp = env_insertp_ DisallowShadowing

env_merge_ :: HasCallStack => EnvInsertMode -> SrcLoc -> SLEnv -> SLEnv -> SLEnv
env_merge_ imode at left righte = foldl' (env_insertp_ imode at) left $ M.toList righte

env_merge :: HasCallStack => SrcLoc -> SLEnv -> SLEnv -> SLEnv
env_merge = env_merge_ DisallowShadowing

-- | The "_" ident may never be looked up.
env_lookup :: HasCallStack => SrcLoc -> SLVar -> SLEnv -> SLSVal
env_lookup at "_" _ = expect_throw at (Err_Eval_LookupUnderscore)
env_lookup at x env =
  case M.lookup x env of
    Just v -> v
    Nothing ->
      expect_throw at (Err_Eval_UnboundId x $ M.keys env)

-- | Make sure all bindings in this SLEnv respect the rule that
-- private vars must be named with a leading underscore.
enforcePrivateUnderscore :: forall m. Monad m => SrcLoc -> SLEnv -> m ()
enforcePrivateUnderscore at = mapM_ enf . M.toList
  where
    enf :: (SLVar, SLSVal) -> m ()
    enf (k, (secLev, _)) = case secLev of
      Secret
        | not (isSpecialIdent k)
            && not (isSecretIdent k) ->
          expect_throw at (Err_Eval_NotSecretIdent k)
      _ -> return ()
