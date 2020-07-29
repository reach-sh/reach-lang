module Reach.NL_Type where

import Control.Monad.ST
import qualified Data.Map.Strict as M
import Data.STRef
import Reach.Util

import Reach.NL_AST

data TypeError
  = Err_Type_Mismatch SLType SLType SLVal
  | Err_Type_None SLVal
  | Err_Type_NotApplicable SLType
  | Err_TypeMeets_None
  | Err_TypeMeets_Mismatch SrcLoc (SrcLoc, SLType) (SrcLoc, SLType)
  | Err_Type_TooFewArguments [SLType]
  | Err_Type_TooManyArguments [SLVal]
  deriving (Eq, Show)

typeMeet :: SrcLoc -> (SrcLoc, SLType) -> (SrcLoc, SLType) -> SLType
typeMeet top_at x@(_, xt) y@(_, yt) =
  --- FIXME Find meet of objects
  if xt == yt
    then xt
    else expect_throw top_at $ Err_TypeMeets_Mismatch top_at x y

typeMeets :: SrcLoc -> [(SrcLoc, SLType)] -> SLType
typeMeets top_at l =
  case l of
    [] ->
      expect_throw top_at $ Err_TypeMeets_None
    [(_, xt)] -> xt
    [x, y] -> typeMeet top_at x y
    x : more -> typeMeet top_at x $ (top_at, typeMeets top_at more)

type TypeEnv s = M.Map SLVar (STRef s (Maybe SLType))

typeSubst :: SrcLoc -> TypeEnv s -> SLType -> ST s SLType
typeSubst at env ty =
  case ty of
    T_Fun doms rng -> do
      doms' <- mapM iter doms
      rng' <- typeSubst at env rng
      return $ T_Fun doms' rng'
    T_Array ts -> do
      ts' <- mapM iter ts
      return $ T_Array ts'
    T_Obj oenv -> do
      oenv' <- mapM iter oenv
      return $ T_Obj oenv'
    T_Var var ->
      case M.lookup var env of
        Nothing ->
          impossible $ "typeSubst: unbound type variable"
        Just var_ref -> do
          mvar <- readSTRef var_ref
          case mvar of
            Nothing ->
              impossible $ "typeSubst: uninstantiated type variable"
            Just var_ty ->
              iter var_ty
    T_Forall _ _ ->
      impossible $ "typeSubst: forall in output"
    _ -> return ty
  where
    iter = typeSubst at env

typeCheck_help :: SrcLoc -> TypeEnv s -> SLType -> SLVal -> SLType -> DLArg -> ST s DLArg
typeCheck_help at env ty val val_ty res =
  case (val_ty, ty) of
    (T_Var _, _) ->
      impossible $ "typeCheck: value has type var: " ++ show val
    (_, T_Var var) ->
      case M.lookup var env of
        Nothing ->
          impossible $ "typeCheck: unbound type variable"
        Just var_ref -> do
          mvar_ty <- readSTRef var_ref
          case mvar_ty of
            Nothing -> do
              writeSTRef var_ref (Just val_ty)
              return res
            Just var_ty ->
              typeCheck_help at env var_ty val val_ty res
    (_, _) ->
      case val_ty == ty of
        True -> return res
        False ->
          expect_throw at $ Err_Type_Mismatch ty val_ty val

typeOf :: SrcLoc -> SLVal -> (SLType, DLArg)
typeOf at v =
  case v of
    SLV_Null _ _ -> (T_Null, DLA_Con $ DLC_Null)
    SLV_Bool _ b -> (T_Bool, DLA_Con $ DLC_Bool b)
    SLV_Int _ i -> (T_UInt256, DLA_Con $ DLC_Int i)
    SLV_Bytes _ bs -> (T_Bytes, DLA_Con $ DLC_Bytes bs)
    SLV_Array at' vs -> (T_Array ts, DLA_Array das)
      where
        tdas = map (typeOf at') vs
        ts = map fst tdas
        das = map snd tdas
    SLV_Object at' fenv -> (T_Obj tenv, DLA_Obj aenv)
      where
        cenv = M.map (typeOf at' . snd) fenv
        tenv = M.map fst cenv
        aenv = M.map snd cenv
    SLV_Clo _ _ _ _ _ -> none
    SLV_DLVar dv@(DLVar _ _ t _) -> (t, DLA_Var dv)
    SLV_Type _ -> none
    SLV_Participant _ _ _ -> none --- XXX get the address
    SLV_Prim _ -> none --- XXX an interact may be non-function typed
    SLV_Form _ -> none
  where
    none = expect_throw at $ Err_Type_None v

typeCheck :: SrcLoc -> TypeEnv s -> SLType -> SLVal -> ST s DLArg
typeCheck at env ty val = typeCheck_help at env ty val val_ty res
  where
    (val_ty, res) = typeOf at val

typeChecks :: SrcLoc -> TypeEnv s -> [SLType] -> [SLVal] -> ST s [DLArg]
typeChecks at env ts vs =
  case (ts, vs) of
    ([], []) ->
      return []
    ((t : ts'), (v : vs')) -> do
      d <- typeCheck at env t v
      ds' <- typeChecks at env ts' vs'
      return $ d : ds'
    ((_ : _), _) ->
      expect_throw at $ Err_Type_TooFewArguments ts
    (_, (_ : _)) ->
      expect_throw at $ Err_Type_TooManyArguments vs

checkAndConvert_i :: SrcLoc -> TypeEnv s -> SLType -> [SLVal] -> ST s (SLType, [DLArg])
checkAndConvert_i at env t args =
  case t of
    T_Fun dom rng -> do
      dargs <- typeChecks at env dom args
      return (rng, dargs)
    T_Forall var ft -> do
      var_ref <- newSTRef Nothing
      let env' = M.insert var var_ref env
      (vrng, dargs) <- checkAndConvert_i at env' ft args
      rng <- typeSubst at env' vrng
      return (rng, dargs)
    _ -> expect_throw at $ Err_Type_NotApplicable t

checkAndConvert :: SrcLoc -> SLType -> [SLVal] -> (SLType, [DLArg])
checkAndConvert at t args = runST $ checkAndConvert_i at mempty t args
