module Reach.NL_Type where

import Control.Monad.ST
import qualified Data.Map.Strict as M
import Data.STRef
import Reach.NL_AST
import Reach.Util

--- FIXME implement a custom show that is useful
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

conTypeOf :: DLConstant -> SLType
conTypeOf c =
  case c of
    DLC_Null -> T_Null
    DLC_Bool _ -> T_Bool
    DLC_Int _ -> T_UInt256
    DLC_Bytes _ -> T_Bytes

argTypeOf :: DLArg -> SLType
argTypeOf d =
  case d of
    DLA_Var (DLVar _ _ t _) -> t
    DLA_Con c -> conTypeOf c
    DLA_Array as -> T_Array $ map argTypeOf as
    DLA_Obj senv -> T_Obj $ M.map argTypeOf senv
    DLA_Interact _ _ t -> t

slToDL :: SrcLoc -> SLVal -> DLArg
slToDL at v =
  case v of
    SLV_Null _ _ -> DLA_Con $ DLC_Null
    SLV_Bool _ b -> DLA_Con $ DLC_Bool b
    SLV_Int _ i -> DLA_Con $ DLC_Int i
    SLV_Bytes _ bs -> DLA_Con $ DLC_Bytes bs
    SLV_Array _ vs -> DLA_Array $ map (slToDL at) vs
    SLV_Object _ fenv ->
      DLA_Obj $ M.map ((slToDL at) . snd) fenv
    SLV_Clo _ _ _ _ _ -> none
    SLV_DLVar dv -> DLA_Var dv
    SLV_Type _ -> none
    SLV_Participant _ _ _ _ mdv ->
      case mdv of
        Nothing -> none
        Just dv -> DLA_Var dv
    SLV_Prim (SLPrim_interact _ who m t) ->
      case t of
        T_Var {} -> none
        T_Forall {} -> none
        T_Fun {} -> none
        _ -> DLA_Interact who m t
    SLV_Prim _ -> none
    SLV_Form _ -> none
  where
    none = expect_throw at $ Err_Type_None v

typeOf :: SrcLoc -> SLVal -> (SLType, DLArg)
typeOf at v = (t, da)
  where
    da = slToDL at v
    t = argTypeOf da

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
