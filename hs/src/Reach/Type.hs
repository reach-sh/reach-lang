module Reach.Type
  ( TypeError
  , typeMeet
  , typeMeets
  , checkAndConvert
  , argTypeOf
  , typeOf
  , typeOfM
  , checkType
  )
where

import Control.Monad.ST
import qualified Data.Map.Strict as M
import Data.STRef
import Data.Text.Prettyprint.Doc
import GHC.Stack (HasCallStack)
import Generics.Deriving
import Reach.AST
import Reach.Pretty ()
import Reach.Util

data TypeError
  = Err_Type_Mismatch SLType SLType SLVal
  | Err_Type_None SLVal
  | Err_Type_NotApplicable SLType
  | Err_TypeMeets_None
  | Err_TypeMeets_Mismatch SrcLoc (SrcLoc, SLType) (SrcLoc, SLType)
  | Err_Type_TooFewArguments [SLType]
  | Err_Type_TooManyArguments [SLVal]
  deriving (Eq, Generic)

instance Show TypeError where
  show (Err_Type_Mismatch expected actual _val) =
    ("TypeError: Mismatch. Expected " <> show expected)
      <> (" but got " <> show actual)
  show (Err_Type_None val) =
    "TypeError: Value cannot exist at runtime: " <> show (pretty val)
  show (Err_Type_NotApplicable ty) =
    "TypeError: NotApplicable. Cannot apply this like a function: " <> show ty
  show (Err_TypeMeets_None) =
    "TypeError: TypeMeets_None. Cannot find the meet of []"
  show (Err_TypeMeets_Mismatch _at t1 t2) =
    "TypeError: TypeMeets_Mismatch. These types are mismatched: "
      <> (show t1 <> " vs " <> show t2)
  show (Err_Type_TooFewArguments ts) =
    "TypeError: TooFewArguments. Expected: " <> show ts
  show (Err_Type_TooManyArguments vs) =
    "TypeError: TooManyArguments. Surplus: " <> show (length vs)

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
    T_Array t sz -> do
      t' <- iter t
      return $ T_Array t' sz
    T_Tuple ts -> do
      ts' <- mapM iter ts
      return $ T_Tuple ts'
    T_Object oenv -> do
      oenv' <- mapM iter oenv
      return $ T_Object oenv'
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
conTypeOf = \case
  DLC_Null -> T_Null
  DLC_Bool _ -> T_Bool
  DLC_Int _ -> T_UInt256
  DLC_Bytes _ -> T_Bytes

argTypeOf :: DLArg -> SLType
argTypeOf = \case
  DLA_Var (DLVar _ _ t _) -> t
  DLA_Con c -> conTypeOf c
  DLA_Array t as -> T_Array t $ fromIntegral (length as)
  DLA_Tuple as -> T_Tuple $ map argTypeOf as
  DLA_Obj senv -> T_Object $ M.map argTypeOf senv
  DLA_Data t _ _ -> T_Data t
  DLA_Interact _ _ t -> t

--- FIXME change this to give a reason?
slToDL :: HasCallStack => SLLimits -> SrcLoc -> SLVal -> Maybe DLArg
slToDL lims _at v =
  case v of
    SLV_Null _ _ -> return $ DLA_Con $ DLC_Null
    SLV_Bool _ b -> return $ DLA_Con $ DLC_Bool b
    SLV_Int _ i ->
      case 0 <= i && i <= lim_maxUInt lims of
        False -> Nothing
        True -> return $ DLA_Con $ DLC_Int i
    SLV_Bytes _ bs -> return $ DLA_Con $ DLC_Bytes bs
    SLV_Array at' t vs -> do
      ds <- mapM (slToDL lims at') vs
      return $ DLA_Array t ds
    SLV_Tuple at' vs -> do
      ds <- mapM (slToDL lims at') vs
      return $ DLA_Tuple $ ds
    SLV_Object at' _ fenv -> do
      denv <- mapM ((slToDL lims at') . sss_val) fenv
      return $ DLA_Obj denv
    SLV_Clo _ _ _ _ _ -> Nothing
    SLV_Data at' t vn sv ->
      DLA_Data t vn <$> slToDL lims at' sv
    SLV_DLVar dv -> return $ DLA_Var dv
    SLV_Type _ -> Nothing
    SLV_Connector _ -> Nothing
    SLV_Participant _ _ _ _ mdv ->
      case mdv of
        Nothing -> Nothing
        Just dv -> return $ DLA_Var dv
    SLV_Prim (SLPrim_interact _ who m t) ->
      case t of
        T_Var {} -> Nothing
        T_Forall {} -> Nothing
        T_Fun {} -> Nothing
        _ -> return $ DLA_Interact who m t
    SLV_Prim _ -> Nothing
    SLV_Form _ -> Nothing

typeOfM :: HasCallStack => SLLimits -> SrcLoc -> SLVal -> Maybe (SLType, DLArg)
typeOfM lims at v = do
  da <- slToDL lims at v
  return $ (argTypeOf da, da)

typeOf :: HasCallStack => SLLimits -> SrcLoc -> SLVal -> (SLType, DLArg)
typeOf lims at v =
  case typeOfM lims at v of
    Just x -> x
    Nothing -> expect_throw at $ Err_Type_None v

typeCheck :: SLLimits -> SrcLoc -> TypeEnv s -> SLType -> SLVal -> ST s DLArg
typeCheck lims at env ty val = typeCheck_help at env ty val val_ty res
  where
    (val_ty, res) = typeOf lims at val

typeChecks :: SLLimits -> SrcLoc -> TypeEnv s -> [SLType] -> [SLVal] -> ST s [DLArg]
typeChecks lims at env ts vs =
  case (ts, vs) of
    ([], []) ->
      return []
    ((t : ts'), (v : vs')) -> do
      d <- typeCheck lims at env t v
      ds' <- typeChecks lims at env ts' vs'
      return $ d : ds'
    ((_ : _), _) ->
      expect_throw at $ Err_Type_TooFewArguments ts
    (_, (_ : _)) ->
      expect_throw at $ Err_Type_TooManyArguments vs

checkAndConvert_i :: SLLimits -> SrcLoc -> TypeEnv s -> SLType -> [SLVal] -> ST s (SLType, [DLArg])
checkAndConvert_i lims at env t args =
  case t of
    T_Fun dom rng -> do
      dargs <- typeChecks lims at env dom args
      return (rng, dargs)
    T_Forall var ft -> do
      var_ref <- newSTRef Nothing
      let env' = M.insert var var_ref env
      (vrng, dargs) <- checkAndConvert_i lims at env' ft args
      rng <- typeSubst at env' vrng
      return (rng, dargs)
    _ -> expect_throw at $ Err_Type_NotApplicable t

checkAndConvert :: SLLimits -> SrcLoc -> SLType -> [SLVal] -> (SLType, [DLArg])
checkAndConvert lims at t args = runST $ checkAndConvert_i lims at mempty t args

checkType :: SLLimits -> SrcLoc -> SLType -> SLVal -> DLArg
checkType lims at et v =
  case et == t of
    True -> da
    False -> expect_throw at $ Err_Type_Mismatch et t v
  where
    (t, da) = typeOf lims at v
