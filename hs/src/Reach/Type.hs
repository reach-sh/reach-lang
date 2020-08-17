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
import GHC.Stack (HasCallStack)
import Generics.Deriving
import Reach.AST
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
  deriving (Eq, Generic)

instance Show TypeError where
  show (Err_Type_Mismatch expected actual _val) =
    ("TypeError: Mismatch. Expected " <> show expected)
      <> ("but got" <> show actual)
  show (Err_Type_None _val) =
    "TypeError: Value cannot exist at runtime."
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
    DLA_Array t as -> T_Array t $ fromIntegral (length as)
    DLA_Tuple as -> T_Tuple $ map argTypeOf as
    DLA_Obj senv -> T_Obj $ M.map argTypeOf senv
    DLA_Interact _ _ t -> t

slToDL :: HasCallStack => SrcLoc -> SLVal -> Maybe DLArg
slToDL _at v =
  case v of
    SLV_Null _ _ -> return $ DLA_Con $ DLC_Null
    SLV_Bool _ b -> return $ DLA_Con $ DLC_Bool b
    SLV_Int _ i -> return $ DLA_Con $ DLC_Int i
    SLV_Bytes _ bs -> return $ DLA_Con $ DLC_Bytes bs
    SLV_Array at' t vs -> do
      ds <- mapM (slToDL at') vs
      return $ DLA_Array t ds
    SLV_Tuple at' vs -> do
      ds <- mapM (slToDL at') vs
      return $ DLA_Tuple $ ds
    SLV_Object at' fenv -> do
      denv <- mapM ((slToDL at') . snd) fenv
      return $ DLA_Obj denv
    SLV_Clo _ _ _ _ _ -> Nothing
    SLV_DLVar dv -> return $ DLA_Var dv
    SLV_Type _ -> Nothing
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

typeOfM :: HasCallStack => SrcLoc -> SLVal -> Maybe (SLType, DLArg)
typeOfM at v = do
  da <- slToDL at v
  return $ (argTypeOf da, da)

typeOf :: HasCallStack => SrcLoc -> SLVal -> (SLType, DLArg)
typeOf at v =
  case typeOfM at v of
    Just x -> x
    Nothing -> expect_throw at $ Err_Type_None v

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

checkType :: SrcLoc -> SLType -> SLVal -> DLArg
checkType at et v =
  case et == t of
    True -> da
    False -> expect_throw at $ Err_Type_Mismatch et t v
  where
    (t, da) = typeOf at v
