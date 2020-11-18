module Reach.Type
  ( TypeError
  , typeMeet
  , typeMeets
  , checkAndConvert
  , argTypeOf
  , typeOf
  , typeOfM
  , checkType
  , checkIntLiteral
  )
where

import Control.Monad.ST
import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as M
import Data.STRef
import GHC.Stack (HasCallStack)
import Generics.Deriving
import Reach.AST
import Reach.Pretty ()
import Reach.Texty
import Reach.Util

data TypeError
  = Err_Type_None SLVal
  | Err_Type_NotApplicable SLType
  | Err_TypeMeets_None
  | Err_TypeMeets_Mismatch SrcLoc (SrcLoc, SLType) (SrcLoc, SLType)
  | Err_Type_TooFewArguments [SLType]
  | Err_Type_TooManyArguments [SLVal]
  | Err_Type_IntLiteralRange Integer Integer Integer
  deriving (Eq, Generic)

instance Show TypeError where
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
  show (Err_Type_IntLiteralRange rmin x rmax) =
    "TypeError: int literal out of range: " <> show x <> " not in [" <> show rmin <> "," <> show rmax <> "]"

checkIntLiteral :: SrcLoc -> Integer -> Integer -> Integer -> Integer
checkIntLiteral at rmin x rmax =
  case rmin <= x && x <= rmax of
    True -> x
    False -> expect_throw at $ Err_Type_IntLiteralRange rmin x rmax

typeMeet :: SrcLoc -> (SrcLoc, SLType) -> (SrcLoc, SLType) -> SLType
typeMeet _ (_, T_Bytes xz) (_, T_Bytes yz) =
  T_Bytes (max xz yz)
typeMeet top_at x@(_, xt) y@(_, yt) =
  --- FIXME Find meet of objects
  case xt == yt of
    True -> xt
    False ->
      expect_throw top_at $ Err_TypeMeets_Mismatch top_at x y

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
      typeMeet at (at, val_ty) (at, ty) `seq` return res

conTypeOf :: DLConstant -> SLType
conTypeOf = \case
  DLC_UInt_max -> T_UInt

litTypeOf :: DLLiteral -> SLType
litTypeOf = \case
  DLL_Null -> T_Null
  DLL_Bool _ -> T_Bool
  DLL_Int {} -> T_UInt
  DLL_Bytes bs -> T_Bytes $ fromIntegral $ B.length bs

argTypeOf :: DLArg -> SLType
argTypeOf = \case
  DLA_Var (DLVar _ _ t _) -> t
  DLA_Constant c -> conTypeOf c
  DLA_Literal c -> litTypeOf c
  DLA_Array t as -> T_Array t $ fromIntegral (length as)
  DLA_Tuple as -> T_Tuple $ map argTypeOf as
  DLA_Obj senv -> T_Object $ M.map argTypeOf senv
  DLA_Data t _ _ -> T_Data t
  DLA_Interact _ _ t -> t

slToDL :: HasCallStack => SrcLoc -> SLVal -> Maybe DLArg
slToDL _at v =
  case v of
    SLV_Null _ _ -> return $ DLA_Literal $ DLL_Null
    SLV_Bool _ b -> return $ DLA_Literal $ DLL_Bool b
    SLV_Int at i -> return $ DLA_Literal $ DLL_Int at i
    SLV_Bytes _ bs -> return $ DLA_Literal $ DLL_Bytes bs
    SLV_Array at' t vs -> do
      ds <- mapM (slToDL at') vs
      return $ DLA_Array t ds
    SLV_Tuple at' vs -> do
      ds <- mapM (slToDL at') vs
      return $ DLA_Tuple $ ds
    SLV_Object at' _ fenv -> do
      denv <- mapM ((slToDL at') . sss_val) fenv
      return $ DLA_Obj denv
    SLV_Clo _ _ _ _ _ -> Nothing
    SLV_Data at' t vn sv ->
      DLA_Data t vn <$> slToDL at' sv
    SLV_DLC c -> return $ DLA_Constant c
    SLV_DLVar dv -> return $ DLA_Var dv
    SLV_Type _ -> Nothing
    SLV_Connector _ -> Nothing
    SLV_Participant _ _ _ mdv ->
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
    SLV_Kwd _ -> Nothing

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
  typeMeet at (at, et) (at, t) `seq` da
  where
    (t, da) = typeOf at v
