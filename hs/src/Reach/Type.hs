module Reach.Type
  ( TypeError (..)
  , typeEqual
  , typeMeet
  , typeMeets
  , DLArgExpr (..)
  , checkAndConvert
  , argExprTypeOf
  , argTypeOf
  , largeArgTypeOf
  , typeOf
  , typeOfM
  , checkType
  , checkIntLiteral
  )
where

import Control.Applicative
import Control.Monad.ST
import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as M
import Data.STRef
import GHC.Stack (HasCallStack)
import Generics.Deriving
import Reach.AST.Base
import Reach.AST.DLBase
import Reach.AST.SL
import Reach.Pretty ()
import Reach.Texty
import Reach.Util

data TypeError
  = Err_Type_None SLVal
  | Err_Type_NotApplicable SLType
  | Err_TypeMeets_None
  | Err_TypeMeets_Mismatch SrcLoc (SrcLoc, DLType) (SrcLoc, DLType)
  | Err_Type_TooFewArguments [DLType]
  | Err_Type_TooManyArguments [SLVal]
  | Err_Type_IntLiteralRange Integer Integer Integer
  deriving (Eq, Generic, ErrorMessageForJson, ErrorSuggestions)

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

type MCFS = Maybe [SLCtxtFrame]

type PDVS = M.Map SLPart DLVar

type TINT = (MCFS, PDVS)

checkIntLiteral :: SrcLoc -> Integer -> Integer -> Integer -> Integer
checkIntLiteral at rmin x rmax =
  case rmin <= x && x <= rmax of
    True -> x
    False -> expect_thrown at $ Err_Type_IntLiteralRange rmin x rmax

typeEqual :: SrcLoc -> (SrcLoc, DLType) -> (SrcLoc, DLType) -> Either TypeError DLType
typeEqual top_at x@(_, xt) y@(_, yt) =
  case xt == yt of
    True -> Right xt
    False -> Left $ Err_TypeMeets_Mismatch top_at x y

typeMeet :: HasCallStack => MCFS -> SrcLoc -> (SrcLoc, DLType) -> (SrcLoc, DLType) -> DLType
typeMeet _ _ (_, T_Bytes xz) (_, T_Bytes yz) =
  T_Bytes (max xz yz)
typeMeet mcfs top_at x@(_, xt) y =
  --- FIXME Find meet of objects
  case typeEqual top_at x y of
    Right _ -> xt
    Left err -> expect_throw mcfs top_at err

typeMeets :: HasCallStack => MCFS -> SrcLoc -> [(SrcLoc, DLType)] -> DLType
typeMeets mcfs top_at l =
  case l of
    [] ->
      expect_throw mcfs top_at $ Err_TypeMeets_None
    [(_, xt)] -> xt
    [x, y] -> typeMeet mcfs top_at x y
    x : more -> typeMeet mcfs top_at x $ (top_at, typeMeets mcfs top_at more)

type TypeEnv s = M.Map SLVar (STRef s (Maybe SLType))

typeSubst :: SrcLoc -> TypeEnv s -> SLType -> ST s SLType
typeSubst at env ty =
  case ty of
    ST_Fun doms rng -> do
      doms' <- mapM iter doms
      rng' <- typeSubst at env rng
      return $ ST_Fun doms' rng'
    ST_Array t sz -> do
      t' <- iter t
      return $ ST_Array t' sz
    ST_Tuple ts -> do
      ts' <- mapM iter ts
      return $ ST_Tuple ts'
    ST_Object oenv -> do
      oenv' <- mapM iter oenv
      return $ ST_Object oenv'
    ST_Var var ->
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
    ST_Forall _ _ ->
      impossible $ "typeSubst: forall in output"
    _ -> return ty
  where
    iter = typeSubst at env

typeCheck_help :: MCFS -> SrcLoc -> TypeEnv s -> SLType -> SLVal -> DLType -> a -> ST s a
typeCheck_help mcfs at env ty val val_ty res =
  case (val_ty, ty) of
    -- XXX can't happen anymore
    -- (ST_Var _, _) ->
    --   impossible $ "typeCheck: value has type var: " ++ show val
    (_, ST_Var var) ->
      case M.lookup var env of
        Nothing ->
          impossible $ "typeCheck: unbound type variable"
        Just var_ref -> do
          mvar_ty <- readSTRef var_ref
          case mvar_ty of
            Nothing -> do
              writeSTRef var_ref (Just $ dt2st val_ty)
              return res
            Just var_ty ->
              typeCheck_help mcfs at env var_ty val val_ty res
    (_, _) ->
      typeMeet mcfs at (at, val_ty) (at, (st2dt ty)) `seq` return res

conTypeOf :: DLConstant -> DLType
conTypeOf = \case
  DLC_UInt_max -> T_UInt

litTypeOf :: DLLiteral -> DLType
litTypeOf = \case
  DLL_Null -> T_Null
  DLL_Bool _ -> T_Bool
  DLL_Int {} -> T_UInt
  DLL_Bytes bs -> T_Bytes $ fromIntegral $ B.length bs

argTypeOf :: DLArg -> DLType
argTypeOf = \case
  DLA_Var (DLVar _ _ t _) -> t
  DLA_Constant c -> conTypeOf c
  DLA_Literal c -> litTypeOf c
  DLA_Interact _ _ t -> t

largeArgTypeOf :: DLLargeArg -> DLType
largeArgTypeOf = \case
  DLLA_Array sz as -> argExprTypeOf $ DLAE_Array sz $ map DLAE_Arg as
  DLLA_Tuple as -> argExprTypeOf $ DLAE_Tuple $ map DLAE_Arg as
  DLLA_Obj m -> argExprTypeOf $ DLAE_Obj $ M.map DLAE_Arg m
  DLLA_Data m v a -> argExprTypeOf $ DLAE_Data m v $ DLAE_Arg a

data DLArgExpr
  = DLAE_Arg DLArg
  | DLAE_Array DLType [DLArgExpr]
  | DLAE_Tuple [DLArgExpr]
  | DLAE_Obj (M.Map SLVar DLArgExpr)
  | DLAE_Data (M.Map SLVar DLType) String DLArgExpr

argExprTypeOf :: DLArgExpr -> DLType
argExprTypeOf = \case
  DLAE_Arg a -> argTypeOf a
  DLAE_Array t as -> T_Array t $ fromIntegral (length as)
  DLAE_Tuple as -> T_Tuple $ map argExprTypeOf as
  DLAE_Obj senv -> T_Object $ M.map argExprTypeOf senv
  DLAE_Data t _ _ -> T_Data t

slToDL :: HasCallStack => PDVS -> SrcLoc -> SLVal -> Maybe DLArgExpr
slToDL pdvs _at v =
  case v of
    SLV_Null _ _ -> return $ DLAE_Arg $ DLA_Literal $ DLL_Null
    SLV_Bool _ b -> return $ DLAE_Arg $ DLA_Literal $ DLL_Bool b
    SLV_Int at i -> return $ DLAE_Arg $ DLA_Literal $ DLL_Int at i
    SLV_Bytes _ bs -> return $ DLAE_Arg $ DLA_Literal $ DLL_Bytes bs
    SLV_Array at' t vs -> do
      ds <- mapM (slToDL pdvs at') vs
      return $ DLAE_Array (st2dt t) ds
    SLV_Tuple at' vs -> do
      ds <- mapM (slToDL pdvs at') vs
      return $ DLAE_Tuple $ ds
    SLV_Object at' _ fenv -> do
      denv <- mapM ((slToDL pdvs at') . sss_val) fenv
      return $ DLAE_Obj denv
    SLV_Clo _ _ _ _ _ -> Nothing
    SLV_Data at' t vn sv ->
      DLAE_Data (M.map st2dt t) vn <$> slToDL pdvs at' sv
    SLV_DLC c -> return $ DLAE_Arg $ DLA_Constant c
    SLV_DLVar dv -> return $ DLAE_Arg $ DLA_Var dv
    SLV_Type _ -> Nothing
    SLV_Connector _ -> Nothing
    SLV_Participant _ who _ mdv ->
      case (M.lookup who pdvs) <|> mdv of
        Just dv -> return $ DLAE_Arg $ DLA_Var dv
        Nothing -> Nothing
    SLV_RaceParticipant {} -> Nothing
    SLV_Prim (SLPrim_interact _ who m t) ->
      case t of
        -- XXX can't happen anymore?
        -- ST_Var {} -> Nothing
        -- ST_Forall {} -> Nothing
        -- ST_Fun {} -> Nothing
        _ -> return $ DLAE_Arg $ DLA_Interact who m (st2dt t)
    SLV_Prim _ -> Nothing
    SLV_Form _ -> Nothing
    SLV_Kwd _ -> Nothing

typeOfM :: HasCallStack => PDVS -> SrcLoc -> SLVal -> Maybe (DLType, DLArgExpr)
typeOfM pdvs at v = do
  dae <- slToDL pdvs at v
  return $ (argExprTypeOf dae, dae)

typeOf :: HasCallStack => TINT -> SrcLoc -> SLVal -> (DLType, DLArgExpr)
typeOf (mcfs, pdvs) at v =
  case typeOfM pdvs at v of
    Just x -> x
    Nothing -> expect_throw mcfs at $ Err_Type_None v

typeCheck :: TINT -> SrcLoc -> TypeEnv s -> SLType -> SLVal -> ST s DLArgExpr
typeCheck tint@(mcfs, _) at env ty val = typeCheck_help mcfs at env ty val val_ty res
  where
    (val_ty, res) = typeOf tint at val

typeChecks :: TINT -> SrcLoc -> TypeEnv s -> [DLType] -> [SLVal] -> ST s [DLArgExpr]
typeChecks tint@(mcfs, _) at env ts vs =
  case (ts, vs) of
    ([], []) ->
      return []
    ((t : ts'), (v : vs')) -> do
      d <- typeCheck tint at env (dt2st t) v
      ds' <- typeChecks tint at env ts' vs'
      return $ d : ds'
    ((_ : _), _) ->
      expect_throw mcfs at $ Err_Type_TooFewArguments ts
    (_, (_ : _)) ->
      expect_throw mcfs at $ Err_Type_TooManyArguments vs

checkAndConvert_i :: TINT -> SrcLoc -> TypeEnv s -> SLType -> [SLVal] -> ST s (DLType, [DLArgExpr])
checkAndConvert_i tint@(mcfs, _) at env t args =
  case t of
    ST_Fun dom rng -> do
      dargs <- typeChecks tint at env (map st2dt dom) args
      return (st2dt rng, dargs)
    ST_Forall var ft -> do
      var_ref <- newSTRef Nothing
      let env' = M.insert var var_ref env
      (rng, dargs) <- checkAndConvert_i tint at env' ft args
      -- XXX happens prior to this point now?
      -- rng <- typeSubst at env' vrng
      return (rng, dargs)
    _ -> expect_throw mcfs at $ Err_Type_NotApplicable t

checkAndConvert :: TINT -> SrcLoc -> SLType -> [SLVal] -> (DLType, [DLArgExpr])
checkAndConvert tint at t args = runST $ checkAndConvert_i tint at mempty t args

checkType :: TINT -> SrcLoc -> DLType -> SLVal -> DLArgExpr
checkType tint@(mcfs, _) at et v =
  typeMeet mcfs at (at, et) (at, t) `seq` da
  where
    (t, da) = typeOf tint at v
