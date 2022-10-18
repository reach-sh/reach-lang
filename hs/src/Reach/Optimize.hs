module Reach.Optimize (optimize_, optimize, opt_sim, Optimize) where

import Control.Monad.Reader
import qualified Data.Aeson as AS
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import Reach.AST.Base
import Reach.AST.DLBase
import Reach.AST.LL
import Reach.AST.PL
import Reach.AST.EP
import Reach.AST.CP
import Reach.Counter
import Reach.Sanitize
import Reach.UnrollLoops
import Reach.Util
import Safe (atMay)
import qualified Data.ByteString as B
import Reach.CollectSvs
import Reach.AST.SL

type App = ReaderT Env IO

type ConstApp = ReaderT ConstEnv IO

type AppT a = a -> App a

type ConstT a = a -> ConstApp ()

class Optimize a where
  opt :: AppT a
  gcs :: ConstT a

data Focus
  = F_Ctor
  | F_All
  | F_One SLPart
  | F_Consensus
  deriving (Eq, Ord, Show)

type RepeatedT = Either DLArg DLLargeArg

data CommonEnv = CommonEnv
  { ceReplaced :: M.Map DLVar (DLVar, Maybe DLArg)
  , cePrev :: M.Map DLExpr RepeatedT
  , ceNots :: M.Map DLVar DLArg
  , ceKnownLargeArgs :: M.Map DLVar DLLargeArg
  , ceExpr :: M.Map DLVar DLExpr
  }

instance Show CommonEnv where
  show (CommonEnv {..}) = show ceNots

instance Semigroup CommonEnv where
  x <> y =
    CommonEnv
      { ceReplaced = g ceReplaced
      , cePrev = g cePrev
      , ceNots = g ceNots
      , ceKnownLargeArgs = g ceKnownLargeArgs
      , ceExpr = g ceExpr
      }
    where
      g f = f x <> f y

instance Monoid CommonEnv where
  mempty = CommonEnv mempty mempty mempty mempty mempty

type FuseEnv = M.Map DLVar (SwitchCases DLTail)

data Env = Env
  { eFocus :: Focus
  , eParts :: [SLPart]
  , eEnvsR :: IORef (M.Map Focus CommonEnv)
  , eCounter :: Counter
  , eDroppedAsserts :: Counter
  , eConst :: S.Set DLVar
  , eMaps :: DLMapInfos
  , eClearMaps :: Bool
  , eSimulate :: Bool
  , eSvs :: S.Set DLVar
  , eVarsSet ::IORef (M.Map DLVar Int)
  , eFusionCases :: IORef FuseEnv
  }

mkVar :: SrcLoc -> DLType -> App DLVar
mkVar at t = do
  ctr <- asks eCounter
  idx <- liftIO $ incCounter ctr
  return $ DLVar at Nothing t idx

updateVarSet :: DLVar -> (Maybe Int -> Maybe Int) -> App (Maybe Int)
updateVarSet dv update = do
  eVarsSetR <- asks eVarsSet
  eVarsSet <- (liftIO . readIORef) eVarsSetR
  let mVal = update $ M.lookup dv eVarsSet
  liftIO . modifyIORef' eVarsSetR $ flip M.alter dv $ const mVal
  return $ mVal

readVarSet :: DLVar -> App (Maybe Int)
readVarSet dv = updateVarSet dv id

updateClearMaps :: Bool -> Env -> Env
updateClearMaps b e = e { eClearMaps = b }

getMapTy :: DLMVar -> App (Maybe DLType)
getMapTy mpv = do
  ms <- asks eMaps
  return $ fmap dlmi_ty $ M.lookup mpv ms

data ConstEnv = ConstEnv
  { eConstR :: IORef (M.Map DLVar Integer)
  }

focus :: Focus -> App a -> App a
focus f = local (\e -> e {eFocus = f})

focus_ctor :: App a -> App a
focus_ctor = focus F_Ctor

focus_all :: App a -> App a
focus_all = focus F_All

focus_one :: SLPart -> App a -> App a
focus_one = focus . F_One

focus_con :: App a -> App a
focus_con = focus F_Consensus

optConst :: DLVar -> App Bool
optConst v = S.member v <$> asks eConst

newScope :: App x -> App x
newScope m = do
  Env {..} <- ask
  eEnvsR' <- liftIO $ dupeIORef eEnvsR
  eVarsSet' <- liftIO $ dupeIORef eVarsSet
  eFusionCases' <- liftIO $ dupeIORef eFusionCases
  local (\e -> e { eEnvsR = eEnvsR'
                 , eVarsSet = eVarsSet'
                 , eFusionCases = eFusionCases' }) m

lookupCommon :: Ord a => (CommonEnv -> M.Map a b) -> a -> App (Maybe b)
lookupCommon dict obj = do
  Env {..} <- ask
  eEnvs <- liftIO $ readIORef eEnvsR
  return $ do
    cenv <- M.lookup eFocus eEnvs
    M.lookup obj (dict cenv)

rewrittenp :: DLVar -> App (Maybe (DLVar, Maybe DLArg))
rewrittenp = lookupCommon ceReplaced

repeated :: DLExpr -> App (Maybe RepeatedT)
repeated = \case
  DLE_Arg _ a -> return $ Just $ Left a
  e@(DLE_LArg _ a) ->
    lookupCommon cePrev e >>= \case
      Nothing -> return $ Just $ Right a
      Just x -> return $ Just $ x
  e -> lookupCommon cePrev e

optNotHuh :: DLArg -> App (Maybe DLArg)
optNotHuh = \case
  DLA_Var v -> lookupCommon ceNots v
  _ -> return $ Nothing

recordNotHuh :: DLLetVar -> DLArg -> App ()
recordNotHuh = \case
  DLV_Eff -> const $ return ()
  DLV_Let _ v -> \a -> do
    updateLookup
      (\cenv ->
         cenv
           { ceNots = M.insert v a $ ceNots cenv
           })

optKnownVariant :: DLVar -> App (Maybe (SLVar, DLArg))
optKnownVariant v = do
  optKnownLargeArg v >>= \case
    Nothing -> return $ Nothing
    Just (DLLA_Data _ k va) -> return $ Just (k, va)
    Just _ -> impossible "optKnownVariant"

optKnownLargeArg :: DLVar -> App (Maybe DLLargeArg)
optKnownLargeArg = lookupCommon ceKnownLargeArgs

recordKnownLargeArg :: DLVar -> DLLargeArg -> App ()
recordKnownLargeArg dv v =
  updateLookup (\e -> e {ceKnownLargeArgs = M.insert dv v $ ceKnownLargeArgs e})

remember_ :: Bool -> DLVar -> DLExpr -> App ()
remember_ always v e =
  updateLookup (\cenv -> cenv {cePrev = up $ cePrev cenv, ceExpr = M.insert v e $ ceExpr cenv })
  where
    up prev =
      case always || not (M.member e prev) of
        True -> M.insert e (Left $ DLA_Var v) prev
        False -> prev

remember :: DLVar -> DLExpr -> App ()
remember = remember_ True

mremember :: DLVar -> DLExpr -> App ()
mremember = remember_ False

rewrite :: DLVar -> (DLVar, Maybe DLArg) -> App ()
rewrite v r = do
  updateLookup (\cenv -> cenv {ceReplaced = M.insert v r (ceReplaced cenv)})

updateLookupWhen :: (Focus -> Bool) -> (CommonEnv -> CommonEnv) -> App ()
updateLookupWhen writeHuh up = do
  Env {..} <- ask
  let update1 (f, cenv) = (f, (if writeHuh f then up else id) cenv)
  let update = M.fromList . (map update1) . M.toList
  liftIO $ modifyIORef eEnvsR update

updateLookup :: (CommonEnv -> CommonEnv) -> App ()
updateLookup up = do
  Env {..} <- ask
  let writeHuh f =
        case eFocus of
          F_Ctor ->
            case f of
              F_Ctor -> True
              F_All -> True -- False
              F_Consensus -> False -- True -- False
              F_One _ -> True
          F_All -> True
          F_Consensus -> True
          F_One _ -> f == eFocus
  updateLookupWhen writeHuh up

mkEnv0 :: Counter -> Counter -> S.Set DLVar -> [SLPart] -> DLMapInfos -> Bool -> S.Set DLVar -> IO Env
mkEnv0 eCounter eDroppedAsserts eConst eParts eMaps eSimulate eSvs = do
  let eFocus = F_Ctor
  let eClearMaps = False
  let eEnvs =
        M.fromList $
          map (\x -> (x, mempty)) $ F_Ctor : F_All : F_Consensus : map F_One eParts
  eEnvsR <- liftIO $ newIORef eEnvs
  eVarsSet <- liftIO $ newIORef mempty
  eFusionCases <- liftIO $ newIORef mempty
  return $ Env {..}

maybeClearMaps :: App a -> App a
maybeClearMaps m = asks eClearMaps >>= \case
  True -> doClearMaps m
  False -> m

doClearMaps :: App a -> App a
doClearMaps m = do
  let clear = M.filterWithKey $ \k _ ->
        case k of
          DLE_MapRef {} -> False
          _ -> True
  updateLookupWhen (const True) $ \ce ->
      ce {cePrev = clear $ cePrev ce}
  m

opt_v2p :: DLVar -> App (DLVar, DLArg)
opt_v2p v = do
  r <- rewrittenp v
  let both v' = return $ (v', DLA_Var v')
  case r of
    Nothing -> both v
    Just (v', Nothing) -> both v'
    Just (v', Just a') -> return $ (v', a')

opt_v2v :: DLVar -> App DLVar
opt_v2v v = fst <$> opt_v2p v

opt_v2a :: DLVar -> App DLArg
opt_v2a v = snd <$> opt_v2p v

-- This deconstructs an `a` into the pieces of a X_Com
class IsCom a where
  isCom :: a -> Maybe (DLStmt, a)

instance IsCom DLTail where
  isCom = \case
    DT_Com m k -> Just (m, k)
    _ -> Nothing

instance IsCom CTail where
  isCom = \case
    CT_Com m k -> Just (m, k)
    _ -> Nothing

instance IsCom ETail where
  isCom = \case
    ET_Com m k -> Just (m, k)
    _ -> Nothing

instance IsCom LLStep where
  isCom = \case
    LLS_Com m k -> Just (m, k)
    _ -> Nothing

instance IsCom LLConsensus where
  isCom = \case
    LLC_Com m k -> Just (m, k)
    _ -> Nothing

class LiftPure a where
  -- Returns what `DLVar` is `DL_Set` to and the pure `DLStmt`s along the way
  -- Returns `Nothing` if there are impure stmts in `a` or there is no `DL_Set`
  liftPure :: DLVar -> a -> Maybe (DLArg, [DLStmt])

instance LiftPure DLTail where
  liftPure dv = \case
    DT_Return _ -> Nothing
    DT_Com (DL_Set _ lhs rhs) (DT_Return _)
      | lhs == dv -> Just (rhs, [])
    DT_Com ds dt ->
      case isPure ds of
        True -> do
          case liftPure dv dt of
            Just (a, dt') -> Just (a, ds : dt')
            Nothing -> Nothing
        False -> Nothing

floatVar :: (DLStmt -> t -> t) -> SrcLoc -> DLVar -> t -> App t
floatVar mkk at dv k = do
  readVarSet dv >>= \case
    -- There were zero branches that could not be lifted, i.e.
    -- we generated a `const` assign, now drop this `var`.
    Just 0 -> return $ k
    _ -> return $ mkk (DL_Var at dv) k

floatIf :: (DLStmt -> b -> b) -> SrcLoc -> DLVar -> DLArg -> DLTail -> DLTail -> b -> App b
floatIf mkk at ansDv c tt ft k = do
  case isPure tt && isPure ft of
    True ->
      case (liftPure ansDv tt, liftPure ansDv ft) of
        (Just (ta, t_lifts), Just (fa, f_lifts)) -> do
          mVal <- updateVarSet ansDv mSub1
          -- Track that we successfully lifted this assignment
          let asn v = DL_Let at (DLV_Let DVC_Many v) $ DLE_PrimOp at IF_THEN_ELSE [c, ta, fa]
          t <- case mVal of
                Just 0 -> return $ mkk (asn ansDv) k
                Just _ -> do
                  tmp <- mkVar at (varType ansDv)
                  let s2 = DL_Set at ansDv (DLA_Var tmp)
                  return $ mkk (asn tmp) (mkk s2 k)
                Nothing -> impossible "floatIf: trying to float if that does not set a dlvar"
          return $ foldr mkk t $ t_lifts <> f_lifts
        _ -> meh
    False -> meh
  where
    meh = return $ mkk (DL_LocalIf at (Just ansDv) c tt ft) k
    mSub1 = Just . maybe 0 (\ x -> x - 1)

float :: IsCom a => (DLStmt -> a -> a) -> a -> App a
float mk t =
  case isCom t of
    Just (DL_Var at dv, k) -> floatVar mk at dv k
    Just (DL_LocalIf at (Just dv) c tt ft, k) -> floatIf mk at dv c tt ft k
    _ -> return t

instance (Traversable t, Optimize a) => Optimize (t a) where
  opt = traverse opt
  gcs = mapM_ gcs

instance {-# OVERLAPS #-} (Optimize a, Optimize b) => Optimize (a, b) where
  opt (x, y) = (,) <$> opt x <*> opt y
  gcs (x, y) = gcs x >> gcs y

instance {-# OVERLAPS #-} (Optimize a, Optimize b, Optimize c) => Optimize (a, b, c) where
  opt (x, y, z) = (,,) <$> opt x <*> opt y <*> opt z
  gcs (x, y, z) = gcs x >> gcs y >> gcs z

instance {-# OVERLAPS #-} (Optimize a, Optimize b) => Optimize (Either a b) where
  opt = \case
    Left x -> Left <$> opt x
    Right x -> Right <$> opt x
  gcs = \case
    Left x -> gcs x
    Right x -> gcs x

instance Optimize IType where
  opt = return
  gcs _ = return ()

instance Optimize DLVar where
  opt = opt_v2v
  gcs _ = return ()

instance Optimize Bool where
  opt = return
  gcs _ = return ()

instance Optimize B.ByteString where
  opt = return
  gcs _ = return ()

instance Optimize DLArg where
  opt = \case
    DLA_Var v -> opt_v2a v
    DLA_Constant c -> return $ DLA_Constant c
    DLA_Literal c -> return $ DLA_Literal c
    DLA_Interact p m t -> return $ DLA_Interact p m t
  gcs _ = return ()

instance Optimize DLLargeArg where
  opt = \case
    DLLA_Array t as -> DLLA_Array t <$> opt as
    DLLA_Tuple as -> DLLA_Tuple <$> opt as
    DLLA_Obj m -> DLLA_Obj <$> opt m
    DLLA_Data t vn vv -> DLLA_Data t vn <$> opt vv
    DLLA_Struct kvs -> DLLA_Struct <$> mapM go kvs
    DLLA_Bytes b -> return $ DLLA_Bytes b
    DLLA_StringDyn t -> return $ DLLA_StringDyn t
    where
      go (k, v) = (,) k <$> opt v
  gcs _ = return ()

instance Optimize DLTokenNew where
  opt (DLTokenNew {..}) =
    DLTokenNew
      <$> opt dtn_name
      <*> opt dtn_sym
      <*> opt dtn_url
      <*> opt dtn_metadata
      <*> opt dtn_supply
      <*> opt dtn_decimals
  gcs _ = return ()

instance Optimize DLWithBill where
  opt (DLWithBill x y z) =
    DLWithBill x <$> opt y <*> opt z
  gcs _ = return ()

instance Optimize DLRemoteALGOOC where
  opt = return
  gcs _ = return ()

instance Optimize DLRemoteALGOSTR where
  opt = \case
    RA_Unset -> return RA_Unset
    RA_List at l -> RA_List at <$> opt l
    RA_Tuple t -> RA_Tuple <$> opt t
  gcs _ = return ()

instance Optimize DLRemoteALGO where
  opt (DLRemoteALGO a b c d e f g h i j k) =
    DLRemoteALGO <$> opt a <*> opt b <*> opt c <*> opt d <*> opt e <*> opt f <*> opt g <*> opt h <*>
                     opt i <*> opt j <*> opt k
  gcs _ = return ()

instance Optimize AS.Value where
  opt = return
  gcs _ = return ()

instance Optimize DLContractNew where
  opt (DLContractNew a b) =
    DLContractNew <$> opt a <*> opt b
  gcs _ = return ()

unsafeAt :: [a] -> Int -> a
unsafeAt l i =
  case atMay l i of
    Nothing -> impossible "unsafeMay"
    Just x -> x

isAnEqual :: PrimOp -> Bool
isAnEqual = \case
  PEQ _ -> True
  DIGEST_EQ -> True
  ADDRESS_EQ -> True
  TOKEN_EQ -> True
  _ -> False

instance Optimize DLRemote where
  opt (DLRemote m amta as wbill malgo) = DLRemote <$> pure m <*> opt amta <*> opt as <*> opt wbill <*> opt malgo
  gcs _ = return ()

instance Optimize Integer where
  opt i = return i
  gcs _ = return ()

instance Optimize DLExpr where
  opt = \case
    DLE_Arg at a -> DLE_Arg at <$> opt a
    DLE_LArg at a -> DLE_LArg at <$> opt a
    DLE_Impossible at tag lab ->
      return $ DLE_Impossible at tag lab
    DLE_VerifyMuldiv at f cl args err ->
      DLE_VerifyMuldiv at f cl <$> opt args <*> pure err
    DLE_PrimOp at p as -> do
      as' <- opt as
      let meh = return $ DLE_PrimOp at p as'
      let zero t = DLA_Literal $ DLL_Int at t 0
      let staticN t n = return $ DLE_Arg at $ DLA_Literal $ DLL_Int at t n
      let staticB b = return $ DLE_Arg at $ DLA_Literal $ DLL_Bool b
      case (p, as') of
        (ADD t _, [(DLA_Literal (DLL_Int _ _ lhs)), (DLA_Literal (DLL_Int _ _ rhs))]) -> staticN t $ (+) lhs rhs
        (SUB t _, [(DLA_Literal (DLL_Int _ _ lhs)), (DLA_Literal (DLL_Int _ _ rhs))]) -> staticN t $ (-) lhs rhs
        (MUL t _, [(DLA_Literal (DLL_Int _ _ lhs)), (DLA_Literal (DLL_Int _ _ rhs))]) -> staticN t $ (*) lhs rhs
        (DIV t _, [(DLA_Literal (DLL_Int _ _ lhs)), (DLA_Literal (DLL_Int _ _ rhs))]) -> staticN t $ (div) lhs rhs
        (MOD t _, [(DLA_Literal (DLL_Int _ _ lhs)), (DLA_Literal (DLL_Int _ _ rhs))]) -> staticN t $ (mod) lhs rhs
        (PLT _, [(DLA_Literal (DLL_Int _ _ lhs)), (DLA_Literal (DLL_Int _ _ rhs))]) -> staticB $ (<) lhs rhs
        (PLE _, [(DLA_Literal (DLL_Int _ _ lhs)), (DLA_Literal (DLL_Int _ _ rhs))]) -> staticB $ (<=) lhs rhs
        (PEQ _, [(DLA_Literal (DLL_Int _ _ lhs)), (DLA_Literal (DLL_Int _ _ rhs))]) -> staticB $ (==) lhs rhs
        (PGE _, [(DLA_Literal (DLL_Int _ _ lhs)), (DLA_Literal (DLL_Int _ _ rhs))]) -> staticB $ (>=) lhs rhs
        (PGT _, [(DLA_Literal (DLL_Int _ _ lhs)), (DLA_Literal (DLL_Int _ _ rhs))]) -> staticB $ (>) lhs rhs
        (ADD _ _, [(DLA_Literal (DLL_Int _ _ 0)), rhs]) ->
          return $ DLE_Arg at rhs
        (ADD _ _, [lhs, (DLA_Literal (DLL_Int _ _ 0))]) ->
          return $ DLE_Arg at lhs
        (SUB _ _, [lhs, (DLA_Literal (DLL_Int _ _ 0))]) ->
          return $ DLE_Arg at lhs
        (MUL _ _, [(DLA_Literal (DLL_Int _ _ 1)), rhs]) ->
          return $ DLE_Arg at rhs
        (MUL _ _, [lhs, (DLA_Literal (DLL_Int _ _ 1))]) ->
          return $ DLE_Arg at lhs
        (MUL t _, [(DLA_Literal (DLL_Int _ _ 0)), _]) ->
          return $ DLE_Arg at $ zero t
        (MUL t _, [_, (DLA_Literal (DLL_Int _ _ 0))]) ->
          return $ DLE_Arg at $ zero t
        (DIV _ _, [lhs, (DLA_Literal (DLL_Int _ _ 1))]) ->
          return $ DLE_Arg at lhs
        (SQRT t, [DLA_Literal (DLL_Int _ _ n)]) ->
          return $ DLE_Arg at $ DLA_Literal $ DLL_Int at t (isqrt n)
        (MUL_DIV _, [l, r, d])
          | l == d -> return $ DLE_Arg at r
          | r == d -> return $ DLE_Arg at l
        (MUL_DIV pv, [l, r, DLA_Literal (DLL_Int _ _ 1)]) ->
          opt $ DLE_PrimOp at (MUL UI_Word pv) [l, r]
        (MUL_DIV pv, [DLA_Literal (DLL_Int _ _ 1), r, d]) ->
          opt $ DLE_PrimOp at (DIV UI_Word pv) [r, d]
        (MUL_DIV pv, [l, DLA_Literal (DLL_Int _ _ 1), d]) ->
          opt $ DLE_PrimOp at (DIV UI_Word pv) [l, d]
        (MUL_DIV _, [DLA_Literal (DLL_Int _ _ 0), _, _]) ->
          return $ DLE_Arg at $ zero UI_Word
        (MUL_DIV _, [_, DLA_Literal (DLL_Int _ _ 0), _, _]) ->
          return $ DLE_Arg at $ zero UI_Word
        (IF_THEN_ELSE, [c, (DLA_Literal (DLL_Bool True)), (DLA_Literal (DLL_Bool False))]) ->
          return $ DLE_Arg at $ c
        (IF_THEN_ELSE, [(DLA_Literal (DLL_Bool c)), t, f]) ->
          return $ DLE_Arg at $ if c then t else f
        (IF_THEN_ELSE, [c, t, f]) ->
          optNotHuh c >>= \case
            Nothing -> meh
            Just c' ->
              return $ DLE_PrimOp at IF_THEN_ELSE [c', f, t]
        (aneq, [lhs, rhs])
          | isAnEqual aneq && sani lhs == sani rhs ->
            return $ DLE_Arg at $ DLA_Literal $ DLL_Bool True
        _ -> meh
    DLE_ArrayRef at a i -> DLE_ArrayRef at <$> opt a <*> opt i
    DLE_ArraySet at a i v -> optSet DLE_ArraySet DLE_ArrayRef at a i v
    DLE_ArrayConcat at x0 y0 -> DLE_ArrayConcat at <$> opt x0 <*> opt y0
    DLE_TupleRef at t i -> do
      t' <- opt t
      let meh = return $ DLE_TupleRef at t' i
      case t' of
        DLA_Var tv ->
          optKnownLargeArg tv >>= \case
            Just (DLLA_Tuple as) -> do
              let a = unsafeAt as $ fromIntegral i
              DLE_Arg at <$> opt a
            _ -> meh
        _ -> meh
    DLE_ObjectRef at o k -> DLE_ObjectRef at <$> opt o <*> pure k
    DLE_Interact at fs p m t as -> DLE_Interact at fs p m t <$> opt as
    DLE_Digest at as -> DLE_Digest at <$> opt as
    DLE_Claim at fs t a m -> do
      a' <- opt a
      let meh = return $ DLE_Claim at fs t a' m
      isSim <- asks eSimulate
      case (t, a', isSim) of
        (_, _, True) -> nop at
        (CT_Possible, _, _) -> meh
        (_, DLA_Literal (DLL_Bool True), _) -> do
          Env {..} <- ask
          void $ liftIO $ incCounter eDroppedAsserts
          nop at
        (_, DLA_Var dv, _) -> do
          void $ rememberVarIsArg at dv $ DLA_Literal $ DLL_Bool True
          meh
        _ -> meh
    DLE_Transfer at t a m -> do
      a' <- opt a
      case a' of
        DLA_Literal (DLL_Int _ _ 0) -> nop at
        _ ->
          DLE_Transfer at <$> opt t <*> pure a' <*> opt m
    DLE_TokenInit at t -> DLE_TokenInit at <$> opt t
    DLE_TokenAccepted at addr tok -> DLE_TokenAccepted at <$> opt addr <*> opt tok
    DLE_CheckPay at fs a m -> DLE_CheckPay at fs <$> opt a <*> opt m
    DLE_Wait at a -> DLE_Wait at <$> opt a
    DLE_PartSet at who a -> DLE_PartSet at who <$> opt a
    DLE_MapRef at mv fa -> DLE_MapRef at mv <$> opt fa
    DLE_MapSet at mv fa na -> DLE_MapSet at mv <$> opt fa <*> opt na
    DLE_Remote at fs av rt dr -> DLE_Remote at fs <$> opt av <*> pure rt <*> opt dr
    DLE_TokenNew at tns -> DLE_TokenNew at <$> opt tns
    DLE_TokenBurn at tok amt -> DLE_TokenBurn at <$> opt tok <*> opt amt
    DLE_TokenDestroy at tok -> DLE_TokenDestroy at <$> opt tok
    DLE_TimeOrder at op a b -> DLE_TimeOrder at op <$> opt a <*> opt b
    DLE_EmitLog at k a -> DLE_EmitLog at k <$> opt a
    DLE_setApiDetails s p ts mc f -> return $ DLE_setApiDetails s p ts mc f
    DLE_GetUntrackedFunds at mt tb -> DLE_GetUntrackedFunds at <$> opt mt <*> opt tb
    DLE_DataTag at d -> DLE_DataTag at <$> opt d
    DLE_FromSome at mo da -> do
      mo' <- opt mo
      da' <- opt da
      let meh = return $ DLE_FromSome at mo' da'
      case mo' of
        DLA_Var mv ->
          optKnownLargeArg mv >>= \case
            Just (DLLA_Data _ vn a) ->
              case vn of
                "Some" -> DLE_Arg at <$> opt a
                _ -> return $ DLE_Arg at da'
            _ -> meh
        _ -> meh
    DLE_ContractNew at cns dr -> DLE_ContractNew at <$> opt cns <*> opt dr
    DLE_ContractFromAddress at addr -> DLE_ContractFromAddress at <$> opt addr
    DLE_ObjectSet at o k v -> DLE_ObjectSet at <$> opt o <*> pure k <*> opt v
    DLE_TupleSet at t k v -> optSet DLE_TupleSet DLE_TupleRef at t k v
    where
      nop at = return $ DLE_Arg at $ DLA_Literal $ DLL_Null
  gcs _ = return ()

-- Opt:
--  const x = xs[idx];
--  const xs' = xs.set(idx, x);
-- =>
--  const xs' = xs
optSet :: Optimize t => (SrcLoc -> DLArg -> t -> DLArg -> DLExpr) -> (SrcLoc -> DLArg -> t -> DLExpr) -> SrcLoc -> DLArg -> t -> DLArg -> App DLExpr
optSet setE refE at a i v = do
  a' <- opt a
  i' <- opt i
  v' <- opt v
  v'' <- repeated $ sani $ refE at a' i'
  case v'' of
    Just (Left dv) | dv == v' -> opt $ DLE_Arg at a'
    _ -> return $ setE at a' i' v'

instance Optimize DLAssignment where
  opt (DLAssignment m) = DLAssignment <$> opt m
  gcs _ = return ()

class Extract a where
  extract :: a -> Maybe DLVar

instance Extract (Maybe DLVar) where
  extract = id

allTheSame :: (Eq a, Sanitize a) => [a] -> Either (Maybe (a, a)) a
allTheSame = allEqual . map sani

-- Given that the result of `a` is the given `DLArg`, learn new bindings
class Learn a where
  learn :: DLArg -> a -> [(DLVar, DLArg)]

instance Learn DLArg where
  learn result = \case
    -- Trivial base case: a var is equal to a value
    DLA_Var dv -> [(dv, result)]
    _ -> []

instance Learn DLExpr where
  learn result = \case
    DLE_PrimOp _ IF_THEN_ELSE [DLA_Var dv, lhs, rhs]
      -- You might think that we can go from
      --
      -- lhs == result && rhs != result => dv = true
      --
      -- based on the idea that "We must have chosen lhs", like:
      --
      -- x := v ? true : false
      -- x = true
      -- =>
      -- v = true
      --
      -- And the reverse, but learning `dv = false`.
      -- However, this is not generally true, because...
      --
      -- x1 := v1 ? false : true
      -- x2 := v2 ? false : x1
      -- x2 = false
      --
      -- Notice that...
      --   lhs (false) = result (x2/false)
      --   rhs (x1) != result (x2/false)
      --
      -- But, we cannot conclude that `v2 = false`, because `x1` might be
      -- false!
      --
      -- This rule only applies when lhs & rhs are bools!
      | areBool [lhs, rhs] && lhs `equiv` result && not (rhs `equiv` result) -> [(dv, true)]
      | areBool [lhs, rhs] && rhs `equiv` result && not (lhs `equiv` result) -> [(dv, false)]

      -- r = false, x ? true : v3   => [(x, false), (v3, false)]
      | areBool [result, lhs] && (lhs /= result) -> (dv, false) : mAsn result rhs
      | areBool [result, rhs] && (rhs /= result) -> (dv, true) : mAsn result lhs
    DLE_PrimOp _ op [DLA_Var dv, other]
      | chkOpEqAndResultTrue op -> [(dv, other)]
    DLE_PrimOp _ op [other, DLA_Var dv]
      | chkOpEqAndResultTrue op -> [(dv, other)]
    -- Base case, it's an arg, so they are the same
    DLE_Arg _ darg -> learn result darg
    _ -> []
    where
      chkOpEqAndResultTrue op = isAnEqual op && result == true
      false = DLA_Literal $ DLL_Bool False
      true  = DLA_Literal $ DLL_Bool True
      mAsn v = \case
        DLA_Var dv' -> [(dv', v)]
        _ -> []
      areBool = all isBool
      isBool = \case
        DLA_Literal (DLL_Bool {}) -> True
        _ -> False

-- Record what we know about v if it is the same as vIs
recLearn :: DLVar -> DLArg -> App ()
recLearn v vIs = do
  mExpr <- lookupCommon ceExpr v
  void $ rememberVarIsArg sb v vIs
  -- Given that v is really mExpr, and that it is vIs, then record what else we
  -- know
  mapM_ (uncurry recLearn) $ maybe [] (learn vIs) mExpr

optIf :: (Eq k, Sanitize k, Optimize k) => (k -> r) -> (DLArg -> k -> k -> r) -> DLArg -> k -> k -> App r
optIf mkDo mkIf c t f =
  opt c >>= \case
    DLA_Literal (DLL_Bool True) -> mkDo <$> opt t
    DLA_Literal (DLL_Bool False) -> mkDo <$> opt f
    c' -> do
      let learnC b =
            case c' of
              DLA_Var v -> recLearn v $ DLA_Literal $ DLL_Bool b
              _ -> return ()
      t' <- newScope $ learnC True >> opt t
      f' <- newScope $ learnC False >> opt f
      case allTheSame [t', f'] of
        Right s -> return $ mkDo s
        Left _ ->
          optNotHuh c' >>= \case
            Just c'' ->
              return $ mkIf c'' f' t'
            Nothing ->
              return $ mkIf c' t' f'

gcsSwitch :: Optimize k => ConstT (SwitchCases k)
gcsSwitch = mapM_ (\(_, _, n) -> gcs n)

optSwitch :: (Eq k, Sanitize k, Optimize k) => (k -> r) -> (DLStmt -> k -> k) -> (SrcLoc -> DLVar -> SwitchCases k -> r) -> SrcLoc -> DLVar -> SwitchCases k -> App r
optSwitch mkDo mkLet mkSwitch at ov csm = do
  ov' <- opt ov
  optKnownVariant ov' >>= \case
    Just (var, var_val) -> do
      let (var_var, _, var_k) = (M.!) csm var
      let var_k' = mkLet (DL_Let at (DLV_Let DVC_Many var_var) (DLE_Arg at var_val)) var_k
      newScope $ mkDo <$> opt var_k'
    Nothing -> do
      let tm = dataTypeMap $ varType ov
      let rkv dv k va = recordKnownLargeArg dv $ DLLA_Data tm k va
      let cm1 k (v_v, vnu, n) = (,,) v_v vnu <$> (newScope $ rkv ov' k (DLA_Var v_v) >> opt n)
      csm' <- mapWithKeyM cm1 csm
      let csm'kl = map (\(_k, (_v_v, _vnu, n)) -> n) $ M.toAscList csm'
      case allTheSame csm'kl of
        Right s -> return $ mkDo s
        Left _ -> return $ mkSwitch at ov' csm'

optWhile :: Optimize a => (DLAssignment -> DLBlock -> a -> a -> a) -> DLAssignment -> DLBlock -> a -> a -> App a
optWhile mk asn cond body k = do
  asn' <- opt asn
  cond'@(DLBlock _ _ _ ca) <- newScope $ opt cond
  let mca b m = case ca of
        DLA_Var dv -> do
          rewrite dv (dv, Just (DLA_Literal $ DLL_Bool b))
          optNotHuh ca >>= \case
            Just (DLA_Var dv') -> do
              rewrite dv' (dv', Just (DLA_Literal $ DLL_Bool $ not b))
            _ -> return ()
          m
        _ -> m
  body' <- newScope $ mca True $ opt body
  k' <- newScope $ mca False $ opt k
  return $ mk asn' cond' body' k'

rememberVarIsArg :: SrcLoc -> DLVar -> DLArg -> App DLExpr
rememberVarIsArg at dv a = do
  rewrite dv (dv, Just a)
  let e = DLE_Arg at a
  mremember dv (sani e)
  return e

optLet :: SrcLoc -> DLLetVar -> DLExpr -> App DLStmt
optLet at x e = do
  e' <- opt e
  let meh = return $ DL_Let at x e'
  let argCase dv at' a' = do
        e'' <- rememberVarIsArg at' dv a'
        return $ DL_Let at x e''
  let largCase dv at' a' = do
        recordKnownLargeArg dv a'
        let e'' = (DLE_LArg at' a')
        mremember dv (sani e'')
        return $ DL_Let at x e''
  let doit dv = do
        case e' of
          DLE_Arg at' a'
            | canDupe a' ->
              argCase dv at' a'
          -- DLE_LArg at' a'
          --   | canDupe a' ->
          --     largCase dv at' a'
          _ -> do
            let e'' = sani e'
            common <- repeated e''
            case common of
              Just (Left (DLA_Var rt)) -> do
                rewrite dv (rt, Nothing)
                return $ DL_Nop at
              Just (Left a')
                | canDupe a' ->
                  argCase dv at a'
              Just (Right a')
                | canDupe a' ->
                  largCase dv at a'
              _ -> do
                remember dv e''
                case e' of
                  DLE_PrimOp _ IF_THEN_ELSE [c, DLA_Literal (DLL_Bool False), DLA_Literal (DLL_Bool True)] -> do
                    recordNotHuh x c
                  _ ->
                    return ()
                meh
  svs <- asks eSvs
  let allowedToRemove = not . flip S.member svs
  case (extract x, (isPure e && canDupe e), e) of
    (Just dv, True, _) | allowedToRemove dv -> doit dv
    -- Optimize arithmetic even if it is impure (PV_Safe).
    -- It may trap as an effect, but we don't need to worry about it happening multiple times
    (Just dv, _, DLE_PrimOp {}) | allowedToRemove dv -> doit dv
    (_, _, DLE_MapSet _ mv fa nva) -> do
      let ref = DLE_MapRef sb mv fa
      mmt <- getMapTy mv
      let clear = M.filterWithKey $ \k _ ->
            case k of
              DLE_MapRef _ mv_ _ -> mv /= mv_
              _ -> True
      let upf =
            case mmt of
              Nothing -> id
              Just mt -> M.insert ref $ Right $ mdaToMaybeLA mt nva
      let up ce = ce {cePrev = (upf . clear) $ cePrev ce}
      updateLookup up
      meh
    _ -> meh

instance Optimize DLStmt where
  opt = \case
    DL_Nop at -> return $ DL_Nop at
    DL_Let at x e -> optLet at x e
    DL_Var at v ->
      optConst v >>= \case
        True -> return $ DL_Nop at
        False -> return $ DL_Var at v
    DL_Set at v a ->
      optConst v >>= \case
        False -> DL_Set at v <$> opt a
        True -> optLet at (DLV_Let DVC_Many v) (DLE_Arg at a)
    DL_LocalIf at mans c t f ->
      optIf (DL_LocalDo at mans) (DL_LocalIf at mans) c t f
    DL_LocalSwitch at ov csm ->
      optSwitch (DL_LocalDo at Nothing) DT_Com DL_LocalSwitch at ov csm
    DL_ArrayMap at ans xs as i f -> do
      s' <- DL_ArrayMap at ans <$> opt xs <*> pure as <*> pure i <*> newScope (opt f)
      maybeUnroll s' xs $ return s'
    DL_ArrayReduce at ans xs z b as i f -> do
      s' <- DL_ArrayReduce at ans <$> opt xs <*> opt z <*> (opt b) <*> (pure as) <*> pure i <*> newScope (opt f)
      maybeUnroll s' xs $ return s'
    DL_MapReduce at mri ans x z b a f -> do
      DL_MapReduce at mri ans x <$> opt z <*> (pure b) <*> (pure a) <*> newScope (opt f)
    DL_Only at ep l -> do
      let w = case ep of
            Left p -> focus_one p
            Right _ -> id
      l' <- w $ opt l
      case l' of
        DT_Return _ -> return $ DL_Nop at
        _ -> return $ DL_Only at ep l'
    DL_LocalDo at mans t ->
      opt t >>= \case
        DT_Return _ -> return $ DL_Nop at
        t' -> return $ DL_LocalDo at mans t'
    where
      maybeUnroll :: DLStmt -> [DLArg] -> App DLStmt -> App DLStmt
      maybeUnroll s xs def = do
        let len = arraysLength xs
        case len <= 1 of
          True -> do
            c <- asks eCounter
            let at = srclocOf s
            let t = DL_LocalDo at Nothing $ DT_Com s $ DT_Return at
            UnrollWrapper _ t' <- liftIO $ unrollLoops $ UnrollWrapper c t
            return t'
          _ -> def
  gcs = \case
    DL_Nop {} -> return ()
    DL_Let {} -> return ()
    DL_Var {} -> return ()
    DL_Set _ v _ -> do
      cr <- asks eConstR
      let f = Just . (+) 1 . fromMaybe 0
      liftIO $ modifyIORef cr $ M.alter f v
    DL_LocalIf _ _ _ t f -> gcs t >> gcs f
    DL_LocalSwitch _ _ csm -> gcsSwitch csm
    DL_ArrayMap _ _ _ _ _ f -> gcs f
    DL_ArrayReduce _ _ _ _ _ _ _ f -> gcs f
    DL_MapReduce _ _ _ _ _ _ _ f -> gcs f
    DL_Only _ _ l -> gcs l
    DL_LocalDo _ _ t -> gcs t

-- Track that we are attempting to lift this assignment
preFloat :: DLStmt -> App ()
preFloat = \case
  DL_LocalIf _ (Just dv) _ _ _ ->
    void $ updateVarSet dv $ Just . maybe 1 succ
  _ -> return ()

updateFusion :: DLVar -> SwitchCases DLTail -> App ()
updateFusion dv scs = do
  fusionCasesR <- asks eFusionCases
  liftIO $ modifyIORef' fusionCasesR $ M.insert dv scs

readFusionCases :: DLVar -> App (Maybe (SwitchCases DLTail))
readFusionCases dv = do
  fusionCases <- liftIO . readIORef =<< asks eFusionCases
  return $ M.lookup dv fusionCases

preFuse :: DLStmt -> App FuseEnv
preFuse s = do
  oldEnv <- liftIO . readIORef =<< asks eFusionCases
  case s of
    DL_LocalSwitch _ dv _ -> updateFusion dv mempty
    DL_Let {} -> cantFuse
    -- XXX We could fuse if the `DL_Var` we encounter is set within a switch.
    -- The env would need to track the vars a switch needs to set to fuse.
    -- We'd want to update `DL_LocalSwitch` to track whether it sets a var, for efficiency.
    -- Q: If we end up fusing, how do we efficiently drop these `DL_Var`?
    DL_Var {} -> cantFuse
    _ | not (isPure s) -> cantFuse
    _ -> return ()
  return oldEnv
  where cantFuse = liftIO . flip writeIORef mempty =<< asks eFusionCases

fuseSwitch :: (DLStmt -> a -> a) -> SrcLoc -> DLVar -> SwitchCases DLTail -> a -> App a
fuseSwitch mk at dv scs k =
  readFusionCases dv >>= \case
    Just cases -> ret $ M.mapWithKey (fuse cases) scs
    Nothing -> ret scs
  where
    ret cases = return $ mk (DL_LocalSwitch at dv cases) k
    fuse cases c (v1, b1, t1) =
      case M.lookup c cases of
        Just (v2, _, t2) -> do
          -- The branch we're inling binds `v2`.
          -- The branch we're inling *into* binds `v1`.
          -- Set v2 to v1
          let t2' = DT_Com (DL_Let at (DLV_Let DVC_Many v2) $ DLE_Arg at $ DLA_Var v1) t2
          (v1, b1, dtReplace DT_Com t2' t1)
        Nothing -> (v1, b1, t1)

fuseStmt :: IsCom a => FuseEnv -> (DLStmt -> a -> a) -> a -> App a
fuseStmt fuseEnv mk t =
  case isCom t of
    Just (DL_LocalSwitch at dv scs, k) -> do
      -- We've processed everything in the tail.
      -- Now, look in the env to see if there are any
      -- subsequent switches to fuse into this stmt.
      k' <- fuseSwitch mk at dv scs k
      fusionCasesR <- asks eFusionCases
      case isCom k' of
        Just (DL_LocalSwitch _ _ scs1, k1) | M.member dv fuseEnv -> do
          -- If there are previous switches looking to fuse,
          -- reset the fusionCase env to how it was
          -- before we processed this branch, add the info
          -- for this switch to the env, and drop this stmt
            liftIO $ writeIORef fusionCasesR $ M.insert dv scs1 fuseEnv
            return $ mk (DL_Nop at) k1
        -- If there are no previous switches looking to fuse, keep this stmt and return
        _ -> do
          liftIO $ writeIORef fusionCasesR fuseEnv
          return k'
    _ -> return t

-- Optimize is generally top-down, but this is locally bottom-up. We float/fuse after optimizing the tail
optCom :: (IsCom b, Optimize b) => (DLStmt -> b -> b) -> DLStmt -> b -> App b
optCom mk m k = do
  preFloat m
  fuseEnv <- preFuse m
  fuseStmt fuseEnv mk =<< float mk =<< mkCom mk <$> opt m <*> opt k

instance Optimize DLTail where
  opt = \case
    DT_Return at -> return $ DT_Return at
    DT_Com m k -> optCom DT_Com m k
  gcs = \case
    DT_Return _ -> return ()
    DT_Com m k -> gcs m >> gcs k

instance Optimize DLBlock where
  opt (DLBlock at fs b a) =
    -- newScope $
    DLBlock at fs <$> opt b <*> opt a
  gcs (DLBlock _ _ b _) = gcs b

instance {-# OVERLAPPING #-} Optimize a => Optimize (DLinExportBlock a) where
  opt (DLinExportBlock at vs b) =
    newScope $ DLinExportBlock at vs <$> opt b
  gcs (DLinExportBlock _ _ b) = gcs b

instance {-# OVERLAPS #-} Optimize a => Optimize (DLInvariant a) where
  opt (DLInvariant inv lab) =
    DLInvariant <$> (newScope $ opt inv) <*> pure lab
  gcs (DLInvariant inv _) = gcs inv

instance Optimize LLConsensus where
  opt = \case
    LLC_Com m k -> optCom LLC_Com m k
    LLC_If at c t f ->
      optIf id (LLC_If at) c t f
    LLC_Switch at ov csm ->
      optSwitch id LLC_Com LLC_Switch at ov csm
    LLC_While at asn inv cond body k -> do
      inv' <- newScope $ doClearMaps $ opt inv
      optWhile (\asn' cond' body' k' -> LLC_While at asn' inv' cond' body' k') asn cond body k
    LLC_Continue at asn ->
      LLC_Continue at <$> opt asn
    LLC_FromConsensus at1 at2 fs s ->
      LLC_FromConsensus at1 at2 fs <$> (maybeClearMaps $ focus_all $ opt s)
    LLC_ViewIs at vn vk a k ->
      LLC_ViewIs at vn vk <$> opt a <*> opt k
  gcs = \case
    LLC_Com m k -> gcs m >> gcs k
    LLC_If _ _ t f -> gcs t >> gcs f
    LLC_Switch _ _ csm -> gcsSwitch csm
    LLC_While _ _ _ cond body k -> gcs cond >> gcs body >> gcs k
    LLC_Continue {} -> return ()
    LLC_FromConsensus _ _ _ s -> gcs s
    LLC_ViewIs _ _ _ _ k -> gcs k

_opt_dbg :: Show a => App a -> App a
_opt_dbg m = do
  e <- ask
  let f = eFocus e
  liftIO $ putStrLn $ show $ f
  fm <- liftIO $ readIORef $ eEnvsR e
  let mce = M.lookup f fm
  let ced = fmap ceReplaced mce
  liftIO $ putStrLn $ show $ ced
  x <- m
  liftIO $ putStrLn $ "got " <> show x
  return x

opt_mtime :: (Optimize a, Optimize b) => AppT (Maybe (a, b))
opt_mtime = \case
  Nothing -> pure $ Nothing
  Just (d, s) -> Just <$> (pure (,) <*> (focus_con $ opt d) <*> (newScope $ opt s))

gcs_mtime :: (Optimize b) => ConstT (Maybe (a, b))
gcs_mtime = \case
  Nothing -> return ()
  Just (_, s) -> gcs s

instance Optimize DLPayAmt where
  opt (DLPayAmt {..}) = DLPayAmt <$> opt pa_net <*> opt pa_ks
  gcs _ = return ()

opt_send :: AppT (SLPart, DLSend)
opt_send (p, DLSend isClass args amta whena) =
  focus_one p $
    (,) p <$> (DLSend isClass <$> opt args <*> opt amta <*> opt whena)

instance Optimize LLStep where
  opt = \case
    LLS_Com m k -> optCom LLS_Com m k
    LLS_Stop at -> pure $ LLS_Stop at
    LLS_ToConsensus at lct send recv mtime ->
      LLS_ToConsensus at <$> opt lct <*> send' <*> recv' <*> mtime'
      where
        send' = M.fromList <$> mapM opt_send (M.toList send)
        k' = newScope $ focus_con $ opt $ dr_k recv
        recv' = (\k -> recv {dr_k = k}) <$> k'
        mtime' = opt_mtime mtime
  gcs = \case
    LLS_Com m k -> gcs m >> gcs k
    LLS_Stop _ -> return ()
    LLS_ToConsensus _ _ _ recv mtime -> gcs (dr_k recv) >> gcs_mtime mtime

instance Optimize DLInit where
  opt (DLInit {..}) = do
    return $
      DLInit
        { dli_maps = dli_maps
        }
  gcs _ = return ()

instance Optimize LLProg where
  opt (LLProg llp_at llp_opts llp_parts@SLParts{..} llp_init llp_exports llp_views llp_apis llp_aliases llp_events llp_step) = do
    let psl = M.keys sps_ies
    cs <- asks eConst
    let mis = dli_maps llp_init
    env0 <- liftIO $ mkEnv0 (getCounter llp_opts) (llo_droppedAsserts llp_opts) cs psl mis False mempty
    local (const env0) $ local (updateClearMaps $ llo_untrustworthyMaps llp_opts) $
      focus_ctor $
        LLProg llp_at llp_opts llp_parts <$> opt llp_init <*> opt llp_exports <*> pure llp_views
               <*> pure llp_apis <*> pure llp_aliases <*> pure llp_events <*> opt llp_step
  gcs (LLProg { llp_step }) = gcs llp_step

-- This is a bit of a hack...

instance Extract DLLetVar where
  extract = \case
    DLV_Eff -> Nothing
    DLV_Let _ v -> Just v

opt_svs :: AppT [(DLVar, DLArg)]
opt_svs = mapM $ \(v, a) -> (\x -> (v, x)) <$> opt a

instance Optimize FromInfo where
  opt = \case
    FI_Continue svs -> FI_Continue <$> opt_svs svs
    FI_Halt toks -> FI_Halt <$> opt toks
  gcs _ = return ()

instance {-# OVERLAPPING #-} (Optimize a, Optimize b, Optimize c, Optimize d, Optimize e) => Optimize (a, b, c, d, e) where
  opt (a, b, c, d, e) = (,,,,) <$> opt a <*> opt b <*> opt c <*> opt d <*> opt e
  gcs (a, b, c, d, e) = gcs a >> gcs b >> gcs c >> gcs d >> gcs e

instance Optimize ETail where
  opt = \case
    ET_Com m k -> optCom ET_Com m k
    ET_Stop at -> return $ ET_Stop at
    ET_If at c t f ->
      optIf id (ET_If at) c t f
    ET_Switch at ov csm ->
      optSwitch id ET_Com ET_Switch at ov csm
    ET_FromConsensus at vi fi k ->
      ET_FromConsensus at vi fi <$> (focus_one "" $ maybeClearMaps $ opt k)
    ET_ToConsensus {..} -> do
      ET_ToConsensus et_tc_at et_tc_from et_tc_prev <$> opt et_tc_lct <*> pure et_tc_which <*> opt et_tc_from_me <*> pure et_tc_from_msg <*> pure et_tc_from_out <*> pure et_tc_from_timev <*> pure et_tc_from_secsv <*> pure et_tc_from_didSendv <*> opt_mtime et_tc_from_mtime <*> (focus_con $ opt et_tc_cons)
    ET_While at asn cond body k -> optWhile (ET_While at) asn cond body k
    ET_Continue at asn -> ET_Continue at <$> opt asn
  gcs = \case
    ET_Com m k -> gcs m >> gcs k
    ET_Stop _ -> return ()
    ET_If _ _ t f -> gcs t >> gcs f
    ET_Switch _ _ csm -> gcsSwitch csm
    ET_FromConsensus _ _ _ k -> gcs k
    ET_ToConsensus {..} -> gcs et_tc_cons >> gcs_mtime et_tc_from_mtime
    ET_While _ _ cond body k -> gcs cond >> gcs body >> gcs k
    ET_Continue {} -> return ()

instance Optimize CTail where
  opt = \case
    CT_Com m k -> optCom CT_Com m k
    CT_If at c t f ->
      optIf id (CT_If at) c t f
    CT_Switch at ov csm ->
      optSwitch id CT_Com CT_Switch at ov csm
    CT_From at w fi ->
      CT_From at w <$> opt fi
    CT_Jump at which vs asn ->
      CT_Jump at which <$> opt vs <*> opt asn
  gcs = \case
    CT_Com m k -> gcs m >> gcs k
    CT_If _ _ t f -> gcs t >> gcs f
    CT_Switch _ _ csm -> gcsSwitch csm
    CT_From {} -> return ()
    CT_Jump {} -> return ()

instance Optimize CHandler where
  opt ch = do
    let svs = collectSvs ch
    local (\e -> e { eSvs = svs }) $
      case ch of
        C_Handler {..} ->
          C_Handler ch_at ch_int ch_from ch_last ch_svs ch_msg ch_timev ch_secsv <$> opt ch_body
        C_Loop {..} -> do
          C_Loop cl_at cl_svs cl_vars <$> opt cl_body
  gcs = \case
    C_Handler {..} -> gcs ch_body
    C_Loop {..} -> gcs cl_body

instance Optimize ViewInfo where
  opt (ViewInfo vs vi) = ViewInfo vs <$> (newScope $ opt vi)
  gcs _ = return ()

instance Optimize EPart where
  opt (EPart ep_at ep_isApi ep_interactEnv ep_tail) =
    newScope $ EPart ep_at ep_isApi ep_interactEnv <$> (focus_one "" $ opt ep_tail)
  gcs = gcs . ep_tail

updateClearMaps' :: HasUntrustworthyMaps a => a -> App b -> App b
updateClearMaps' opts m = local (updateClearMaps $ getUntrustworthyMaps opts) m

instance Optimize EPProg where
  opt (EPProg {..}) = updateClearMaps' epp_opts $
    EPProg epp_opts epp_init <$> opt epp_exports <*> opt epp_views <*> pure epp_stateSrcMap <*> pure epp_apis <*> pure epp_events <*> opt epp_m
  gcs (EPProg {..}) = gcs epp_m

instance Optimize CHandlers where
  opt (CHandlers hs) = CHandlers <$> mapM (newScope . opt) hs
  gcs (CHandlers hs) = gcs hs

instance Optimize DLViewsX where
  opt (DLViewsX a b) = DLViewsX <$> opt a <*> opt b
  gcs (DLViewsX a b) = gcs a >> gcs b

instance Optimize CPProg where
  opt (CPProg {..}) = updateClearMaps' cpp_opts $
    CPProg cpp_at cpp_opts cpp_init <$> (newScope $ opt cpp_views) <*> pure cpp_apis <*> pure cpp_events <*> opt cpp_handlers
  gcs (CPProg {..}) = gcs cpp_views >> gcs cpp_handlers

instance {-# OVERLAPS #-} (Optimize a, Optimize b) => Optimize (PLProg a b) where
  opt (PLProg {..}) = PLProg plp_at <$> opt plp_epp <*> opt plp_cpp
  gcs (PLProg {..}) = gcs plp_epp >> gcs plp_cpp

optimize_ :: (Optimize a) => Counter -> Bool -> S.Set DLVar -> a -> IO a
optimize_ c sim svs t = do
  eConstR <- newIORef $ mempty
  flip runReaderT (ConstEnv {..}) $ gcs t
  cs <- readIORef eConstR
  let csvs = M.keysSet $ M.filter (\x -> x < 2) cs
  dac <- newCounter 0
  env0 <- mkEnv0 c dac csvs [] mempty sim svs
  flip runReaderT env0 $
    opt t

opt_sim :: (Optimize a) => Counter -> a -> IO a
opt_sim c t = optimize_ c True mempty t

optimize :: (HasCounter a, Optimize a) => a -> IO a
optimize t = optimize_ (getCounter t) False mempty t
