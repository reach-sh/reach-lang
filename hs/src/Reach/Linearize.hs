module Reach.Linearize (linearize, Error (..)) where

import Control.Monad.Reader
import Data.IORef
import Data.List.Extra
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import GHC.Stack (HasCallStack)
import Generics.Deriving (Generic)
import Reach.AST.Base
import Reach.AST.DK
import Reach.AST.DL
import Reach.AST.DLBase
import Reach.AST.LL
import Reach.Counter
import Reach.Freshen
import Reach.Texty
import Reach.Util
import Data.Tuple.Extra

data Error
  = Err_Unreachable String
  deriving (Eq, Generic, ErrorMessageForJson, ErrorSuggestions)

instance HasErrorCode Error where
  errPrefix = const "RL"

  -- These indices are part of an external interface; they
  -- are used in the documentation of Error Codes.
  -- If a constructor is obsolete, do NOT delete it nor re-allocate its number.
  -- Add new error codes at the end.
  errIndex = \case
    Err_Unreachable {} -> 0

instance Show Error where
  show = \case
    Err_Unreachable s -> "code must not be reachable: " <> s

-- Remove returns, duplicate continuations, and transform into dk

type DKApp = ReaderT DKEnv IO

type LLRetRHS = (DLVar, Bool, DKTail)

type LLRet = (Int, LLRetRHS)

data Handler = Handler
  { hV :: DLVar
  , hK :: DKTail
  }

data DKEnv = DKEnv
  { eRet :: Maybe LLRet
  , eExnHandler :: Maybe Handler
  }

withReturn :: Int -> LLRetRHS -> DKApp a -> DKApp a
withReturn rv rvv = local (\e -> e {eRet = Just (rv, rvv)})

data DKBranchMode
  = DKBM_Con
  | DKBM_Do

getDKBM :: IsLocal a => a -> DKApp DKBranchMode
getDKBM x =
  case isLocal x of
    False -> return $ DKBM_Con
    True -> do
      asks eRet >>= \case
        Nothing -> return $ DKBM_Do
        Just (_, (_, True, _)) -> return $ DKBM_Con
        Just (_, (_, False, _)) -> return $ DKBM_Do

dk_block :: SrcLoc -> DLSBlock -> DKApp DKBlock
dk_block _ (DLSBlock at fs l a) =
  DKBlock at fs <$> dk_top at l <*> pure a

turnVarIntoLet :: Bool
turnVarIntoLet = True

d_inv :: (Applicative m) => (a -> m b) -> DLInvariant a -> m (DLInvariant b)
d_inv block (DLInvariant inv_b minv_lab) =
  DLInvariant <$> block inv_b <*> pure minv_lab

getRet :: Maybe DLVar -> DKApp (Maybe DLVar)
getRet = \case
  Just a  -> return $ Just a
  Nothing -> do
    ret <- asks eRet
    return $ fst3 . snd <$> ret

dk1 :: DKTail -> DLSStmt -> DKApp DKTail
dk1 k s =
  case s of
    DLS_Let at mdv de -> com $ DKC_Let at mdv de
    DLS_ArrayMap at ans xs as i f ->
      com' $ DKC_ArrayMap at ans xs as i <$> dk_block at f
    DLS_ArrayReduce at ans xs z b as i f ->
      com' $ DKC_ArrayReduce at ans xs z b as i <$> dk_block at f
    DLS_If at mans c _ t f -> do
      -- If `mans = Just dv`, we already know that this `if` will set dv.
      -- If we do not have that metadata, try to lookup in the env what DLVar the return value will be assigned to.
      ans <- getRet mans
      let con = DK_If at ans c
      let loc t' f' = DK_Com (DKC_LocalIf at ans c t' f') k
      let mt = DK_Stop at
      (mk, k') <-
        getDKBM s >>= \case
          DKBM_Con -> return $ (con, k)
          DKBM_Do -> return $ (loc, mt)
      mk <$> dk_ k' t <*> dk_ k' f
    DLS_Switch at v _ (SwitchCases csm) -> do
      let con = DK_Switch at v . SwitchCases
      let loc csm' = DK_Com (DKC_LocalSwitch at v (SwitchCases csm')) k
      let mt = DK_Stop at
      (mk, k') <-
        getDKBM s >>= \case
          DKBM_Con -> return $ (con, k)
          DKBM_Do -> return $ (loc, mt)
      let cm1 (SwitchCase dv' l) = SwitchCase dv' <$> dk_ k' l
      csm' <- mapM cm1 csm
      case all ((== k) . sc_k) csm' of
        True -> return k
        False -> return $ mk csm'
    DLS_Return at ret da ->
      asks eRet >>= \case
        Nothing -> impossible $ "return not in prompt"
        Just (ret', (dv, isCon, rk)) ->
          case ret == ret' of
            False ->
              impossible $ "return not nested: " <> show (ret, ret')
            True ->
              case turnVarIntoLet && isCon of
                True ->
                  return $ DK_Com (DKC_Let at (DLV_Let DVC_Many dv) $ DLE_Arg at da) rk
                False ->
                  return $ DK_Com (DKC_Set at dv da) rk
    DLS_Prompt at dv@(DLVar _ _ _ ret) _ ss ->
      case isLocal s of
        True -> do
          ss' <- withReturn ret (dv, False, DK_Stop at) $ dk_top at ss
          return $ DK_Com (DKC_Var at dv) $ DK_Com (DKC_LocalDo at (Just dv) ss') k
        False ->
          withReturn ret (dv, True, k) $
            case turnVarIntoLet of
              True -> dk_ k ss
              False -> DK_Com (DKC_Var at dv) <$> dk_ k ss
    DLS_Stop at -> return $ DK_Stop at
    DLS_Unreachable at fs m -> return $ DK_Unreachable at fs m
    DLS_ToConsensus at send recv mtime -> do
      let cs0 = dr_k recv
      let cs =
            case cs0 of
              -- We are forcing an initial switch to be in CPS, assuming that
              -- this is a fork and that this is a good idea
              ((Seq.:<|) (DLS_Prompt pa pb pc ((Seq.:<|) (DLS_Switch sa swb sc sd) Seq.Empty)) r) ->
                ((Seq.<|) (DLS_Prompt pa pb (go pc) ((Seq.<|) (DLS_Switch sa swb (go sc) sd) Seq.empty)) r)
                where
                  go x = x {sa_local = False}
              _ -> cs0
      cs' <- dk_ k cs
      let recv' = recv {dr_k = cs'}
      let go (ta, time_ss) = (,) ta <$> dk_ k time_ss
      DK_ToConsensus at send recv' <$> mapM go mtime
    DLS_FromConsensus at fs ss -> DK_FromConsensus at at fs <$> dk_ k ss
    DLS_While at asn invs cond_b body -> do
      let body' = dk_top at body
      let block = dk_block at
      invs' <- forM invs $ d_inv block
      DK_While at asn invs' <$> block cond_b <*> body' <*> pure k
    DLS_Continue at asn -> return $ DK_Continue at asn
    DLS_FluidSet at fv a -> com $ DKC_FluidSet at fv a
    DLS_FluidRef at v fv -> com $ DKC_FluidRef at v fv
    DLS_MapReduce at mri ans x z b mk a f ->
      com' $ DKC_MapReduce at mri ans x z b mk a <$> dk_block at f
    DLS_Only at who ss ->
      com' $ DKC_Only at who <$> dk_top at ss
    DLS_Throw at da _ -> do
      asks eExnHandler >>= \case
        Nothing ->
          impossible "dk: encountered `throw` without an exception handler"
        Just (Handler {..}) ->
          com'' (DKC_Let at (DLV_Let DVC_Many hV) $ DLE_Arg at da) hK
    DLS_Try _at e hV hs -> do
      hK <- dk_ k hs
      local (\env -> env {eExnHandler = Just (Handler {..})}) $
        dk_ k e
    DLS_ViewIs at vn vk mva -> do
      mva' <- maybe (return $ Nothing) (\eb -> Just <$> dk_eb eb) mva
      return $ DK_ViewIs at vn vk mva' k
    DLS_TokenMetaGet ty at dv a mp -> com $ DKC_TokenMetaGet ty at dv a mp
    DLS_TokenMetaSet ty at a v mp i -> com $ DKC_TokenMetaSet ty at a v mp i
  where
    com :: DKCommon -> DKApp DKTail
    com = flip com'' k
    com' :: DKApp DKCommon -> DKApp DKTail
    com' m = com =<< m
    com'' :: DKCommon -> DKTail -> DKApp DKTail
    com'' m k' = return $ DK_Com m k'

dk_ :: DKTail -> DLStmts -> DKApp DKTail
dk_ k = \case
  Seq.Empty -> return k
  s Seq.:<| ks -> flip dk1 s =<< dk_ k ks

dk_top :: SrcLoc -> DLStmts -> DKApp DKTail
dk_top = dk_ . DK_Stop

dk_eb :: DLSExportBlock -> DKApp DKExportBlock
dk_eb (DLinExportBlock at vs b) =
  resetDK $ DLinExportBlock at vs <$> dk_block at b

resetDK :: DKApp a -> DKApp a
resetDK = local (\e -> e {eRet = Nothing, eExnHandler = Nothing})

dekont :: DLProg -> IO DKProg
dekont (DLProg dlp_at dlp_opts dlp_parts dlp_init dlp_exports dlp_views dlp_apis dlp_aliases dlp_events dlp_stmts) = do
  let eRet = Nothing
  let eExnHandler = Nothing
  flip runReaderT (DKEnv {..}) $ do
    dex' <- mapM dk_eb dlp_exports
    DKProg dlp_at dlp_opts dlp_parts dlp_init dex' dlp_views dlp_apis dlp_aliases dlp_events <$> dk_top dlp_at dlp_stmts

-- Lift common things to the previous consensus
type LCApp = ReaderT LCEnv IO

type LCAppT a = a -> LCApp a

data LCEnv = LCEnv
  { eLifts :: Maybe (IORef (Seq.Seq DKCommon))
  }

-- FIXME: I think this always returns True
class CanLift a where
  canLift :: a -> Bool

instance CanLift DLExpr where
  canLift = isLocal

instance CanLift a => CanLift (SwitchCases a) where
  canLift (SwitchCases m) = getAll $ mconcatMap (All . canLift) $ M.elems m

instance CanLift a => CanLift (SwitchCase a) where
  canLift (SwitchCase {..}) = canLift sc_k

instance CanLift DKTail where
  canLift = \case
    DK_Stop {} -> True
    DK_Com m k -> canLift m && canLift k
    _ -> impossible "canLift on DKLTail"

instance CanLift DKBlock where
  canLift = \case
    DKBlock _ _ t _ -> canLift t

instance CanLift DKCommon where
  canLift = \case
    DKC_Let _ _ e -> canLift e
    DKC_ArrayMap _ _ _ _ _ f -> canLift f
    DKC_ArrayReduce _ _ _ _ _ _ _ f -> canLift f
    DKC_Var {} -> True
    DKC_Set {} -> True
    DKC_LocalDo _ _ t -> canLift t
    DKC_LocalIf _ _ _ t f -> canLift t && canLift f
    DKC_LocalSwitch _ _ csm -> canLift csm
    DKC_MapReduce _ _ _ _ _ _ _ _ f -> canLift f
    DKC_FluidSet {} -> True
    DKC_FluidRef {} -> True
    DKC_Only {} -> False --- XXX maybe okay
    DKC_setApiDetails {} -> False
    DKC_TokenMetaGet {} -> True
    DKC_TokenMetaSet {} -> True

noLifts :: LCApp a -> LCApp a
noLifts = local (\e -> e {eLifts = Nothing})

doLift :: DKCommon -> LCApp DKTail -> LCApp DKTail
doLift m mk = do
  LCEnv {..} <- ask
  case (eLifts, canLift m) of
    (Just lr, True) -> do
      liftIO $ modifyIORef lr (Seq.|> m)
      mk
    _ -> DK_Com m <$> mk

captureLifts :: SrcLoc -> LCApp DKTail -> LCApp DKTail
captureLifts at mc = do
  lr <- liftIO $ newIORef mempty
  c <- local (\e -> e {eLifts = Just lr}) mc
  ls <- liftIO $ readIORef lr
  return $ DK_LiftBoundary at $ foldr DK_Com c ls

class LiftCon a where
  lc :: LCAppT a

instance LiftCon a => LiftCon (Maybe a) where
  lc = \case
    Nothing -> return $ Nothing
    Just x -> Just <$> lc x

instance LiftCon z => LiftCon (a, z) where
  lc (a, z) = (\z' -> (a, z')) <$> lc z

instance LiftCon z => LiftCon (DLRecv z) where
  lc r = (\z' -> r {dr_k = z'}) <$> lc (dr_k r)

instance LiftCon a => LiftCon (SwitchCases a) where
  lc (SwitchCases m) = SwitchCases <$> mapM lc m

instance LiftCon a => LiftCon (SwitchCase a) where
  lc (SwitchCase {..}) = SwitchCase sc_vl <$> lc sc_k

instance LiftCon DKBlock where
  lc (DKBlock at sf b a) =
    DKBlock at sf <$> lc b <*> pure a

instance LiftCon a => LiftCon (DLinExportBlock a) where
  lc (DLinExportBlock at vs b) =
    DLinExportBlock at vs <$> lc b

instance LiftCon DKExports where
  lc = mapM lc

instance LiftCon DKTail where
  lc = \case
    DK_Com m k -> doLift m (lc k)
    DK_Stop at -> return $ DK_Stop at
    DK_ToConsensus at send recv mtime -> do
      DK_ToConsensus at send <$> (noLifts $ lc recv) <*> lc mtime
    DK_If at mans c t f -> DK_If at mans c <$> lc t <*> lc f
    DK_Switch at v csm -> DK_Switch at v <$> lc csm
    DK_FromConsensus at1 at2 fs k ->
      captureLifts at1 $ DK_FromConsensus at1 at2 fs <$> lc k
    DK_While at asn inv cond body k ->
      DK_While at asn inv cond <$> lc body <*> lc k
    DK_Continue at asn -> return $ DK_Continue at asn
    DK_ViewIs at vn vk a k ->
      DK_ViewIs at vn vk a <$> lc k
    DK_Unreachable at fs m ->
      expect_throw (Just fs) at $ Err_Unreachable m
    DK_LiftBoundary {} ->
      impossible "lift boundary before liftcon"

liftcon :: DKProg -> IO DKProg
liftcon (DKProg dkp_at dkp_opts dkp_parts dkp_init dkp_exports dkp_views dkp_apis dkp_aliases dkp_events dkp_tail) = do
  let eLifts = Nothing
  flip runReaderT (LCEnv {..}) $
    DKProg dkp_at dkp_opts dkp_parts dkp_init <$>
      lc dkp_exports <*> pure dkp_views <*> pure dkp_apis <*> pure dkp_aliases <*> pure dkp_events <*> lc dkp_tail

-- Remove fluid variables and convert to proper linear shape
type FluidEnv = M.Map FluidVar (SrcLoc, DLArg)

type FVMap = M.Map FluidVar DLVar

type DFApp = ReaderT DFEnv IO

data DFEnv = DFEnv
  { eCounter_df :: Counter
  , eFVMm :: Maybe FVMap
  , eFVE :: FluidEnv
  , eFVs :: [FluidVar]
  , eBals :: Integer
  }

instance HasCounter DFEnv where
  getCounter = eCounter_df

fluidRefm :: FluidVar -> DFApp (Maybe (SrcLoc, DLArg))
fluidRefm fv = do
  DFEnv {..} <- ask
  return $ M.lookup fv eFVE

fluidRef :: SrcLoc -> FluidVar -> DFApp (SrcLoc, DLArg)
fluidRef at fv = do
  r <- fluidRefm fv
  case r of
    Nothing -> impossible $ "fluid ref unbound: " <> show fv <> " at: " <> show at
    Just x -> return x

fluidSet :: FluidVar -> (SrcLoc, DLArg) -> DFApp a -> DFApp a
fluidSet fv fvv = local (\e@DFEnv {..} -> e {eFVE = M.insert fv fvv eFVE})

withWhileFVMap :: FVMap -> DFApp a -> DFApp a
withWhileFVMap fvm' = local (\e -> e {eFVMm = Just fvm'})

readWhileFVMap :: DFApp FVMap
readWhileFVMap = do
  DFEnv {..} <- ask
  case eFVMm of
    Nothing -> impossible "attempt to read fvm with no fvm"
    -- Do not treat the `tokens` array as mutable in loops.
    Just x -> return $ M.delete FV_tokens x

unpackFVMap :: SrcLoc -> DKTail -> DFApp DKTail
unpackFVMap at k = do
  fvm <- readWhileFVMap
  let go k' (fv, dv) = DK_Com (DKC_FluidSet at fv (DLA_Var dv)) k'
  let k' = foldl' go k (M.toList fvm)
  return $ k'

block_unpackFVMap :: SrcLoc -> DKBlock -> DFApp DKBlock
block_unpackFVMap uat (DKBlock at fs t a) =
  (\x -> DKBlock at fs x a) <$> unpackFVMap uat t

expandFromFVMap :: SrcLoc -> DLAssignment -> DFApp DLAssignment
expandFromFVMap at (DLAssignment updatem) = do
  fvm <- readWhileFVMap
  let go (fv, dv) = do
        (_, da) <- fluidRef at fv
        return $ (dv, da)
  fvm'l <- mapM go $ M.toList fvm
  let updatem' = M.union (M.fromList $ fvm'l) updatem
  return $ DLAssignment updatem'

tokenInfoType :: DFApp DLType
tokenInfoType = do
  eBals <- asks eBals
  return $ T_Array tokenInfoElemTy eBals

tokenArrType :: DFApp DLType
tokenArrType = do
  eBals <- asks eBals
  return $ T_Array T_Token eBals

assign :: SrcLoc -> DLVar -> DLExpr -> DLStmt
assign at dv de = DL_Let at (DLV_Let DVC_Many dv) de

mkVar :: SrcLoc -> String -> DLType -> DFApp DLVar
mkVar at lab ty = DLVar at (Just (at, lab)) ty <$> allocVarIdx

lookupTokenIdx :: SrcLoc -> DLArg -> DLArg -> DFApp ([DLStmt], DLArg)
lookupTokenIdx at tok toks = do
  let asn = assign at
  let uint = T_UInt UI_Word
  let accTy = T_Tuple [T_Bool, uint]
  init_acc_dv <- mkVar at "initAcc" $ accTy
  acc_dv <- mkVar at "acc" $ accTy
  reduce_res <- mkVar at "res" $ accTy
  succ_acc <- mkVar at "succAcc" $ accTy
  bl_res <- mkVar at "bl" $ accTy
  fail_acc <- mkVar at "failAcc" $ accTy
  elem_dv <- mkVar at "elem" T_Token
  tok_idx <- mkVar at "tokIdx" uint
  idx' <- mkVar at "searchIdx'" uint
  idx <- mkVar at "searchIdx" uint
  toks_eq <- mkVar at "toksEq" T_Bool
  cnd <- mkVar at "cnd" T_Bool
  found <- mkVar at "isFound" T_Bool
  found' <- mkVar at "isFound'" T_Bool
  i_dv <- mkVar at "arrIdx" uint
  -- ([is_found, idx], tok') =>
  --    let acc' = (is_found || tok == tok') ? [ true, idx ] : [ false, idx + 1 ];
  --    return acc';
  let block_tl =
        DT_Com (asn found $ DLE_TupleRef at (DLA_Var acc_dv) 0) $
        DT_Com (asn idx $ DLE_TupleRef at (DLA_Var acc_dv) 1) $
        DT_Com (asn toks_eq $ DLE_PrimOp at TOKEN_EQ [DLA_Var elem_dv, tok]) $
        DT_Com (asn cnd $ DLE_PrimOp at IF_THEN_ELSE [DLA_Var found, DLA_Literal $ DLL_Bool True, DLA_Var toks_eq]) $
        DT_Com (asn idx' $ DLE_PrimOp at (ADD UI_Word PV_Veri) [DLA_Var idx, DLA_Literal $ DLL_Int at UI_Word 1]) $
        DT_Com (asn fail_acc $ DLE_LArg at $ DLLA_Tuple [DLA_Literal $ DLL_Bool False, DLA_Var idx']) $
        DT_Com (asn succ_acc $ DLE_LArg at $ DLLA_Tuple [DLA_Literal $ DLL_Bool True, DLA_Var idx]) $
        DT_Com (asn bl_res $ DLE_PrimOp at IF_THEN_ELSE [DLA_Var cnd, DLA_Var succ_acc, DLA_Var fail_acc]) $
        DT_Return at
  let bl = DLBlock at [] block_tl $ DLA_Var bl_res
  let ss =
        [ asn init_acc_dv $ DLE_LArg at $ DLLA_Tuple [DLA_Literal $ DLL_Bool False, DLA_Literal $ DLL_Int at UI_Word 0]
        , DL_ArrayReduce at (v2lv reduce_res) [toks] (DLA_Var init_acc_dv) (v2vl acc_dv) [v2vl elem_dv] (v2vl i_dv) bl
        , asn tok_idx $ DLE_TupleRef at (DLA_Var reduce_res) 1
        , asn found' $ DLE_TupleRef at (DLA_Var reduce_res) 0
        , DL_Let at DLV_Eff $ DLE_Claim at [] CT_Assert (DLA_Var found') $ Just "Token is tracked" ]
  return (ss, DLA_Var tok_idx)

df_com :: HasCallStack => (DLStmt -> a -> a) -> (DKTail -> DFApp a) -> DKTail -> DFApp a
df_com mkk back = \case
  DK_Com (DKC_FluidSet at fv da) k -> do
    fluidSet fv (at, da) (back k)
  DK_Com (DKC_FluidRef at dv fv) k -> do
    (at', da) <- fluidRef at fv
    mkk <$> (pure $ DL_Let at (DLV_Let DVC_Many dv) (DLE_Arg at' da)) <*> back k
  DK_Com (DKC_TokenMetaGet meta at res tok mpos) k -> do
    let asn = assign at
    (_, tokA)  <- fluidRef at FV_tokens
    (_, infos) <- fluidRef at FV_tokenInfos
    (lookup_ss, idx) <- case mpos of
              Just i  -> return ([], DLA_Literal $ DLL_Int at UI_Word $ fromIntegral i)
              Nothing -> lookupTokenIdx at tok tokA
    let meta_idx = fromIntegral $ fromEnum meta
    tokInfo <- mkVar at "tokInfo" tokenInfoElemTy
    let ss =
          [ asn tokInfo $ DLE_ArrayRef at infos idx
          , asn res $ DLE_TupleRef at (DLA_Var tokInfo) meta_idx ]
    rst <- flip (foldr mkk) ss <$> back k
    return $ foldr mkk rst lookup_ss
  DK_Com (DKC_TokenMetaSet meta at tok newVal mpos init_tok) k -> do
    let asn = assign at
    (_, tokA) <- fluidRef at FV_tokens
    (_, infos) <- fluidRef at FV_tokenInfos
    (lookup_ss, idx) <- case mpos of
      Just i -> return ([], DLA_Literal $ DLL_Int at UI_Word $ fromIntegral i)
      Nothing -> lookupTokenIdx at tok tokA
    infoTy <- tokenInfoType
    info <- mkVar at "tokInfo" tokenInfoElemTy
    infos' <- mkVar at "tokInfos'" infoTy
    info' <- mkVar at "tokInfo'" tokenInfoElemTy
    let newValIndex = case meta of
          TM_Balance -> 0
          TM_Supply -> 1
          TM_Destroyed -> 2
    let bs =
          [ asn info $ DLE_ArrayRef at infos idx
          , asn info' $ DLE_TupleSet at (DLA_Var info) newValIndex newVal
          , asn infos' $ DLE_ArraySet at infos idx (DLA_Var info')
          ]
    let fs = DKC_FluidSet at FV_tokenInfos $ DLA_Var infos'
    -- If we're initializing a token's balance, set the appropriate
    -- index in the `tokens` array to `tok`
    as <-
      case init_tok of
        False -> return [fs]
        True -> do
          tokA' <- mkVar at "tokens'" =<< tokenArrType
          return [ fs
                 , DKC_Let at (DLV_Let DVC_Many tokA') $ DLE_ArraySet at tokA idx tok
                 , DKC_FluidSet at FV_tokens $ DLA_Var tokA' ]
    rst <- flip (foldr mkk) bs <$> rec (foldl' (flip DK_Com) k as)
    return $ foldr mkk rst lookup_ss
  DK_Com m k -> do
    m' <-
      case m of
        DKC_Let a b c -> do
          return $ DL_Let a b c
        DKC_ArrayMap a b c d e x -> DL_ArrayMap a b c d e <$> df_bl x
        DKC_ArrayReduce a b c d e f g x -> DL_ArrayReduce a b c d e f g <$> df_bl x
        DKC_Var a b -> return $ DL_Var a b
        DKC_Set a b c -> return $ DL_Set a b c
        DKC_LocalDo a mans x -> DL_LocalDo a mans <$> df_t x
        DKC_LocalIf a mans b x y -> DL_LocalIf a mans b <$> df_t x <*> df_t y
        DKC_LocalSwitch a b x -> DL_LocalSwitch a b <$> df_csm df_t x
        DKC_MapReduce a mri b c d e f g x -> DL_MapReduce a mri b c d e f g <$> df_bl x
        DKC_Only a b c -> DL_Only a (Left b) <$> df_t c
        _ -> impossible "df_com"
    mkk m' <$> back k
  DK_ViewIs _ _ _ _ k ->
    -- This can only occur inside of the while cond & invariant and it is safe
    -- to throw out
    back k
  t -> impossible $ show $ "df_com " <> pretty t
  where
    rec = df_com mkk back

df_bl :: DKBlock -> DFApp DLBlock
df_bl (DKBlock at fs t a) =
  DLBlock at fs <$> df_t t <*> pure a

df_t :: DKTail -> DFApp DLTail
df_t = \case
  DK_Stop at -> return $ DT_Return at
  x -> df_com (mkCom DT_Com) df_t x

df_csm :: (a -> DFApp b) -> SwitchCases a -> DFApp (SwitchCases b)
df_csm f (SwitchCases m) = SwitchCases <$> mapM go m
  where
    go (SwitchCase {..}) = SwitchCase sc_vl <$> f sc_k

df_con :: DKTail -> DFApp LLConsensus
df_con = \case
  DK_If a _ c t f ->
    LLC_If a c <$> df_con t <*> df_con f
  DK_Switch a v csm -> LLC_Switch a v <$> df_csm df_con csm
  DK_While at asn invs cond body k -> do
    fvs <- eFVs <$> ask
    let go fv = do
          r <- fluidRefm fv
          case r of
            Nothing -> return $ Nothing
            Just _ -> do
              ty <- case fv of
                      FV_tokenInfos -> tokenInfoType
                      FV_tokens -> tokenArrType
                      _ -> return $ fluidVarType fv
              dv <- DLVar at (Just (sb, show $ pretty fv)) ty <$> allocVarIdx
              return $ Just (fv, dv)
    fvm <- M.fromList <$> catMaybes <$> mapM go fvs
    let body_fvs' = df_con =<< unpackFVMap at body
    --- Note: The invariant and condition can't return
    let block b = df_bl =<< block_unpackFVMap at b
    (makeWhile, k') <-
      withWhileFVMap fvm $ do
        invs' <- forM invs $ d_inv block
        (,) <$> (LLC_While at <$> expandFromFVMap at asn <*> pure invs' <*> block cond <*> body_fvs') <*> (unpackFVMap at k)
    makeWhile <$> df_con k'
  DK_Continue at asn ->
    LLC_Continue at <$> expandFromFVMap at asn
  DK_LiftBoundary at t -> do
    -- This was formerly done inside of Eval.hs, but that meant that these refs
    -- and sets would dominate the lifted ones in the step body, which defeats
    -- the purpose of lifting fluid variable interactions, so we instead build
    -- it into this pass
    tct <- fluidRef at FV_thisConsensusTime
    tcs <- fluidRef at FV_thisConsensusSecs
    fluidSet FV_lastConsensusTime tct $
      fluidSet FV_lastConsensusSecs tcs $
        fluidSet FV_baseWaitTime tct $
          fluidSet FV_baseWaitSecs tcs $
            df_con t
  DK_FromConsensus at1 at2 fs t -> do
    LLC_FromConsensus at1 at2 fs <$> df_step t
  DK_ViewIs at vn vk mva k -> do
    mva' <- maybe (return $ Nothing) (\eb -> Just <$> df_eb eb) mva
    k' <- df_con k
    return $ LLC_ViewIs at vn vk mva' k'
  x -> df_com (mkCom LLC_Com) df_con x

df_step :: DKTail -> DFApp LLStep
df_step = \case
  DK_Stop at -> return $ LLS_Stop at
  DK_ToConsensus at send recv mtime -> do
    lt <- fmap snd <$> fluidRefm FV_thisConsensusTime
    let tt = dr_time recv
    ls <- fmap snd <$> fluidRefm FV_thisConsensusSecs
    let ts = dr_secs recv
    k' <-
      df_con $
        DK_Com (DKC_Let at DLV_Eff (DLE_TimeOrder at (PGT UI_Word) lt tt)) $
          DK_Com (DKC_Let at DLV_Eff (DLE_TimeOrder at (PGE UI_Word) ls ts)) $
            dr_k recv
    let recv' = recv {dr_k = k'}
    mtime' <-
      case mtime of
        Nothing -> return $ Nothing
        Just (ta, tk) -> do
          tk' <- df_step tk
          return $ Just (ta, tk')
    let lt' = fromMaybe (DLA_Literal $ DLL_Int at UI_Word 0) lt
    return $ LLS_ToConsensus at lt' send recv' mtime'
  x -> df_com (mkCom LLS_Com) df_step x

df_eb :: DKExportBlock -> DFApp DLExportBlock
df_eb (DLinExportBlock at vs b) =
  DLinExportBlock at vs <$> df_bl b

-- Initialize the fluid arrays
df_init :: DKTail -> DFApp DKTail
df_init k = do
  eBals <- asks eBals
  infoTy <- tokenInfoType
  infoA <- mkVar sb "tokInfos" infoTy
  tokA  <- mkVar sb "tokens" $ T_Array T_Token eBals
  info  <- mkVar sb "initialInfo" tokenInfoElemTy
  let false = DLA_Literal $ DLL_Bool False
  let zero  = DLA_Literal $ DLL_Int sb UI_Word 0
  let tokz  = DLA_Constant DLC_Token_zero
  let infos = map DLA_Var $ take (fromIntegral eBals) $ repeat info
  let asn v e = DKC_Let sb (DLV_Let DVC_Many v) e
  let cs =
        [ asn info $ DLE_LArg sb $ DLLA_Tuple [zero, zero, false]
        , asn infoA $ DLE_LArg sb $ DLLA_Array tokenInfoElemTy infos
        , DKC_FluidSet sb FV_tokenInfos $ DLA_Var infoA
        -- We keep a separate array for the token references so we can treat the token positions
        -- as if they are static once initialized.
        , asn tokA $ DLE_LArg sb $ DLLA_Array T_Token $ take (fromIntegral eBals) $ repeat tokz
        , DKC_FluidSet sb FV_tokens $ DLA_Var tokA
        ]
  return $ foldr DK_Com k cs

defluid :: DKProg -> IO LLProg
defluid (DKProg dkp_at (DLOpts {..}) dkp_parts dkp_init dkp_exports dkp_views dkp_apis dkp_aliases dkp_events dkp_tail) = do
  let llo_counter = dlo_counter
  let llo_droppedAsserts = dlo_droppedAsserts
  let llo_aem = dlo_aem
  let opts' = LLOpts {..}
  let eCounter_df = getCounter opts'
  let eFVMm = mempty
  let eFVE = mempty
  let eFVs = allFluidVars
  let eBals = fromIntegral dlo_bals - 1
  flip runReaderT (DFEnv {..}) $ do
    dex' <- mapM df_eb dkp_exports
    k' <- df_step =<< df_init dkp_tail
    return $ LLProg dkp_at opts' dkp_parts dkp_init dex' dkp_views dkp_apis dkp_aliases dkp_events k'

-- Stich it all together
linearize :: (forall a. Pretty a => T.Text -> a -> IO a) -> DLProg -> IO LLProg
linearize outm p =
  return p >>= out "dk" dekont >>= out "lc" liftcon >>= out "df" defluid >>= out "fu" freshen_top
  where
    out lab f p' = outm lab =<< f p'
