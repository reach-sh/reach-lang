module Reach.APICut
  ( apicut
  , APICutError (..)
  )
where

import Control.Monad.Reader
import Data.IORef
import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as M
import Data.Foldable
import Data.Maybe
import Data.Monoid
import Generics.Deriving (Generic)
import Reach.AST.Base
import Reach.AST.DLBase
import Reach.AST.PL
import Reach.AST.EP
import Reach.CollectCounts
import Reach.Counter
import Reach.Freshen
import Reach.Texty
import Reach.Util
import Data.Tuple.Extra

-- What is going on in this file?
--
-- We are transforming a program that is like
--
-- a;
-- X.publish();
-- b;
-- commit();
-- P.only(() => {
--   const dom = interact.in(); });
-- P.publish(dom);
-- c;
-- P.only(() => {
--   interact.out(rng); });
-- d;
-- commit();
-- e;
--
-- into
--
-- commit();
-- P.only(() => {
--   const dom = interact.in(); });
-- P.publish(dom);
-- c;
-- P.only(() => {
--   interact.out(rng); });
-- d;
--
-- by "seek"ing to the first `in`, then slurping to the after the first `out`.
-- But, also, we deal with the whiles, switches, continues, and all that stuff
-- too.

data APICutError
  = API_NoIn String
  | API_OutBeforeIn String
  | API_Twice String
  | API_NoOut String
  | API_NonCS [DLVar] String
  deriving (Eq, ErrorMessageForJson, ErrorSuggestions, Generic)

instance HasErrorCode APICutError where
  errPrefix = const "RAPI"
  errIndex = \case
    API_NoIn {} -> 0
    API_OutBeforeIn {} -> 1
    API_Twice {} -> 2
    API_NoOut {} -> 3
    API_NonCS {} -> 4

instance Show APICutError where
  show = \case
    API_NoIn p -> p <> "does not occur in program"
    API_OutBeforeIn p -> p <> "calls interact.out() before interact.in()"
    API_Twice p -> p <> "occurs multiple times in program"
    API_NoOut p -> p <> "does not always return result in same consensus step"
    API_NonCS vs p -> p <> "refers to non-consensus state: " <> (show $ pretty $ map showErr vs)

type App = ReaderT Env IO

data EWhileT = EWhileT (Maybe (EWhileT, DLBlock, ETail, ETail))

data Env = Env
  { eWho :: SLPart
  , eSeenOut :: Bool
  , eSeenOutR :: IORef Bool
  , eSeenInR :: IORef Bool
  , eWhile :: EWhileT
  , eCounter :: Counter
  , eBeforeFirstTC :: Bool
  , eConstsR :: IORef (Seq.Seq DLStmt)
  }

class Contains a where
  has :: (DLExpr -> Bool) -> a -> Bool

instance Contains DLStmt where
  has q = \case
    DL_Nop _ -> False
    DL_Let _ _ e -> q e
    DL_ArrayMap {} -> False
    DL_ArrayReduce {} -> False
    DL_Var {} -> False
    DL_Set {} -> False
    DL_LocalDo _ _ t -> has q t
    DL_LocalIf _ _ _ t f -> has q t || has q f
    DL_LocalSwitch _ _ csm -> has q csm
    DL_Only _ (Right _) t -> has q t
    DL_Only {} -> False
    DL_MapReduce {} -> False

instance Contains DLTail where
  has q = \case
    DT_Return {} -> False
    DT_Com s t -> has q s || has q t

instance Contains ETail where
  has q = \case
    ET_Com ds et -> has q ds || has q et
    ET_Stop _ -> False
    ET_If _ _ et et' -> has q et || has q et'
    ET_Switch _ _ m -> any (has q . thd3) m
    ET_FromConsensus _ _ _ et -> has q et
    ET_ToConsensus _ _ _ _ _ _ _ _ _ _ _ (Just (_, et')) et -> has q et || has q et'
    ET_ToConsensus _ _ _ _ _ _ _ _ _ _ _ _ et -> has q et
    ET_While _ _ _ et et' -> has q et || has q et'
    ET_Continue _ _ -> False

third :: (a, b, c) -> c
third (_, _, z) = z

csml :: SwitchCases a -> [a]
csml = map (third . snd) . M.toAscList

instance Contains a => Contains (SwitchCases a) where
  has q = getAny . mconcat . map (Any . has q) . csml

interactX :: String -> DLExpr -> Bool
interactX t = \case
  DLE_Interact _ _ _ m _ _ -> m == t
  _ -> False

interactIn :: DLExpr -> Bool
interactIn = interactX "in"

interactOut :: DLExpr -> Bool
interactOut = interactX "out"

is_setApiDetails :: SLPart -> DLExpr -> Bool
is_setApiDetails who = \case
  DLE_setApiDetails _ p _ _ _ -> p == who
  _ -> False

err :: SrcLoc -> (String -> APICutError) -> App a
err at mk = do
  w <- asks eWho
  let msg = "API " <> show w <> " "
  expect_thrown at $ mk msg

seek :: ETail -> App (Maybe ETail)
seek = \case
  ET_Com c k -> do
    let at = srclocOf c
    when (has interactOut c) $
      err at API_OutBeforeIn
    Env {..} <- ask
    case has interactIn c of
      False -> do
        when eBeforeFirstTC $ do
          liftIO $ modifyIORef eConstsR $ flip (Seq.|>) c
        seek k
      True -> do
        (liftIO $ readIORef eSeenInR) >>= \case
          False -> liftIO $ writeIORef eSeenInR True
          True -> impossible "API called `interact.in` multiple times"
        slurp k >>= \case
          Nothing -> err at API_NoOut
          Just k' -> return $ Just $ mkCom ET_Com c k'
  ET_Stop _ -> return Nothing
  ET_If at c t f -> do
    t' <- seek t
    f' <- seek f
    let stop = ET_Stop at
    let go tt ff = return $ Just $ ET_If at c tt ff
    case (t', f') of
      (Just tt, _) | isCut tt -> return t'
      (_, Just ff) | isCut ff -> return f'
      (Just tt, _) -> go tt stop
      (_, Just ff) -> go stop ff
      _ -> return $ Nothing
  ET_Switch at x m -> do
    found <- liftIO $ newIORef Nothing
    let stop = ET_Stop at
    let f (y, z, k) = do
          seek k >>= \case
            Nothing -> return (y, z, stop)
            Just k' -> do
              liftIO $ writeIORef found (Just k')
              return (y, z, k')
    m' <- mapM f m
    (liftIO $ readIORef found) >>= \case
      Nothing -> return Nothing
      Just t' | isCut t' -> return $ Just t'
      Just _ -> return $ Just $ ET_Switch at x m'
  ET_FromConsensus x y z k ->
    seek k >>= \case
      Nothing -> return $ Nothing
      Just t | isCut t -> return $ Just t
      Just k' -> return $ Just $ ET_FromConsensus x y z k'
  ET_ToConsensus {..} -> do
    let noMore = local (\e -> e { eBeforeFirstTC = False })
    many_ <$> ((:) <$> noMore (seek et_tc_cons) <*> (mapM seek $ fmap snd (maybeToList et_tc_from_mtime)))
  ET_While {..} -> do
    b' <-
      local (\e -> e {eWhile = EWhileT (Just (eWhile e, et_w_cond, et_w_body, et_w_k))}) $
        seek et_w_body
    k' <- seek et_w_k
    return $ many_ [b', k']
  ET_Continue {} -> return Nothing
  where
    isCut = \case
      ET_FromConsensus {} -> True
      _ -> False

seekNoMore :: ETail -> App ()
seekNoMore = void . seek

locSeenOut :: App b -> App (Bool, b)
locSeenOut m = do
  seenOutR <- asks eSeenOutR
  seenOutD <- liftIO $ dupeIORef seenOutR
  res <- local (\ e -> e { eSeenOutR = seenOutD }) $ m
  seenOut <- liftIO $ readIORef seenOutD
  return (seenOut, res)

clipAtFrom :: ETail -> ETail
clipAtFrom = \case
  ET_Com c k -> ET_Com c (r k)
  ET_Stop at -> ET_Stop at
  ET_If at c t f -> ET_If at c (r t) (r f)
  ET_Switch at x m -> ET_Switch at x (M.map (\(y, z, k) -> (y, z, r k)) m)
  ET_FromConsensus at x y _ -> ET_FromConsensus at x y (ET_Stop at)
  ET_ToConsensus {} -> impossible "to consensus at start of while body"
  ET_While at asn cb b k -> ET_While at asn cb (r b) (r k)
  ET_Continue {} -> impossible "continue at start of while body"
  where
    r = clipAtFrom

-- NOTE It really sucks how similar this function is to seek
slurp :: ETail -> App (Maybe ETail)
slurp = \case
  ET_Com c k -> do
    let m = fmap (ET_Com c) <$> slurp k
    case has interactOut c of
      False -> m
      True -> do
        Env {..} <- ask
        (liftIO $ readIORef eSeenOutR) >>= \case
          False -> liftIO $ writeIORef eSeenOutR True
          -- OK because there could be different paths that call `interact.out`
          True -> return ()
        local (\e -> e {eSeenOut = True}) $
          -- We only keep reading the program because we need to grab the tail
          -- for simulation. It would be better if (a) Algorand didn't need
          -- that, or (b) if we had the simulation separate from the end-point
          -- path and put it somewhere else
          m
  ET_Stop _ -> return Nothing
  ET_If at c t f -> do
    (so_t, t') <- locSeenOut $ slurp t
    (so_f, f') <- locSeenOut $ slurp f
    let stop = ET_Stop at
    let go tt ff = return $ Just $ ET_If at c tt ff
    let alwaysSeeOut = and [so_t, so_f]
    when alwaysSeeOut $ do
      liftIO . flip writeIORef True =<< asks eSeenOutR
    case (t', f', alwaysSeeOut) of
      (Just tt, Just ff, True) -> go tt ff
      (Just tt, _, True) -> go tt stop
      (_, Just ff, True) -> go stop ff
      _ -> return $ Nothing
  ET_Switch at x m -> do
    found <- liftIO $ newIORef False
    alwaysSeeOutR <- liftIO $ newIORef Nothing
    let stop = ET_Stop at
    who <- asks eWho
    let isRace = any (has (is_setApiDetails who) . thd3) m
    let f (y, z, k) = do
          (so, k') <- locSeenOut $ slurp k
          -- We don't expect to see `interact.out` called on
          -- every `case` when we're switching on who won a `race`.
          let shouldSeeOut = case isRace of
                              True  -> has (is_setApiDetails who) k
                              False -> True
          when shouldSeeOut $ do
            liftIO $ modifyIORef alwaysSeeOutR $
              maybe (return so) $ return . (&&) so
          case k' of
            Nothing -> return (y, z, stop)
            Just et -> do
              liftIO $ writeIORef found True
              return (y, z, et)
    m' <- mapM f m
    alwaysSeeOut <- liftIO $ readIORef alwaysSeeOutR
    isFound <- liftIO $ readIORef found
    case (isFound, alwaysSeeOut) of
      (True, Just True) -> do
        liftIO . flip writeIORef True =<< asks eSeenOutR
        return $ Just $ ET_Switch at x m'
      _ -> return Nothing
  ET_FromConsensus at x y k -> do
    seekNoMore k
    ensureSeen $ return $ Just $ ET_FromConsensus at x y $ ET_Stop at
  ET_ToConsensus {..} -> do
    case et_tc_from_mtime of
      Just (_, mk) -> seekNoMore mk
      _ -> return ()
    fmap (ET_ToConsensus et_tc_at et_tc_from et_tc_prev Nothing et_tc_which et_tc_from_me et_tc_from_msg et_tc_from_out et_tc_from_timev et_tc_from_secsv et_tc_from_didSendv Nothing) <$> slurp et_tc_cons
  ET_While at asn cb b k ->
    -- Why not set eWhile here (or in doWhile)?
    --
    -- We know we're going to stop at a commit() and the only point of eWhile
    -- is to handle a continue, which we know isn't between us and the commit.
    doWhile at asn cb b k
  ET_Continue at asn ->
    asks eWhile >>= \case
      EWhileT Nothing -> impossible "continue not in while"
      EWhileT (Just (ow, cb, b, k)) ->
        local (\e -> e { eWhile = ow }) $
          doWhile at asn cb b k
  where
    ensureSeen m = do
      asks eSeenOut >>= \case
        True -> m
        False -> return $ Nothing
    doWhile at asn cb b k = do
      let m = doWhile_ at asn cb b k
      asks eSeenOut >>= \case
        True -> m
        False ->
          m >>= \case
            Nothing -> return $ Nothing
            Just _ -> err at API_NoOut
    doWhile_ at (DLAssignment m) (DLBlock _ _ ct c) b k = do
      let ml = M.toAscList m
      let (mvs, mas) = unzip ml
      let ift = ET_If at c (clipAtFrom b) k
      let condt = dtReplace ET_Com ift ct
      cnter <- asks eCounter
      (condt', mvs') <- liftIO $ freshen_ cnter condt mvs
      let ml' = zip mvs' mas
      let go (v', a) = DT_Com $ DL_Let at (DLV_Let DVC_Many v') (DLE_Arg at a)
      let asnt = foldr go (DT_Return at) ml'
      slurp $ dtReplace ET_Com condt' asnt

many_ :: [Maybe a] -> Maybe a
many_ = getFirst . mconcat . map First

_many :: (a -> App (Maybe a)) -> [a] -> App (Maybe a)
_many f l = many_ <$> mapM f l

apc :: HasCounter a => a -> (SLPart, Maybe Int) -> EPart -> IO EPart
apc hc (eWho, _) = \case
  EPart {..} | ep_isApi -> do
    let eSeenOut = False
    eSeenInR <- newIORef False
    eSeenOutR <- newIORef False
    let eWhile = EWhileT Nothing
    let eCounter = getCounter hc
    let eBeforeFirstTC = True
    eConstsR <- newIORef mempty
    let env0 = Env {..}
    et' <- flip runReaderT env0 $ do
      seek ep_tail >>= \case
        Just k' -> do
          ms <- liftIO $ readIORef eConstsR
          let mst = dtList ep_at $ toList ms
          let c' = DL_LocalDo ep_at Nothing mst
          let k'' = mkCom ET_Com c' k'
          let badVars = countsl k''
          unless (null badVars) $
            err ep_at (API_NonCS badVars)
          return k''
        Nothing -> err ep_at API_NoIn
    return $ EPart ep_at True ep_interactEnv et'
  p -> return p

apicut :: PLProg EPProg b -> IO (PLProg EPProg b)
apicut = plp_epp_mod $ \EPProg {..} -> EPProg epp_opts epp_init epp_exports epp_views epp_stateSrcMap epp_apis epp_events <$> mapWithKeyM (apc epp_opts) epp_m
