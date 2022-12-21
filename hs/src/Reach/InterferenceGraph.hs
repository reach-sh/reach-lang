module Reach.InterferenceGraph
  ( clig
  , IGg (..)
  , IGd (..)
  , Coloring
  , igVars
  , colorEasy
  , colorHardLim
  , colorHardNoLim
  ) where

import Control.Monad
import Control.Monad.Extra
import Control.Monad.Reader
import Data.Foldable
import Data.IORef
import Data.List ((\\))
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import Data.Ord
import qualified Data.Set as S
import Reach.AST.DLBase
import Reach.AST.CL
import Reach.CollectCounts
import Reach.Dotty
import Reach.FixedPoint
import Reach.Texty

-- Types and interface
type DLVarS = S.Set DLVar
newtype Graph = Graph (M.Map DLVar DLVarS)

instance Semigroup Graph where
  (Graph xm) <> (Graph ym) = Graph $ M.unionWith S.union xm ym

instance Monoid Graph where
  mempty = Graph mempty

instance Pretty Graph where
  pretty (Graph g) = dotty $ concatMap go $ M.toAscList g
    where
      go (f, ts) = map (go' f) $ S.toAscList ts
      go' f t = (show f, show t, mempty)

gVars :: Graph -> DLVarS
gVars (Graph m) = M.keysSet m

gRef :: Graph -> DLVar -> DLVarS
gRef (Graph m) x = fromMaybe mempty $ M.lookup x m

gMod :: DLVar -> (DLVarS -> DLVarS) -> Graph -> Graph
gMod x xm (Graph m) = Graph $ M.alter mf x m
  where
    mf = f . fromMaybe mempty
    f = Just . xm

gIns :: DLVar -> DLVar -> Graph -> Graph
gIns x y g = gMod x (S.insert y) g

gAdd :: DLVar -> Graph -> Graph
gAdd x g = gMod x id g

gIns2 :: DLVar -> DLVar -> Graph -> Graph
gIns2 x y g =
  case x == y of
    True -> g
    False -> gIns y x $ gIns x y g

data IGg = IGg
  { igInter :: Graph
  , igMove :: Graph
  }

igVars :: IGg -> DLVarS
igVars (IGg {..}) = gVars igInter

instance Semigroup IGg where
  (IGg xi xm) <> (IGg yi ym) = IGg (xi <> yi) (xm <> ym)

instance Monoid IGg where
  mempty = IGg mempty mempty

instance Pretty IGg where
  pretty (IGg {..}) = ""
    <> "// Interference" <> hardline
    <> pretty igInter <> hardline
    <> "// Move" <> hardline
    <> pretty igMove

data IGd a = IGd a IGg

instance (HasStateMap a) => HasStateMap (IGd a) where
  getStateMap (IGd x _) = getStateMap x

instance (HasFunVars a) => HasFunVars (IGd a) where
  getFunVars (IGd x _) = getFunVars x

instance (HasCounter a) => HasCounter (IGd a) where
  getCounter (IGd x _) = getCounter x

instance (Pretty a) => Pretty (IGd a) where
  pretty (IGd x y) = ""
    <> "// Original" <> hardline
    <> pretty x <> hardline
    <> "// Intereference Graph" <> hardline
    <> pretty y

clig :: (HasStateMap a, HasFunVars a, IG a) => a -> IO (IGd a)
clig x = do
  eIG <- newIORef mempty
  let eFunVars = getFunVars x
  let eState = getStateMap x
  let eSpecials = mempty
  let eOnces = mempty
  flip runReaderT (Env {..}) $ void $ ig (return mempty) x
  y <- readIORef eIG
  return $ IGd x y

-- Analysis

data Env = Env
  { eIG :: IORef IGg
  , eFunVars :: FunVars
  , eState :: CLState
  , eSpecials :: DLVarS
  , eOnces :: M.Map DLVar DLVarS
  }

instance HasFunVars Env where
  getFunVars = eFunVars

class AddSpecial a where
  addSpecial :: a -> App DLVarS -> App DLVarS

class Intf a where
  intf :: DLVar -> a -> App ()

class MoveVar a where
  mVar :: a -> Maybe DLVar

class IG a where
  ig :: App DLVarS -> a -> App DLVarS

class IGdef a where
  igDef :: App DLVarS -> DLVarS -> a -> App DLVarS

-- How does all this work?
--
-- The purpose of this file is to compute an interference graph of the
-- variables in the program. The main way to do this is to compute the "live
-- set" (`ls`) of what variables are live at any given point in the program. A
-- variable is live if it is used.
--
--    .                 | Live-Before | Live-After
--    let x = 5         |  { }        | { x }
--    let y = x + 1     |  { x }      | { x, y }
--    let z = x + y     |  { x, y }   | { x, z }
--    let u = x + z     |  { x, z }   | { x }
--    return x          |  { x }      | {}
--
-- ig k c = ls
--   iff
--     k  = computation returning Live-After
--     c  = the code
--     ls = computation returning Live-Before
--
-- Once we have live sets, we can make a note that particular variables
-- interfere with others. Basically, variables interfere with the variables
-- live after their definition...
--
-- u - x - z
--      \ /
--       y
--
-- This graph will be stored in `eIG.igInter` and ultimately returned
--
-- We also compute a "move graph" (`eIG.igMove`) that stores when one variable
-- is moved into the same spot as another. If we had `let v = u`, then `v - u`
-- would be in the move graph. The move graph is used to bias graph coloring
-- into making variables share the same spot (because we assume that the
-- connector can optimize away the move.)
--
--      (The point of `eFunVars` is to record the parameters to functions that
--      we can jump to so that we can insert moves from the actual arguments to
--      parameters.)
--
-- But, there are some subtleties about Reach that make it un-wise to do this
-- "by the book"...
--   1. We know that unused variables (like u) will be removed
--   2. We know that variables will be flagged with their use count, and some
--      are used once.
--   3. We know that some variables are "free" in the connector, like the
--      message sender.
--
-- We deal with 1 by ignoring them, unlike a normal analysis that might try
-- hard to find unused.
--
-- We deal with 3 by recording them in `eSpecials` and never including them in
-- the interference graph, ever.
--
-- We deal with 2 by storing an extra map, `eOnces`, where the keys are
-- variables used only once and the value are the multi-use variables that they
-- reference (because referencing the once variable implies you will reference
-- that variable). This is because we know that the underlying connector can
-- save the RHS of the `let` and insert it directly at the only use of the
-- variable. Thus, they don't need registers, so they won't get them.
--
-- In the example above,
--   - y and z are used once
--   - u is never used
--
-- So it will actually be like:
--
--    .                 | Live-Before | Live-After | eOnces
--    let x = 5         |  { }        | { x }      | mt
--    let y = x + 1     |  { x }      | { x }      | " [ y -> {x} ]
--    let z = x + y     |  { x }      | { x }      | " [ z -> {x, y} ]
--    let u = x + z     |  { x }      | { x }      | "
--    return x          |  { x }      | {}         | "
--
-- And so the interference graph will be
--
--    x
--
-- `ig` is in continuation passing style to make it possible to record `eOnces`
-- in the `local` state. But, the computation has to actually go from the
-- bottom, because we are concerned about Live-After
--
-- --
--
-- We do a few cute class-y things to make the code easier to write
--
-- - `MoveVar/mVar x` is used to abstract over extracting what variable (in
--   `x`) is being moved. It is useful as a type class so we don't have to
--   plumb around and annotate the types
--
-- - `Intf/intf x y` is used to make a single variable (`x`) interfere with
--   everything in `y`
--
-- - `AddSpecial/addSpecial v k` is used to record that `v` is special inside
--   of `k`
--
-- - `IG` has these special values `IGseq`, `IGpar`, `Par`, and `Seq` that are
--   used to abstract whether the binding in a program is sequential or
--   parallel. This is to ensure that the Live-After values flow correctly,
--   while ensuring that `eOnces` gets to look from the top-down.
--
--   This is almost general enough to be an abstraction of all analyses in the
--   Reach compiler, but it mainly fails at doing an `fmap`.
--
modIG :: (IGg -> IGg) -> App ()
modIG f = do
  Env {..} <- ask
  liftIO $ modifyIORef eIG f

addv :: DLVar -> App ()
addv x = modIG $ \g -> g { igInter = gAdd x (igInter g) }

inter2 :: DLVar -> DLVar -> App ()
inter2 x y = modIG $ \g -> g { igInter = gIns2 x y (igInter g) }

move2 :: DLVar -> DLVar -> App ()
move2 x y = modIG $ \g -> g { igMove = gIns2 x y (igMove g) }

type App = ReaderT Env IO

instance Intf DLVar where
  intf = inter2

instance (Intf a) => Intf (S.Set a) where
  intf v = mapM_ (intf v)

instance MoveVar DLVar where
  mVar = return

instance MoveVar DLLetVar where
  mVar = \case
    DLV_Eff -> Nothing
    DLV_Let _ v -> mVar v

instance MoveVar DLVarLet where
  mVar = mVar . vl2lv

instance MoveVar DLArg where
  mVar = \case
    DLA_Var v -> return v
    _ -> Nothing

instance MoveVar DLExpr where
  mVar = \case
    DLE_Arg _ a -> mVar a
    _ -> Nothing

instance MoveVar DLBlock where
  mVar (DLBlock _ _ _ a) = mVar a

move :: (MoveVar a, MoveVar b) => a -> b -> App ()
move x y =
  case (mVar x, mVar y) of
    (Just x', Just y') -> move2 x' y'
    _ -> return ()

rm :: DLVar -> DLVarS -> DLVarS
rm = S.delete

viaCount :: Countable a => App DLVarS -> a -> App DLVarS
viaCount lsm x = S.union (countsS x) <$> lsm

closeOnces :: DLVarS -> App DLVarS
closeOnces xs = do
  o <- asks eOnces
  xs' <- liftIO $ fixedPoint_ xs $ \_ xs0 -> do
    let xtra = mconcat $ M.elems $ M.restrictKeys o xs0
    return $ xs0 <> xtra
  return $ S.difference xs' $ M.keysSet o

data IGseq a b = IGseq a b
instance (IG a, IG b) => IG (IGseq a b) where
  ig ls (IGseq x y) = ig (ig ls y) x
data IGpar a b = IGpar a b
instance (IG a, IG b) => IG (IGpar a b) where
  ig ls (IGpar x y) = do
    x' <- ig ls x
    y' <- ig ls y
    return $ S.union x' y'
newtype Par a = Par a
newtype Seq a = Seq a

instance IG DLVar where
  ig = viaCount

instance IGdef DLVarLet where
  igDef lsm vs (DLVarLet mvc v) = do
    st <- asks $ isInState v
    let mvc' = case st of
          True -> Just DVC_Many
          False -> mvc
    igDef lsm vs $
      case mvc' of
        Nothing -> DLV_Eff
        Just vc -> DLV_Let vc v

instance IGdef DLLetVar where
  igDef lsm uses = \case
    DLV_Eff -> S.union uses <$> lsm
    DLV_Let vc v -> rm v <$> do
      Env {..} <- ask
      uses' <- closeOnces uses
      let ignored = eSpecials
      let dbg_ :: String -> [(String, DLVarS)] -> App ()
          dbg_ lab l = liftIO $ do
            putStrLn $ "igDef " <> lab <> ": " <> show v
            forM_ l $ \(x, s) -> do
              putStrLn $ "  " <> x <> ": " <> show s
            putStrLn ""
      let dbg__ :: String -> [(String, DLVarS)] -> App ()
          dbg__ lab = dbg_ lab . (<>) [("ignored", ignored), ("uses", uses), ("uses'", uses)]
      let loud = False
      let dbg = if loud then dbg__ else const $ const $ return ()
      st <- asks $ isInState v
      case (vc, st) of
        (DVC_Once, False) ->
          local (\e -> e { eOnces = M.insert v uses' eOnces }) $ do
            ls <- lsm
            ls' <- closeOnces ls
            dbg "once" [("ls", ls), ("ls'", ls')]
            return ls'
        _ -> do
          ls <- lsm
          ls' <- closeOnces ls
          let ls'' = ls' <> uses'
          unless (S.member v ignored) $ do
            addv v
            let int = S.difference ls'' ignored
            intf v int
            dbg "many" [("ls", ls), ("ls'", ls'), ("ls''", ls''), ("int", int)]
          return ls''

viaDef :: (IGdef a) => App DLVarS -> a -> App DLVarS
viaDef ls x = igDef ls mempty x

instance IG DLLetVar where
  ig = viaDef

instance IG DLVarLet where
  ig = viaDef

instance IG DLArg where
  ig = viaCount

instance (Countable a) => IG (CInterval a) where
  ig = viaCount

instance IG DLStmt where
  ig ls = \case
    DL_Nop _ -> ls
    DL_Let _ x e -> do
      move x e
      igDef ls (countsS e) x
    DL_ArrayMap _ ans xs as i f -> do
      move ans f
      ig ls (IGseq ans (IGseq (Par $ i : as) (IGseq (IGseq (Seq xs) f) (varLetVar i))))
    DL_ArrayReduce _ ans xs z b as i f -> do
      move b z
      move ans f
      move b f
      ig ls (IGseq ans (IGseq z (IGseq (Par $ b : i : as) (IGseq (IGseq (varLetVar b) (IGseq (Seq xs) f)) (varLetVar i)))))
    DL_Var _ v -> ig ls (v2vl v)
    DL_Set _ v a -> do
      move v a
      ig ls a
    DL_LocalDo _ _ t -> ig ls t
    DL_LocalIf _ _ a t f -> igIf ls a t f
    DL_LocalSwitch _ v csm -> igSwitch ls v csm
    DL_Only _ _ t -> ig ls t
    DL_MapReduce _ _ ans _x z b k a f -> do
      move b z
      move ans f
      move b f
      ig ls (IGseq ans (IGseq z (IGseq (Seq $ [b, k, a]) f)))

instance IG DLTail where
  ig ls = \case
    DT_Return _ -> ls
    DT_Com m t -> ig ls (IGseq m t)

instance IG DLBlock where
  ig ls (DLBlock _ _ t a) = ig ls (IGseq t a)

instance AddSpecial DLLetVar where
  addSpecial = \case
    DLV_Eff -> id
    DLV_Let _ v -> \m ->
      S.delete v <$> local (\e -> e { eSpecials = S.insert v $ eSpecials e }) m

class IsInState a where
  isInState_ :: DLVar -> a -> Bool

instance IsInState DLVar where
  isInState_ = (==)

instance IsInState Env where
  isInState_ v = isInState_ v . eState

instance (Foldable f, IsInState a) => IsInState (f a) where
  isInState_ v = getAny . foldMap (Any . isInState_ v)

isInState :: (MoveVar a, IsInState b) => a -> b -> Bool
isInState x y =
  case mVar x of
    Nothing -> False
    Just v -> isInState_ v y

instance IG SvsGet where
  ig ls (SvsGet {..}) = do
    ig ls (IGseq svsg_var svsg_svs)

instance IG CLStmt where
  ig ls = \case
    CLDL m -> ig ls m
    CLBindSpecial _ lv _s -> asks (isInState lv) >>= \case
      True -> ig ls lv
      False -> addSpecial lv $ ls
    CLTimeCheck _ x -> ig ls x
    CLEmitPublish _ _ vs -> ig ls (Seq vs)
    CLStateBind _ _ vs _ ->
      ig ls (Seq vs)
    CLIntervalCheck _ x y z -> ig ls (IGseq (Seq [x, y]) z)
    CLStateSet _ _ vs -> do
      mapM_ (\SvsPut{..} -> move svsp_svs svsp_val) vs
      ig ls (Seq $ map svsp_val vs)
    CLTokenUntrack _ a -> ig ls a
    CLMemorySet _ _v a -> do
      -- XXX move v a
      ig ls a

instance (IG a) => IG (DLVar, Bool, a) where
  ig ls (v, used, x) = ig ls (IGseq (DLVarLet (if used then Just DVC_Many else Nothing) v) x)

instance IG CLTail where
  ig ls = \case
    CL_Com m t -> ig ls (IGseq m t)
    CL_If _ a t f -> igIf ls a t f
    CL_Switch _ v csm -> igSwitch ls v csm
    CL_Jump _ _f as True _ -> do
      ig ls (Seq as)
    CL_Jump _ f as False _ -> do
      vs <- askFunVars f
      zipWithM_ move vs as
      ig ls (Seq $ map (DLA_Var . varLetVar) vs <> as)
    CL_Halt {} -> ls

instance IG a => IG (SwitchCaseUse a) where
  ig ls (SwitchCaseUse ov _vn (SwitchCase {..})) =
    ig ls (IGseq sc_vl (IGseq ov sc_k))

instance IG a => IG (SwitchCasesUse a) where
  ig ls (SwitchCasesUse v csm) = ig ls (IGseq v (Par $ switchUses v csm))

igSwitch :: (IG a) => App DLVarS -> DLVar -> SwitchCases a -> App DLVarS
igSwitch ls v csm = ig ls (SwitchCasesUse v csm)

igIf :: (IG a, IG b) => App DLVarS -> a -> b -> b -> App DLVarS
igIf ls a t f = ig ls (IGseq a (Par [t, f]))

instance IG CLFun where
  ig ls (CLFun {..}) = ig ls (IGseq (Seq clf_dom) (IGseq (Seq (map varLetVar clf_dom)) clf_tail))

instance IG CLExtFun where
  ig ls (CLExtFun {..}) = ig ls cef_fun

instance IG CLIntFun where
  ig ls (CLIntFun {..}) = ig ls (manyifyDom cif_fun)

manyifyDom :: CLFun -> CLFun
manyifyDom (CLFun {..}) =
  CLFun clf_at (washVars clf_dom) clf_view clf_tail

washVars :: [DLVarLet] -> [DLVarLet]
washVars = map (v2vl . varLetVar)

igList :: (IG a) => App DLVarS -> [a] -> App DLVarS
igList ls = \case
  [] -> ls
  x : xs -> ig ls (IGseq x (Seq xs))

instance (IG a, Foldable t) => IG (Seq (t a)) where
  ig ls (Seq m) = igList ls $ toList m

instance (IG a, Traversable t) => IG (Par (t a)) where
  ig ls (Par m) = foldr S.union mempty <$> mapM (ig ls) m

instance IG CLProg where
  ig ls (CLProg {..}) = ig ls (IGpar (Par clp_funs) (Par clp_api))

-- Coloring
type Coloring = M.Map DLVar Int
type ColoringR = Either String (Int, Coloring)

-- This function makes the observation that if the number of variables is less
-- than the number of registers, there's no need to do any work... each
-- variable can get its own register. On Algorand, there are hundreds of
-- registers, so this is plausible.
--
-- We don't actually use this typically, because we want to exploit the move
-- graph system to get more compressable code.
colorEasy :: IGg -> DLVarS -> Int -> IO ColoringR
colorEasy i s maxColor = do
  let act = S.size s
  case act <= maxColor of
    True ->
      return $ Right $ (,) act $ M.fromList $ zip (S.toAscList s) [0..]
    False -> do
      colorHardLim i s maxColor

colorHardLim :: IGg -> DLVarS -> Int -> IO ColoringR
colorHardLim i s maxColor =
  colorSat i s $ take maxColor [0..]

colorHardNoLim :: IGg -> DLVarS -> IO ColoringR
colorHardNoLim i s = colorSat i s [0..]

type SatS = S.Set Int
colorSat :: IGg -> DLVarS -> [Int] -> IO ColoringR
colorSat (IGg {..}) s all_cs = do
  maxc <- newIORef 0
  asnr <- newIORef mempty
  w <- newIORef $ S.toAscList s
  let consult_asn cv = (maybeToList . M.lookup cv) <$> readIORef asnr
  let neighbors_colors :: Graph -> DLVar -> IO SatS
      neighbors_colors g v = do
        let ns = S.toList $ S.intersection s $ gRef g v
        ns_cs <- mconcatMapM consult_asn ns
        return $ S.fromList ns_cs
  let compute_sat :: DLVar -> IO (DLVar, SatS)
      compute_sat v = (,) v <$> neighbors_colors igInter v
  let cmp_sizes = comparing (S.size . snd)
  let extractMaxSat :: IO a -> (IO a -> DLVar -> SatS -> IO a) -> IO a
      extractMaxSat sk f = do
        readIORef w >>= \case
          [] -> sk
          cw -> do
            cw_ws <- mapM compute_sat cw
            let (v, sv) = maximumBy cmp_sizes cw_ws
            writeIORef w $ cw \\ [v]
            f sk v sv
  let tryToAssign :: IO (Either String a) -> DLVar -> SatS -> [Int] -> IO (Either String a)
      tryToAssign sk v sv = \case
        [] -> return $ Left $ "colorSat: no color opts for " <> show v
        c0 : cols ->
          case S.member c0 sv of
            True -> tryToAssign sk v sv cols
            False -> do
              modifyIORef maxc $ max c0
              modifyIORef asnr $ M.insert v c0
              sk
  let color_loop sk = extractMaxSat sk $ \sk' v sv -> do
        vs_move_ns_cs <- S.toList <$> neighbors_colors igMove v
        tryToAssign (color_loop sk') v sv $
          vs_move_ns_cs <> all_cs
  color_loop $ (Right <$> ((,) <$> readIORef maxc <*> readIORef asnr))
