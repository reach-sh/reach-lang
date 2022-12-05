module Reach.InterferenceGraph
  ( clig
  , IGg (..)
  , IGd (..)
  ) where

import Control.Monad
import Control.Monad.Reader
import Data.Foldable
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import Reach.AST.DLBase
import Reach.AST.CL
import Reach.CollectCounts
import Reach.Dotty
import Reach.Texty
import Reach.Util

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

gIns :: DLVar -> DLVar -> Graph -> Graph
gIns x y (Graph m) = Graph $ M.alter mf x m
  where
    mf = f . fromMaybe mempty
    f = Just . S.insert y

gIns2 :: DLVar -> DLVar -> Graph -> Graph
gIns2 x y g =
  case x == y of
    True -> g
    False -> gIns y x $ gIns x y g

-- XXX add a list of "special" variables that we know are already
-- "register-like", like the FROM, STATE, etc
data IGg = IGg
  { igInter :: Graph
  , igMove :: Graph
  }

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

instance (HasCounter a) => HasCounter (IGd a) where
  getCounter (IGd x _) = getCounter x

instance (Pretty a) => Pretty (IGd a) where
  pretty (IGd x y) = ""
    <> "// Original" <> hardline
    <> pretty x <> hardline
    <> "// Intereference Graph" <> hardline
    <> pretty y

class GetFunVars a where
  getFunVars :: a -> FunVars

clig :: (GetFunVars a, IG a) => a -> IO (IGd a)
clig x = do
  eIG <- newIORef mempty
  let eFunVars = getFunVars x
  flip runReaderT (Env {..}) $ void $ ig (return mempty) x
  y <- readIORef eIG
  return $ IGd x y

-- Analysis

-- XXX add a map for what vars a mentioned-once variable uses
data Env = Env
  { eIG :: IORef IGg
  , eFunVars :: FunVars
  }

modIG :: (IGg -> IGg) -> App ()
modIG f = do
  Env {..} <- ask
  liftIO $ modifyIORef eIG f

inter2 :: DLVar -> DLVar -> App ()
inter2 x y = modIG $ \g -> g { igInter = gIns2 x y (igInter g) }

move2 :: DLVar -> DLVar -> App ()
move2 x y = modIG $ \g -> g { igMove = gIns2 x y (igMove g) }

lookupFunVars :: CLVar -> App [DLVarLet]
lookupFunVars f = do
  m <- asks eFunVars
  case M.lookup f m of
    Just x -> return x
    Nothing -> impossible $ "lookupFunVars: not in map " <> show f

type App = ReaderT Env IO

type FunVars = M.Map CLVar [DLVarLet]

class Intf a where
  intf :: DLVar -> a -> App ()

instance Intf DLVar where
  intf = inter2

instance (Intf a) => Intf (S.Set a) where
  intf v = mapM_ (intf v)

class MoveVar a where
  mVar :: a -> Maybe DLVar

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

class IG a where
  ig :: App DLVarS -> a -> App DLVarS

viaCount :: Countable a => App DLVarS -> a -> App DLVarS
viaCount lsm x = do
  ls <- lsm
  return $ S.union ls $ countsS x

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

instance IG DLVarLet where
  ig lsm (DLVarLet mvc v) = do
    ls <- lsm
    case mvc of
      Nothing -> return ()
      -- XXX should I treat things that are read once specially? Maybe put them
      -- in a special set... put them in the env and then look up later
      Just _ -> intf v ls
    return $ rm v ls

instance IG DLLetVar where
  ig ls = \case
    DLV_Eff -> ls
    DLV_Let vc v -> ig ls (DLVarLet (Just vc) v)

instance IG DLArg where
  ig = viaCount

instance (Countable a) => IG (CInterval a) where
  ig = viaCount

instance IG DLExpr where
  ig = viaCount

instance IG DLStmt where
  ig ls = \case
    DL_Nop _ -> ls
    DL_Let _ x e -> do
      move x e
      ig ls (IGseq x e)
    DL_ArrayMap _ ans xs as i f -> do
      move ans f
      ig ls (IGseq ans (IGseq (Seq xs) (IGseq (Seq $ i : as) f)))
    DL_ArrayReduce _ ans xs z b as i f -> do
      move b z
      move ans f
      move b f
      ig ls (IGseq ans (IGseq (Seq $ z : xs) (IGseq (Seq $ b : i : as) f)))
    DL_Var _ v -> ig ls (v2vl v)
    DL_Set _ v a -> do
      move v a
      ig ls a
    DL_LocalDo _ _ t -> ig ls t
    DL_LocalIf _ _ a t f -> igIf ls a t f
    DL_LocalSwitch _ v csm -> igSwitch ls v csm
    DL_Only _ _ t -> ig ls t
    DL_MapReduce _ _ ans _x z b a f -> do
      move b z
      move ans f
      move b f
      ig ls (IGseq ans (IGseq z (IGseq (Seq $ [b, a]) f)))

instance IG DLTail where
  ig ls = \case
    DT_Return _ -> ls
    DT_Com m t -> ig ls (IGseq m t)

instance IG DLBlock where
  ig ls (DLBlock _ _ t a) = ig ls (IGseq t a)

instance IG CLStmt where
  ig ls = \case
    CLDL m -> ig ls m
    CLBindSpecial _ lv _s -> ig ls lv
    CLTimeCheck _ x -> ig ls x
    CLEmitPublish _ _ vs -> ig ls (Seq vs)
    CLStateBind _ _ vs _ -> ig ls (Seq vs)
    CLIntervalCheck _ x y z -> ig ls (IGseq (Seq [x, y]) z)
    CLStateSet _ _ vs -> do
      mapM_ (uncurry move) vs
      ig ls (Seq $ map snd vs)
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
    CL_Jump _ f as _ _ -> do
      vs <- lookupFunVars f
      zipWithM_ move vs as
      ig ls (Seq as)
    CL_Halt {} -> ls

igSwitch :: (IG a) => App DLVarS -> DLVar -> SwitchCases a -> App DLVarS
igSwitch ls v csm = ig ls (IGseq v (Par csm))

igIf :: (IG a, IG b) => App DLVarS -> a -> b -> b -> App DLVarS
igIf ls a t f = ig ls (IGseq a (Par [t, f]))

instance IG CLFun where
  ig ls (CLFun {..}) = ig ls (IGseq (Seq clf_dom) clf_tail)

instance IG CLExtFun where
  ig ls (CLExtFun {..}) = ig ls cef_fun

instance IG CLIntFun where
  ig ls (CLIntFun {..}) = ig ls cif_fun

igList :: (IG a) => App DLVarS -> [a] -> App DLVarS
igList ls = \case
  [] -> ls
  x : xs -> ig ls (IGseq x (Seq xs))

instance (IG a, Foldable t) => IG (Seq (t a)) where
  ig ls (Seq m) = igList ls $ toList m

instance (IG a, Traversable t) => IG (Par (t a)) where
  ig ls (Par m) = foldr S.union mempty <$> mapM (ig ls) m

instance IG CLProg where
  ig ls (CLProg {..}) = ig ls (IGseq (Par clp_funs) (Par clp_api))

instance GetFunVars CLProg where
  getFunVars (CLProg {..}) = M.map go clp_funs
    where
      go (CLIntFun {..}) = go' cif_fun
      go' (CLFun {..}) = clf_dom
