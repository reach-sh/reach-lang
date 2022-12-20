module Reach.CollectSvs (CollectSvs(..)) where

import qualified Data.Map as M
import qualified Data.Set as S
import Reach.AST.DLBase
import Reach.AST.CP

class CollectSvs a where
  collectSvs :: a -> S.Set DLVar

-- This does not collect the `svs` for THIS handler.
-- A handler's `svs` is an interface, so any `jump`s inside
-- of this handler need to use the same variable names as
-- the handler it is jumping to.
-- We collect the names of these interface vars so we
-- do NOT optimize those variables away.
instance CollectSvs CHandler where
  collectSvs = \case
    C_Loop {..}    -> collectSvs cl_body
    C_Handler {..} -> collectSvs ch_body

instance (CollectSvs a) => CollectSvs (SwitchCases a) where
  collectSvs (SwitchCases m) = S.unions $ map collectSvs $ M.elems m

instance CollectSvs a => CollectSvs (SwitchCase a) where
  collectSvs (SwitchCase {..}) = collectSvs sc_k

instance CollectSvs FromInfo where
  collectSvs = \case
    FI_Continue xs -> S.fromList $ map svsp_svs xs
    FI_Halt _      -> mempty

instance CollectSvs CTail where
  collectSvs = \case
    CT_Com _ ct       -> collectSvs ct
    CT_If _ _ ct ct'  -> collectSvs ct <> collectSvs ct'
    CT_Switch _ _ m   -> collectSvs m
    CT_From _ _ fi    -> collectSvs fi
    CT_Jump _ _ svs _ -> S.fromList svs
