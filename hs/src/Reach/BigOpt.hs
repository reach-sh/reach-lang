module Reach.BigOpt (bigopt, bigopt_sim) where

import qualified Data.Text as T
import Reach.AST.DLBase
import Reach.AddCounts
import Reach.FixedPoint
import Reach.Optimize
import Reach.Util
import Reach.Counter

i2t :: Integer -> T.Text
i2t = s2t . show

-- Top-down and then bottom-up optimization
bigopt :: (HasCounter a, Optimize a, AC a, Eq a) => (T.Text -> a -> IO a, T.Text) -> a -> IO a
bigopt (showp, lab) = flip fixedPoint_ rec
  where
    rec i x0 = do
      x1 <- showp (i2t i <> ".opt." <> lab) =<< optimize x0
      x2 <- showp (i2t i <> ".ac." <> lab) =<< add_counts x1
      -- XXX it would be better for optimize and add_counts to say "I did
      -- something"
      return x2

bigopt_sim :: (Optimize a, AC a, Eq a) => Counter -> a -> IO a
bigopt_sim ctr = flip fixedPoint_ rec
  where
    rec _ x0 = do
      x1 <- opt_sim ctr x0
      x2 <- add_counts_sim x1
      return x2
