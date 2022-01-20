module Reach.BigOpt (bigopt) where

import qualified Data.Text as T
import Reach.AST.DLBase
import Reach.AddCounts
import Reach.FixedPoint
import Reach.Optimize
import Reach.Util

i2t :: Integer -> T.Text
i2t = s2t . show

-- Top-down and then bottom-up optimization
bigopt :: (HasCounter a, Optimize a, AC a, Eq a) => (T.Text -> a -> IO (), T.Text) -> a -> IO a
bigopt (showp, lab) = flip fixedPoint_ rec
  where
    rec i x0 = do
      x1 <- optimize x0
      showp (i2t i <> ".opt." <> lab) x1
      x2 <- add_counts x1
      showp (i2t i <> ".ac." <> lab) x2
      -- XXX it would be better for optimize and add_counts to say "I did
      -- something"
      return x2
