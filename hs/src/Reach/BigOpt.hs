module Reach.BigOpt (bigopt) where

import Control.Monad
import Reach.Optimize
import Reach.AddCounts

-- Top-down and then bottom-up optimization
bigopt1 :: (Optimize a, AC a) => a -> IO a
bigopt1 = optimize >=> add_counts

bigopt :: (Optimize a, AC a, Eq a) => a -> IO a
bigopt x = do
  x' <- bigopt1 x
  -- XXX it would be better for optimize and add_counts to say "I did
  -- something"
  case x == x' of
    True -> return $ x'
    False -> bigopt x'
