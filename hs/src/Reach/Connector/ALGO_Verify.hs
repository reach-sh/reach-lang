{-# LANGUAGE CPP #-}

module Reach.Connector.ALGO_Verify ( run ) where

import Reach.Connector.ALGO_SourceMap
import Data.Word
#ifdef REACH_EVEREST
import qualified Reach.Closed.TAF.Verify
#endif

run :: String -> CodeAndMap -> [Word8] -> [Word8] -> IO ()
#ifdef REACH_EVEREST
run = Reach.Closed.TAF.Verify.run
#else
run _ _ _ _ = return ()
#endif
