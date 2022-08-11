{-# LANGUAGE CPP #-}

module Reach.Connector.ALGO_Verify ( run ) where

import qualified Data.ByteString as BS
import Data.Word
#ifdef REACH_EVEREST
import qualified Reach.Closed.TAF.Verify
#endif

run :: BS.ByteString -> [Word8] -> IO ()
#ifdef REACH_EVEREST
run = Reach.Closed.TAF.Verify.run
#else
run _ _ = return ()
#endif
