module Reach.CompilerNL where

import Data.Text.Prettyprint.Doc
import Debug.Trace
import Reach.Compiler (CompilerOpts, output, source)
import Reach.NL_Eval
import Reach.NL_Linearize
import Reach.NL_Parser
import Reach.NL_Pretty ()

-- Main entry point
compileNL :: CompilerOpts -> IO ()
compileNL copts = do
  let out = output copts
  djp <- gatherDeps_top $ source copts
  let dp = compileBundle djp "main"
  out "dl" $ show $ pretty dp
  let linear = linearize dp
  out "nl" $ show $ pretty linear
  traceM $ "XXX Finish"
  return ()
