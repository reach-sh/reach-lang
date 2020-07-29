module Reach.CompilerNL where

import Reach.NL_Eval
import Reach.NL_Pretty
import Debug.Trace
import Reach.Compiler (CompilerOpts, output, source)
import Reach.NL_Parser
import Reach.NL_Linearize

-- Main entry point
compileNL :: CompilerOpts -> IO ()
compileNL copts = do
  let out = output copts
  djp <- gatherDeps_top $ source copts
  let dp = compileBundle djp "main"
  out "dl" $ show $ render_dp dp
  let linear = linearize dp
  out "nl" $ show $ render_step linear
  traceM $ "XXX Finish"
  return ()
