module Reach.CompilerNL where

import Data.Text.Prettyprint.Doc
import Debug.Trace
import Reach.Compiler (CompilerOpts, output, source)
import Reach.NL_Eval
import Reach.NL_Linearize
import Reach.NL_Parser
import Reach.NL_Pretty ()
import Reach.NL_EPP

-- Main entry point
compileNL :: CompilerOpts -> IO ()
compileNL copts = do
  let out = output copts
  djp <- gatherDeps_top $ source copts
  let dp = compileBundle djp "main"
  out "dl" $ show $ pretty dp
  let linear = linearize dp
  out "ll" $ show $ pretty linear
  let projected = epp linear
  out "pl" $ show $ pretty projected
  traceM $ "XXX Finish"
  return ()
