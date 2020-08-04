module Reach.CompilerNL where

import Data.Text.Prettyprint.Doc
import Debug.Trace
import Reach.Compiler (CompilerOpts, output, output_name, source, verifier)
import Reach.NL_Eval
import Reach.NL_Linearize
import Reach.NL_Parser
import Reach.NL_Pretty ()
import Reach.NL_EPP
import Reach.Verify

-- Main entry point
compileNL :: CompilerOpts -> IO ()
compileNL copts = do
  let out = output copts
  let outn = output_name copts
  djp <- gatherDeps_top $ source copts
  let dp = compileBundle djp "main"
  out "dl" $ show $ pretty dp
  let linear = linearize dp
  out "ll" $ show $ pretty linear
  --- FIXME the verifier might be dependent on the backend/connector
  --- chosen and should not be choosable by the tool.
  verify outn (verifier copts) linear
  let projected = epp linear
  out "pl" $ show $ pretty projected
  traceM $ "XXX backends"
  return ()
