module Reach.CompilerNL where

import Data.Text.Prettyprint.Doc
import Debug.Trace
import Reach.NL_EPP
import Reach.NL_Eval
import Reach.NL_Linearize
import Reach.NL_Parser
import Reach.NL_Pretty ()
import Reach.Verify

import qualified Data.Text as T

data CompilerOpts = CompilerOpts
  { output :: T.Text -> String -> IO ()
  , output_name :: T.Text -> String
  , source :: FilePath
  , -- | Enable experimental connectors
    expCon :: Bool
  , verifier :: VerifierName
  }

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
  traceM $ "XXX connectors"
  traceM $ "XXX backends"
  return ()
