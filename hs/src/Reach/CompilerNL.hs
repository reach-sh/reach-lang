module Reach.CompilerNL where

import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import Debug.Trace
import Reach.NL_EPP
import Reach.NL_Eval
import Reach.NL_Linearize
import Reach.NL_Parser
import Reach.NL_Pretty ()
import Reach.Verify

data CompilerOpts = CompilerOpts
  { output :: T.Text -> String
  , source :: FilePath
  , -- | Enable experimental connectors
    expCon :: Bool
  }

compileNL :: CompilerOpts -> IO ()
compileNL copts = do
  let outn = output copts
  let out = writeFile . outn
  djp <- gatherDeps_top $ source copts
  let dl = compileBundle djp "main"
  out "dl" $ show $ pretty dl
  let ll = linearize dl
  out "ll" $ show $ pretty ll
  verify outn ll
  let pl = epp ll
  out "pl" $ show $ pretty pl
  traceM $ "XXX connectors"
  traceM $ "XXX backends"
  return ()
