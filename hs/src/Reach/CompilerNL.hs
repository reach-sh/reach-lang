module Reach.CompilerNL where

import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import Reach.Backend.JS_NL
import Reach.Connector.ALGO_NL
import Reach.Connector.ETH_Solidity_NL
import Reach.NL_EPP
import Reach.NL_Eval
import Reach.NL_Linearize
import Reach.NL_Parser
import Reach.NL_Pretty ()
import Reach.Verify

data CompilerOpts = CompilerOpts
  { output :: T.Text -> String
  , source :: FilePath
  , tops :: [String]
  }

compileNL :: CompilerOpts -> IO ()
compileNL copts = do
  djp <- gatherDeps_top $ source copts
  let compile1 which = do
        let outn = (output copts) . ((T.pack which <> ".") <>)
        let out = writeFile . outn
        let dl = compileBundle djp which
        out "dl" $ show $ pretty dl
        let ll = linearize dl
        out "ll" $ show $ pretty ll
        verify outn ll
        let pl = epp ll
        out "pl" $ show $ pretty pl
        --- FIXME The particular connector/backend should be part of
        --- the `opts` argument to Reach.App
        crs <- mempty
               <> connect_eth outn pl
               <> connect_algo outn pl
        backend_js outn crs pl
        return ()
  mapM_ compile1 $ tops copts
  return ()
