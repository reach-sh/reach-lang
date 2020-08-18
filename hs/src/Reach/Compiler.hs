module Reach.Compiler (CompilerOpts (..), compileNL) where

import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import Reach.Backend.JS
import Reach.Connector.ALGO
import Reach.Connector.ETH_Solidity
import Reach.EPP
import Reach.Eval
import Reach.Linearize
import Reach.Parser
import Reach.Pretty ()
import Reach.Util
import Reach.Verify

data CompilerOpts = CompilerOpts
  { output :: T.Text -> String
  , source :: FilePath
  , tops :: [String]
  , intermediateFiles :: Bool
  }

compileNL :: CompilerOpts -> IO ()
compileNL copts = do
  djp <- gatherDeps_top $ source copts
  let compile1 which = do
        let outn = (output copts) . ((T.pack which <> ".") <>)
        let outnMay = case intermediateFiles copts of
              True -> Just outn
              False -> Nothing
        let interOut = case outnMay of
              Just f -> writeFile . f
              Nothing -> \_ _ -> return ()
        let dl = compileBundle djp which
        interOut "dl" $ show $ pretty dl
        let ll = linearize dl
        interOut "ll" $ show $ pretty ll
        verify outnMay ll >>= maybeDie
        let pl = epp ll
        interOut "pl" $ show $ pretty pl
        --- FIXME The particular connector/backend should be part of
        --- the `opts` argument to Reach.App
        !crs <-
          mempty
            <> connect_eth outnMay pl
            <> connect_algo outnMay pl
        backend_js outn crs pl
        return ()
  mapM_ compile1 $ tops copts
  return ()
