module Reach.Compiler (CompilerOpts (..), compile, all_connectors) where

import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import qualified Data.Map.Strict as M
import Reach.AST
import Reach.Backend.JS
import Reach.Connector
import Reach.Connector.ALGO
import Reach.Connector.ETH_Solidity
import Reach.EPP
import Reach.Eval
import Reach.Linearize
import Reach.Optimize
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

all_connectors :: Connectors
all_connectors =
  M.fromList $
    map (\x -> (conName x, x))
    [ connect_eth
    , connect_algo ]

compile :: CompilerOpts -> IO ()
compile copts = do
  djp <- gatherDeps_top $ source copts
  let compile1 which = do
        let outn = (output copts) . ((T.pack which <> ".") <>)
        let outnMay = case intermediateFiles copts of
              True -> Just outn
              False -> Nothing
        let interOut = case outnMay of
              Just f -> writeFile . f
              Nothing -> \_ _ -> return ()
        let dl = compileBundle all_connectors djp which
        let DLProg _ (DLOpts {..}) _ _ = dl
        let connectors = map (all_connectors M.!) dlo_connectors
        interOut "dl" $ show $ pretty dl
        let ll = linearize dl
        interOut "ll" $ show $ pretty ll
        ol <- optimize ll
        interOut "ol" $ show $ pretty ol
        let vconnectors =
              case dlo_verifyPerConnector of
                False -> Nothing
                True -> Just connectors
        verify outnMay vconnectors ol >>= maybeDie
        let pl = epp ol
        interOut "pl" $ show $ pretty pl
        let runConnector c = (,) (conName c) <$> conGen c outnMay pl
        crs <- M.fromList <$> mapM runConnector connectors
        backend_js outn crs pl
        return ()
  mapM_ compile1 $ tops copts
  return ()
