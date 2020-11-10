module Reach.Compiler (CompilerOpts (..), compile, all_connectors) where

import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as LTIO
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
import Reach.Texty
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
    map
      (\x -> (conName x, x))
      [ connect_eth
      , connect_algo
      ]

compile :: CompilerOpts -> IO ()
compile copts = do
  let outn = output copts
  let outnMay = case intermediateFiles copts of
        True -> Just outn
        False -> Nothing
  let interOut = case outnMay of
        Just f -> LTIO.writeFile . f
        Nothing -> \_ _ -> return ()
  djp <- gatherDeps_top $ source copts
  interOut "bundle.js" $ render $ pretty djp
  let compile1 which = do
        let winterOut = interOut . ((T.pack which <> ".") <>)
        let dl = compileBundle all_connectors djp which
        let DLProg _ (DLOpts {..}) _ _ = dl
        let connectors = map (all_connectors M.!) dlo_connectors
        winterOut "dl" $ render $ pretty dl
        let ll = linearize dl
        winterOut "ll" $ render $ pretty ll
        ol <- optimize ll
        winterOut "ol" $ render $ pretty ol
        let vconnectors =
              case dlo_verifyPerConnector of
                False -> Nothing
                True -> Just connectors
        verify outnMay vconnectors ol >>= maybeDie
        let pl = epp ol
        winterOut "pl" $ render $ pretty pl
        let runConnector c = (,) (conName c) <$> conGen c outnMay pl
        crs <- HM.fromList <$> mapM runConnector connectors
        backend_js outn crs pl
        return ()
  mapM_ compile1 $ tops copts
  return ()
