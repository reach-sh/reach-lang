module Reach.Compiler (CompilerOpts (..), compile, connectors) where

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

connectors :: Connectors
connectors =
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
        let dl = compileBundle connectors djp which
        let DLProg _ (DLOpts {..}) _ _ = dl
        interOut "dl" $ show $ pretty dl
        let ll = linearize dl
        interOut "ll" $ show $ pretty ll
        verify outnMay ll >>= maybeDie
        let pl = epp ll
        interOut "pl" $ show $ pretty pl
        let runConnector cn = (,) cn <$> conGen c outnMay pl
              where c = connectors M.! cn
        crs <- M.fromList <$> mapM runConnector dlo_connectors
        backend_js outn crs pl
        return ()
  mapM_ compile1 $ tops copts
  return ()
