module Reach.Compiler (CompilerOpts (..), compile, all_connectors) where

import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as LTIO
import Reach.AST.DL
import Reach.Backend.JS
import Reach.Connector
import Reach.Connector.ALGO
import Reach.Connector.ETH_Solidity
import Reach.EPP
import Reach.EraseLogic
import Reach.Eval
import Reach.Linearize
import Reach.Optimize
import Reach.Parser
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
  let outnMay outn_ = case intermediateFiles copts of
        True -> Just outn_
        False -> Nothing
  let interOut outn_ = case outnMay outn_ of
        Just f -> LTIO.writeFile . f
        Nothing -> \_ _ -> return ()
  djp <- gatherDeps_top $ source copts
  interOut outn "bundle.js" $ render $ pretty djp
  let compile1 which = do
        let addWhich = ((T.pack which <> ".") <>)
        let woutn = outn . addWhich
        let woutnMay = outnMay woutn
        let winterOut = interOut woutn
        let showp :: (forall a. Pretty a => T.Text -> a -> IO ())
            showp l = winterOut l . render . pretty
        let showp' :: (forall a. Pretty a => String -> a -> IO ())
            showp' = showp . T.pack
        dl <- compileBundle all_connectors djp which
        let DLProg _ (DLOpts {..}) _ _ _ _ = dl
        let connectors = map (all_connectors M.!) dlo_connectors
        showp "dl" dl
        ll <- linearize showp' dl
        ol <- optimize ll
        showp "ol" ol
        let vconnectors =
              case dlo_verifyPerConnector of
                False -> Nothing
                True -> Just connectors
        verify woutnMay vconnectors ol >>= maybeDie
        el <- erase_logic ol
        showp "el" el
        pil <- epp el
        showp "pil" pil
        let runConnector c = (,) (conName c) <$> conGen c woutnMay pil
        crs <- HM.fromList <$> mapM runConnector connectors
        backend_js woutn crs pil
        return ()
  mapM_ compile1 $ tops copts
  return ()
