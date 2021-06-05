module Reach.Compiler (CompilerOpts (..), compile, all_connectors) where

import Control.Monad
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Maybe
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
import Reach.BigOpt
import Reach.Parser (gatherDeps_top)
import Reach.Texty
import Reach.Util
import Reach.Verify
import System.Directory

data CompilerOpts = CompilerOpts
  { output :: T.Text -> String
  , source :: FilePath
  , tops :: Maybe (S.Set String)
  , intermediateFiles :: Bool
  , dirDotReach :: FilePath
  , installPkgs :: Bool
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
  dirDotReach' <- makeAbsolute $ dirDotReach copts
  let doPkgs = installPkgs copts
  djp <- gatherDeps_top (source copts) doPkgs dirDotReach'
  interOut outn "bundle.js" $ render $ pretty djp
  unless doPkgs $ do
    (avail, compileDApp) <- evalBundle all_connectors djp
    let chosen = fromMaybe avail $ tops copts
    forM_ (S.toAscList chosen) $ \ which -> do
      let addWhich = ((T.pack which <> ".") <>)
      let woutn = outn . addWhich
      let woutnMay = outnMay woutn
      let winterOut = interOut woutn
      let showp :: (forall a. Pretty a => T.Text -> a -> IO ())
          showp l = winterOut l . render . pretty
      let showp' :: (forall a. Pretty a => String -> a -> IO ())
          showp' = showp . T.pack
      dl <- compileDApp which
      let DLProg _ (DLOpts {..}) _ _ _ _ _ = dl
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
      eol <- bigopt (showp, "eol") el
      pil <- epp eol
      showp "pil" pil
      pl <- bigopt (showp, "pl") pil
      let runConnector c = (,) (conName c) <$> conGen c woutnMay pl
      crs <- HM.fromList <$> mapM runConnector connectors
      backend_js woutn crs pl
      return ()
