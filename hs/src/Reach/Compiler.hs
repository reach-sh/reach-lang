module Reach.Compiler (CompilerOpts (..), compile, make_connectors) where

import Control.Monad
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as LTIO
import qualified Filesystem.Path.CurrentOS as FP
import Reach.APICut
import Reach.AST.DL
import Reach.Backend.JS
import Reach.BigOpt
import Reach.CommandLine
import Reach.Connector
import Reach.Connector.ALGO
import Reach.Connector.ETH_Solidity
import Reach.EditorInfo
import Reach.EPP
import Reach.EraseLogic
import Reach.Eval
import Reach.Linearize
import Reach.Parser (gatherDeps_top)
import Reach.Simulator.Server
import Reach.StateDiagram
import Reach.Texty
import Reach.Util
import Reach.Verify
import System.Directory
import System.Exit
import System.FilePath

make_connectors :: CompilerToolEnv -> Connectors
make_connectors env =
  M.fromList $
    map
      (\x -> (conName x, x))
      [ connect_eth env
      , connect_algo env
      ]

compile :: CompilerToolEnv -> CompilerOpts -> IO ()
compile env (CompilerOpts {..}) = do
  when co_printKeywordInfo $ do
    printKeywordInfo
    exitSuccess
  let outd = fromMaybe (takeDirectory co_source </> "build") co_moutputDir
  let co_dirDotReach = fromMaybe (takeDirectory co_source </> ".reach") co_mdirDotReach
  let co_output ext = FP.encodeString $ FP.append (FP.decodeString outd) $ (FP.filename $ FP.decodeString co_source) `FP.replaceExtension` ext
  createDirectoryIfMissing True outd
  let co_tops = if null co_topl then Nothing else Just (S.fromList co_topl)
  let outnMay = flip doIf (co_intermediateFiles || cte_REACH_DEBUG env)
  let interOut outn_ = case outnMay outn_ of
        Just f -> LTIO.writeFile . f
        Nothing -> \_ _ -> return ()
  dirDotReach' <- makeAbsolute co_dirDotReach
  djp <- gatherDeps_top co_source co_installPkgs dirDotReach'
  unless co_installPkgs $ do
    let all_connectors = make_connectors env
    (avail, compileDApp) <- evalBundle all_connectors djp
    let chosen = S.toAscList $ fromMaybe avail co_tops
    forM_ chosen $ \which -> do
      let woutn = co_output . ((T.pack which <> ".") <>)
      let woutnMay = outnMay woutn
      let showp :: Pretty a => T.Text -> a -> IO ()
          showp l = interOut woutn l . render . pretty
      -- showp "bundle.js" $ render $ pretty djp
      dl <- compileDApp which
      let DLProg _ (DLOpts {..}) _ _ _ _ _ _ _ = dl
      let connectors = map (all_connectors M.!) dlo_connectors
      showp "dl" dl
      unless co_stopAfterEval $ do
        ll <- linearize showp dl
        showp "ll" ll
        ol <- bigopt (showp, "ol") ll
        -- ol <- optimize ll
        showp "ol" ol
        let vo_out = woutnMay
        let vo_mvcs = doIf connectors dlo_verifyPerConnector
        let vo_timeout = co_verifyTimeout
        let vo_dir = dirDotReach'
        verify (VerifyOpts {..}) ol >>= maybeDie
        el <- erase_logic ol
        showp "el" el
        unless (not co_sim) $ do
          src <- readFile co_source
          startServer el src
        eol <- bigopt (showp, "eol") el
        showp "eol" eol
        pil <- epp eol
        showp "state.dot" $ stateDiagram pil
        showp "pil" pil
        apc <- apicut pil
        showp "apc" apc
        pl <- bigopt (showp, "pl") apc
        showp "pl" pl
        let runConnector c = (,) (conName c) <$> conGen c woutnMay pl
        crs <- HM.fromList <$> mapM runConnector connectors
        backend_js woutn crs pl
        return ()

doIf :: a -> Bool -> Maybe a
doIf b = \case
  True -> Just b
  False -> Nothing
