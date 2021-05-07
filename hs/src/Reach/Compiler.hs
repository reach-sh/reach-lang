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
import Reach.Eval.Core (SLLibs, expect_, e_lifts)
import Reach.AST.Base (SLVar)
import Reach.AST.SL (SLPrimitive(SLPrim_App_Delay), SLVal (SLV_Prim), sss_val)
import Data.Maybe (mapMaybe)
import Control.Monad (forM_, when)
import Reach.Eval.Module (evalLibs)
import Control.Monad.Reader (runReaderT)
import Reach.Eval.Error (EvalError(Err_Top_NotApp))
import Data.IORef (readIORef, writeIORef)

data CompilerOpts = CompilerOpts
  { output :: T.Text -> String
  , source :: FilePath
  , tops :: Top
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

findReachApps :: JSBundle -> SLLibs -> [SLVar]
findReachApps (JSBundle mods) libm = do
  let exe = case mods of
        [] -> impossible $ "findReachApps: no files"
        ((x, _) : _) -> x
  let exe_ex = libm M.! exe
  mapMaybe (\ (k, v) ->
    case sss_val v of
      SLV_Prim SLPrim_App_Delay {} -> Just k
      _ -> Nothing
    ) $ M.toList exe_ex

filterOtherApps :: SLVar -> SLLibs -> SLLibs
filterOtherApps which =
  M.map (M.filterWithKey (\ k v ->
      case sss_val v of
        SLV_Prim SLPrim_App_Delay {} -> k == which
        _ -> True ))

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
  -- Either compile all the Reach.Apps or those specified by user
  evalEnv <- compileEnv all_connectors
  let JSBundle mods = djp
  libm <- flip runReaderT evalEnv $ evalLibs all_connectors mods
  lifts <- readIORef $ e_lifts evalEnv
  let whichs = case tops copts of
        CompileAll -> findReachApps djp libm
        CompileJust tops -> tops
  when (null whichs) $
    flip runReaderT evalEnv $ expect_ Err_Top_NotApp
  forM_ whichs $ \ which -> do
      let addWhich = ((T.pack which <> ".") <>)
      let woutn = outn . addWhich
      let woutnMay = outnMay woutn
      let winterOut = interOut woutn
      let showp :: (forall a. Pretty a => T.Text -> a -> IO ())
          showp l = winterOut l . render . pretty
      let showp' :: (forall a. Pretty a => String -> a -> IO ())
          showp' = showp . T.pack
      let libm' = filterOtherApps which libm
      -- Reutilize env from parsing module, but remove any lifts
      -- from processing previous top
      writeIORef (e_lifts evalEnv) lifts
      dl <- compileBundle evalEnv all_connectors djp libm' which
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
      pil <- epp el
      showp "pil" pil
      let runConnector c = (,) (conName c) <$> conGen c woutnMay pil
      crs <- HM.fromList <$> mapM runConnector connectors
      backend_js woutn crs pil
      return ()
