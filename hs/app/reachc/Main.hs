module Main (main) where

import Control.Exception
import Control.Monad
import Data.Aeson
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Data.Either.Extra
import Data.List.Extra (replace)
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Set as S
import Data.Typeable (cast)
import Reach.AST.Base
import Reach.CommandLine
import Reach.Compiler
import Reach.Report
import Reach.Version
import Safe
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.Process
import System.IO.Temp

shouldReport :: CompilerToolArgs -> CompilerToolEnv -> Bool
shouldReport CompilerToolArgs {..} CompilerToolEnv {..} =
  not cta_disableReporting
    && all emptyish [cte_CI, cte_GITHUB_ACTIONS, cte_TF_BUILD]
  where
    emptyish Nothing = True
    emptyish (Just x) = x == ""

apMA :: Monad m => m (a -> m b) -> a -> m b
apMA f = join . ap f . pure

main :: IO ()
main = do
  env@CompilerToolEnv {..} <- getCompilerEnv
  let hashStr = case cte_REACHC_HASH of
        Just hash -> " (" <> hash <> ")"
        Nothing -> ""
  let versionCliDisp = ("reachc " <> versionStr <> hashStr <> " - Reach compiler")
  rawArgs <- getArgs
  when ("--version" `elem` rawArgs) $ do
    putStrLn versionCliDisp
    exitSuccess
  when ("--numeric-version" `elem` rawArgs) $ do
    putStrLn versionStr
    exitSuccess
  when ("--hash" `elem` rawArgs) $ do
    maybe exitFailure putStrLn $ cte_REACHC_HASH
    exitSuccess
  when ("--report" `elem` rawArgs) $ do
    rid <- lookupEnv "REACHC_ID"
    (startReport rid "compile" `apMA`) =<< case atMay rawArgs 2 of
      Nothing -> do
        putStrLn "Missing reportable compilation result."
        exitWith $ ExitFailure 1
      Just e' -> (pure . eitherDecode' $ pack e') >>= \case
        Right (e'' :: Either CompileErrorException ()) ->
          pure $ mapLeft SomeException e''
        Left x -> do
          print x
          exitWith $ ExitFailure 1
    exitSuccess
  args@CompilerToolArgs {..} <- getCompilerArgs versionCliDisp
  let CompilerOpts {..} = cta_co
  let ccSource = co_source
  let ccInstallPkgs = co_installPkgs
  let ccVerifyFirstFailQuit = co_verifyFirstFailQuit
  let ccVerifyTimeout = co_verifyTimeout
  let ccStopAfterEval = co_stopAfterEval
  let ccShouldVerify = not cte_REACH_ACCURSED_UNUTTERABLE_DISABLE_VERIFICATION_AND_LOSE_ALL_YOUR_MONEY_AND_YOUR_USERS_MONEY
  ccDotReachDir <- makeAbsolute $ fromMaybe (takeDirectory co_source </> ".reach") co_mdirDotReach
  let outd = fromMaybe (takeDirectory co_source </> "build") co_moutputDir
  createDirectoryIfMissing True outd
  let ccTops = if null co_topl then Nothing else Just (S.fromList co_topl)
  (e :: Either SomeException ()) <-
    try $
      case co_printKeywordInfo of
        True -> printKeywordInfo
        False ->
          withSystemTempDirectory "reachc" $ \tmp -> do
            let ccOutput opt f = (wr, p)
                  where
                    wr = opt || co_intermediateFiles || cte_REACH_DEBUG
                    p = (if wr then outd else tmp) </> T.unpack f
            compile $ CompilerConfig {..}
  case shouldReport args env of
    False -> pure ()
    True -> case e of
      Right _ -> report $ Right ()
      Left (SomeException i) -> case (cast i :: Maybe CompileErrorException) of
        Just i' -> report $ Left i'
        Nothing -> startReport cte_REACHC_ID "compile" `apMA` e
  case e of
    Left exn -> throwIO exn
    Right _ -> return ()
 where
  report :: Either CompileErrorException () -> IO ()
  report a = do
    x <- getExecutablePath
    let f = replace "'" "'\\''" . unpack . encode
    void . spawnCommand $ x <> " --error-format-json --report '" <> f a <> "'"
