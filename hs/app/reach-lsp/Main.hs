{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Exception          (try)
import           Control.Lens               ((^.))
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.IORef
import qualified Data.Text                  as T
import           Language.JavaScript.Parser (TokenPosn (TokenPn))
import           Language.LSP.Diagnostics
import           Language.LSP.Server
import           Language.LSP.Types
import qualified Language.LSP.Types.Lens    as LSPLens
import           Reach.AST.Base             (ReachExcept (ReachExcept),
                                             SrcLoc (..))
import           Reach.CommandLine
import           Reach.Compiler
import           Reach.Eval.Core            (Env (..), SLScope (..))
import           Reach.Texty
import           System.Log.Logger


type CompileResult = Either ReachExcept (Maybe Env)


data LspEnv = LspEnv
  { e_eval_env :: IORef (Maybe Env) }


type App = ReaderT LspEnv (LspM ())


logger :: MonadIO m => String -> m ()
logger msg = do
  liftIO $ emergencyM "reachide" msg


getFilePath :: LSPLens.HasUri s Uri => s -> Maybe FilePath
getFilePath = uriToFilePath . grabUri


grabUri :: LSPLens.HasUri s Uri => s -> Uri
grabUri doc = doc ^. LSPLens.uri


srcToPos :: SrcLoc -> Position
srcToPos (SrcLoc _ mpos _) =
  case mpos of
    Just (TokenPn _ l c) -> Position (l - 1) (c - 1)
    Nothing              -> Position 0 0


maxDiags :: Int
maxDiags = 100


getCompilerOpts :: FilePath -> CompilerOpts
getCompilerOpts filepath =
  CompilerOpts
    { co_moutputDir = Nothing
    , co_source = filepath
    -- XXX be smart
    , co_topl = ["main"]
    , co_intermediateFiles = False
    , co_mdirDotReach = Nothing
    , co_installPkgs = False
    , co_stopAfterEval = True
    , co_verifyTimeout = 60000
    , co_sim = False
    }


clearDiags :: MonadLsp config m => DiagnosticSource -> m ()
clearDiags uri = flushDiagnosticsBySource 0 $ Just uri


makeErrorDiag :: Position -> Position -> String -> Diagnostic
makeErrorDiag start end msg =
  Diagnostic (Range start end) (Just DsError) Nothing (Just "reachide") (T.pack msg) Nothing (Just $ List [])


publishDiags :: NormalizedUri -> [Diagnostic] -> App ()
publishDiags nUri diags = publishDiagnostics maxDiags nUri (Just 0) $ partitionBySource diags


doCompile :: Uri -> App ()
doCompile uri = do
  logger "doCompile"
  case mPath of
    Nothing -> return ()
    Just filepath -> do
      compilerEnv <- liftIO $ getCompilerEnv
      let compilerOpt = getCompilerOpts filepath
      (e :: CompileResult) <- liftIO $ try $ compile compilerEnv compilerOpt
      clearDiags uriT
      case e of
        Left (ReachExcept at msg) -> do
          let pos   = srcToPos at
          let diags = [makeErrorDiag pos pos msg]
          logger $ "Error during compilation: " <> msg
          publishDiags nUri diags
        Right (Just menv) -> do
          logger "Compiled successfully"
          evalEnv <- asks e_eval_env
          liftIO $ writeIORef evalEnv $ Just menv
        Right Nothing      -> logger "Instructed not to compile"
  where
    uriT  = getUri uri
    nUri  = toNormalizedUri uri
    mPath = uriToFilePath uri


onDocumentHover :: Handler App 'TextDocumentHover
onDocumentHover req responder = do
  logger "onDocumentHover"
  let RequestMessage _ _ _ (HoverParams doc pos _) = req
  let range = Range pos pos
  let path = getFilePath doc
  let msg = HoverContents $ markedUpContent "Reach IDE" $ T.pack (show path)
  let rsp = Hover msg $ Just range
  responder $ Right $ Just rsp


onDocumentChange :: Handler App 'TextDocumentDidChange
onDocumentChange msg = do
  logger "onDocumentChange"
  let uri = grabUri $ msg ^. LSPLens.params . LSPLens.textDocument
  doCompile uri


onWorkspaceDidChange :: Handler App 'WorkspaceDidChangeWatchedFiles
onWorkspaceDidChange msg = do
  logger "onWorkspaceDidChange"
  let uris = fmap grabUri $ msg ^. LSPLens.params . LSPLens.changes
  mapM_ doCompile uris


onDocumentCompletion :: Handler App 'TextDocumentCompletion
onDocumentCompletion _req responder = do
  evalEnvR <- asks e_eval_env
  evalEnv  <- liftIO $ readIORef evalEnvR
  completions <-
        case evalEnv of
          Nothing -> return []
          Just env  -> do
            -- I think these are empty because we evaluate scopes with local
            let cenv = sco_cenv $ e_sco env
            logger $ show (pretty cenv)
            let penvs = sco_penvs $ e_sco env
            logger $ show (pretty penvs)
            return []
  let l = CompletionList False (List completions)
  responder $ Right $ InR l


onInitialized :: MonadIO m => p -> m ()
onInitialized _ = do
  logger "Initialized Reach LSP"


handlers :: Handlers App
handlers = mconcat
  [ notificationHandler SInitialized onInitialized
  , requestHandler      STextDocumentHover onDocumentHover
  , notificationHandler STextDocumentDidChange onDocumentChange
  , notificationHandler SWorkspaceDidChangeWatchedFiles onWorkspaceDidChange
  , requestHandler      STextDocumentCompletion onDocumentCompletion ]


main :: IO Int
main = do
  e_eval_env <- liftIO $ newIORef Nothing
  let appEnv = LspEnv {..}
  runServer $ ServerDefinition
    { onConfigurationChange = const $ pure $ Right ()
    , doInitialize = \ env _req -> pure $ Right env
    , staticHandlers = handlers
    , interpretHandler = \ env ->
        Iso (runLspT env . flip runReaderT appEnv) liftIO
    , options = defaultOptions
    }
