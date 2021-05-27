module Reach.Parser
  ( ParserError (..)
  , JSBundle (..)
  , parseJSFormals
  , jsArrowFormalsToFunFormals
  , parseJSArrowFormals
  , jsCallLike
  , parseIdent
  , jse_expect_id
  , jso_expect_id
  , gatherDeps_top
  , readJsExpr
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)
import Control.Monad.Reader (ReaderT, runReaderT, ask, asks, local)
import qualified Data.ByteString.Char8 as B
import qualified Data.Graph as G
import Data.IORef
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as M
import GHC.IO.Encoding
import GHC.Stack (HasCallStack)
import Generics.Deriving (Generic, conNameOf)
import Language.JavaScript.Parser
import Language.JavaScript.Parser.AST hiding (showStripped)
import Language.JavaScript.Parser.Lexer
import Reach.AST.Base
import Reach.EmbeddedFiles
import Reach.Eval.ImportSource
import Reach.JSUtil
import Reach.Texty
import Reach.UnsafeUtil
import Reach.Util
import System.Directory
import System.FilePath
import Text.Read (readMaybe)
import Text.Show.Pretty (ppShow)


data Env = Env
  { srcloc      :: SrcLoc
  , bundle      :: IORef JSBundleMap
  , mhostgit    :: Maybe HostGit     -- ^ Tracks descent into transitive `git` deps
  , canGit      :: Bool              -- ^ See Reach.Eval.ImportSource
  , dirDotReach :: FilePath          -- ^ See Reach.Eval.ImportSource
  }

type App    = ReaderT Env IO
type AppT a = a -> App a

withAt :: String -> Maybe TokenPosn -> App a -> App a
withAt lab mp app = local (\e -> e { srcloc = srcloc_at lab mp (srcloc e) }) app


data ParserError
  = Err_Parse_CyclicImport ReachSource
  | Err_Parser_Arrow_NoFormals
  | Err_Parse_ExpectIdentifier JSExpression
  | Err_Parse_ExpectIdentifierProp JSObjectProperty
  | Err_Parse_IllegalBinOp JSBinOp
  | Err_Parse_IllegalLiteral String
  | Err_Parse_IllegalUnaOp JSUnaryOp
  | Err_Parse_ImportAbsolute FilePath
  | Err_Parse_ImportDotDot FilePath
  | Err_Parse_NotModule JSAST
  | Err_Parse_NotCallLike JSExpression
  | Err_Parse_JSIdentNone
  deriving (Generic, Eq, ErrorMessageForJson, ErrorSuggestions)

--- FIXME implement a custom show that is useful
instance Show ParserError where
  show (Err_Parse_CyclicImport rs) =
    "Cyclic import! " <> show rs
  show Err_Parser_Arrow_NoFormals =
    "Arg list for function is missing."
  show Err_Parse_JSIdentNone =
    "Expected identifier, found none"
  show (Err_Parse_ExpectIdentifier _e) =
    "Expected identifier, got expression."
  show (Err_Parse_NotCallLike _e) =
    "Expected a call-like expression, got something else."
  show (Err_Parse_ExpectIdentifierProp _e) =
    "Expected identifier or one name and value in object properties list, got something else."
  show (Err_Parse_IllegalBinOp op) =
    "Illegal binary operation: " <> show op
  show (Err_Parse_IllegalLiteral lit) =
    "Illegal literal: " <> show lit
  show (Err_Parse_IllegalUnaOp unop) =
    "Illegal unary operator: " <> show unop
  show (Err_Parse_ImportAbsolute fp) =
    "Invalid import: absolute path imports are not supported: " <> fp
  show (Err_Parse_ImportDotDot fp) =
    "Invalid import: dotdot path imports are not supported: " <> fp
  show (Err_Parse_NotModule ast) =
    "Not a module: " <> (take 256 $ show ast)

--- Helpers
parseIdent :: SrcLoc -> JSIdent -> (SrcLoc, String)
parseIdent at = \case
  JSIdentName a ident ->
    (srcloc_jsa ident a at, ident)
  JSIdentNone ->
    expect_thrown at $ Err_Parse_JSIdentNone

jse_expect_id :: HasCallStack => SrcLoc -> JSExpression -> String
jse_expect_id at = \case
  (JSIdentifier _ x) -> x
  j -> expect_thrown at (Err_Parse_ExpectIdentifier j)

jso_expect_id :: HasCallStack => SrcLoc -> JSObjectProperty -> String
jso_expect_id at = \case
  JSPropertyIdentRef _ x -> x
  j -> expect_thrown at $ Err_Parse_ExpectIdentifierProp j

parseJSFormals :: SrcLoc -> JSCommaList JSExpression -> [JSExpression]
parseJSFormals _at jsformals = jscl_flatten jsformals

jsArrowFormalsToFunFormals :: SrcLoc -> JSArrowParameterList -> JSCommaList JSExpression
jsArrowFormalsToFunFormals at = \case
  JSUnparenthesizedArrowParameter (JSIdentName a x) ->
    JSLOne (JSIdentifier a x)
  JSUnparenthesizedArrowParameter JSIdentNone ->
    expect_thrown at Err_Parser_Arrow_NoFormals
  JSParenthesizedArrowParameterList _ l _ -> l

parseJSArrowFormals :: SrcLoc -> JSArrowParameterList -> [JSExpression]
parseJSArrowFormals at aformals =
  parseJSFormals at $ jsArrowFormalsToFunFormals at aformals

jsCallLike :: SrcLoc -> JSExpression -> (JSExpression, [JSExpression])
jsCallLike at = \case
  JSCallExpression rator _ rands _ ->
    (rator, jscl_flatten rands)
  JSMemberExpression rator _ rands _ ->
    (rator, jscl_flatten rands)
  e ->
    expect_thrown at $ Err_Parse_NotCallLike e

readJsExpr :: HasCallStack => String -> JSExpression
readJsExpr s =
  case readJs s of
    JSAstProgram [JSExpressionStatement e _] _ -> e
    v -> impossible $ "readJsExpr " <> show v

--- Dependency Gatherer
type BundleMap a b = ((M.Map a [a]), (M.Map a (Maybe b)))

type JSBundleMap = BundleMap ReachSource [JSModuleItem]

data JSBundle = JSBundle [(ReachSource, [JSModuleItem])]
  deriving (Eq, Show, Generic)

instance Pretty JSBundle where
  pretty (JSBundle ds) = vsep $ map go ds
    where
      go (rs, jms) =
        vsep $
          ("// " <> viaShow rs) :
          map (pretty . ppShow) jms


gatherDeps_fc :: AppT JSFromClause
gatherDeps_fc (JSFromClause ab aa s) = do
  withAt "import from" (tp ab) $ do
    s_abs <- gatherDeps_file GatherNotTop $ trimQuotes s
    return $ JSFromClause ab aa s_abs

gatherDeps_imd :: AppT JSImportDeclaration
gatherDeps_imd = \case
  JSImportDeclaration ic fc sm -> do
    fc' <- gatherDeps_fc fc
    return $ JSImportDeclaration ic fc' sm
  JSImportDeclarationBare a s sm -> do
    withAt "import bare" (tp a) $ do
      s_abs <- gatherDeps_file GatherNotTop $ trimQuotes s
      return $ JSImportDeclarationBare a s_abs sm

gatherDeps_exd :: AppT JSExportDeclaration
gatherDeps_exd = \case
  JSExportFrom ec fc sp -> do
    fc' <- gatherDeps_fc fc
    return $ JSExportFrom ec fc' sp
  exd ->
    return exd

gatherDeps_mi :: AppT JSModuleItem
gatherDeps_mi = \case
  JSModuleImportDeclaration a imd -> do
    withAt "import" (tp a) $ do
      imd' <- gatherDeps_imd imd
      return $ JSModuleImportDeclaration a imd'
  JSModuleExportDeclaration a exd -> do
    withAt "export" (tp a) $ do
      exd' <- gatherDeps_exd exd
      return $ JSModuleExportDeclaration a exd'
  mi -> return mi

gatherDeps_ast :: JSAST -> App [JSModuleItem]
gatherDeps_ast = \case
  JSAstModule mis _ ->
    mapM gatherDeps_mi mis
  j -> do
    at <- asks srcloc
    expect_thrown at (Err_Parse_NotModule j)

updatePartialAvoidCycles :: Maybe ReachSource -> [ReachSource] -> (() -> App ReachSource) -> (ReachSource -> c) -> (ReachSource -> ParserError) -> (ReachSource -> App [JSModuleItem]) -> App c
updatePartialAvoidCycles mfrom def_a get_key ret_key err_key proc_key = do
  key <- get_key ()
  let res = ret_key key
  at <- asks srcloc
  fmr <- asks bundle
  (dm, fm) <- liftIO $ readIORef fmr
  case (M.lookup key fm) of
    Nothing -> do
      liftIO $ writeIORef fmr ((M.insert key def_a dm), (M.insert key Nothing fm))
      content <- proc_key key
      (dm', fm') <- liftIO $ readIORef fmr
      let fm'' = (M.insert key (Just content) fm')
          add_key ml = Just $ key : (maybe [] id ml)
          dm'' = case mfrom of
            Just from -> (M.alter add_key from dm')
            Nothing -> dm'
      liftIO $ writeIORef fmr (dm'', fm'')
      return res
    Just Nothing ->
      expect_thrown at $ err_key key
    Just (Just _) ->
      return res

gatherDeps_from :: SrcLoc -> Maybe ReachSource
gatherDeps_from (SrcLoc _ _ mrs) = mrs

tryPrettifyError :: SrcLoc -> String -> String
tryPrettifyError at' e = case readMaybe e of
  Just (t :: Token) ->
    "Unexpected token, " <> ty <> " at " <> atStr <> tokLit
    where
      ty = conNameOf t
      ts = tokenSpan t
      SrcLoc sl0 _ sl2 = at'
      at = SrcLoc sl0 (Just ts) sl2
      atStr = unsafeRedactAbsStr $ show at
      unsafeTlit = " \"" <> tokenLiteral t <> "\""
      -- Don't want to enumerate them all because it's a lot
      tokLit = case t of
        IdentifierToken {} -> unsafeTlit
        _ -> ""
  _ -> e

gatherDeps_ast_rewriteErr :: String -> App [JSModuleItem]
gatherDeps_ast_rewriteErr s = do
  at' <- asks srcloc
  case parseModule s (show $ get_srcloc_src at') of
    Left e -> error $ tryPrettifyError at' e -- TODO: prettify
    Right r -> gatherDeps_ast r

data GatherContext = GatherTop | GatherNotTop
  deriving (Eq)

gatherDeps_file :: GatherContext -> AppT FilePath
gatherDeps_file gctxt src_rel = do
  Env {..} <- ask

  (mh', isrc) <- liftIO $ importSource srcloc src_rel >>= \case
    i@(ImportRemoteGit h) -> pure (Just h,   i)
    i@(ImportLocal     _) -> pure (mhostgit, i)

  let no_stdlib = impossible $ "gatherDeps_file: source file became stdlib"

      ret_key (ReachSourceFile x) = x
      ret_key ReachStdLib         = no_stdlib

      get_key () = case isrc of
        ImportRemoteGit h -> ReachSourceFile
          <$> (liftIO $ lockModuleAbsPath srcloc canGit dirDotReach h)

        ImportLocal src_rel' -> case mh' of
          Just h -> ReachSourceFile
            <$> (liftIO $ lockModuleAbsPathGitLocalDep srcloc canGit dirDotReach h src_rel')

          Nothing -> do
            src_abs <- liftIO $ makeAbsolute src_rel'
            reRel   <- liftIO $ makeRelativeToCurrentDirectory src_abs

            when (gctxt == GatherNotTop) $ do
              when (isAbsolute src_rel')
                $ expect_thrown srcloc (Err_Parse_ImportAbsolute src_rel')

              when ("../" `isPrefixOf` reRel)
                $ expect_thrown srcloc (Err_Parse_ImportDotDot src_rel')

            pure $ ReachSourceFile src_abs


      proc_key ReachStdLib                   = no_stdlib
      proc_key src@(ReachSourceFile src_abs) = do
        e <- ask
        let e' = e { srcloc = srcloc_src src, mhostgit = mh' }
        liftIO $ do
          setLocaleEncoding utf8
          content <- readFile src_abs
          withCurrentDirectory
            (takeDirectory src_abs)
            (flip runReaderT e' (gatherDeps_ast_rewriteErr content))

  updatePartialAvoidCycles
    (gatherDeps_from srcloc) [ReachStdLib] get_key ret_key Err_Parse_CyclicImport proc_key


gatherDeps_stdlib :: App ()
gatherDeps_stdlib = do
  let proc_key _ =
        local (\e -> e { mhostgit = Nothing, srcloc = srcloc_src ReachStdLib })
          $ gatherDeps_ast_rewriteErr (B.unpack stdlib_rsh)

  at' <- gatherDeps_from <$> asks srcloc
  updatePartialAvoidCycles at' [] get_key ret_key err_key proc_key
  where
    get_key () = return $ ReachStdLib
    ret_key _ = ()
    err_key x = Err_Parse_CyclicImport x

map_order :: Ord a => M.Map a [a] -> [a]
map_order dm = order
  where
    order = map (getNodePart . nodeFromVertex) order_v
    order_v = G.topSort graph
    (graph, nodeFromVertex, _vertexFromKey) = G.graphFromEdges edgeList
    edgeList = map (\(from, to) -> (from, from, to)) $ M.toList dm
    getNodePart (n, _, _) = n

gatherDeps_top :: FilePath -> Bool -> FilePath -> IO JSBundle
gatherDeps_top src_p canGit' dirDotReach' = do
  fmr <- liftIO $ newIORef (mempty, mempty)
  let e = Env srcloc_top fmr Nothing canGit' dirDotReach'
  flip runReaderT e $ do
    _src_abs_p <- gatherDeps_file GatherTop src_p
    gatherDeps_stdlib
    (dm, fm) <- liftIO $ readIORef fmr
    return $ JSBundle $ map (\k -> (k, ensureJust (fm M.! k))) $ map_order dm
    where
      ensureJust Nothing = impossible "gatherDeps: Did not close all Reach files"
      ensureJust (Just x) = x
