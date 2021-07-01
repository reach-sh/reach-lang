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

import Control.Monad.Reader
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
import Reach.JSUtil
import Reach.PackageImport
import Reach.Texty
import Reach.UnsafeUtil
import Reach.Util
import System.Directory
import System.FilePath
import Text.Read (readMaybe)
import Text.Show.Pretty (ppShow)

data Env = Env
  { e_at :: SrcLoc
  , e_bm :: IORef JSBundleMap
  , e_install :: Bool
  , e_dreachp :: FilePath
  }

type App = ReaderT Env IO

type AppT a = a -> App a

withAt :: String -> Maybe TokenPosn -> App a -> App a
withAt lab mp app =
  local (\e -> e {e_at = srcloc_at lab mp (e_at e)}) app

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

instance HasErrorCode ParserError where
  errPrefix = const "RP"
  -- These indices are part of an external interface; they
  -- are used in the documentation of Error Codes.
  -- Do not modify & add new error codes at the end.
  errIndex = \case
    Err_Parse_CyclicImport {} -> 0
    Err_Parser_Arrow_NoFormals {} -> 1
    Err_Parse_ExpectIdentifier {} -> 2
    Err_Parse_ExpectIdentifierProp {} -> 3
    Err_Parse_IllegalBinOp {} -> 4
    Err_Parse_IllegalLiteral {} -> 5
    Err_Parse_IllegalUnaOp {} -> 6
    Err_Parse_ImportAbsolute {} -> 7
    Err_Parse_ImportDotDot {} -> 8
    Err_Parse_NotModule {} -> 9
    Err_Parse_NotCallLike {} -> 10
    Err_Parse_JSIdentNone {} -> 11

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
    at <- asks e_at
    expect_thrown at (Err_Parse_NotModule j)

updatePartialAvoidCycles :: Maybe ReachSource -> [ReachSource] -> (() -> App ReachSource) -> (ReachSource -> c) -> (ReachSource -> ParserError) -> (ReachSource -> App [JSModuleItem]) -> App c
updatePartialAvoidCycles mfrom def_a get_key ret_key err_key proc_key = do
  key <- get_key ()
  let res = ret_key key
  Env {..} <- ask
  (dm, fm) <- liftIO $ readIORef e_bm
  case (M.lookup key fm) of
    Nothing -> do
      liftIO $
        writeIORef e_bm $
          ((M.insert key def_a dm), (M.insert key Nothing fm))
      content <- proc_key key
      (dm', fm') <- liftIO $ readIORef e_bm
      let fm'' = (M.insert key (Just content) fm')
          add_key ml = Just $ key : (maybe [] id ml)
          dm'' = case mfrom of
            Just from -> (M.alter add_key from dm')
            Nothing -> dm'
      liftIO $ writeIORef e_bm (dm'', fm'')
      return res
    Just Nothing ->
      expect_thrown e_at $ err_key key
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
  at' <- asks e_at
  case parseModule s (show $ get_srcloc_src at') of
    Left e -> error $ tryPrettifyError at' e -- TODO: prettify
    Right r -> gatherDeps_ast r

data GatherContext = GatherTop | GatherNotTop
  deriving (Eq)

gatherDeps_file :: GatherContext -> AppT FilePath
gatherDeps_file gctxt raw = do
  Env {..} <- ask
  src_abs <-
    case "@" `isPrefixOf` raw of
      True -> do
        liftIO $ packageImport e_at e_install e_dreachp raw
      False -> do
        src_abs <- liftIO $ makeAbsolute raw
        reRel <- liftIO $ makeRelativeToCurrentDirectory src_abs
        when (gctxt == GatherNotTop) $ do
          when (isAbsolute raw) $
            expect_thrown e_at (Err_Parse_ImportAbsolute raw)
          when ("../" `isPrefixOf` reRel) $
            expect_thrown e_at (Err_Parse_ImportDotDot raw)
        return src_abs
  let no_stdlib = impossible $ "gatherDeps_file: source file became stdlib"
  let ret_key = \case
        ReachSourceFile x -> x
        ReachStdLib -> no_stdlib
  let get_key () = return $ ReachSourceFile src_abs
  let proc_key = \case
        ReachStdLib -> no_stdlib
        src@(ReachSourceFile _) -> do
          e <- ask
          let e' = e {e_at = srcloc_src src}
          liftIO $ do
            setLocaleEncoding utf8
            content <- readFile src_abs
            withCurrentDirectory
              (takeDirectory src_abs)
              (flip runReaderT e' (gatherDeps_ast_rewriteErr content))
  updatePartialAvoidCycles
    (gatherDeps_from e_at)
    [ReachStdLib]
    get_key
    ret_key
    Err_Parse_CyclicImport
    proc_key

gatherDeps_stdlib :: App ()
gatherDeps_stdlib = do
  let proc_key _ =
        local (\e -> e {e_at = srcloc_src ReachStdLib}) $
          gatherDeps_ast_rewriteErr (B.unpack stdlib_rsh)

  at' <- gatherDeps_from <$> asks e_at
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
gatherDeps_top src_p e_install e_dreachp = do
  let e_at = srcloc_top
  e_bm <- liftIO $ newIORef (mempty, mempty)
  flip runReaderT (Env {..}) $ do
    _src_abs_p <- gatherDeps_file GatherTop src_p
    gatherDeps_stdlib
    (dm, fm) <- liftIO $ readIORef e_bm
    return $ JSBundle $ map (\k -> (k, ensureJust (fm M.! k))) $ map_order dm
  where
    ensureJust Nothing = impossible "gatherDeps: Did not close all Reach files"
    ensureJust (Just x) = x
