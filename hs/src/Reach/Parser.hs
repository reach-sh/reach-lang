module Reach.Parser (ParserError (..), JSBundle (..), parseJSFormals, jsArrowFormalsToFunFormals, parseJSArrowFormals, jsCallLike, jse_expect_id, jso_expect_id, gatherDeps_top) where

import Control.DeepSeq
import Control.Monad (when)
import qualified Data.ByteString.Char8 as B
import qualified Data.Graph as G
import Data.IORef
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as M
import GHC.IO.Encoding
import GHC.Stack (HasCallStack)
import Generics.Deriving (Generic, conNameOf)
import Language.JavaScript.Parser
import Language.JavaScript.Parser.AST
import Language.JavaScript.Parser.Lexer
import Reach.AST
import Reach.EmbeddedFiles
import Reach.JSUtil
import Reach.UnsafeUtil
import Reach.Util
import System.Directory
import System.FilePath
import Text.Read (readMaybe)

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
  deriving (Generic, Eq)

--- FIXME implement a custom show that is useful
instance Show ParserError where
  show (Err_Parse_CyclicImport rs) =
    "Cyclic import! " <> show rs
  show Err_Parser_Arrow_NoFormals =
    "Arg list for function is missing."
  show (Err_Parse_ExpectIdentifier _e) =
    "Expected identifier, got expression."
  show (Err_Parse_NotCallLike _e) =
    "Expected a call-like expression, got something else."
  show (Err_Parse_ExpectIdentifierProp _e) =
    "Expected identifier in object properties list, got expression."
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
jse_expect_id :: HasCallStack => SrcLoc -> JSExpression -> String
jse_expect_id at j =
  case j of
    (JSIdentifier _ x) -> x
    _ -> expect_throw at (Err_Parse_ExpectIdentifier j)

jso_expect_id :: HasCallStack => SrcLoc -> JSObjectProperty -> String
jso_expect_id at = \case
  JSPropertyIdentRef _ x -> x
  j -> expect_throw at $ Err_Parse_ExpectIdentifierProp j

--- FIXME Support more binding forms
parseJSFormals :: SrcLoc -> JSCommaList JSExpression -> [SLVar]
parseJSFormals at jsformals = map (jse_expect_id at) $ jscl_flatten jsformals

jsArrowFormalsToFunFormals :: SrcLoc -> JSArrowParameterList -> JSCommaList JSExpression
jsArrowFormalsToFunFormals at aformals =
  case aformals of
    JSUnparenthesizedArrowParameter (JSIdentName a x) ->
      JSLOne (JSIdentifier a x)
    JSUnparenthesizedArrowParameter JSIdentNone ->
      expect_throw at Err_Parser_Arrow_NoFormals
    JSParenthesizedArrowParameterList _ l _ -> l

parseJSArrowFormals :: SrcLoc -> JSArrowParameterList -> [SLVar]
parseJSArrowFormals at aformals =
  parseJSFormals at $ jsArrowFormalsToFunFormals at aformals

jsCallLike :: SrcLoc -> JSExpression -> (JSExpression, [JSExpression])
jsCallLike at = \case
  JSCallExpression rator _ rands _ ->
    (rator, jscl_flatten rands)
  JSMemberExpression rator _ rands _ ->
    (rator, jscl_flatten rands)
  e ->
    expect_throw at $ Err_Parse_NotCallLike e

--- Dependency Gatherer
type BundleMap a b = ((M.Map a [a]), (M.Map a (Maybe b)))

type JSBundleMap = BundleMap ReachSource [JSModuleItem]

data JSBundle = JSBundle [(ReachSource, [JSModuleItem])]
  deriving (Eq, Show, Generic)

instance NFData JSBundle where
  rnf (JSBundle xs) = go xs
    where
      go [] = ()
      go ((rs, jmi) : rest) = rnf rs `seq` jmi `seq` go rest

gatherDeps_imd :: SrcLoc -> IORef JSBundleMap -> JSImportDeclaration -> IO JSImportDeclaration
gatherDeps_imd at fmr j =
  case j of
    JSImportDeclaration ic (JSFromClause ab aa s) sm -> do
      s_abs <- gatherDeps_file GatherNotTop (srcloc_at "import from" (tp ab) at) fmr $ trimQuotes s
      return $ JSImportDeclaration ic (JSFromClause ab aa s_abs) sm
    JSImportDeclarationBare a s sm -> do
      s_abs <- gatherDeps_file GatherNotTop (srcloc_at "import bare" (tp a) at) fmr $ trimQuotes s
      return $ JSImportDeclarationBare a s_abs sm

gatherDeps_mi :: SrcLoc -> IORef JSBundleMap -> JSModuleItem -> IO JSModuleItem
gatherDeps_mi at fmr j =
  case j of
    JSModuleImportDeclaration a imd -> do
      imd' <- gatherDeps_imd (srcloc_at "import" (tp a) at) fmr imd
      return $ JSModuleImportDeclaration a imd'
    mi -> return mi

gatherDeps_ast :: SrcLoc -> IORef JSBundleMap -> JSAST -> IO [JSModuleItem]
gatherDeps_ast at fmr j =
  case j of
    JSAstModule mis _ ->
      mapM (gatherDeps_mi at fmr) mis
    _ ->
      expect_throw at (Err_Parse_NotModule j)

updatePartialAvoidCycles :: Ord a => SrcLoc -> IORef (BundleMap a b) -> Maybe a -> [a] -> (() -> IO a) -> (a -> c) -> (a -> ParserError) -> (a -> IO b) -> IO c
updatePartialAvoidCycles at fmr mfrom def_a get_key ret_key err_key proc_key = do
  key <- get_key ()
  let res = ret_key key
  (dm, fm) <- readIORef fmr
  case (M.lookup key fm) of
    Nothing -> do
      writeIORef fmr ((M.insert key def_a dm), (M.insert key Nothing fm))
      content <- proc_key key
      (dm', fm') <- readIORef fmr
      let fm'' = (M.insert key (Just content) fm')
          add_key ml = Just $ key : (maybe [] (\x -> x) ml)
          dm'' = case mfrom of
            Just from -> (M.alter add_key from dm')
            Nothing -> dm'
      writeIORef fmr (dm'', fm'')
      return res
    Just Nothing ->
      expect_throw at $ err_key key
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

gatherDeps_ast_rewriteErr :: SrcLoc -> IORef JSBundleMap -> String -> IO [JSModuleItem]
gatherDeps_ast_rewriteErr at' fmr s = case parseModule s (show $ get_srcloc_src at') of
  Left e -> error $ tryPrettifyError at' e -- TODO: prettify
  Right r -> gatherDeps_ast at' fmr r

data GatherContext = GatherTop | GatherNotTop
  deriving (Eq)

gatherDeps_file :: GatherContext -> SrcLoc -> IORef JSBundleMap -> FilePath -> IO FilePath
gatherDeps_file gctxt at fmr src_rel =
  updatePartialAvoidCycles at fmr (gatherDeps_from at) [ReachStdLib] get_key ret_key err_key proc_key
  where
    get_key () = do
      src_abs <- makeAbsolute src_rel
      reRel <- makeRelativeToCurrentDirectory src_abs
      when (gctxt == GatherNotTop) $ do
        when (isAbsolute src_rel) $ do
          expect_throw at (Err_Parse_ImportAbsolute src_rel)
        when ("../" `isPrefixOf` reRel) $ do
          expect_throw at (Err_Parse_ImportDotDot src_rel)
      return $ ReachSourceFile src_abs
    ret_key (ReachSourceFile x) = x
    ret_key (ReachStdLib) = no_stdlib
    no_stdlib = impossible $ "gatherDeps_file: source file became stdlib"
    err_key x = Err_Parse_CyclicImport x
    proc_key (ReachStdLib) = no_stdlib
    proc_key src@(ReachSourceFile src_abs) = do
      let at' = srcloc_src src
      setLocaleEncoding utf8
      content <- readFile src_abs
      withCurrentDirectory
        (takeDirectory src_abs)
        (gatherDeps_ast_rewriteErr at' fmr content)

gatherDeps_stdlib :: SrcLoc -> IORef JSBundleMap -> IO ()
gatherDeps_stdlib at fmr =
  updatePartialAvoidCycles at fmr (gatherDeps_from at) [] get_key ret_key err_key proc_key
  where
    get_key () = return $ ReachStdLib
    ret_key _ = ()
    err_key x = Err_Parse_CyclicImport x
    proc_key _ = do
      let at' = srcloc_src ReachStdLib
      (gatherDeps_ast_rewriteErr at' fmr $ B.unpack stdlib_rsh)

map_order :: Ord a => M.Map a [a] -> [a]
map_order dm = order
  where
    order = map (getNodePart . nodeFromVertex) order_v
    order_v = G.topSort graph
    (graph, nodeFromVertex, _vertexFromKey) = G.graphFromEdges edgeList
    edgeList = map (\(from, to) -> (from, from, to)) $ M.toList dm
    getNodePart (n, _, _) = n

gatherDeps_top :: FilePath -> IO JSBundle
gatherDeps_top src_p = do
  fmr <- newIORef (mempty, mempty)
  let at = srcloc_top
  _src_abs_p <- gatherDeps_file GatherTop at fmr src_p
  gatherDeps_stdlib at fmr
  (dm, fm) <- readIORef fmr
  return $ JSBundle $ map (\k -> (k, ensureJust (fm M.! k))) $ map_order dm
  where
    ensureJust Nothing = impossible "gatherDeps: Did not close all Reach files"
    ensureJust (Just x) = x
