module Reach.NL_Parser (ParserError(..), JSBundle(..), parseJSFormals, parseJSArrowFormals, jse_expect_id, gatherDeps_top) where

import Control.DeepSeq
import qualified Data.ByteString.Char8 as B
import qualified Data.Graph as G
import Data.IORef
import qualified Data.Map.Strict as M
import GHC.Generics (Generic)
import GHC.IO.Encoding
import Language.JavaScript.Parser
import Language.JavaScript.Parser.AST
import Reach.EmbeddedFiles
import Reach.JSUtil
import Reach.NL_AST
import Reach.Util
import System.Directory
import System.FilePath

data ParserError
  = Err_Parse_CyclicImport ReachSource
  | Err_Parser_Arrow_NoFormals
  | Err_Parse_ExpectIdentifier JSExpression
  | Err_Parse_IllegalBinOp JSBinOp
  | Err_Parse_IllegalLiteral String
  | Err_Parse_IllegalUnaOp JSUnaryOp
  | Err_Parse_NotModule JSAST
  deriving (Generic, Eq)

--- FIXME implement a custom show that is useful
instance Show ParserError where
  show (Err_Parse_CyclicImport rs) =
    "Cyclic import! " <> show rs
  show Err_Parser_Arrow_NoFormals =
    "Arg list for function is missing."
  show (Err_Parse_ExpectIdentifier _e) =
    "Expected identifier, got expression."
  show (Err_Parse_IllegalBinOp op) =
    "Illegal binary operation: " <> show op
  show (Err_Parse_IllegalLiteral lit) =
    "Illegal literal: " <> show lit
  show (Err_Parse_IllegalUnaOp unop) =
    "Illegal unary operator: " <> show unop
  show (Err_Parse_NotModule ast) =
    "Not a module: " <> (take 256 $ show ast)

--- Helpers
jse_expect_id :: SrcLoc -> JSExpression -> String
jse_expect_id at j =
  case j of
    (JSIdentifier _ x) -> x
    _ -> expect_throw at (Err_Parse_ExpectIdentifier j)

--- FIXME Support more binding forms
parseJSFormals :: SrcLoc -> JSCommaList JSExpression -> [SLVar]
parseJSFormals at jsformals = map (jse_expect_id at) $ jscl_flatten jsformals

parseJSArrowFormals :: SrcLoc -> JSArrowParameterList -> [SLVar]
parseJSArrowFormals at aformals =
  case aformals of
    JSUnparenthesizedArrowParameter (JSIdentName _ x) -> [x]
    JSUnparenthesizedArrowParameter JSIdentNone ->
      expect_throw at Err_Parser_Arrow_NoFormals
    JSParenthesizedArrowParameterList _ l _ ->
      parseJSFormals at l

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
      s_abs <- gatherDeps_file (srcloc_at "import from" (tp ab) at) fmr $ trimQuotes s
      return $ JSImportDeclaration ic (JSFromClause ab aa s_abs) sm
    JSImportDeclarationBare a s sm -> do
      s_abs <- gatherDeps_file (srcloc_at "import bare" (tp a) at) fmr $ trimQuotes s
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

gatherDeps_file :: SrcLoc -> IORef JSBundleMap -> FilePath -> IO FilePath
gatherDeps_file at fmr src_rel =
  updatePartialAvoidCycles at fmr (gatherDeps_from at) [ReachStdLib] get_key ret_key err_key proc_key
  where
    get_key () = do
      src_abs <- makeAbsolute src_rel
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
        (gatherDeps_ast at' fmr $ readJsModule content)

gatherDeps_stdlib :: SrcLoc -> IORef JSBundleMap -> IO ()
gatherDeps_stdlib at fmr =
  updatePartialAvoidCycles at fmr (gatherDeps_from at) [] get_key ret_key err_key proc_key
  where
    get_key () = return $ ReachStdLib
    ret_key _ = ()
    err_key x = Err_Parse_CyclicImport x
    proc_key _ = do
      let at' = srcloc_src ReachStdLib
      (gatherDeps_ast at' fmr $ readJsModule $ B.unpack stdlib_exp_rsh)

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
  _src_abs_p <- gatherDeps_file at fmr src_p
  gatherDeps_stdlib at fmr
  (dm, fm) <- readIORef fmr
  return $ JSBundle $ map (\k -> (k, ensureJust (fm M.! k))) $ map_order dm
  where
    ensureJust Nothing = impossible "gatherDeps: Did not close all Reach files"
    ensureJust (Just x) = x
