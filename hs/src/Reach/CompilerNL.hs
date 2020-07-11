{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Reach.CompilerNL where

import Data.IORef
import System.Directory
import System.FilePath
import Language.JavaScript.Parser
import Language.JavaScript.Parser.AST
--import Text.ParserCombinators.Parsec.Number (numberValue)
--import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import qualified Data.Set as S
--import Control.Monad
import Data.FileEmbed
import GHC.IO.Encoding
--import Data.Data
--import Test.SmallCheck.Series
--import GHC.Generics
--import qualified Data.ByteString as BS

import Reach.Util
import Reach.Compiler(CompilerOpts, output, source)

-- JavaScript Helpers
string_trim_quotes :: [a] -> [a]
string_trim_quotes x = reverse $ tail $ reverse $ tail x

-- Static Language
data ReachSource
  = ReachStdLib
  | ReachSourceFile FilePath
  deriving (Eq,Show,Ord)

instance Ord TokenPosn where
  compare (TokenPn x_a x_l x_c) (TokenPn y_a y_l y_c) = compare [x_a, x_l, x_c] [y_a, y_l, y_c] 

data SrcLoc
  = SrcLoc_Top
  | SrcLoc_Src FilePath ReachSource SrcLoc
  | SrcLoc_At String (Maybe TokenPosn) SrcLoc
  deriving (Eq,Show,Ord)

type SLVar = String

data SecurityLevel
  = Secret
  | Public
  deriving (Show,Eq)

instance Semigroup SecurityLevel where
  Secret <> _ = Secret
  _ <> Secret = Secret
  Public <> Public = Public

instance Monoid SecurityLevel where
  mempty = Public

data SLVal
  = SLV_NLVarRef SrcLoc SecurityLevel NLVar
  | SLV_Declassify SrcLoc SLVal
  deriving (Eq,Show)

type SLEnv = M.Map SLVar SLVal

type SLLib = SLEnv

-- Dynamic Language
data NLPart
  = NLPart SrcLoc String
  deriving (Eq,Show,Ord)

data NLType --- XXX
  deriving (Eq,Show)

data NLVar
  = NLVar SrcLoc String Int NLType
  deriving (Eq,Show)

data NLLocalExpr --- XXX
  deriving (Eq,Show)

data NLLocalStep --- XXX
  deriving (Eq,Show)

data NLConsensusExpr --- XXX
  deriving (Eq,Show)

data NLConsensusStep --- XXX
  deriving (Eq,Show)

data NL_PubOkay
  = NL_PubOkay Bool (Seq.Seq NLVar) NLConsensusExpr NLConsensusStep
  deriving (Eq,Show)

data NL_PubTimeout
  = NL_PubTimeout NLConsensusExpr NLStep
  deriving (Eq,Show)

data NLTransfer
  = NL_Pub SrcLoc NLPart NL_PubOkay (Maybe NL_PubTimeout)
  deriving (Eq,Show)

data NLStep
  = NL_Step SrcLoc (M.Map NLPart NLLocalStep) NLTransfer
  deriving (Eq,Show)

data NLProgram
  = NL_Prog SrcLoc (S.Set NLPart) NLStep
  deriving (Eq,Show)

-- General compiler utilities
tp :: JSAnnot -> Maybe TokenPosn
tp (JSAnnot x _) = Just x
tp JSAnnotSpace = Nothing
tp JSNoAnnot = Nothing

srcloc_jsa :: String -> JSAnnot -> SrcLoc -> SrcLoc
srcloc_jsa s a at = SrcLoc_At s (tp a) at

data CompilerError
  = Err_Parse_NotModule JSAST
  | Err_Parse_CyclicImport ReachSource
  | Err_Exe_NoHeader [JSModuleItem]
  | Err_Exe_EarlyEnd
  | Err_UnsupportedJS String
  deriving (Eq,Show)

expect_throw :: SrcLoc -> CompilerError -> b
expect_throw src ce = error $ "XXX " ++ show src ++ " " ++ show ce

-- Parser
type JSBundleMapPartial = M.Map ReachSource (Maybe [JSModuleItem])
type JSBundleMap = M.Map ReachSource [JSModuleItem]
data JSBundle = JSBundle ReachSource JSBundleMap
  deriving (Eq,Show)

gatherDeps_imd :: SrcLoc -> IORef JSBundleMapPartial -> JSImportDeclaration -> IO JSImportDeclaration
gatherDeps_imd at fmr j =
  case j of
    JSImportDeclaration ic (JSFromClause ab aa s) sm -> do
      s_abs <- gatherDeps_file (SrcLoc_At "import from" (tp ab) at) fmr $ string_trim_quotes s
      return $ JSImportDeclaration ic (JSFromClause ab aa s_abs) sm
    JSImportDeclarationBare a s sm -> do
      s_abs <- gatherDeps_file (SrcLoc_At "import bare" (tp a) at) fmr $ string_trim_quotes s
      return $ JSImportDeclarationBare a s_abs sm

gatherDeps_mi :: SrcLoc -> IORef JSBundleMapPartial -> JSModuleItem -> IO JSModuleItem
gatherDeps_mi at fmr j =
  case j of
    JSModuleImportDeclaration a imd -> do
      imd' <- gatherDeps_imd (SrcLoc_At "import" (tp a) at) fmr imd
      return $ JSModuleImportDeclaration a imd'
    mi -> return mi

gatherDeps_ast :: SrcLoc -> IORef JSBundleMapPartial -> JSAST -> IO [JSModuleItem]
gatherDeps_ast at fmr j =
  case j of
    JSAstModule mis _ ->
      mapM (gatherDeps_mi at fmr) mis
    _ ->
      expect_throw at (Err_Parse_NotModule j)

updatePartialAvoidCycles :: Ord a => SrcLoc -> IORef (M.Map a (Maybe b)) -> (() -> IO a) -> (a -> c) -> (a -> CompilerError) -> (a -> IO b) -> IO c
updatePartialAvoidCycles at fmr get_key ret_key err_key proc_key = do
  key <- get_key ()
  let res = ret_key key
  fm <- readIORef fmr
  case (M.lookup key fm) of
    Nothing -> do
      writeIORef fmr (M.insert key Nothing fm)
      content <- proc_key key
      fm' <- readIORef fmr
      writeIORef fmr (M.insert key (Just content) fm')
      return res
    Just Nothing ->
      expect_throw at $ err_key key
    Just (Just _) ->
      return res

gatherDeps_file :: SrcLoc -> IORef JSBundleMapPartial -> FilePath -> IO FilePath
gatherDeps_file at fmr src_rel =
  updatePartialAvoidCycles at fmr get_key ret_key err_key proc_key
  where get_key () = do
          src_abs <- makeAbsolute src_rel
          return $ ReachSourceFile src_abs
        ret_key (ReachSourceFile x) = x
        ret_key (ReachStdLib) = no_stdlib
        no_stdlib = impossible $ "gatherDeps_file: source file became stdlib"
        err_key x = Err_Parse_CyclicImport x
        proc_key (ReachStdLib) = no_stdlib
        proc_key src@(ReachSourceFile src_abs) = do
          let at' = SrcLoc_Src src_rel src at
          setLocaleEncoding utf8
          content <- readFile src_abs
          withCurrentDirectory (takeDirectory src_abs)
            (gatherDeps_ast at' fmr $ readJsModule content)

stdlib_str :: String
stdlib_str = $(embedStringFile "./rsh/stdlib.rsh")

gatherDeps_stdlib :: SrcLoc -> IORef JSBundleMapPartial -> IO ()
gatherDeps_stdlib at fmr =
  updatePartialAvoidCycles at fmr get_key ret_key err_key proc_key
  where get_key () = return $ ReachStdLib
        ret_key _ = ()
        err_key x = Err_Parse_CyclicImport x
        proc_key _ = do
          let at' = SrcLoc_Src "(standard library)" ReachStdLib at
          (gatherDeps_ast at' fmr $ readJsModule stdlib_str)

gatherDeps_top :: FilePath -> IO JSBundle
gatherDeps_top src_p = do
  fmr <- newIORef mempty
  let at = SrcLoc_Top
  src_abs_p <- gatherDeps_file at fmr src_p
  let src = (ReachSourceFile src_abs_p)
  gatherDeps_stdlib (SrcLoc_Src src_p src at) fmr
  fm' <- readIORef fmr
  return $ JSBundle src $ M.map ensureJust fm'
  where ensureJust (Just x) = x
        ensureJust Nothing = impossible $ "gatherDeps_top : did not close all Reach source files"

-- Compiler
data SLCtxt
  = SLC_Exe
  | SLC_ExeBody [JSModuleItem] SLLocalSt SLCtxt
  deriving (Eq,Show)

data SLGlobalSt = SLGlobalSt
  { mods_js :: JSBundleMap
  , mods :: M.Map ReachSource SLLib
  , next_var :: Int
  , context :: SLCtxt
  }
  deriving (Eq,Show)

data SLLocalSt = SLLocalSt
  { outer_at :: SrcLoc
  , prev_at :: SrcLoc
  , con_env :: SLEnv
  , part_envs :: M.Map NLPart SLEnv
  }
  deriving (Eq,Show)

compileExeTopStmt :: SLGlobalSt -> SLLocalSt -> JSStatement -> [JSModuleItem] -> NLProgram
compileExeTopStmt _gst _lst s _tail = error $ "XXX " ++ (take 256 $ show s)
  
compileExeTop :: SLGlobalSt -> SLLocalSt -> [JSModuleItem] -> NLProgram
compileExeTop gst lst mis =
  case mis of
    [] -> expect_throw (prev_at lst) Err_Exe_EarlyEnd
    (mi:tail_mis) ->
      case mi of
        (JSModuleImportDeclaration _a imd) ->
          error $ "XXX " ++ (take 256 $ show imd)
        (JSModuleExportDeclaration a _) ->
          expect_throw (srcloc_jsa "module export declaration" a (outer_at lst)) (Err_UnsupportedJS "module exports are not supported")
        (JSModuleStatementListItem s) ->
          compileExeTopStmt gst lst s tail_mis
          
compileExe :: SLGlobalSt -> SLLocalSt -> ReachSource -> NLProgram
compileExe gst lst exe = compileExeTop gst' lst' exe_mis'
  where t_mod_js = mods_js gst
        exe_mis = (t_mod_js M.! exe)
        outer_at' = SrcLoc_Src "main executable" exe (outer_at lst)
        t_mods_js' = M.delete exe t_mod_js
        gst' = gst { mods_js = t_mods_js' }
        lst' = lst { outer_at = outer_at'
                   , prev_at = prev_at' }
        (prev_at', exe_mis') =
          case exe_mis of
            ((JSModuleStatementListItem (JSExpressionStatement (JSStringLiteral _ "\'reach 0.1 exe\'") (JSSemi a))):j) -> ((srcloc_jsa "exe header" a outer_at'), j)
            _ -> expect_throw outer_at' (Err_Exe_NoHeader exe_mis)

compileBundle :: JSBundle -> NLProgram
compileBundle (JSBundle exe t_mods_js) =
  compileExe gst lst exe
  where gst = (SLGlobalSt
               { mods_js = t_mods_js
               , mods = mempty
               , next_var = 0
               , context = SLC_Exe })
        lst = (SLLocalSt
                { outer_at = SrcLoc_Top
                , prev_at = SrcLoc_Top
                , con_env = mempty
                , part_envs = mempty })

-- Main entry point
compileNL :: CompilerOpts -> IO ()
compileNL copts = do
  let out = output copts
  djp <- gatherDeps_top $ source copts
  let nlp = compileBundle djp
  out "nl" $ show nlp
  return ()
