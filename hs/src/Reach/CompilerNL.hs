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
import Text.ParserCombinators.Parsec.Number (numberValue)
import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Data.Graph as G
--import Control.Monad
import Data.FileEmbed
import GHC.IO.Encoding
--import Data.Data
--import Test.SmallCheck.Series
--import GHC.Generics
--import qualified Data.ByteString as BS
import Data.List

import Reach.Util
import Reach.Compiler(CompilerOpts, output, source)

-- Helpers
zipEq :: SrcLoc -> (Int -> Int -> CompilerError) -> [a] -> [b] -> [(a, b)]
zipEq at ce x y =
  if lx == ly then zip x y
  else expect_throw at (ce lx ly)
  where lx = length x
        ly = length y

-- JavaScript Helpers
string_trim_quotes :: [a] -> [a]
string_trim_quotes x = reverse $ tail $ reverse $ tail x

jscl_flatten :: JSCommaList a -> [a]
jscl_flatten (JSLCons a _ b) = (jscl_flatten a) ++ [b]
jscl_flatten (JSLOne a) = [a]
jscl_flatten (JSLNil) = []

jsctl_flatten :: JSCommaTrailingList a -> [a]
jsctl_flatten (JSCTLComma a _) = jscl_flatten  a
jsctl_flatten (JSCTLNone a) = jscl_flatten  a

jsa_flatten :: [JSArrayElement] -> [JSExpression]
jsa_flatten a = concatMap f a
  where f (JSArrayComma _) = []
        f (JSArrayElement e) = [e]

jse_expect_id :: SrcLoc -> JSExpression -> String
jse_expect_id at j =
  case j of
    (JSIdentifier _ x) -> x
    _ -> expect_throw at (Err_Parse_ExpectIdentifier j)

-- Static Language
data ReachSource
  = ReachStdLib
  | ReachSourceFile FilePath
  deriving (Eq,Ord)

instance Show ReachSource where
  show ReachStdLib = "reach standard library"
  show (ReachSourceFile fp) = fp

instance Ord TokenPosn where
  compare (TokenPn x_a x_l x_c) (TokenPn y_a y_l y_c) =
    compare [x_a, x_l, x_c] [y_a, y_l, y_c] 

data SrcLoc
  = SrcLoc_Top
  | SrcLoc_Src ReachSource SrcLoc
  | SrcLoc_At String (Maybe TokenPosn) SrcLoc
  deriving (Eq,Ord)

instance Show SrcLoc where
  show SrcLoc_Top = "\tcommand invocation"
  show (SrcLoc_Src src more) = "\tin " ++ show src ++ "\n" ++ show more
  show (SrcLoc_At lab mtp more) = "\tat " ++ stp ++ "(" ++ lab ++ ")" ++ "\n" ++ show more
    where stp = case mtp of
                  Nothing -> ""
                  Just (TokenPn _ l c) -> show l ++ ":" ++ show c ++ " "

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
  = SLV_Clo SrcLoc [SLVar] JSBlock SLEnv
  | SLV_Array SrcLoc [SLVal]
  | SLV_Object SrcLoc SLEnv
  | SLV_Prim SLPrimitive
  | SLV_Int SrcLoc Int
  | SLV_Bool SrcLoc Bool
  | SLV_Bytes SrcLoc B.ByteString
  | SLV_Participant SrcLoc B.ByteString
  deriving (Eq, Show)

data SLPrimitive
  = SLPrim_makeEnum
  | SLPrim_DApp
  | SLPrim_DApp_Delay SrcLoc [SLVal]
  deriving (Eq,Show)

type SLEnv = M.Map SLVar SLVal

base_env :: SLEnv
base_env = M.fromList [
  ("makeEnum", SLV_Prim SLPrim_makeEnum),
  ("Reach", (SLV_Object SrcLoc_Top $
             M.fromList [
                ("DApp", SLV_Prim SLPrim_DApp)
                ]))]

env_insert :: SrcLoc -> SLVar -> SLVal -> SLEnv -> SLEnv
env_insert at k v env =
  case M.lookup k env of
    Nothing -> M.insert k v env
    Just _ ->
      expect_throw at (Err_Shadowed k)

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
srcloc_jsa lab a at = SrcLoc_At lab (tp a) at

srcloc_after_semi :: String -> JSAnnot -> JSSemi -> SrcLoc -> SrcLoc 
srcloc_after_semi lab a sp at = at'
  where at' = srcloc_jsa ("after " ++ lab) spa at
        spa = case sp of
                JSSemi x -> x
                _ -> expect_throw (srcloc_jsa lab a at) Err_Parse_ExpectSemi

--- XXX Maybe this is dumb
data CompilerError
  = Err_Parse_NotModule JSAST
  | Err_Parse_CyclicImport ReachSource
  | Err_Parse_ExpectSemi
  | Err_Parse_ExpectIdentifier JSExpression
  | Err_Parse_IllegalLiteral String
  | Err_NoHeader [JSModuleItem]
  | Err_Top_IllegalJS JSStatement
  | Err_Import_ShadowedImport SLVar
  | Err_Import_IllegalJS JSImportDeclaration
  | Err_Export_IllegalJS JSExportDeclaration
  | Err_Decl_IllegalJS JSExpression
  | Err_DeclLHS_IllegalJS JSExpression
  | Err_Decl_WrongArrayLength Int Int
  | Err_Decl_NotArray SLVal
  | Err_Eval_IllegalJS JSExpression
  | Err_Eval_UnboundId SLVar
  | Err_Apply_ArgCount Int Int
  | Err_Eval_NotObject SLVal
  | Err_Eval_NotApplicable SLVal
  | Err_Eval_IfCondNotBool SLVal
  | Err_Obj_IllegalJS JSObjectProperty
  | Err_Fun_NamesIllegal
  | Err_Arrow_NoFormals
  | Err_Dot_InvalidField SLVal String
  | Err_Prim_InvalidArgs SLPrimitive [SLVal]
  | Err_Shadowed SLVar
  | Err_Top_NotDApp SLVal
  | Err_DApp_PartNotString SLVal
  deriving (Eq,Show)

expect_throw :: SrcLoc -> CompilerError -> b
expect_throw src ce = error $ "error:\n\t" ++ (take 128 $ show ce) ++ "\nat:\n" ++ show src

-- Parser
type BundleMap a b = ((M.Map a [a]), (M.Map a (Maybe b)))
type JSBundleMap = BundleMap ReachSource [JSModuleItem]
data JSBundle = JSBundle [(ReachSource,[JSModuleItem])]
  deriving (Eq,Show)

gatherDeps_imd :: SrcLoc -> IORef JSBundleMap -> JSImportDeclaration -> IO JSImportDeclaration
gatherDeps_imd at fmr j =
  case j of
    JSImportDeclaration ic (JSFromClause ab aa s) sm -> do
      s_abs <- gatherDeps_file (SrcLoc_At "import from" (tp ab) at) fmr $ string_trim_quotes s
      return $ JSImportDeclaration ic (JSFromClause ab aa s_abs) sm
    JSImportDeclarationBare a s sm -> do
      s_abs <- gatherDeps_file (SrcLoc_At "import bare" (tp a) at) fmr $ string_trim_quotes s
      return $ JSImportDeclarationBare a s_abs sm

gatherDeps_mi :: SrcLoc -> IORef JSBundleMap -> JSModuleItem -> IO JSModuleItem
gatherDeps_mi at fmr j =
  case j of
    JSModuleImportDeclaration a imd -> do
      imd' <- gatherDeps_imd (SrcLoc_At "import" (tp a) at) fmr imd
      return $ JSModuleImportDeclaration a imd'
    mi -> return mi

gatherDeps_ast :: SrcLoc -> IORef JSBundleMap -> JSAST -> IO [JSModuleItem]
gatherDeps_ast at fmr j =
  case j of
    JSAstModule mis _ ->
      mapM (gatherDeps_mi at fmr) mis
    _ ->
      expect_throw at (Err_Parse_NotModule j)

updatePartialAvoidCycles :: Ord a => SrcLoc -> IORef (BundleMap a b) -> Maybe a -> [a] -> (() -> IO a) -> (a -> c) -> (a -> CompilerError) -> (a -> IO b) -> IO c
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
          add_key ml = Just $ key : (maybe [] (\x->x) ml)
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
gatherDeps_from SrcLoc_Top = Nothing
gatherDeps_from (SrcLoc_Src rs _) = Just rs
gatherDeps_from (SrcLoc_At _ _ sl) = gatherDeps_from sl

gatherDeps_file :: SrcLoc -> IORef JSBundleMap -> FilePath -> IO FilePath
gatherDeps_file at fmr src_rel =
  updatePartialAvoidCycles at fmr (gatherDeps_from at) [ReachStdLib] get_key ret_key err_key proc_key
  where get_key () = do
          src_abs <- makeAbsolute src_rel
          return $ ReachSourceFile src_abs
        ret_key (ReachSourceFile x) = x
        ret_key (ReachStdLib) = no_stdlib
        no_stdlib = impossible $ "gatherDeps_file: source file became stdlib"
        err_key x = Err_Parse_CyclicImport x
        proc_key (ReachStdLib) = no_stdlib
        proc_key src@(ReachSourceFile src_abs) = do
          let at' = SrcLoc_Src src at
          setLocaleEncoding utf8
          content <- readFile src_abs
          withCurrentDirectory (takeDirectory src_abs)
            (gatherDeps_ast at' fmr $ readJsModule content)

stdlib_str :: String
stdlib_str = $(embedStringFile "./rsh/stdlib.rsh")

gatherDeps_stdlib :: SrcLoc -> IORef JSBundleMap -> IO ()
gatherDeps_stdlib at fmr =
  updatePartialAvoidCycles at fmr (gatherDeps_from at) [] get_key ret_key err_key proc_key
  where get_key () = return $ ReachStdLib
        ret_key _ = ()
        err_key x = Err_Parse_CyclicImport x
        proc_key _ = do
          let at' = SrcLoc_Src ReachStdLib at
          (gatherDeps_ast at' fmr $ readJsModule stdlib_str)

map_order :: Ord a => M.Map a [a] -> [a]
map_order dm = order
  where order = map (getNodePart . nodeFromVertex) order_v
        order_v = G.topSort graph
        (graph, nodeFromVertex, _vertexFromKey) = G.graphFromEdges edgeList
        edgeList = map (\(from,to) -> (from,from,to)) $ M.toList dm
        getNodePart (n, _, _) = n

gatherDeps_top :: FilePath -> IO JSBundle
gatherDeps_top src_p = do
  fmr <- newIORef (mempty, mempty)
  let at = SrcLoc_Top
  _src_abs_p <- gatherDeps_file at fmr src_p
  gatherDeps_stdlib at fmr
  (dm, fm) <- readIORef fmr
  return $ JSBundle $ map (\k -> (k, ensureJust (fm M.! k))) $ map_order dm
  where ensureJust Nothing = impossible "gatherDeps: Did not close all Reach files"
        ensureJust (Just x) = x

-- Compiler
evalDot :: SrcLoc -> SLVal -> String -> SLVal
evalDot at obj field =
  case obj of
    SLV_Object _ env ->
      case M.lookup field env of
        Just v -> v
        Nothing ->
          expect_throw at (Err_Dot_InvalidField obj field)
    v ->
      expect_throw at (Err_Eval_NotObject v)

evalPrim :: SrcLoc -> SLPrimitive -> [SLVal] -> SLVal
evalPrim at p args =
  case p of
    SLPrim_DApp ->
      case args of
        [ (SLV_Object _ _), (SLV_Array _ _), (SLV_Clo _ _ _ _) ] ->
          SLV_Prim $ SLPrim_DApp_Delay at args
        _ ->
          expect_throw at (Err_Prim_InvalidArgs p args)
    SLPrim_makeEnum ->
      case args of
        [ SLV_Int _ i ] ->
          SLV_Array at' (enum_pred : map (SLV_Int at') [ 0 .. (i-1) ])
          where at' = (SrcLoc_At "makeEnum" Nothing at)
                --- XXX This sucks... maybe parse an embed string? Would that suck less?
                enum_pred = SLV_Clo at' ["x"] pbody mempty
                pbody = JSBlock JSNoAnnot [(JSReturn JSNoAnnot (Just (JSExpressionBinary lhs (JSBinOpAnd JSNoAnnot) rhs)) JSSemiAuto)] JSNoAnnot
                lhs = (JSExpressionBinary (JSDecimal JSNoAnnot "0") (JSBinOpLe JSNoAnnot) (JSIdentifier JSNoAnnot "x"))
                rhs = (JSExpressionBinary (JSIdentifier JSNoAnnot "x") (JSBinOpLt JSNoAnnot) (JSDecimal JSNoAnnot (show i)))
        _ ->
          expect_throw at (Err_Prim_InvalidArgs p args)
    SLPrim_DApp_Delay _ _ ->
      expect_throw at (Err_Eval_NotApplicable $ SLV_Prim p)

binaryToPrim :: JSBinOp -> SLPrimitive
binaryToPrim o = error $ "XXX binaryToPrim " ++ show o

unaryToPrim :: JSUnaryOp -> SLPrimitive
unaryToPrim o = error $ "XXX unaryToPrim " ++ show o

parseJSFormals :: SrcLoc -> JSCommaList JSExpression -> [SLVar]
parseJSFormals at jsformals = map (jse_expect_id at) $ jscl_flatten jsformals

parseJSArrowFormals :: SrcLoc -> JSArrowParameterList -> [SLVar]
parseJSArrowFormals at aformals =
  case aformals of
    JSUnparenthesizedArrowParameter (JSIdentName _ x) -> [x]
    JSUnparenthesizedArrowParameter JSIdentNone ->
      expect_throw at Err_Arrow_NoFormals
    JSParenthesizedArrowParameterList _ l _ ->
      parseJSFormals at l

evalApply :: SrcLoc -> SLVal -> [SLVal] -> SLVal
evalApply at rator rands =
  --- XXX Update call stack
  case rator of
    SLV_Prim p ->
      evalPrim at p rands
    SLV_Clo clo_at formals body env ->
      evalBlock clo_at env' body
      where env' = foldl' (flip (uncurry (env_insert clo_at))) env kvs
            kvs = zipEq clo_at Err_Apply_ArgCount formals rands
    v ->
      expect_throw at (Err_Eval_NotApplicable v)

evalExpr :: SrcLoc -> SLEnv -> JSExpression -> SLVal
evalExpr at env e =
  case e of
    JSIdentifier a x ->
      case M.lookup x env of
        Just v -> v
        Nothing ->
          expect_throw (srcloc_jsa "id ref" a at) (Err_Eval_UnboundId x)
    JSDecimal a ns -> SLV_Int (srcloc_jsa "decimal" a at) $ numberValue 10 ns
    JSLiteral a l ->
      case l of
        "true" -> SLV_Bool (srcloc_jsa "true" a at) True
        "false" -> SLV_Bool (srcloc_jsa "false" a at) False
        _ -> expect_throw (srcloc_jsa "literal" a at) (Err_Parse_IllegalLiteral l)
    JSHexInteger a ns -> SLV_Int (srcloc_jsa "hex" a at) $ numberValue 16 ns
    JSOctal a ns -> SLV_Int (srcloc_jsa "octal" a at) $ numberValue 8 ns
    JSStringLiteral a s -> SLV_Bytes (srcloc_jsa "string" a at) (bpack (string_trim_quotes s))
    JSRegEx _ _ -> illegal
    JSArrayLiteral a as _ -> SLV_Array at' $ map (evalExpr at' env) $ jsa_flatten as
      where at' = (srcloc_jsa "array" a at)
    JSAssignExpression _ _ _ -> illegal
    JSAwaitExpression _ _ -> illegal
    JSCallExpression rator a rands _ -> doCall rator a $ jscl_flatten rands
    JSCallExpressionDot obj a field -> doDot obj a field
    JSCallExpressionSquare arr a idx _ -> doRef arr a idx
    JSClassExpression _ _ _ _ _ _ -> illegal
    JSCommaExpression _ _ _ -> illegal
    JSExpressionBinary lhs op rhs -> doCallV (SLV_Prim (binaryToPrim op)) JSNoAnnot [ lhs, rhs ]
    JSExpressionParen a ie _ -> evalExpr (srcloc_jsa "paren" a at) env ie
    JSExpressionPostfix _ _ -> illegal
    JSExpressionTernary c a t _ f ->
      case evalExpr at' env c of
        SLV_Bool _ cb -> evalExpr at' env ke
          where ke = if cb then t else f
        cv ->
          expect_throw at' (Err_Eval_IfCondNotBool cv)
      where at' = srcloc_jsa "ternary" a at
    JSArrowExpression aformals a bodys -> SLV_Clo at' formals body env
      where at' = srcloc_jsa "arrow" a at
            body = JSBlock JSNoAnnot [bodys] JSNoAnnot
            formals = parseJSArrowFormals at' aformals
    JSFunctionExpression a name _ jsformals _ body ->
      case name of
        JSIdentNone -> SLV_Clo at' formals body env
        JSIdentName na _ -> expect_throw (srcloc_jsa "function name" na at') Err_Fun_NamesIllegal
      where at' = srcloc_jsa "function exp" a at
            formals = parseJSFormals at' jsformals
    JSGeneratorExpression _ _ _ _ _ _ _ -> illegal
    JSMemberDot obj a field -> doDot obj a field
    JSMemberExpression rator a rands _ -> doCall rator a $ jscl_flatten rands
    JSMemberNew _ _ _ _ _ -> illegal
    JSMemberSquare arr a idx _ -> doRef arr a idx
    JSNewExpression _ _ -> illegal
    JSObjectLiteral a plist _ -> SLV_Object at' fields
      where at' = srcloc_jsa "obj" a at
            fields = foldl' add_field mempty $ jsctl_flatten plist
            add_field _fenv op =
              case op of
                JSPropertyNameandValue _pn _ _vs ->
                  error "XXX JSPropertyNameandValue"
                _ ->
                  expect_throw at' (Err_Obj_IllegalJS op)
    JSSpreadExpression _ _ -> illegal
    JSTemplateLiteral _ _ _ _ -> illegal
    JSUnaryExpression op ue -> doCallV (SLV_Prim (unaryToPrim op)) JSNoAnnot [ ue ]
    JSVarInitExpression _ _ -> illegal
    JSYieldExpression _ _ -> illegal
    JSYieldFromExpression _ _ _ -> illegal
  where illegal = expect_throw at (Err_Eval_IllegalJS e)
        doCallV ratorv a rands = evalApply at' ratorv randvs
          where at' = srcloc_jsa "application" a at
                randvs = map (evalExpr at' env) rands
        doCall rator a rands = doCallV (evalExpr at' env rator) a rands          
          where at' = srcloc_jsa "application, rator" a at
        doDot obj a field = evalDot at' (evalExpr at' env obj) fields
          where at' = srcloc_jsa "dot" a at
                fields = (jse_expect_id at') field
        doRef _arr _a _idx = error "XXX doRef"
                
bindDeclLHS :: SrcLoc -> SLExEnv -> JSExpression -> SLVal -> SLExEnv
bindDeclLHS at exenv lhs v =
  case lhs of
    (JSIdentifier a x) ->
      exenv_insert (srcloc_jsa "id" a at) x v exenv
    (JSArrayLiteral a xs _) ->
      foldl' (flip (uncurry (exenv_insert at'))) exenv kvs
      where kvs = zipEq at' Err_Decl_WrongArrayLength ks vs
            ks = map (jse_expect_id at') $ jsa_flatten xs
            vs = case v of
              SLV_Array _ x -> x
              _ ->
                expect_throw at' (Err_Decl_NotArray v)
            at' = srcloc_jsa "array" a at
    _ ->
      expect_throw at (Err_DeclLHS_IllegalJS lhs)

evalDecl :: SrcLoc -> SLExEnv -> JSExpression -> SLExEnv
evalDecl at exenv decl =
  case decl of
    JSVarInitExpression lhs (JSVarInit a rhs) ->
      bindDeclLHS at exenv lhs v
      where at' = srcloc_jsa "var initializer" a at
            v = evalExpr at' (exenv_env exenv) rhs
    _ ->
      expect_throw at (Err_Decl_IllegalJS decl)

evalBlock :: SrcLoc -> SLEnv -> JSBlock -> SLVal
evalBlock _at _env _body =
  error $ "XXX evalBlock"

data SLTopSt = SLTopSt
  { top_prev_at :: SrcLoc
  , top_at :: SrcLoc
  , top_exenv :: SLExEnv}
  deriving (Eq,Show)

data SLExEnv = SLExEnv Bool (S.Set SLVar) SLEnv
  deriving (Eq,Show)

exenv_init :: SLEnv -> SLExEnv
exenv_init env = SLExEnv False mempty env

exenv_set :: Bool -> SLExEnv -> SLExEnv
exenv_set b (SLExEnv _ ex env) = (SLExEnv b ex env)

exenv_reset :: SLExEnv -> SLExEnv
exenv_reset exenv = exenv_set False exenv

exenv_exports :: SLExEnv -> SLEnv
exenv_exports (SLExEnv _ ex env) = M.restrictKeys env ex

exenv_env :: SLExEnv -> SLEnv
exenv_env (SLExEnv _ _ env) = env

exenv_insert :: SrcLoc -> String -> SLVal -> SLExEnv -> SLExEnv
exenv_insert at k v (SLExEnv isExport ex env) = (SLExEnv isExport ex' env')
  where env' = env_insert at k v env
        ex' = if isExport then S.insert k ex else ex

exenv_merge :: SrcLoc -> SLExEnv -> SLEnv -> SLExEnv
exenv_merge at (SLExEnv isExport ex env) libex = (SLExEnv isExport ex env')
  where env' = M.unionWithKey cm env libex
        cm k _ _ = expect_throw at (Err_Import_ShadowedImport k)

evalTop :: SLLibs -> SLTopSt -> JSModuleItem -> SLTopSt
evalTop libs st mi =
  case mi of
    (JSModuleImportDeclaration _ im) ->
      case im of
        JSImportDeclarationBare a libn sp ->
          (st { top_prev_at = srcloc_after_semi "import" a sp at
              , top_exenv = exenv_merge at' exenv libex })
          where at' = srcloc_jsa "import" a at
                libm = libs_map libs
                libex = case M.lookup (ReachSourceFile libn) libm of
                          Just x -> x
                          Nothing ->
                            impossible $ "dependency not found"
        --- XXX support more kinds
        _ -> expect_throw prev_at (Err_Import_IllegalJS im)
    (JSModuleExportDeclaration _ ed) ->
      case ed of
        JSExport s _ -> doStmt True s
        --- XXX support more kinds
        _ -> expect_throw prev_at (Err_Export_IllegalJS ed)
    (JSModuleStatementListItem s) -> doStmt False s
  where prev_at = top_prev_at st
        at = top_at st
        exenv = top_exenv st
        doStmt isExport s =
          case s of
            (JSConstant a decls sp) ->
              (st { top_prev_at = srcloc_after_semi lab a sp at
                  , top_exenv = exenv' })
              where at' = srcloc_jsa lab a at
                    lab = "const def"
                    --- Note: This makes it so that declarations on the
                    --- left are visible on the right, which might be
                    --- different than JavaScript?
                    exenv' = exenv_reset $ foldl' (evalDecl at') exenv_ $ jscl_flatten decls
            (JSFunction a (JSIdentName _ f) _ jsformals _ body sp) ->
              (st { top_prev_at = srcloc_after_semi lab a sp at
                  , top_exenv = exenv' })
              where clo = SLV_Clo at' formals body (exenv_env exenv_)
                    formals = parseJSFormals at' jsformals
                    at' = srcloc_jsa lab a at
                    lab = "function def"
                    exenv' = exenv_reset $ exenv_insert at f clo exenv_
            _ ->
              expect_throw prev_at (Err_Top_IllegalJS s)
          where exenv_ = exenv_set isExport exenv

data SLLibs = SLLibs
  { libs_map :: M.Map ReachSource SLEnv }
  deriving (Eq,Show)

init_libs :: SLLibs
init_libs = (SLLibs { libs_map = mempty })

evalLib :: (ReachSource, [JSModuleItem]) -> SLLibs -> SLLibs
evalLib (src, body) libs = libs'
  where libs' = libs { libs_map = libm' }
        libm' = M.insert src (exenv_exports $ top_exenv st') libm
        libm = (libs_map libs)
        st' = foldl' (evalTop libs) st body'
        st = (SLTopSt
               { top_prev_at = prev_at
               , top_at = at
               , top_exenv = exenv_init stdlib_env })
        stdlib_env =
          case src of
            ReachStdLib -> base_env
            ReachSourceFile _ -> M.union (libm M.! ReachStdLib) base_env
        at = (SrcLoc_Src src SrcLoc_Top)
        (prev_at, body') =
          case body of
            ((JSModuleStatementListItem (JSExpressionStatement (JSStringLiteral a "\'reach 0.1\'") sp)):j) ->
              ((srcloc_after_semi "header" a sp at), j)
            _ -> expect_throw at (Err_NoHeader body)

evalLibs :: [(ReachSource, [JSModuleItem])] -> SLLibs
evalLibs = foldr evalLib init_libs

compileDApp :: SLVal -> NLProgram
compileDApp topv =
  case topv of
    SLV_Prim (SLPrim_DApp_Delay at [ (SLV_Object _ _opts), (SLV_Array _ parts), clo ]) ->
      --- xxx look at opts
      case evalApply at' clo partvs of
        v ->
          error $ "XXX: compileDApp after " ++ show v
      where at' = SrcLoc_At "compileDApp" Nothing at
            partvs = map make_part parts
            make_part v = case v of
                            SLV_Bytes bs_at bs -> SLV_Participant bs_at bs
                            _ -> expect_throw at' (Err_DApp_PartNotString v)
    _ ->
      expect_throw SrcLoc_Top (Err_Top_NotDApp topv)

compileBundle :: JSBundle -> SLVar -> NLProgram
compileBundle (JSBundle mods) top = compileDApp topv
  where libm = libs_map $ evalLibs mods
        exe = case mods of
                [] -> impossible $ "compileBundle: no files"
                ((x,_):_) -> x
        exe_ex = libm M.! exe
        topv = case M.lookup top exe_ex of
                 Just x -> x
                 Nothing -> expect_throw SrcLoc_Top (Err_Eval_UnboundId top)

-- Main entry point
compileNL :: CompilerOpts -> IO ()
compileNL copts = do
  let out = output copts
  djp <- gatherDeps_top $ source copts
  let nlp = compileBundle djp "main"
  out "nl" $ show nlp
  return ()
