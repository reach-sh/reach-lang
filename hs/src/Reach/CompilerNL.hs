{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Reach.CompilerNL where

import Data.IORef
import Data.STRef
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
import Control.Monad.ST
import Data.FileEmbed
import GHC.IO.Encoding
import Data.Bits
--import Data.Data
--import Test.SmallCheck.Series
--import GHC.Generics
--import qualified Data.ByteString as BS
import Data.List

import Reach.Util
import Reach.Compiler(CompilerOpts, output, source)

-- Helpers
zipEq :: SrcLoc -> (Int -> Int -> CompilerError s) -> [a] -> [b] -> [(a, b)]
zipEq at ce x y =
  if lx == ly then zip x y
  else expect_throw at (ce lx ly)
  where lx = length x
        ly = length y

mapk :: ([b] -> ans) -> (a -> (b -> ans) -> ans) -> [a] -> ans
mapk k f l =
  case l of
    [] -> k []
    (x:l') -> f x k'
      where k' x' = mapk k'' f l'
              where k'' l'' = k (x':l'')

foldlk :: (b -> ans) -> (b -> a -> (b -> ans) -> ans) -> b -> [a] -> ans
foldlk k f z l =
  case l of
    [] -> k z
    (x:l') -> f z x k'
      where k' z' = foldlk k f z' l'

foldrk :: (b -> ans) -> (a -> b -> (b -> ans) -> ans) -> b -> [a] -> ans
foldrk k f z l =
  case l of
    [] -> k z
    (x:l') -> foldrk k' f z l'
      where k' z' = f x z' k

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

data SrcLoc = SrcLoc (Maybe String) (Maybe TokenPosn) (Maybe ReachSource)
  deriving (Eq,Ord)

instance Show SrcLoc where
  show (SrcLoc mlab mtp mrs) = concat $ intersperse ":" $ concat [ sr, loc, lab ]
    where lab = case mlab of Nothing -> []
                             Just s -> [s]
          sr = case mrs of Nothing -> []
                           Just s -> [show s]
          loc = case mtp of Nothing -> []
                            Just (TokenPn _ l c) -> [show l, show c]

srcloc_top :: SrcLoc
srcloc_top = SrcLoc (Just "<top level>") Nothing Nothing

srcloc_src :: ReachSource -> SrcLoc
srcloc_src rs = SrcLoc Nothing Nothing (Just rs)

srcloc_at :: String -> (Maybe TokenPosn) -> SrcLoc -> SrcLoc
srcloc_at lab mp (SrcLoc _ _ rs) = SrcLoc (Just lab) mp rs

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

data SLType
  = T_Null
  | T_Bool
  | T_UInt256
  | T_Bytes
  | T_Fun [SLType] SLType
  | T_Array [SLType]
  | T_Obj (M.Map SLVar SLType)
  | T_Forall SLVar SLType
  | T_Var SLVar
  deriving (Eq,Show,Ord)

infix 9 -->
(-->) :: [SLType] -> SLType -> SLType
dom --> rng = T_Fun dom rng

type SLPart = B.ByteString

data SLVal
  = SLV_Null SrcLoc
  | SLV_Bool SrcLoc Bool
  | SLV_Int SrcLoc Int
  | SLV_Bytes SrcLoc B.ByteString
  | SLV_Array SrcLoc [SLVal]
  | SLV_Object SrcLoc SLEnv
  | SLV_Clo SrcLoc (Maybe SLVar) [SLVar] JSBlock SLEnv
  | SLV_DLVar DLVar
  | SLV_Type SLType
  | SLV_Participant SrcLoc SLPart SLVal
  | SLV_Prim SLPrimitive
  | SLV_Form SLForm
  deriving (Eq,Show)

data ToConsensusMode
  = TCM_Publish
  | TCM_Pay
  | TCM_Timeout
  deriving (Eq,Show)

data SLForm
  = SLForm_Part_Only SLVal
  --- XXX Maybe should be DLVar
  | SLForm_Part_ToConsensus SLVal (Maybe ToConsensusMode) (Maybe [SLVar]) (Maybe SLVar) (Maybe SLVar)
  deriving (Eq,Show)

data ConsensusPrimOp
  = ADD
  | SUB
  | MUL
  | DIV
  | MOD
  | PLT
  | PLE
  | PEQ
  | PGE
  | PGT
  | IF_THEN_ELSE
  | BYTES_EQ
  | BALANCE
  | TXN_VALUE
  | LSH
  | RSH
  | BAND
  | BIOR
  | BXOR
  deriving (Show,Eq,Ord)

data PrimOp
  = CP ConsensusPrimOp
  | RANDOM
  deriving (Show,Eq,Ord)

primOpType :: PrimOp -> SLType
primOpType (CP ADD) = [T_UInt256, T_UInt256] --> T_UInt256
primOpType (CP SUB) = [T_UInt256, T_UInt256] --> T_UInt256
primOpType (CP MUL) = [T_UInt256, T_UInt256] --> T_UInt256
primOpType (CP DIV) = [T_UInt256, T_UInt256] --> T_UInt256
primOpType (CP MOD) = [T_UInt256, T_UInt256] --> T_UInt256
primOpType (CP PLT) = [T_UInt256, T_UInt256] --> T_Bool
primOpType (CP PLE) = [T_UInt256, T_UInt256] --> T_Bool
primOpType (CP PEQ) = [T_UInt256, T_UInt256] --> T_Bool
primOpType (CP PGE) = [T_UInt256, T_UInt256] --> T_Bool
primOpType (CP PGT) = [T_UInt256, T_UInt256] --> T_Bool
primOpType (CP IF_THEN_ELSE) = T_Forall "a" ([T_Bool, T_Var "a", T_Var "a"] --> T_Var "a")
primOpType (CP BYTES_EQ) = ([T_Bytes, T_Bytes] --> T_Bool)
primOpType (CP BALANCE) = ([] --> T_UInt256)
primOpType (CP TXN_VALUE) = ([] --> T_UInt256)
primOpType (CP LSH) = [T_UInt256, T_UInt256] --> T_UInt256
primOpType (CP RSH) = [T_UInt256, T_UInt256] --> T_UInt256
primOpType (CP BAND) = [T_UInt256, T_UInt256] --> T_UInt256
primOpType (CP BIOR) = [T_UInt256, T_UInt256] --> T_UInt256
primOpType (CP BXOR) = [T_UInt256, T_UInt256] --> T_UInt256
primOpType RANDOM = ([] --> T_UInt256)

data SLPrimitive
  = SLPrim_makeEnum
  | SLPrim_declassify
  | SLPrim_commit
  | SLPrim_committed
  | SLPrim_assume
  | SLPrim_interact SrcLoc String SLType
  | SLPrim_Fun
  | SLPrim_Array
  | SLPrim_DApp
  | SLPrim_DApp_Delay SrcLoc [SLVal] SLEnv
  | SLPrim_op PrimOp
  deriving (Eq,Show)

type SLEnv = M.Map SLVar SLVal

mt_env :: SLEnv
mt_env = mempty

base_env :: SLEnv
base_env = M.fromList
  [ ("makeEnum", SLV_Prim SLPrim_makeEnum)
  , ("declassify", SLV_Prim SLPrim_declassify)
  , ("commit", SLV_Prim SLPrim_commit)
  , ("assume", SLV_Prim SLPrim_assume)
  , ("Null", SLV_Type T_Null)
  , ("Bool", SLV_Type T_Bool)
  , ("UInt256", SLV_Type T_UInt256)
  , ("Bytes", SLV_Type T_Bytes)
  , ("Array", SLV_Prim SLPrim_Array)
  , ("Fun", SLV_Prim SLPrim_Fun)
  , ("Reach", (SLV_Object srcloc_top $
               M.fromList
                [ ("DApp", SLV_Prim SLPrim_DApp) ]))]

env_insert :: SrcLoc -> SLVar -> SLVal -> SLEnv -> SLEnv
env_insert at k v env =
  case M.lookup k env of
    Nothing -> M.insert k v env
    Just _ ->
      expect_throw at (Err_Shadowed k)

env_merge :: SrcLoc -> SLEnv -> SLEnv -> SLEnv
env_merge at left righte = foldl' (flip (uncurry (env_insert at))) left $ M.toList righte

env_lookup :: SrcLoc -> SLVar -> SLEnv -> SLVal
env_lookup at x env =
  case M.lookup x env of
    Just v -> v
    Nothing ->
      expect_throw at (Err_Eval_UnboundId x)

-- Dynamic Language
data DLConstant
  = DLC_Null
  | DLC_Bool Bool
  | DLC_Int Int
  | DLC_Bytes B.ByteString
  deriving (Eq,Show,Ord)

--- XXX Maybe be different?
type DLType = SLType

data DLVar = DLVar SrcLoc String Int DLType
  deriving (Eq,Show,Ord)

data DLArg
  = DLA_Var DLVar
  | DLA_Con DLConstant
  | DLA_Array [DLArg]
  | DLA_Obj (M.Map String DLArg)
  deriving (Eq,Show)

data DLLocalExpr
  = DLLE_PrimOp SrcLoc PrimOp [DLArg]
  | DLLE_Interact SrcLoc String [DLArg]
  deriving (Eq,Show)

data DLLocalStmt
  = DLLS_Let SrcLoc DLVar DLLocalExpr
  | DLLS_Assume SrcLoc DLArg
  deriving (Eq,Show)

type DLLocalStep = Seq.Seq DLLocalStmt

data DLConsensusExpr
  = DLCE_PrimOp SrcLoc ConsensusPrimOp [DLArg]
  deriving (Eq,Show)

data DLConsensusStmt
  = DLCS_Let SrcLoc DLVar DLConsensusExpr
  deriving (Eq,Show)

type DLConsensusStep = Seq.Seq DLConsensusStmt

-- Old notes
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
srcloc_jsa lab a at = srcloc_at lab (tp a) at

srcloc_after_semi :: String -> JSAnnot -> JSSemi -> SrcLoc -> SrcLoc 
srcloc_after_semi lab a sp at =
  case sp of
    JSSemi x -> srcloc_jsa (alab ++ " semicolon") x at
    _ -> srcloc_jsa alab a at
  where alab = "after " ++ lab

--- XXX Maybe this is dumb
data CompilerError s
  = Err_Parse_NotModule JSAST
  | Err_Parse_CyclicImport ReachSource
  | Err_Parse_ExpectSemi
  | Err_Parse_IllegalBinOp JSBinOp
  | Err_Parse_IllegalUnaOp JSUnaryOp
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
  | Err_Eval_IllegalContext (SLCtxt s) String
  | Err_Eval_UnboundId SLVar
  | Err_Apply_ArgCount Int Int
  | Err_Eval_NotObject SLVal
  | Err_Eval_NotApplicableVals SLVal
  | Err_Eval_NotApplicable SLVal
  | Err_Eval_IfCondNotBool SLVal
  | Err_Obj_IllegalJS JSObjectProperty
  | Err_Obj_SpreadNotObj SLVal
  | Err_Obj_IllegalField JSPropertyName
  | Err_Obj_IllegalComputedField SLVal
  | Err_Fun_NamesIllegal
  | Err_Arrow_NoFormals
  | Err_Dot_InvalidField SLVal String
  | Err_Prim_InvalidArgs SLPrimitive [SLVal]
  | Err_Form_InvalidArgs SLForm [JSExpression]
  | Err_Obj_IllegalFieldValues [JSExpression]
  | Err_Shadowed SLVar
  | Err_Top_NotDApp SLVal
  | Err_DApp_InvalidPartSpec SLVal
  | Err_DApp_InvalidInteract SLVal
  | Err_Block_NotNull SLVal
  | Err_Block_IllegalJS JSStatement
  | Err_Block_Continue
  | Err_Block_While
  | Err_Block_Variable
  | Err_Block_Assign
  | Err_TopFun_NoName
  | Err_TailNotEmpty [JSStatement]
  | Err_Type_TooFewArguments [SLType]
  | Err_Type_NotApplicable SLType
  | Err_Type_TooManyArguments [SLVal]
  | Err_Type_Mismatch SLType SLType SLVal
  | Err_Type_None SLVal
  | Err_ToConsensus_Double ToConsensusMode
  | Err_XXX String
  deriving (Eq,Show)

expect_throw :: SrcLoc -> CompilerError s -> b
expect_throw src ce = error $ "error: " ++ (show src) ++ ": " ++ (take 256 $ show ce)

-- Parser
type BundleMap a b = ((M.Map a [a]), (M.Map a (Maybe b)))
type JSBundleMap = BundleMap ReachSource [JSModuleItem]
data JSBundle = JSBundle [(ReachSource,[JSModuleItem])]
  deriving (Eq,Show)

gatherDeps_imd :: SrcLoc -> IORef JSBundleMap -> JSImportDeclaration -> IO JSImportDeclaration
gatherDeps_imd at fmr j =
  case j of
    JSImportDeclaration ic (JSFromClause ab aa s) sm -> do
      s_abs <- gatherDeps_file (srcloc_at "import from" (tp ab) at) fmr $ string_trim_quotes s
      return $ JSImportDeclaration ic (JSFromClause ab aa s_abs) sm
    JSImportDeclarationBare a s sm -> do
      s_abs <- gatherDeps_file (srcloc_at "import bare" (tp a) at) fmr $ string_trim_quotes s
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

updatePartialAvoidCycles :: Ord a => SrcLoc -> IORef (BundleMap a b) -> Maybe a -> [a] -> (() -> IO a) -> (a -> c) -> (a -> CompilerError s) -> (a -> IO b) -> IO c
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
gatherDeps_from (SrcLoc _ _ mrs) = mrs

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
          let at' = srcloc_src src
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
          let at' = srcloc_src ReachStdLib
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
  let at = srcloc_top
  _src_abs_p <- gatherDeps_file at fmr src_p
  gatherDeps_stdlib at fmr
  (dm, fm) <- readIORef fmr
  return $ JSBundle $ map (\k -> (k, ensureJust (fm M.! k))) $ map_order dm
  where ensureJust Nothing = impossible "gatherDeps: Did not close all Reach files"
        ensureJust (Just x) = x

-- Compiler

typeOf :: SrcLoc -> SLVal -> (DLType, DLArg)
typeOf at v =
  case v of
    SLV_Null _ -> (T_Null, DLA_Con $ DLC_Null)
    SLV_Bool _ b -> (T_Bool, DLA_Con $ DLC_Bool b)
    SLV_Int _ i -> (T_UInt256, DLA_Con $ DLC_Int i)
    SLV_Bytes _ bs -> (T_Bytes, DLA_Con $ DLC_Bytes bs)
    SLV_Array at' vs -> (T_Array ts, DLA_Array das)
      where tdas = map (typeOf at') vs
            ts = map fst tdas
            das = map snd tdas
    SLV_Object at' fenv -> (T_Obj tenv, DLA_Obj aenv)
      where cenv = M.map (typeOf at') fenv
            tenv = M.map fst cenv
            aenv = M.map snd cenv
    SLV_Clo _ _ _ _ _ -> none
    SLV_DLVar dv@(DLVar _ _ _ t) -> (t, DLA_Var dv)
    SLV_Type _ -> none
    SLV_Participant _ _ _ -> none
    SLV_Prim _ -> none
    SLV_Form _ -> none
  where none = expect_throw at $ Err_Type_None v
      
typeCheck :: SrcLoc -> SLType -> SLVal -> DLArg
typeCheck at t v =
  if vt == t then
    res
  else
    expect_throw at $ Err_Type_Mismatch t vt v
  where (vt, res) = typeOf at v
  
typeChecks :: SrcLoc -> [SLType] -> [SLVal] -> [DLArg]
typeChecks at ts vs =
  case (ts, vs) of
    ([], []) -> []
    ((t:ts'), (v:vs')) ->
      (typeCheck at t v) : typeChecks at ts' vs'
    ((_:_), _) ->
      expect_throw at $ Err_Type_TooFewArguments ts
    (_, (_:_)) ->
      expect_throw at $ Err_Type_TooManyArguments vs

checkAndConvert :: SrcLoc -> SLType -> [SLVal] -> (DLType, [DLArg])
checkAndConvert at t args =
  case t of
    T_Fun dom rng -> (rng, typeChecks at dom args)
    _ -> expect_throw at $ Err_Type_NotApplicable t

evalDot :: SrcLoc -> SLVal -> String -> SLVal
evalDot at obj field =
  case obj of
    SLV_Object _ env ->
      case M.lookup field env of
        Just v -> v
        Nothing -> illegal_field
    SLV_Participant _ _ _ ->
      case field of
        "only" -> SLV_Form (SLForm_Part_Only obj)
        "publish" -> SLV_Form (SLForm_Part_ToConsensus obj (Just TCM_Publish) Nothing Nothing Nothing)
        "pay" -> SLV_Form (SLForm_Part_ToConsensus obj (Just TCM_Pay) Nothing Nothing Nothing)
        _ -> illegal_field
    SLV_Form (SLForm_Part_ToConsensus pv Nothing mpub mpay mtime) ->
      case field of
        "publish" -> SLV_Form (SLForm_Part_ToConsensus pv (Just TCM_Publish) mpub mpay mtime)
        "pay" -> SLV_Form (SLForm_Part_ToConsensus pv (Just TCM_Pay) mpub mpay mtime)
        "timeout" -> SLV_Form (SLForm_Part_ToConsensus pv (Just TCM_Timeout) mpub mpay mtime)
        _ -> illegal_field
    v ->
      expect_throw at (Err_Eval_NotObject v)
  where illegal_field =
          expect_throw at (Err_Dot_InvalidField obj field)

evalForm :: SLCtxt s -> SrcLoc -> e -> SLForm -> [JSExpression] -> (SLVal -> ST s ans) -> ST s ans
evalForm ctxt at _env f args k =
  case f of
    SLForm_Part_Only (SLV_Participant _ who _) ->
      case ctxt_mode ctxt of
        SLC_Nested (SLC_Step sst) -> do
          lst <- state_Step2Local sst
          let ctxt_local = ctxt { ctxt_mode = SLC_Local lst }
          let k' eargs =
                case eargs of
                  [ thunk ] -> do
                    lsts <- state_Local2LocalStep lst
                    let ctxt_step = ctxt { ctxt_mode = SLC_LocalStep lsts }
                    --- XXX Expect to get an update part env
                    evalApplyVals ctxt_step at mt_env thunk [] k
                  _ -> illegal_args
          evalExprs ctxt_local at penv args k'
          where penv = (sst_penvs sst) M.! who
        _ -> expect_throw at $ Err_Eval_IllegalContext ctxt "part.only"
    SLForm_Part_Only _ -> impossible "SLForm_Part_Only args"
    SLForm_Part_ToConsensus pv mmode mpub mpay mtime ->
      case ctxt_mode_noNest ctxt of
        SLC_Step sst ->
          case mmode of
            Just TCM_Publish ->
              case mpub of
                Nothing ->
                  foldlk k' ensure_bound () msg 
                  where msg = map (jse_expect_id at) args
                        penv = sst_penvs sst M.! who
                        k' () = k $ SLV_Form $ SLForm_Part_ToConsensus pv Nothing (Just msg) mpay mtime
                        ensure_bound () x k'' =
                          case env_lookup at x penv of
                            _ -> k'' ()
                Just _ ->
                  expect_throw at $ Err_ToConsensus_Double TCM_Publish
            Just TCM_Pay ->
              k $ SLV_Form $ SLForm_Part_ToConsensus pv Nothing mpub (Just $ error "XXX TCM_Pay " ++ show args) mtime
            Just TCM_Timeout ->
              k $ SLV_Form $ SLForm_Part_ToConsensus pv Nothing mpub mpay (Just $ error "XXX TCM_Time " ++ show args)
            Nothing ->
              expect_throw at $ Err_Eval_NotApplicable rator
        _ -> expect_throw at $ Err_Eval_IllegalContext ctxt "toConsensus"
        where who = case pv of
                      (SLV_Participant _ x _) -> x
                      _ -> impossible $ "ToConsensus args"
  where illegal_args = expect_throw at (Err_Form_InvalidArgs f args)
        rator = SLV_Form f

evalPrimOp :: SLCtxt s -> SrcLoc -> PrimOp -> [SLVal] -> (SLVal -> ST s ans) -> ST s ans
evalPrimOp ctxt at p args k =
  case p of
    --- XXX Perhaps these should be sensitive to bit widths
    CP ADD -> nn2n (+)
    CP SUB -> nn2n (-)
    CP MUL -> nn2n (*)
    CP LSH -> nn2n (\a b -> shift a (fromIntegral b))
    CP RSH -> nn2n (\a b -> shift a (fromIntegral $ b * (-1)))
    CP BAND -> nn2n (.&.)
    CP BIOR -> nn2n (.|.)
    CP BXOR -> nn2n (xor)
    CP PLT -> nn2b (<)
    CP PLE -> nn2b (<=)
    CP PEQ -> nn2b (==)
    CP PGE -> nn2b (>=)
    CP PGT -> nn2b (>)
    _ -> make_var
  where
    nn2b op =
      case args of
        [SLV_Int _ lhs, SLV_Int _ rhs] ->
          static $ SLV_Bool at $ op lhs rhs
        _ -> make_var
    nn2n op =
      case args of
        [SLV_Int _ lhs, SLV_Int _ rhs] ->
          static $ SLV_Int at $ op lhs rhs
        _ -> make_var
    static v = k v
    make_var = do
      let (rng, dargs) = checkAndConvert at (primOpType p) args
      dv <- ctxt_bind_prim ctxt at "prim" rng p dargs
      k $ SLV_DLVar dv

evalPrim :: SLCtxt s -> SrcLoc -> SLEnv -> SLPrimitive -> [SLVal] -> (SLVal -> ST s ans) -> ST s ans
evalPrim ctxt at env p args k =
  case p of
    SLPrim_op op -> evalPrimOp ctxt at op args k
    SLPrim_Fun ->
      case args of
        [ (SLV_Array _ dom_arr), (SLV_Type rng) ] ->
          k $ SLV_Type $ T_Fun dom rng
          where dom = map expect_ty dom_arr
        _ -> illegal_args
    SLPrim_Array ->
      k $ SLV_Type $ T_Array $ map expect_ty args
    SLPrim_makeEnum ->
      case args of
        [ SLV_Int _ i ] ->
          k $ SLV_Array at' (enum_pred : map (SLV_Int at') [ 0 .. (i-1) ])
          where at' = (srcloc_at "makeEnum" Nothing at)
                --- FIXME This sucks... maybe parse an embed string? Would that suck less?
                enum_pred = SLV_Clo at' fname ["x"] pbody mempty
                fname = Nothing --- FIXME syntax-local-infer-name
                pbody = JSBlock JSNoAnnot [(JSReturn JSNoAnnot (Just (JSExpressionBinary lhs (JSBinOpAnd JSNoAnnot) rhs)) JSSemiAuto)] JSNoAnnot
                lhs = (JSExpressionBinary (JSDecimal JSNoAnnot "0") (JSBinOpLe JSNoAnnot) (JSIdentifier JSNoAnnot "x"))
                rhs = (JSExpressionBinary (JSIdentifier JSNoAnnot "x") (JSBinOpLt JSNoAnnot) (JSDecimal JSNoAnnot (show i)))
        _ -> illegal_args
    SLPrim_DApp ->
      case ctxt_mode ctxt of
        SLC_Nested (SLC_ModuleTop _) ->
          case args of
            [ (SLV_Object _ _), (SLV_Array _ _), (SLV_Clo _ _ _ _ _) ] ->
              k $ SLV_Prim $ SLPrim_DApp_Delay at args env
            _ -> illegal_args
        _ ->
          expect_throw at (Err_Eval_IllegalContext ctxt "DApp")
    SLPrim_DApp_Delay _ _ _ ->
      expect_throw at (Err_Eval_NotApplicable rator)
    SLPrim_interact _iat m t ->
      case ctxt_mode_noNest ctxt of
        SLC_LocalStep lsst -> do
          let (rng, dargs) = checkAndConvert at t args
          dv <- local_bind lsst at "interact" rng (DLLE_Interact at m dargs)
          k $ SLV_DLVar dv
        _ ->
          expect_throw at (Err_Eval_IllegalContext ctxt "interact")
    SLPrim_declassify ->
      case args of
        [ val ] ->
          --- XXX do declassify
          k $ val
        _ -> illegal_args
    SLPrim_commit ->
      case args of
        [ ] -> k $ SLV_Prim SLPrim_committed
        _ -> illegal_args
    SLPrim_committed -> illegal_args
    SLPrim_assume ->
      case ctxt_mode_noNest ctxt of
        SLC_LocalStep lsst -> do
          let darg =
                case checkAndConvert at (T_Fun [T_Bool] T_Null) args of
                  (_, [x]) -> x
                  _ -> illegal_args
          local_do lsst (DLLS_Assume at darg)
          k $ SLV_Null at
        _ -> illegal_args
  where illegal_args = expect_throw at (Err_Prim_InvalidArgs p args)
        rator = SLV_Prim p
        expect_ty v =
          case v of
            SLV_Type t -> t
            _ -> illegal_args

binaryToPrim :: SrcLoc -> SLEnv -> JSBinOp -> SLVal
binaryToPrim at env o =
  case o of
    JSBinOpAnd a -> fun a "and"
    JSBinOpDivide a -> prim a (CP DIV)
    JSBinOpEq a -> prim a (CP PEQ)
    JSBinOpGe a -> prim a (CP PGE)
    JSBinOpGt a -> prim a (CP PGT)
    JSBinOpLe a -> prim a (CP PLE)
    JSBinOpLt a -> prim a (CP PLT)
    JSBinOpMinus a -> prim a (CP SUB)
    JSBinOpMod a -> prim a (CP MOD)
    JSBinOpNeq a -> fun a "neq"
    JSBinOpOr a -> fun a "or"
    JSBinOpPlus a -> prim a (CP ADD)
    JSBinOpStrictEq a -> prim a (CP BYTES_EQ)
    JSBinOpStrictNeq a -> fun a "bytes_neq"
    JSBinOpTimes a -> prim a (CP MUL)
    JSBinOpLsh a -> prim a (CP LSH)
    JSBinOpRsh a -> prim a (CP RSH)
    JSBinOpBitAnd a -> prim a (CP BAND)
    JSBinOpBitOr a -> prim a (CP BIOR)
    JSBinOpBitXor a -> prim a (CP BXOR)
    j -> expect_throw at $ Err_Parse_IllegalBinOp j
  where fun a s = env_lookup (srcloc_jsa "binop" a at) s env
        prim _a p = SLV_Prim $ SLPrim_op p

unaryToPrim :: SrcLoc -> SLEnv -> JSUnaryOp -> SLVal
unaryToPrim at env o =
  case o of
    JSUnaryOpMinus a -> fun a "minus"
    JSUnaryOpNot a -> fun a "not"
    j -> expect_throw at $ Err_Parse_IllegalUnaOp j
  where fun a s = env_lookup (srcloc_jsa "unop" a at) s env

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

evalApplyVals :: SLCtxt s -> SrcLoc -> SLEnv -> SLVal -> [SLVal] -> (SLVal -> ST s ans) -> ST s ans
evalApplyVals ctxt at env rator randvs k =
  case rator of
    SLV_Prim p ->
      evalPrim ctxt at env p randvs k
    SLV_Clo clo_at mname formals body clo_env ->
      evalBlock ctxt' clo_at env' body k
      where env' = foldl' (flip (uncurry (env_insert clo_at))) clo_env kvs
            kvs = zipEq clo_at Err_Apply_ArgCount formals randvs
            ctxt' = ctxt_stack_push ctxt (SLC_CloApp at clo_at mname)
    v ->
      expect_throw at (Err_Eval_NotApplicableVals v)

evalApply :: SLCtxt s -> SrcLoc -> SLEnv -> SLVal -> [JSExpression] -> (SLVal -> ST s ans) -> ST s ans
evalApply ctxt at env rator rands k =
  case rator of
    SLV_Prim _ -> vals
    SLV_Clo _ _ _ _ _ -> vals
    SLV_Form f -> evalForm ctxt at env f rands k
    v ->
      expect_throw at (Err_Eval_NotApplicable v)
  where vals = evalExprs ctxt at env rands k'
        k' randvs = evalApplyVals ctxt at env rator randvs k
  
kontIf :: SLCtxt s -> SrcLoc -> (a -> (b -> ST s ans) -> ST s ans) -> a -> a -> (b -> ST s ans) -> SLVal -> ST s ans
kontIf _ctxt at f ta fa k cv =
  case cv of
    SLV_Bool _ cb -> f e k
      where e = if cb then ta else fa
    SLV_DLVar (DLVar _ _ _ T_Bool) ->
      expect_throw at $ Err_XXX "if on var"
    _ ->
      expect_throw at (Err_Eval_IfCondNotBool cv)

evalPropertyName :: SLCtxt s -> SrcLoc -> SLEnv -> JSPropertyName -> (String -> ST s ans) -> ST s ans
evalPropertyName ctxt at env pn k =
  case pn of
    JSPropertyIdent _ s -> k $ s
    JSPropertyString _ s -> k $ string_trim_quotes s
    JSPropertyNumber an _ ->
      expect_throw at_n (Err_Obj_IllegalField pn)
      where at_n = srcloc_jsa "number" an at
    JSPropertyComputed an e _ ->
      evalExpr ctxt at_n env e k'
      where at_n = srcloc_jsa "computed field name" an at
            k' v =
              case v of
                SLV_Bytes _ b -> k $ B.unpack b
                _ -> expect_throw at_n (Err_Obj_IllegalComputedField v)

evalPropertyPair :: SLCtxt s -> SrcLoc -> SLEnv -> SLEnv -> JSObjectProperty -> (SLEnv -> ST s ans) -> ST s ans
evalPropertyPair ctxt at env fenv p k =
  case p of
    JSPropertyNameandValue pn a vs ->
      evalPropertyName ctxt at' env pn k_value
      where at' = srcloc_jsa "property binding" a at
            k_value f =
              case vs of
                [ e ] ->
                  evalExpr ctxt at' env e (k_insert f)
                _ ->
                  expect_throw at' (Err_Obj_IllegalFieldValues vs)
            k_insert f v = k $ env_insert at' f v fenv
    JSPropertyIdentRef a v ->
      evalPropertyPair ctxt at env fenv p' k
      where p' = JSPropertyNameandValue pn a vs
            pn = JSPropertyIdent a v
            vs = [ JSIdentifier a v ]
    JSObjectSpread a se ->
      evalExpr ctxt at' env se k'
      where k' envv =
              case envv of
                SLV_Object _ senv -> k $ env_merge at' fenv senv
                _ -> expect_throw at (Err_Obj_SpreadNotObj envv)
            at' = srcloc_jsa "...obj" a at
    _ ->
      expect_throw at (Err_Obj_IllegalJS p)

evalExpr :: SLCtxt s -> SrcLoc -> SLEnv -> JSExpression -> (SLVal -> ST s ans) -> ST s ans
evalExpr ctxt at env e k =
  case e of
    JSIdentifier a x -> k $ env_lookup (srcloc_jsa "id ref" a at) x env
    JSDecimal a ns -> k $ SLV_Int (srcloc_jsa "decimal" a at) $ numberValue 10 ns
    JSLiteral a l ->
      case l of
        "null" -> k $ SLV_Null at'
        "true" -> k $ SLV_Bool at' True
        "false" -> k $ SLV_Bool at' False
        _ -> expect_throw at' (Err_Parse_IllegalLiteral l)
      where at' = (srcloc_jsa "literal" a at)
    JSHexInteger a ns -> k $ SLV_Int (srcloc_jsa "hex" a at) $ numberValue 16 ns
    JSOctal a ns -> k $ SLV_Int (srcloc_jsa "octal" a at) $ numberValue 8 ns
    JSStringLiteral a s -> k $ SLV_Bytes (srcloc_jsa "string" a at) (bpack (string_trim_quotes s))
    JSRegEx _ _ -> illegal
    JSArrayLiteral a as _ -> evalExprs ctxt at' env (jsa_flatten as) k'
      where k' avl = k $ SLV_Array at' avl
            at' = (srcloc_jsa "array" a at)
    JSAssignExpression _ _ _ -> illegal
    JSAwaitExpression _ _ -> illegal
    JSCallExpression rator a rands _ -> doCall rator a $ jscl_flatten rands
    JSCallExpressionDot obj a field -> doDot obj a field
    JSCallExpressionSquare arr a idx _ -> doRef arr a idx
    JSClassExpression _ _ _ _ _ _ -> illegal
    JSCommaExpression _ _ _ -> illegal
    JSExpressionBinary lhs op rhs -> doCallV (binaryToPrim at env op) JSNoAnnot [ lhs, rhs ]
    JSExpressionParen a ie _ -> evalExpr ctxt (srcloc_jsa "paren" a at) env ie k
    JSExpressionPostfix _ _ -> illegal
    JSExpressionTernary c a t _ f ->
      evalExpr ctxt at' env c k'
      where k' = kontIf ctxt at' (evalExpr ctxt at' env) t f k
            at' = srcloc_jsa "ternary" a at
    JSArrowExpression aformals a bodys ->
      k $ SLV_Clo at' fname formals body env
      where at' = srcloc_jsa "arrow" a at
            fname = Nothing --- FIXME syntax-local-infer-name
            body = JSBlock JSNoAnnot [bodys] JSNoAnnot
            formals = parseJSArrowFormals at' aformals
    JSFunctionExpression a name _ jsformals _ body ->
      k $ SLV_Clo at' fname formals body env
      where at' = srcloc_jsa "function exp" a at
            fname =
              case name of
                JSIdentNone -> Nothing --- FIXME syntax-local-infer-name
                JSIdentName na _ -> expect_throw (srcloc_jsa "function name" na at') Err_Fun_NamesIllegal
            formals = parseJSFormals at' jsformals
    JSGeneratorExpression _ _ _ _ _ _ _ -> illegal
    JSMemberDot obj a field -> doDot obj a field
    JSMemberExpression rator a rands _ -> doCall rator a $ jscl_flatten rands
    JSMemberNew _ _ _ _ _ -> illegal
    JSMemberSquare arr a idx _ -> doRef arr a idx
    JSNewExpression _ _ -> illegal
    JSObjectLiteral a plist _ ->
      foldlk k_ret (evalPropertyPair ctxt at' env) mempty $ jsctl_flatten plist
      where at' = srcloc_jsa "obj" a at
            k_ret fenv = k $ SLV_Object at' fenv
    JSSpreadExpression _ _ -> illegal
    JSTemplateLiteral _ _ _ _ -> illegal
    JSUnaryExpression op ue -> doCallV (unaryToPrim at env op) JSNoAnnot [ ue ]
    JSVarInitExpression _ _ -> illegal
    JSYieldExpression _ _ -> illegal
    JSYieldFromExpression _ _ _ -> illegal
  where illegal = expect_throw at (Err_Eval_IllegalJS e)
        doCallV ratorv a rands = evalApply ctxt at' env ratorv rands k
          where at' = srcloc_jsa "application" a at
        doCall rator a rands = evalExpr ctxt at' env rator k'
          where k' ratorv = doCallV ratorv a rands
                at' = srcloc_jsa "application, rator" a at
        doDot obj a field = evalExpr ctxt at' env obj k'
          where k' objv = k $ evalDot at' objv fields
                at' = srcloc_jsa "dot" a at
                fields = (jse_expect_id at') field
        doRef _arr a _idx = expect_throw at' (Err_XXX "doRef")
          where at' = srcloc_jsa "array ref" a at

evalExprs :: SLCtxt s -> SrcLoc -> SLEnv -> [JSExpression] -> ([SLVal] -> ST s ans) -> ST s ans
evalExprs ctxt at env rands k =
  mapk k (evalExpr ctxt at env) rands

bindDeclLHS :: SLCtxt s -> SrcLoc -> SLEnv -> JSExpression -> SLVal -> (SLEnv -> ST s a) -> ST s a
bindDeclLHS ctxt at o_env lhs v k = do
  ctxt_declf ctxt update
  k $ update o_env
  where update env =
          case lhs of
            (JSIdentifier a x) ->
              env_insert (srcloc_jsa "id" a at) x v env
            (JSArrayLiteral a xs _) ->
              foldl' (flip (uncurry (env_insert at'))) env kvs
              where kvs = zipEq at' Err_Decl_WrongArrayLength ks vs
                    ks = map (jse_expect_id at') $ jsa_flatten xs
                    vs = case v of
                      SLV_Array _ x -> x
                      _ ->
                        expect_throw at' (Err_Decl_NotArray v)
                    at' = srcloc_jsa "array" a at
            _ ->
              expect_throw at (Err_DeclLHS_IllegalJS lhs)

evalDecl :: SLCtxt s -> SrcLoc -> SLEnv -> JSExpression -> (SLEnv -> ST s ans) -> ST s ans
evalDecl ctxt at env decl k =
  case decl of
    JSVarInitExpression lhs (JSVarInit a rhs) ->
      evalExpr (ctxt_nest ctxt) at' env rhs k'
      where at' = srcloc_jsa "var initializer" a at
            k' v = bindDeclLHS ctxt at env lhs v k
    _ ->
      expect_throw at (Err_Decl_IllegalJS decl)

evalDecls :: SLCtxt s -> SrcLoc -> SLEnv -> (JSCommaList JSExpression) -> (SLEnv -> ST s b) -> ST s b
evalDecls ctxt at env decls k =
  --- Note: This makes it so that declarations on the left are visible
  --- on the right, which might be different than JavaScript?
  foldlk k (evalDecl ctxt at) env $ jscl_flatten decls

evalFunctionStmt :: SLCtxt s -> SrcLoc -> SLEnv -> JSAnnot -> JSIdent -> JSCommaList JSExpression -> JSBlock -> JSSemi -> ((SrcLoc, SLEnv) -> ST s ans) -> ST s ans
evalFunctionStmt ctxt at env a name jsformals body sp k = do
  ctxt_decl ctxt at f clo
  k $ (at_after, env')
  where clo = SLV_Clo at' (Just f) formals body env
        formals = parseJSFormals at' jsformals
        at' = srcloc_jsa lab a at
        at_after = srcloc_after_semi lab a sp at
        lab = "function def"
        env' = env_insert at f clo env
        f = case name of
              JSIdentNone -> expect_throw at' (Err_TopFun_NoName)
              JSIdentName _ x -> x

evalStmt :: SLCtxt s -> SrcLoc -> SLEnv -> [JSStatement] -> (SLEnv -> SLVal -> ST s ans) -> ST s ans
evalStmt octxt at env ss k =
  case ss of
    [] -> k env $ SLV_Null at
    ((JSStatementBlock a ss' _ sp):ks) ->
      evalStmt nctxt at_in env ss' $
      kontNull at_in (\_ -> evalStmt octxt at_after env ks k)
      where at_in = srcloc_jsa "block" a at
            at_after = srcloc_after_semi "block" a sp at
    (s@(JSBreak a _ _):_) -> illegal a s "break"
    (s@(JSLet a _ _):_) -> illegal a s "let"
    (s@(JSClass a _ _ _ _ _ _):_) -> illegal a s "class"
    ((JSConstant a decls sp):ks) ->
      evalDecls octxt at_in env decls k'
      where at_after = srcloc_after_semi lab a sp at
            at_in = srcloc_jsa lab a at
            lab = "const"
            k' env' = evalStmt octxt at_after env' ks k
    ((JSContinue a _ _):_) ->
      expect_throw (srcloc_jsa "continue" a at) (Err_Block_Continue)
    (s@(JSDoWhile a _ _ _ _ _ _):_) -> illegal a s "do while"
    (s@(JSFor a _ _ _ _ _ _ _ _):_) -> illegal a s "for"
    (s@(JSForIn a _ _ _ _ _ _):_) -> illegal a s "for in"
    (s@(JSForVar a _ _ _ _ _ _ _ _ _):_) -> illegal a s "for var"
    (s@(JSForVarIn a _ _ _ _ _ _ _):_) -> illegal a s "for var in"
    (s@(JSForLet a _ _ _ _ _ _ _ _ _):_) -> illegal a s "for let"
    (s@(JSForLetIn a _ _ _ _ _ _ _):_) -> illegal a s "for let in"
    (s@(JSForLetOf a _ _ _ _ _ _ _):_) -> illegal a s "for let of"
    (s@(JSForConst a _ _ _ _ _ _ _ _ _):_) -> illegal a s "for const"
    (s@(JSForConstIn a _ _ _ _ _ _ _):_) -> illegal a s "for const in"
    (s@(JSForConstOf a _ _ _ _ _ _ _):_) -> illegal a s "for const of"
    (s@(JSForOf a _ _ _ _ _ _):_) -> illegal a s "for of"
    (s@(JSForVarOf a _ _ _ _ _ _ _):_) -> illegal a s "for var of"
    (s@(JSAsyncFunction a _ _ _ _ _ _ _):_) -> illegal a s "async function"
    ((JSFunction a name _ jsformals _ body sp):ks) ->
      evalFunctionStmt octxt at env a name jsformals body sp k'
      where k' (at_after, env') = evalStmt octxt at_after env' ks k
    (s@(JSGenerator a _ _ _ _ _ _ _):_) -> illegal a s "generator"
    ((JSIf a la ce ra ts):ks) ->
      evalStmt octxt at env ((JSIfElse a la ce ra ts ea fs):ks) k
      where ea = ra
            fs = (JSEmptyStatement ea)
    ((JSIfElse a _ ce _ ts _ fs):ks) ->
      evalExpr nctxt at' env ce k'
      where k' = kontIf nctxt at' (\s k'' -> evalStmt octxt at' env ((JSStatementBlock a [s] a (JSSemi a)):ks) (curry k'')) ts fs (uncurry k)
            at' = srcloc_jsa "if" a at
    (s@(JSLabelled _ a _):_) -> illegal a s "labelled"
    ((JSEmptyStatement a):ks) ->
      evalStmt octxt at' env ks k
      where at' = srcloc_jsa "empty" a at
    ((JSExpressionStatement e sp):ks) ->
      evalExpr nctxt at env e k'
      where at_after =
              srcloc_after_semi "expr stmt" JSNoAnnot sp at
            k' ev =
              case ctxt_mode octxt of
                SLC_Step sst ->
                  case ev of
                    SLV_Form (SLForm_Part_ToConsensus _who Nothing _mmsg _mamt _mtime) -> do
                      cst <- state_Step2ConsensusStep sst
                      let cctxt = octxt { ctxt_mode = SLC_ConsensusStep cst }
                      evalStmt cctxt at_after env ks k
                    _ -> should_be_null --- XXX or above
                SLC_ConsensusStep csst ->
                  case ev of
                    SLV_Prim SLPrim_committed -> do
                      sst <- state_ConsensusStep2Step csst
                      let sctxt = octxt { ctxt_mode = SLC_Step sst }
                      evalStmt sctxt at_after env ks k
                    _ -> should_be_null --- XXX or above
                _ -> should_be_null
              where should_be_null =
                      kontNull at
                      (\() -> evalStmt octxt at_after env ks k)
                      () ev
    ((JSAssignStatement _lhs op _rhs _asp):ks) ->
      case (op, ks) of
        ((JSAssign _), ((JSContinue a _bl sp):cont_ks)) ->
          expect_empty_tail lab a sp at cont_ks res
          where lab = "continue"
                at' = srcloc_jsa lab a at
                res = expect_throw at' (Err_XXX lab)
        _ ->
          expect_throw (srcloc_jsa "assign" JSNoAnnot at) (Err_Block_Assign)
    ((JSMethodCall e a args ra sp):ks) ->
      evalStmt octxt at env ss' k
      where ss' = (JSExpressionStatement e' sp):ks
            e' = (JSCallExpression e a args ra)
    ((JSReturn a me sp):ks) ->
      expect_empty_tail lab a sp at ks res
      where lab = "return"
            at' = srcloc_jsa lab a at
            retk = expect_throw at' (Err_XXX "retk")
            res = case me of
                    Nothing -> retk $ SLV_Null at'
                    Just e -> evalExpr nctxt at' env e retk
    (s@(JSSwitch a _ _ _ _ _ _ _):_) -> illegal a s "switch"
    (s@(JSThrow a _ _):_) -> illegal a s "throw"
    (s@(JSTry a _ _ _):_) -> illegal a s "try"
    ((JSVariable a _while_decls _vsp):ks) ->
      case ks of
        ((JSMethodCall (JSIdentifier _ "invariant") _ _invariant_args _ _isp):
          (JSWhile _ _ _while_cond _ _while_body):_while_ks) ->
          expect_throw at' (Err_XXX "while")
        _ ->
          expect_throw at' (Err_Block_Variable)
      where at' = (srcloc_jsa "var" a at)
    ((JSWhile a _ _ _ _):_) ->
      expect_throw (srcloc_jsa "while" a at) (Err_Block_While)
    (s@(JSWith a _ _ _ _ _):_) -> illegal a s "with"
  where nctxt = ctxt_nest octxt
        illegal a s lab =
          expect_throw (srcloc_jsa lab a at) (Err_Block_IllegalJS s)

kontNull :: SrcLoc -> (a -> ans) -> a -> SLVal -> ans
kontNull at res arg cv =
  case typeOf at cv of
    (T_Null, _) -> res arg
    _ -> expect_throw at (Err_Block_NotNull cv)

expect_empty_tail :: String -> JSAnnot -> JSSemi -> SrcLoc -> [JSStatement] -> a -> a
expect_empty_tail lab a sp at ks res =
  case ks of
    [] -> res
    _ ->
      expect_throw at' (Err_TailNotEmpty ks)
      where at' = srcloc_after_semi lab a sp at
  
evalBlock :: SLCtxt s -> SrcLoc -> SLEnv -> JSBlock -> (SLVal -> ST s ans) -> ST s ans
evalBlock ctxt at env (JSBlock a ss _) k =
  evalStmt ctxt at' env ss (\_ v -> k v)
  where at' = srcloc_jsa "block" a at

type SLKont = SLVal -> SLVal

data SLCtxt s = SLCtxt
  { ctxt_mode :: (SLCtxtMode s)
  , ctxt_stack :: [ SLCtxtFrame ] }
  deriving (Eq)

instance Show (SLCtxt s) where
  show ctxt = show $ ctxt_mode ctxt

data SLStepState s = SLStepState
  { sst_idx :: STRef s Int
  , sst_penvs :: (M.Map SLPart SLEnv) }
  deriving (Eq)

instance Show (SLStepState s) where
  show _ = "<ctxt step>"

data SLLocalState s = SLLocalState
  { lst_sst :: SLStepState s }
  deriving (Eq)

data SLLocalStepState s = SLLocalStepState
  { lsst_lst :: SLLocalState s
  , lsst_stmts :: STRef s DLLocalStep }
  deriving (Eq)

lsst_sst :: SLLocalStepState s -> SLStepState s
lsst_sst lsst = lst_sst $ lsst_lst lsst

data SLConsensusStepState s = SLConsensusStepState
  { csst_sst :: SLStepState s
  , csst_stmts :: STRef s DLConsensusStep }
  deriving (Eq)

instance Show (SLLocalStepState s) where
  show _ = "<local steps>"

data SLCtxtMode s
  = SLC_ModuleTop (STRef s SLEnv)
  | SLC_Step (SLStepState s)
  | SLC_Local (SLLocalState s)
  | SLC_LocalStep (SLLocalStepState s)
  | SLC_ConsensusStep (SLConsensusStepState s)
  | SLC_Nested (SLCtxtMode s)
  deriving (Eq)

instance Show (SLCtxtMode s) where
  show (SLC_ModuleTop _) = "<module top-level>"
  show (SLC_Step _) = "<DApp step>"
  show (SLC_Local _) = "<DApp local>"
  show (SLC_LocalStep _) = "<DApp local step>"
  show (SLC_ConsensusStep _) = "<DApp consensus step>"
  show (SLC_Nested m) = "<nested ctxt " ++ show m ++ ">"

allocVarId :: SLStepState s -> ST s Int
allocVarId sst = do
  let idr = sst_idx sst
  idx <- readSTRef idr
  writeSTRef idr $ idx + 1
  return idx

ctxt_do :: STRef s (Seq.Seq a) -> a -> ST s ()
ctxt_do ssr s = modifySTRef ssr (\ss -> ss Seq.|> s)
  
ctxt_bind :: SLStepState s -> STRef s (Seq.Seq a) -> SrcLoc -> String -> DLType -> (DLVar -> a) -> ST s DLVar
ctxt_bind sst ssr at v t dv_to_s = do
  idx <- allocVarId sst
  let dv = DLVar at v idx t
  ctxt_do ssr $ dv_to_s dv
  return dv

ctxt_bind_prim :: SLCtxt s -> SrcLoc -> String -> DLType -> PrimOp -> [DLArg] -> ST s DLVar
ctxt_bind_prim ctxt at v t op dargs = do
  case ctxt_mode_noNest ctxt of
    SLC_LocalStep lsst ->
      ctxt_bind (lsst_sst lsst) (lsst_stmts lsst) at v t (\dv -> DLLS_Let at dv $ DLLE_PrimOp at op dargs)
    SLC_ConsensusStep csst ->
      case op of
        CP cop ->
          ctxt_bind (csst_sst csst) (csst_stmts csst) at v t (\dv -> DLCS_Let at dv $ DLCE_PrimOp at cop dargs)
        _ -> illegal
    _ -> illegal
  where illegal = expect_throw at $ Err_Eval_IllegalContext ctxt "bind primitive"

local_bind :: SLLocalStepState s -> SrcLoc -> String -> DLType -> DLLocalExpr -> ST s DLVar
local_bind lsst at v t e =
  ctxt_bind (lsst_sst lsst) (lsst_stmts lsst) at v t (\dv -> DLLS_Let at dv e)

local_do :: SLLocalStepState s -> DLLocalStmt -> ST s ()
local_do lsst s =
  ctxt_do (lsst_stmts lsst) s

state_Step2Local :: (SLStepState s) -> ST s (SLLocalState s)
state_Step2Local sst = return $ SLLocalState { lst_sst = sst }

state_Local2LocalStep :: (SLLocalState s) -> ST s (SLLocalStepState s)
state_Local2LocalStep lst = do
  stmts_ref <- newSTRef mempty
  return $ SLLocalStepState
    { lsst_lst = lst
    , lsst_stmts = stmts_ref }

state_Step2ConsensusStep :: (SLStepState s) -> ST s (SLConsensusStepState s)
state_Step2ConsensusStep sst = do
  stmts_ref <- newSTRef mempty
  return $ SLConsensusStepState
    { csst_sst = sst
    , csst_stmts = stmts_ref }

state_ConsensusStep2Step :: (SLConsensusStepState s) -> ST s (SLStepState s)
state_ConsensusStep2Step csst = do
  return $ csst_sst csst
  
ctxt_declf :: SLCtxt s -> (SLEnv -> SLEnv) -> ST s ()
ctxt_declf ctxt f =
  case ctxt_mode ctxt of
    SLC_ModuleTop exenvr -> do
      exenv <- readSTRef exenvr
      writeSTRef exenvr $ f exenv
    --- XXX Do this for local steps
    _ -> return ()

ctxt_decl :: SLCtxt s -> SrcLoc -> SLVar -> SLVal -> ST s ()
ctxt_decl ctxt at x v =
  ctxt_declf ctxt (env_insert at x v)

data SLCtxtFrame
  = SLC_CloApp SrcLoc SrcLoc (Maybe SLVar)
  deriving (Eq, Show)

ctxt_stack_push :: SLCtxt s -> SLCtxtFrame -> SLCtxt s
ctxt_stack_push ctxt f =
  (ctxt { ctxt_stack = f : (ctxt_stack ctxt) })

ctxt_nest :: SLCtxt s -> SLCtxt s
ctxt_nest ctxt =
  (ctxt { ctxt_mode = m' })
  where m = ctxt_mode ctxt
        m' = case m of
               SLC_Nested _ -> m
               _ -> SLC_Nested m

ctxt_mode_noNest :: SLCtxt s -> SLCtxtMode s
ctxt_mode_noNest ctxt =
  case ctxt_mode ctxt of
    SLC_Nested m -> m
    m -> m

ctxt_export :: Bool -> SLCtxt s -> SLCtxt s
ctxt_export isExport ctxt =
  if isExport then ctxt
  else ctxt_nest ctxt

ctxt_init :: SLCtxtMode s -> SLCtxt s
ctxt_init m = (SLCtxt { ctxt_mode = m
                      , ctxt_stack = [] })

evalTopBody :: SLCtxt s -> SrcLoc -> SLLibs -> SLEnv -> [JSModuleItem] -> (() -> ST s ans) -> ST s ans
evalTopBody ctxt at libm env body k =
  case body of
    [] -> k ()
    mi:body' ->
      case mi of
        (JSModuleImportDeclaration _ im) ->
          case im of
            JSImportDeclarationBare a libn sp ->
              evalTopBody ctxt at_after libm env' body' k
              where at_after = srcloc_after_semi lab a sp at
                    at' = srcloc_jsa lab a at
                    lab = "import"
                    env' = env_merge at' env libex
                    libex =
                      case M.lookup (ReachSourceFile libn) libm of
                        Just x -> x
                        Nothing ->
                          impossible $ "dependency not found"
            --- XXX support more kinds
            _ -> expect_throw at (Err_Import_IllegalJS im)
        (JSModuleExportDeclaration a ed) ->
          case ed of
            JSExport s _ -> doStmt at' True s
            --- XXX support more kinds
            _ -> expect_throw at' (Err_Export_IllegalJS ed)
          where at' = srcloc_jsa "export" a at
        (JSModuleStatementListItem s) -> doStmt at False s
      where doStmt at' isExport sm =
              evalStmt ctxt' at' env [sm] $
              kontNull at'
              (\env' -> evalTopBody ctxt at' libm env' body' k)
              where ctxt' = ctxt_export isExport ctxt

type SLMod = (ReachSource, [JSModuleItem])
type SLLibs = (M.Map ReachSource SLEnv)

evalLib :: SLMod -> SLLibs -> (SLLibs -> ST s ans) -> ST s ans
evalLib (src, body) libm k = do
  exenvr <- newSTRef $ mt_env
  evalTopBody (ctxt_init $ SLC_ModuleTop exenvr) prev_at libm stdlib_env body'
    (\() -> (do exenv <- readSTRef exenvr
                k $ M.insert src exenv libm))
  where stdlib_env =
          case src of
            ReachStdLib -> base_env
            ReachSourceFile _ -> M.union (libm M.! ReachStdLib) base_env
        at = (srcloc_src src)
        (prev_at, body') =
          case body of
            ((JSModuleStatementListItem (JSExpressionStatement (JSStringLiteral a "\'reach 0.1\'") sp)):j) ->
              ((srcloc_after_semi "header" a sp at), j)
            _ -> expect_throw at (Err_NoHeader body)

evalLibs :: [SLMod] -> (SLLibs -> ST s ans) -> ST s ans
evalLibs mods k = foldrk k evalLib mempty mods

makeInteract :: SrcLoc -> SLEnv -> SLVal
makeInteract at spec = SLV_Object at spec'
  where spec' = M.mapWithKey wrap_ty spec
        wrap_ty k (SLV_Type t) = SLV_Prim $ SLPrim_interact at k t
        wrap_ty _ v = expect_throw at $ Err_DApp_InvalidInteract v

compileDApp :: SLVal -> ST s NLProgram
compileDApp topv =
  case topv of
    SLV_Prim (SLPrim_DApp_Delay at [ (SLV_Object _ _opts), (SLV_Array _ parts), clo ] top_env) -> do
      --- xxx look at opts
      idxr <- newSTRef $ 0
      let sst = SLStepState { sst_idx = idxr
                            , sst_penvs = penvs }
      let ctxt = ctxt_init $ SLC_Step sst
      evalApplyVals ctxt at' mt_env clo partvs k
      where k v = expect_throw at' (Err_XXX $ "compileDApp after: " ++ show v)
            at' = srcloc_at "compileDApp" Nothing at
            penvs = M.fromList $ map make_penv partvs
            make_penv (SLV_Participant _ pn io) =
              (pn, env_insert at' "interact" io top_env)
            make_penv _ = impossible "SLPrim_DApp_Delay make_penv" 
            partvs = map make_part parts
            make_part v =
              case v of
                SLV_Array p_at [ SLV_Bytes _ bs, SLV_Object iat io ] -> SLV_Participant p_at bs (makeInteract iat io)
                _ -> expect_throw at' (Err_DApp_InvalidPartSpec v)
    _ ->
      expect_throw srcloc_top (Err_Top_NotDApp topv)

compileBundle :: JSBundle -> SLVar -> NLProgram
compileBundle (JSBundle mods) top =
  runST $ evalLibs mods k
  where exe = case mods of
                [] -> impossible $ "compileBundle: no files"
                ((x,_):_) -> x
        k libm = compileDApp topv
          where exe_ex = libm M.! exe
                topv = case M.lookup top exe_ex of
                  Just x -> x
                  Nothing ->
                    expect_throw srcloc_top (Err_Eval_UnboundId top)

-- Main entry point
compileNL :: CompilerOpts -> IO ()
compileNL copts = do
  let out = output copts
  djp <- gatherDeps_top $ source copts
  let nlp = compileBundle djp "main"
  out "nl" $ show nlp
  return ()
