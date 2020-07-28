{-# OPTIONS_GHC -fno-warn-orphans #-}

module Reach.CompilerNL where

import Control.Monad
import Control.Monad.ST
import Data.Bits
import qualified Data.ByteString.Char8 as B
import Data.FileEmbed
import Data.Foldable
import qualified Data.Graph as G
import Data.IORef
import Data.List
import qualified Data.Map.Strict as M
import Data.Monoid
import Data.STRef
import qualified Data.Sequence as Seq
import Data.Text.Prettyprint.Doc
import Debug.Trace
import GHC.IO.Encoding
import GHC.Stack (HasCallStack)
import Language.JavaScript.Parser
import Language.JavaScript.Parser.AST
import Reach.Compiler (CompilerOpts, output, source)
import Reach.Util
import Safe (atMay)
import System.Directory
import System.FilePath
import Text.ParserCombinators.Parsec.Number (numberValue)

-- Helpers
zipEq :: SrcLoc -> (Int -> Int -> CompilerError s) -> [a] -> [b] -> [(a, b)]
zipEq at ce x y =
  if lx == ly
    then zip x y
    else expect_throw at (ce lx ly)
  where
    lx = length x
    ly = length y

-- JavaScript Helpers
jscl_flatten :: JSCommaList a -> [a]
jscl_flatten (JSLCons a _ b) = (jscl_flatten a) ++ [b]
jscl_flatten (JSLOne a) = [a]
jscl_flatten (JSLNil) = []

jsctl_flatten :: JSCommaTrailingList a -> [a]
jsctl_flatten (JSCTLComma a _) = jscl_flatten a
jsctl_flatten (JSCTLNone a) = jscl_flatten a

jsa_flatten :: [JSArrayElement] -> [JSExpression]
jsa_flatten a = concatMap f a
  where
    f (JSArrayComma _) = []
    f (JSArrayElement e) = [e]

jse_expect_id :: SrcLoc -> JSExpression -> String
jse_expect_id at j =
  case j of
    (JSIdentifier _ x) -> x
    _ -> expect_throw at (Err_Parse_ExpectIdentifier j)

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

dropEmptyJSStmts :: [JSStatement] -> [JSStatement]
dropEmptyJSStmts [] = []
dropEmptyJSStmts (s : ks) =
  case s of
    (JSStatementBlock a ss b sp) ->
      case dropEmptyJSStmts ss of
        [] -> ks'
        ss' -> (JSStatementBlock a ss' b sp) : ks'
    (JSEmptyStatement _) -> ks'
    _ -> s : ks'
  where
    ks' = dropEmptyJSStmts ks

-- Static Language
data ReachSource
  = ReachStdLib
  | ReachSourceFile FilePath
  deriving (Eq, Ord)

instance Show ReachSource where
  show ReachStdLib = "reach standard library"
  show (ReachSourceFile fp) = fp

instance Ord TokenPosn where
  compare (TokenPn x_a x_l x_c) (TokenPn y_a y_l y_c) =
    compare [x_a, x_l, x_c] [y_a, y_l, y_c]

data SrcLoc = SrcLoc (Maybe String) (Maybe TokenPosn) (Maybe ReachSource)
  deriving (Eq, Ord)

instance Show SrcLoc where
  show (SrcLoc mlab mtp mrs) = concat $ intersperse ":" $ concat [sr, loc, lab]
    where
      lab = case mlab of
        Nothing -> []
        Just s -> [s]
      sr = case mrs of
        Nothing -> []
        Just s -> [show s]
      loc = case mtp of
        Nothing -> []
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
  deriving (Show, Eq)

public :: a -> (SecurityLevel, a)
public x = (Public, x)

secret :: a -> (SecurityLevel, a)
secret x = (Secret, x)

ensure_public :: SrcLoc -> [SLSVal] -> [SLVal]
ensure_public at svs = map f svs
    where f (Public, v) = v
          f (Secret, v) = expect_throw at $ Err_ExpectedPublic v

instance Semigroup SecurityLevel where
  Secret <> _ = Secret
  _ <> Secret = Secret
  Public <> Public = Public

lvlMeet :: SecurityLevel -> (SecurityLevel, a) -> (SecurityLevel, a)
lvlMeet lvl (lvl', x) = (lvl <> lvl', x)

lvlMeetR :: SecurityLevel -> SLComp s (SecurityLevel, a) -> SLComp s (SecurityLevel, a)
lvlMeetR lvl m = do
  SLRes lifts v <- m
  return $ SLRes lifts $ lvlMeet lvl v

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
  deriving (Eq, Show, Ord)

typeMeet :: SrcLoc -> (SrcLoc, SLType) -> (SrcLoc, SLType) -> SLType
typeMeet top_at x@(_, xt) y@(_, yt) =
  --- XXX Find meet of objects
  if xt == yt
    then xt
    else expect_throw top_at $ Err_TypeMeets_Mismatch top_at x y

typeMeets :: SrcLoc -> [(SrcLoc, SLType)] -> SLType
typeMeets top_at l =
  case l of
    [] ->
      expect_throw top_at $ Err_TypeMeets_None
    [(_, xt)] -> xt
    [x, y] -> typeMeet top_at x y
    x : more -> typeMeet top_at x $ (top_at, typeMeets top_at more)

infix 9 -->

(-->) :: [SLType] -> SLType -> SLType
dom --> rng = T_Fun dom rng

type SLPart = B.ByteString

data SLVal
  = SLV_Null SrcLoc String
  | SLV_Bool SrcLoc Bool
  | SLV_Int SrcLoc Int
  | SLV_Bytes SrcLoc B.ByteString
  | SLV_Array SrcLoc [SLVal]
  | SLV_Object SrcLoc SLEnv
  | SLV_Clo SrcLoc (Maybe SLVar) [SLVar] JSBlock SLEnv
  | SLV_DLVar DLVar
  | SLV_Type SLType
  | --- XXX Add something about whether it's bound?
    SLV_Participant SrcLoc SLPart SLVal
  | SLV_Prim SLPrimitive
  | SLV_Form SLForm
  deriving (Eq, Show)

data ToConsensusMode
  = TCM_Publish
  | TCM_Pay
  | TCM_Timeout
  deriving (Eq, Show)

data SLForm
  = SLForm_Part_Only SLVal
  | --- XXX Maybe should be DLVar
    SLForm_Part_ToConsensus SrcLoc SLPart (Maybe ToConsensusMode) (Maybe [SLVar]) (Maybe SLVar) (Maybe SLVar)
  | SLForm_Part_OnlyAns SrcLoc SLPart SLEnv SLVal
  deriving (Eq, Show)

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
  deriving (Show, Eq, Ord)

data PrimOp
  = CP ConsensusPrimOp
  | RANDOM
  deriving (Show, Eq, Ord)

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
  | SLPrim_digest
  | SLPrim_commit
  | SLPrim_committed
  | SLPrim_claim ClaimType
  | SLPrim_interact SrcLoc String SLType
  | SLPrim_Fun
  | SLPrim_Array
  | SLPrim_DApp
  | SLPrim_DApp_Delay SrcLoc [SLVal] SLEnv
  | SLPrim_op PrimOp
  | SLPrim_transfer
  | SLPrim_transfer_amt DLArg
  | SLPrim_transfer_amt_to DLArg
  deriving (Eq, Show)

type SLSVal = (SecurityLevel, SLVal)

type SLEnv = M.Map SLVar SLSVal

mt_env :: SLEnv
mt_env = mempty

m_fromList_public :: [(SLVar, SLVal)] -> SLEnv
m_fromList_public kvs =
  M.fromList $ map (\(k, v) -> (k, (Public, v))) kvs

base_env :: SLEnv
base_env =
  m_fromList_public
    [ ("makeEnum", SLV_Prim SLPrim_makeEnum)
    , ("declassify", SLV_Prim SLPrim_declassify)
    , ("commit", SLV_Prim SLPrim_commit)
    , ("digest", SLV_Prim SLPrim_digest)
    , ("transfer", SLV_Prim SLPrim_transfer)
    , ("assert", SLV_Prim $ SLPrim_claim CT_Assert)
    , ("assume", SLV_Prim $ SLPrim_claim CT_Assume)
    , ("require", SLV_Prim $ SLPrim_claim CT_Require)
    , ("possible", SLV_Prim $ SLPrim_claim CT_Possible)
    , ("random", SLV_Prim $ SLPrim_op $ RANDOM)
    , ("Null", SLV_Type T_Null)
    , ("Bool", SLV_Type T_Bool)
    , ("UInt256", SLV_Type T_UInt256)
    , ("Bytes", SLV_Type T_Bytes)
    , ("Array", SLV_Prim SLPrim_Array)
    , ("Fun", SLV_Prim SLPrim_Fun)
    , ( "Reach"
      , (SLV_Object srcloc_top $
           m_fromList_public
             [("DApp", SLV_Prim SLPrim_DApp)])
      )
    ]

env_insert :: HasCallStack => SrcLoc -> SLVar -> SLSVal -> SLEnv -> SLEnv
env_insert at k v env =
  case M.lookup k env of
    Nothing -> M.insert k v env
    Just _ ->
      expect_throw at (Err_Shadowed k)

env_insertp :: HasCallStack => SrcLoc -> SLEnv -> (SLVar, SLSVal) -> SLEnv
env_insertp at = flip (uncurry (env_insert at))

env_merge :: HasCallStack => SrcLoc -> SLEnv -> SLEnv -> SLEnv
env_merge at left righte = foldl' (env_insertp at) left $ M.toList righte

env_lookup :: HasCallStack => SrcLoc -> SLVar -> SLEnv -> SLSVal
env_lookup at x env =
  case M.lookup x env of
    Just v -> v
    Nothing ->
      expect_throw at (Err_Eval_UnboundId x $ M.keys env)

-- Dynamic Language
data DLConstant
  = DLC_Null
  | DLC_Bool Bool
  | DLC_Int Int
  | DLC_Bytes B.ByteString
  deriving (Eq, Show, Ord)

data DLVar = DLVar SrcLoc String SLType Int
  deriving (Eq, Show, Ord)

data DLArg
  = DLA_Var DLVar
  | DLA_Con DLConstant
  | DLA_Array [DLArg]
  | DLA_Obj (M.Map String DLArg)
  deriving (Eq, Show)

data DLExpr
  = DLE_PrimOp SrcLoc PrimOp [DLArg]
  | DLE_ArrayRef SrcLoc DLArg DLArg
  | DLE_Interact SrcLoc String [DLArg]
  | DLE_Digest SrcLoc [DLArg]
  deriving (Eq, Show)

expr_pure :: DLExpr -> Bool
expr_pure e =
  case e of
    DLE_PrimOp {} -> True
    DLE_ArrayRef {} -> True
    DLE_Interact {} -> False
    DLE_Digest {} -> True

data ClaimType
  = CT_Assert --- Verified on all paths
  | CT_Assume --- Always assumed true
  | CT_Require --- Verified in honest, assumed in dishonest. (This may
  --- sound backwards, but by verifying it in honest
  --- mode, then we are checking that the other
  --- participants fulfill the promise when acting
  --- honestly.)
  | CT_Possible --- Check if an assignment of variables exists to make
  --- this true.
  deriving (Show, Eq, Ord)

data DLStmt
  = DLS_Let SrcLoc DLVar DLExpr
  | DLS_Claim SrcLoc [SLCtxtFrame] ClaimType DLArg
  | --- XXX Record whether it is pure or local in the statement and
    --- track in monad results to avoid quadratic behavior
    DLS_If SrcLoc DLArg DLStmts DLStmts
  | DLS_Transfer SrcLoc SLPart DLArg
  | DLS_Return SrcLoc Int SLVal
  | DLS_Prompt SrcLoc (Either Int DLVar) DLStmts
  | DLS_Only SrcLoc SLPart DLStmts
  | DLS_ToConsensus SrcLoc SLPart [DLArg] [DLVar] DLStmts
  | DLS_FromConsensus SrcLoc DLStmts
  deriving (Eq, Show)

stmt_pure :: DLStmt -> Bool
stmt_pure s =
  case s of
    DLS_Let _ _ e -> expr_pure e
    DLS_Claim {} -> False
    DLS_If _ _ x y -> stmts_pure x && stmts_pure y
    DLS_Transfer {} -> False
    DLS_Return {} -> False
    DLS_Prompt _ _ ss -> stmts_pure ss
    DLS_Only _ _ ss -> stmts_pure ss
    DLS_ToConsensus {} -> False
    DLS_FromConsensus _ ss -> stmts_pure ss

stmt_local :: DLStmt -> Bool
stmt_local s =
  case s of
    DLS_Let {} -> True
    DLS_Claim {} -> True
    DLS_If _ _ x y -> stmts_local x && stmts_local y
    DLS_Transfer {} -> True
    DLS_Return {} -> True
    DLS_Prompt _ _ ss -> stmts_local ss
    DLS_Only _ _ ss -> stmts_local ss
    DLS_ToConsensus {} -> False
    DLS_FromConsensus _ ss -> stmts_local ss

type DLStmts = Seq.Seq DLStmt

stmts_pure :: Foldable f => f DLStmt -> Bool
stmts_pure fs = getAll $ foldMap (All . stmt_pure) fs

stmts_local :: Foldable f => f DLStmt -> Bool
stmts_local fs = getAll $ foldMap (All . stmt_local) fs

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
  where
    alab = "after " ++ lab

--- XXX Maybe it is dumb to have this abstraction
data CompilerError s
  = Err_XXX String
  | Err_Apply_ArgCount Int Int
  | Err_Arrow_NoFormals
  | Err_Block_Assign
  | Err_Block_Continue
  | Err_Block_IllegalJS JSStatement
  | Err_Block_NotNull SLVal
  | Err_Block_Variable
  | Err_Block_While
  | Err_DApp_InvalidInteract SLSVal
  | Err_DApp_InvalidPartSpec SLVal
  | Err_DeclLHS_IllegalJS JSExpression
  | Err_Decl_IllegalJS JSExpression
  | Err_Decl_NotArray SLVal
  | Err_Decl_WrongArrayLength Int Int
  | Err_Dot_InvalidField SLVal String
  | Err_Eval_IfCondNotBool SLVal
  | Err_ExpectedPublic SLVal
  | Err_ExpectedPrivate SLVal
  | Err_Eval_IfNotNull SLVal SLVal
  | Err_Eval_IllegalContext SLCtxtMode String
  | Err_Eval_IllegalJS JSExpression
  | Err_Eval_IllegalLift SLCtxtMode
  | Err_Eval_NoReturn
  | Err_Eval_ReturnsDifferentTypes [SLType]
  | Err_Eval_NotApplicable SLVal
  | Err_Eval_NotApplicableVals SLVal
  | Err_Eval_NotObject SLVal
  | Err_Eval_RefEmptyArray
  | Err_Eval_RefOutOfBounds Int Int
  | Err_Eval_RefNotArray SLVal
  | Err_Eval_RefNotInt SLVal
  | Err_EvalRefIndirectNotHomogeneous [SLType]
  | Err_Eval_UnboundId SLVar [SLVar]
  | Err_Export_IllegalJS JSExportDeclaration
  | Err_Form_InvalidArgs SLForm [JSExpression]
  | Err_Fun_NamesIllegal
  | Err_Import_IllegalJS JSImportDeclaration
  | Err_Import_ShadowedImport SLVar
  | Err_Module_Return (SLRes SLStmtRes)
  | Err_NoHeader [JSModuleItem]
  | Err_Obj_IllegalComputedField SLVal
  | Err_Obj_IllegalField JSPropertyName
  | Err_Obj_IllegalFieldValues [JSExpression]
  | Err_Obj_IllegalJS JSObjectProperty
  | Err_Obj_SpreadNotObj SLVal
  | Err_Parse_CyclicImport ReachSource
  | Err_Parse_ExpectIdentifier JSExpression
  | Err_Parse_ExpectSemi
  | Err_Parse_IllegalBinOp JSBinOp
  | Err_Parse_IllegalLiteral String
  | Err_Parse_IllegalUnaOp JSUnaryOp
  | Err_Parse_NotModule JSAST
  | Err_Prim_InvalidArgs SLPrimitive [SLVal]
  | Err_Shadowed SLVar
  | Err_TailNotEmpty [JSStatement]
  | Err_ToConsensus_Double ToConsensusMode
  | Err_TopFun_NoName
  | Err_Top_IllegalJS JSStatement
  | Err_Top_NotDApp SLVal
  | Err_Type_Mismatch SLType SLType SLVal
  | Err_Type_None SLVal
  | Err_Type_NotApplicable SLType
  | Err_TypeMeets_None
  | Err_TypeMeets_Mismatch SrcLoc (SrcLoc, SLType) (SrcLoc, SLType)
  | Err_Type_TooFewArguments [SLType]
  | Err_Type_TooManyArguments [SLVal]
  deriving (Eq, Show)

--- XXX Add ctxt frame stack and display
expect_throw :: HasCallStack => SrcLoc -> CompilerError s -> b
expect_throw src ce = error $ "error: " ++ (show src) ++ ": " ++ (take 512 $ show ce)

-- Parser
type BundleMap a b = ((M.Map a [a]), (M.Map a (Maybe b)))

type JSBundleMap = BundleMap ReachSource [JSModuleItem]

data JSBundle = JSBundle [(ReachSource, [JSModuleItem])]
  deriving (Eq, Show)

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

stdlib_str :: String
stdlib_str = $(embedStringFile "./rsh/stdlib.rsh")

gatherDeps_stdlib :: SrcLoc -> IORef JSBundleMap -> IO ()
gatherDeps_stdlib at fmr =
  updatePartialAvoidCycles at fmr (gatherDeps_from at) [] get_key ret_key err_key proc_key
  where
    get_key () = return $ ReachStdLib
    ret_key _ = ()
    err_key x = Err_Parse_CyclicImport x
    proc_key _ = do
      let at' = srcloc_src ReachStdLib
      (gatherDeps_ast at' fmr $ readJsModule stdlib_str)

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

-- Compiler

data SLCtxt s = SLCtxt
  { ctxt_mode :: SLCtxtMode
  , ctxt_id :: Maybe (STRef s Int)
  , ctxt_ret :: Maybe Int
  , ctxt_must_ret :: Bool
  , ctxt_stack :: [SLCtxtFrame]
  , ctxt_local_mname :: Maybe [SLVar]
  }
  deriving ()

instance Show (SLCtxt s) where
  show ctxt = show $ ctxt_mode ctxt

type SLPartEnvs = M.Map SLPart SLEnv

data SLCtxtMode
  = SLC_Module
  | SLC_Step SLPartEnvs
  | SLC_Local
  | SLC_LocalStep
  | SLC_ConsensusStep SLPartEnvs
  deriving (Eq, Show)

ctxt_local_name :: SLCtxt s -> SLVar -> SLVar
ctxt_local_name ctxt def =
  case ctxt_local_mname ctxt of
    Nothing -> def
    Just [x] -> x ++ as
    Just xs -> "one of " ++ show xs ++ as
  where
    as = " (as " ++ def ++ ")"

ctxt_local_name_set :: SLCtxt s -> [SLVar] -> SLCtxt s
ctxt_local_name_set ctxt lhs_ns =
  --- FIXME come up with a "reset" mechanism for this and embed in expr some places
  ctxt {ctxt_local_mname = Just lhs_ns}

ctxt_alloc :: SLCtxt s -> SrcLoc -> ST s Int
ctxt_alloc ctxt at = do
  let idr = case ctxt_id ctxt of
        Just x -> x
        Nothing -> expect_throw at $ Err_Eval_IllegalLift $ ctxt_mode ctxt
  x <- readSTRef idr
  writeSTRef idr $ x + 1
  return x

ctxt_lift_expr :: SLCtxt s -> SrcLoc -> (Int -> DLVar) -> DLExpr -> ST s (DLVar, DLStmts)
ctxt_lift_expr ctxt at mk_var e = do
  x <- ctxt_alloc ctxt at
  let dv = mk_var x
  let s = DLS_Let at dv e
  return (dv, return s)

data SLCtxtFrame
  = SLC_CloApp SrcLoc SrcLoc (Maybe SLVar)
  deriving (Eq, Show)

data SLRes a = SLRes DLStmts a
  deriving (Eq, Show)

keepLifts :: DLStmts -> SLComp s a -> SLComp s a
keepLifts lifts m = do
  SLRes lifts' r <- m
  return $ SLRes (lifts <> lifts') r

type SLComp s a = ST s (SLRes a)

data SLStmtRes = SLStmtRes SLEnv [(SrcLoc, SLSVal)]
  deriving (Eq, Show)

data SLAppRes = SLAppRes SLEnv SLSVal
  deriving (Eq, Show)

ctxt_stack_push :: SLCtxt s -> SLCtxtFrame -> Int -> SLCtxt s
ctxt_stack_push ctxt f ret =
  (ctxt
     { ctxt_stack = f : (ctxt_stack ctxt)
     , ctxt_must_ret = True
     , ctxt_ret = Just ret
     })

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
  where
    fun a s = snd $ env_lookup (srcloc_jsa "binop" a at) s env
    prim _a p = SLV_Prim $ SLPrim_op p

unaryToPrim :: SrcLoc -> SLEnv -> JSUnaryOp -> SLVal
unaryToPrim at env o =
  case o of
    JSUnaryOpMinus a -> fun a "minus"
    JSUnaryOpNot a -> fun a "not"
    j -> expect_throw at $ Err_Parse_IllegalUnaOp j
  where
    fun a s = snd $ env_lookup (srcloc_jsa "unop" a at) s env

typeOf :: SrcLoc -> SLVal -> (SLType, DLArg)
typeOf at v =
  case v of
    SLV_Null _ _ -> (T_Null, DLA_Con $ DLC_Null)
    SLV_Bool _ b -> (T_Bool, DLA_Con $ DLC_Bool b)
    SLV_Int _ i -> (T_UInt256, DLA_Con $ DLC_Int i)
    SLV_Bytes _ bs -> (T_Bytes, DLA_Con $ DLC_Bytes bs)
    SLV_Array at' vs -> (T_Array ts, DLA_Array das)
      where
        tdas = map (typeOf at') vs
        ts = map fst tdas
        das = map snd tdas
    SLV_Object at' fenv -> (T_Obj tenv, DLA_Obj aenv)
      where
        cenv = M.map (typeOf at' . snd) fenv
        tenv = M.map fst cenv
        aenv = M.map snd cenv
    SLV_Clo _ _ _ _ _ -> none
    SLV_DLVar dv@(DLVar _ _ t _) -> (t, DLA_Var dv)
    SLV_Type _ -> none
    SLV_Participant _ _ _ -> none --- XXX get the address
    SLV_Prim _ -> none --- XXX an interact may be non-function typed
    SLV_Form _ -> none
  where
    none = expect_throw at $ Err_Type_None v

type TypeEnv s = M.Map SLVar (STRef s (Maybe SLType))

typeSubst :: SrcLoc -> TypeEnv s -> SLType -> ST s SLType
typeSubst at env ty =
  case ty of
    T_Fun doms rng -> do
      doms' <- mapM iter doms
      rng' <- typeSubst at env rng
      return $ T_Fun doms' rng'
    T_Array ts -> do
      ts' <- mapM iter ts
      return $ T_Array ts'
    T_Obj oenv -> do
      oenv' <- mapM iter oenv
      return $ T_Obj oenv'
    T_Var var ->
      case M.lookup var env of
        Nothing ->
          impossible $ "typeSubst: unbound type variable"
        Just var_ref -> do
          mvar <- readSTRef var_ref
          case mvar of
            Nothing ->
              impossible $ "typeSubst: uninstantiated type variable"
            Just var_ty ->
              iter var_ty
    T_Forall _ _ ->
      impossible $ "typeSubst: forall in output"
    _ -> return ty
  where
    iter = typeSubst at env

typeCheck_help :: SrcLoc -> TypeEnv s -> SLType -> SLVal -> SLType -> DLArg -> ST s DLArg
typeCheck_help at env ty val val_ty res =
  case (val_ty, ty) of
    (T_Var _, _) ->
      impossible $ "typeCheck: value has type var: " ++ show val
    (_, T_Var var) ->
      case M.lookup var env of
        Nothing ->
          impossible $ "typeCheck: unbound type variable"
        Just var_ref -> do
          mvar_ty <- readSTRef var_ref
          case mvar_ty of
            Nothing -> do
              writeSTRef var_ref (Just val_ty)
              return res
            Just var_ty ->
              typeCheck_help at env var_ty val val_ty res
    (_, _) ->
      case val_ty == ty of
        True -> return res
        False ->
          expect_throw at $ Err_Type_Mismatch ty val_ty val

typeCheck :: SrcLoc -> TypeEnv s -> SLType -> SLVal -> ST s DLArg
typeCheck at env ty val = typeCheck_help at env ty val val_ty res
  where
    (val_ty, res) = typeOf at val

typeChecks :: SrcLoc -> TypeEnv s -> [SLType] -> [SLVal] -> ST s [DLArg]
typeChecks at env ts vs =
  case (ts, vs) of
    ([], []) ->
      return []
    ((t : ts'), (v : vs')) -> do
      d <- typeCheck at env t v
      ds' <- typeChecks at env ts' vs'
      return $ d : ds'
    ((_ : _), _) ->
      expect_throw at $ Err_Type_TooFewArguments ts
    (_, (_ : _)) ->
      expect_throw at $ Err_Type_TooManyArguments vs

checkAndConvert_i :: SrcLoc -> TypeEnv s -> SLType -> [SLVal] -> ST s (SLType, [DLArg])
checkAndConvert_i at env t args =
  case t of
    T_Fun dom rng -> do
      dargs <- typeChecks at env dom args
      return (rng, dargs)
    T_Forall var ft -> do
      var_ref <- newSTRef Nothing
      let env' = M.insert var var_ref env
      (vrng, dargs) <- checkAndConvert_i at env' ft args
      rng <- typeSubst at env' vrng
      return (rng, dargs)
    _ -> expect_throw at $ Err_Type_NotApplicable t

checkAndConvert :: SrcLoc -> SLType -> [SLVal] -> (SLType, [DLArg])
checkAndConvert at t args = runST $ checkAndConvert_i at mempty t args

evalDot :: SrcLoc -> SLVal -> String -> SLSVal
evalDot at obj field =
  case obj of
    SLV_Object _ env ->
      case M.lookup field env of
        Just v -> v
        Nothing -> illegal_field
    SLV_Participant _ who _ ->
      case field of
        "only" -> public $ SLV_Form (SLForm_Part_Only obj)
        "publish" -> public $ SLV_Form (SLForm_Part_ToConsensus at who (Just TCM_Publish) Nothing Nothing Nothing)
        "pay" -> public $ SLV_Form (SLForm_Part_ToConsensus at who (Just TCM_Pay) Nothing Nothing Nothing)
        _ -> illegal_field
    SLV_Form (SLForm_Part_ToConsensus to_at who Nothing mpub mpay mtime) ->
      case field of
        "publish" -> public $ SLV_Form (SLForm_Part_ToConsensus to_at who (Just TCM_Publish) mpub mpay mtime)
        "pay" -> public $ SLV_Form (SLForm_Part_ToConsensus to_at who (Just TCM_Pay) mpub mpay mtime)
        "timeout" -> public $ SLV_Form (SLForm_Part_ToConsensus to_at who (Just TCM_Timeout) mpub mpay mtime)
        _ -> illegal_field
    SLV_Prim (SLPrim_transfer_amt amt_dla) ->
      case field of
        "to" -> public $ SLV_Prim (SLPrim_transfer_amt_to amt_dla)
        _ -> illegal_field
    v ->
      expect_throw at (Err_Eval_NotObject v)
  where
    illegal_field =
      expect_throw at (Err_Dot_InvalidField obj field)

evalForm :: SLCtxt s -> SrcLoc -> SLEnv -> SLForm -> [JSExpression] -> SLComp s SLSVal
evalForm ctxt at _env f args =
  case f of
    SLForm_Part_Only (SLV_Participant _ who _) ->
      case ctxt_mode ctxt of
        SLC_Step penvs -> do
          let penv = penvs M.! who
          let ctxt_local = (ctxt {ctxt_mode = SLC_Local})
          SLRes elifts eargs <- evalExprs ctxt_local at penv args
          case eargs of
            [(_, thunk)] -> do
              let ctxt_localstep = (ctxt {ctxt_mode = SLC_LocalStep})
              SLRes alifts (SLAppRes penv' (_, ans)) <- evalApplyVals ctxt_localstep at (impossible "Part_only expects clo") thunk []
              traceM $ "penv' update = " ++ (show $ M.keys $ M.difference penv' penv)
              return $ SLRes (elifts <> alifts) $ public $ SLV_Form $ SLForm_Part_OnlyAns at who penv' ans
            _ -> illegal_args
        cm -> expect_throw at $ Err_Eval_IllegalContext cm "part.only"
    SLForm_Part_Only _ -> impossible "SLForm_Part_Only args"
    SLForm_Part_OnlyAns {} -> impossible "SLForm_Part_OnlyAns"
    SLForm_Part_ToConsensus to_at who mmode mpub mpay mtime ->
      case ctxt_mode ctxt of
        SLC_Step _penvs ->
          case mmode of
            Just TCM_Publish ->
              case mpub of
                Nothing -> retV $ public $ SLV_Form $ SLForm_Part_ToConsensus to_at who Nothing (Just msg) mpay mtime
                  where msg = map (jse_expect_id at) args
                Just _ ->
                  expect_throw at $ Err_ToConsensus_Double TCM_Publish
            Just TCM_Pay ->
              retV $ public $ SLV_Form $ SLForm_Part_ToConsensus to_at who Nothing mpub (Just $ error "XXX TCM_Pay " ++ show args) mtime
            Just TCM_Timeout ->
              retV $ public $ SLV_Form $ SLForm_Part_ToConsensus to_at who Nothing mpub mpay (Just $ error "XXX TCM_Time " ++ show args)
            Nothing ->
              expect_throw at $ Err_Eval_NotApplicable rator
        cm -> expect_throw at $ Err_Eval_IllegalContext cm "toConsensus"
  where
    illegal_args = expect_throw at (Err_Form_InvalidArgs f args)
    rator = SLV_Form f
    retV v = return $ SLRes mempty v

evalPrimOp :: SLCtxt s -> SrcLoc -> SLEnv -> PrimOp -> [SLSVal] -> SLComp s SLSVal
evalPrimOp ctxt at _env p sargs =
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
    args = map snd sargs
    lvl_ = mconcat $ map fst sargs
    lvl = case p of
            RANDOM -> Secret
            _ -> lvl_
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
    static v = return $ SLRes mempty (lvl, v)
    make_var = do
      let (rng, dargs) = checkAndConvert at (primOpType p) args
      (dv, lifts) <- ctxt_lift_expr ctxt at (DLVar at (ctxt_local_name ctxt "prim") rng) (DLE_PrimOp at p dargs)
      return $ SLRes lifts $ (lvl, SLV_DLVar dv)

evalPrim :: SLCtxt s -> SrcLoc -> SLEnv -> SLPrimitive -> [SLSVal] -> SLComp s SLSVal
evalPrim ctxt at env p sargs =
  case p of
    SLPrim_op op ->
      evalPrimOp ctxt at env op sargs
    SLPrim_Fun ->
      case map snd sargs of
        [(SLV_Array _ dom_arr), (SLV_Type rng)] ->
          retV $ (lvl, SLV_Type $ T_Fun dom rng)
          where lvl = mconcat $ map fst sargs
                dom = map expect_ty dom_arr
        _ -> illegal_args
    SLPrim_Array ->
      retV $ (lvl, SLV_Type $ T_Array $ map expect_ty $ map snd sargs)
      where lvl = mconcat $ map fst sargs
    SLPrim_makeEnum ->
      case sargs of
        [(ilvl, SLV_Int _ i)] ->
          retV $ (ilvl, SLV_Array at' (enum_pred : map (SLV_Int at') [0 .. (i -1)]))
          where
            at' = (srcloc_at "makeEnum" Nothing at)
            --- FIXME This sucks... maybe parse an embed string? Would that suck less?... probably want a custom primitive
            --- FIXME also, env is a weird choice here... really want stdlib_env
            enum_pred = SLV_Clo at' fname ["x"] pbody env
            fname = Just $ ctxt_local_name ctxt "makeEnum"
            pbody = JSBlock JSNoAnnot [(JSReturn JSNoAnnot (Just (JSExpressionBinary lhs (JSBinOpAnd JSNoAnnot) rhs)) JSSemiAuto)] JSNoAnnot
            lhs = (JSExpressionBinary (JSDecimal JSNoAnnot "0") (JSBinOpLe JSNoAnnot) (JSIdentifier JSNoAnnot "x"))
            rhs = (JSExpressionBinary (JSIdentifier JSNoAnnot "x") (JSBinOpLt JSNoAnnot) (JSDecimal JSNoAnnot (show i)))
        _ -> illegal_args
    SLPrim_DApp ->
      case ctxt_mode ctxt of
        SLC_Module ->
          case args of
            [(SLV_Object _ _), (SLV_Array _ _), (SLV_Clo _ _ _ _ _)] ->
              retV $ public $ SLV_Prim $ SLPrim_DApp_Delay at args env
            _ -> illegal_args
          where args = map snd sargs
        cm ->
          expect_throw at (Err_Eval_IllegalContext cm "DApp")
    SLPrim_DApp_Delay _ _ _ ->
      expect_throw at (Err_Eval_NotApplicable rator)
    SLPrim_interact _iat m t ->
      case ctxt_mode ctxt of
        SLC_LocalStep -> do
          let (rng, dargs) = checkAndConvert at t $ map snd sargs
          (dv, lifts) <- ctxt_lift_expr ctxt at (DLVar at (ctxt_local_name ctxt "interact") rng) (DLE_Interact at m dargs)
          return $ SLRes lifts $ secret $ SLV_DLVar dv
        cm ->
          expect_throw at (Err_Eval_IllegalContext cm "interact")
    SLPrim_declassify ->
      case sargs of
        [(lvl, val)] ->
            case lvl of
              Secret -> retV $ public $ val
              Public -> expect_throw at $ Err_ExpectedPrivate val
        _ -> illegal_args
    SLPrim_commit ->
      case sargs of
        [] -> retV $ public $ SLV_Prim SLPrim_committed
        _ -> illegal_args
    SLPrim_committed -> illegal_args
    SLPrim_digest -> do
      let rng = T_UInt256
      let lvl = mconcat $ map fst sargs
      let dargs = map snd $ map ((typeOf at) . snd) sargs
      (dv, lifts) <- ctxt_lift_expr ctxt at (DLVar at (ctxt_local_name ctxt "digest") rng) (DLE_Digest at dargs)
      return $ SLRes lifts $ (lvl, SLV_DLVar dv)
    SLPrim_claim ct ->
      return $ SLRes lifts $ public $ SLV_Null at "claim"
      where
        darg =
          case checkAndConvert at (T_Fun [T_Bool] T_Null) $ map snd sargs of
            (_, [x]) -> x
            _ -> illegal_args
        lifts = return $ DLS_Claim at (ctxt_stack ctxt) ct darg
    SLPrim_transfer ->
      case ctxt_mode ctxt of
        SLC_ConsensusStep _penvs ->
          case map (typeOf at) $ ensure_public at sargs of
            [(T_UInt256, amt_dla)] ->
              return $ SLRes mempty $ public $ SLV_Prim $ SLPrim_transfer_amt amt_dla
            _ -> illegal_args
        cm -> expect_throw at $ Err_Eval_IllegalContext cm "transfer"
    SLPrim_transfer_amt _ -> not_app
    SLPrim_transfer_amt_to amt_dla ->
      case ctxt_mode ctxt of
        SLC_ConsensusStep _penvs ->
          case sargs of
            [(_, SLV_Participant _ who _)] ->
              return $ SLRes lifts $ public $ SLV_Null at "transfer.to"
              where
                lifts = return $ DLS_Transfer at who amt_dla
            _ -> illegal_args
        cm -> expect_throw at $ Err_Eval_IllegalContext cm "transfer.to"
  where
    illegal_args = expect_throw at (Err_Prim_InvalidArgs p $ map snd sargs)
    not_app = expect_throw at (Err_Eval_NotApplicable $ SLV_Prim p)
    retV v = return $ SLRes mempty v
    rator = SLV_Prim p
    expect_ty v =
      case v of
        SLV_Type t -> t
        _ -> illegal_args

evalApplyVals :: SLCtxt s -> SrcLoc -> SLEnv -> SLVal -> [SLSVal] -> SLComp s SLAppRes
evalApplyVals ctxt at env rator randvs =
  case rator of
    SLV_Prim p -> do
      SLRes lifts val <- evalPrim ctxt at env p randvs
      return $ SLRes lifts $ SLAppRes env val
    SLV_Clo clo_at mname formals (JSBlock body_a body _) clo_env -> do
      ret <- ctxt_alloc ctxt at
      let body_at = srcloc_jsa "block" body_a clo_at
      let kvs = zipEq clo_at Err_Apply_ArgCount formals randvs
      let clo_env' = foldl' (env_insertp clo_at) clo_env kvs
      let ctxt' = ctxt_stack_push ctxt (SLC_CloApp at clo_at mname) ret
      SLRes body_lifts (SLStmtRes clo_env'' rs) <- evalStmt ctxt' body_at clo_env' body
      let no_prompt (lvl, v) = do
            let lifts' =
                  case body_lifts of
                    body_lifts' Seq.:|> (DLS_Return _ x y)
                      | x == ret && y == v ->
                        body_lifts'
                    _ ->
                      return $ DLS_Prompt body_at (Left ret) body_lifts
            return $ SLRes lifts' $ SLAppRes clo_env'' $ (lvl, v)
      case rs of
        [] -> no_prompt $ public $ SLV_Null body_at "clo app"
        [(_, x)] -> no_prompt $ x
        _ -> do
          --- FIXME if all the values are actually the same, then we can treat this as a noprompt
          let r_ty = typeMeets body_at $ map (\(r_at, (_r_lvl, r_sv)) -> (r_at, (fst (typeOf r_at r_sv)))) rs
          let lvl = mconcat $ map fst $ map snd rs
          let dv = DLVar body_at (ctxt_local_name ctxt "clo app") r_ty ret
          let lifts' = return $ DLS_Prompt body_at (Right dv) body_lifts
          return $ SLRes lifts' $ SLAppRes clo_env'' (lvl, (SLV_DLVar dv))
    v ->
      expect_throw at (Err_Eval_NotApplicableVals v)

evalApply :: SLCtxt s -> SrcLoc -> SLEnv -> SLVal -> [JSExpression] -> SLComp s SLSVal
evalApply ctxt at env rator rands =
  case rator of
    SLV_Prim _ -> vals
    SLV_Clo _ _ _ _ _ -> vals
    SLV_Form f -> evalForm ctxt at env f rands
    v -> expect_throw at (Err_Eval_NotApplicable v)
  where
    vals = do
      SLRes rlifts randsvs <- evalExprs ctxt at env rands
      SLRes alifts (SLAppRes _ r) <- evalApplyVals ctxt at env rator randsvs
      return $ SLRes (rlifts <> alifts) r

evalPropertyName :: SLCtxt s -> SrcLoc -> SLEnv -> JSPropertyName -> SLComp s (SecurityLevel, String)
evalPropertyName ctxt at env pn =
  case pn of
    JSPropertyIdent _ s -> k_res $ public $ s
    JSPropertyString _ s -> k_res $ public $ trimQuotes s
    JSPropertyNumber an _ ->
      expect_throw at_n (Err_Obj_IllegalField pn)
      where
        at_n = srcloc_jsa "number" an at
    JSPropertyComputed an e _ -> do
      let at_n = srcloc_jsa "computed field name" an at
      SLRes elifts (elvl, ev) <- evalExpr ctxt at_n env e
      keepLifts elifts $
        case ev of
          SLV_Bytes _ fb ->
            return $ SLRes mempty $ (elvl, B.unpack fb)
          _ ->
            expect_throw at_n $ Err_Obj_IllegalComputedField ev
  where
    k_res s = return $ SLRes mempty s

evalPropertyPair :: SLCtxt s -> SrcLoc -> SLEnv -> SLEnv -> JSObjectProperty -> SLComp s (SecurityLevel, SLEnv)
evalPropertyPair ctxt at env fenv p =
  case p of
    JSPropertyNameandValue pn a vs -> do
      let at' = srcloc_jsa "property binding" a at
      SLRes flifts (flvl, f) <- evalPropertyName ctxt at' env pn
      keepLifts flifts $
        case vs of
          [e] -> do
            SLRes vlifts sv <- evalExpr ctxt at' env e
            return $ SLRes vlifts $ (flvl, env_insert at' f sv fenv)
          _ -> expect_throw at' (Err_Obj_IllegalFieldValues vs)
    JSPropertyIdentRef a v ->
      evalPropertyPair ctxt at env fenv p'
      where
        p' = JSPropertyNameandValue pn a vs
        pn = JSPropertyIdent a v
        vs = [JSIdentifier a v]
    JSObjectSpread a se -> do
      let at' = srcloc_jsa "...obj" a at
      SLRes slifts (slvl, sv) <- evalExpr ctxt at' env se
      keepLifts slifts $
        case sv of
          SLV_Object _ senv ->
            return $ SLRes mempty $ (slvl, env_merge at' fenv senv)
          _ -> expect_throw at (Err_Obj_SpreadNotObj sv)
    _ ->
      expect_throw at (Err_Obj_IllegalJS p)

evalExpr :: SLCtxt s -> SrcLoc -> SLEnv -> JSExpression -> SLComp s SLSVal
evalExpr ctxt at env e =
  case e of
    JSIdentifier a x -> retV $ env_lookup (srcloc_jsa "id ref" a at) x env
    JSDecimal a ns -> retV $ public $ SLV_Int (srcloc_jsa "decimal" a at) $ numberValue 10 ns
    JSLiteral a l ->
      case l of
        "null" -> retV $ public $ SLV_Null at' "null"
        "true" -> retV $ public $ SLV_Bool at' True
        "false" -> retV $ public $ SLV_Bool at' False
        _ -> expect_throw at' (Err_Parse_IllegalLiteral l)
      where
        at' = (srcloc_jsa "literal" a at)
    JSHexInteger a ns -> retV $ public $ SLV_Int (srcloc_jsa "hex" a at) $ numberValue 16 ns
    JSOctal a ns -> retV $ public $ SLV_Int (srcloc_jsa "octal" a at) $ numberValue 8 ns
    JSStringLiteral a s -> retV $ public $ SLV_Bytes (srcloc_jsa "string" a at) (bpack (trimQuotes s))
    JSRegEx _ _ -> illegal
    JSArrayLiteral a as _ -> do
      SLRes lifts svs <- evalExprs ctxt at' env (jsa_flatten as)
      let vs = map snd svs
      let lvl = mconcat $ map fst svs
      return $ SLRes lifts $ (lvl, SLV_Array at' vs)
      where
        at' = (srcloc_jsa "array" a at)
    JSAssignExpression _ _ _ -> illegal
    JSAwaitExpression _ _ -> illegal
    JSCallExpression rator a rands _ -> doCall rator a $ jscl_flatten rands
    JSCallExpressionDot obj a field -> doDot obj a field
    JSCallExpressionSquare arr a idx _ -> doRef arr a idx
    JSClassExpression _ _ _ _ _ _ -> illegal
    JSCommaExpression _ _ _ -> illegal
    JSExpressionBinary lhs op rhs -> doCallV (binaryToPrim at env op) JSNoAnnot [lhs, rhs]
    JSExpressionParen a ie _ -> evalExpr ctxt (srcloc_jsa "paren" a at) env ie
    JSExpressionPostfix _ _ -> illegal
    JSExpressionTernary ce a te fa fe -> do
      let at' = srcloc_jsa "?:" a at
      let t_at' = srcloc_jsa "?: > true" a at'
      let f_at' = srcloc_jsa "?: > false" fa t_at'
      SLRes clifts csv@(clvl, cv) <- evalExpr ctxt at' env ce
      tr@(SLRes tlifts tsv@(tlvl, tv)) <- evalExpr ctxt t_at' env te
      fr@(SLRes flifts fsv@(flvl, fv)) <- evalExpr ctxt f_at' env fe
      let lvl = clvl <> tlvl <> flvl
      keepLifts clifts $
        case cv of
          SLV_Bool _ cb -> lvlMeetR lvl $ return $ if cb then tr else fr
          SLV_DLVar cond_dv@(DLVar _ _ T_Bool _) ->
            case stmts_pure tlifts && stmts_pure flifts of
              True ->
                keepLifts (tlifts <> flifts) $ lvlMeetR lvl $ evalPrim ctxt at mempty (SLPrim_op $ CP IF_THEN_ELSE) [csv, tsv, fsv]
              False -> do
                ret <- ctxt_alloc ctxt at'
                let add_ret e_at' elifts ev = (e_ty, (elifts <> (return $ DLS_Return e_at' ret ev)))
                      where
                        (e_ty, _) = typeOf e_at' ev
                let (t_ty, tlifts') = add_ret t_at' tlifts tv
                let (f_ty, flifts') = add_ret f_at' flifts fv
                let ty = typeMeet at' (t_at', t_ty) (f_at', f_ty)
                let ans_dv = DLVar at' (ctxt_local_name ctxt "clo app") ty ret
                let body_lifts = return $ DLS_If at' (DLA_Var cond_dv) tlifts' flifts'
                let lifts' = return $ DLS_Prompt at' (Right ans_dv) body_lifts
                return $ SLRes lifts' $ (lvl, SLV_DLVar ans_dv)
          _ ->
            expect_throw at (Err_Eval_IfCondNotBool cv)
    JSArrowExpression aformals a bodys ->
      retV $ public $ SLV_Clo at' fname formals body env
      where
        at' = srcloc_jsa "arrow" a at
        fname = Just $ ctxt_local_name ctxt "arrow"
        body = case bodys of
          JSStatementBlock ba bodyss aa _ -> JSBlock ba bodyss aa
          _ -> JSBlock JSNoAnnot [bodys] JSNoAnnot
        formals = parseJSArrowFormals at' aformals
    JSFunctionExpression a name _ jsformals _ body ->
      retV $ public $ SLV_Clo at' fname formals body env
      where
        at' = srcloc_jsa "function exp" a at
        fname =
          case name of
            JSIdentNone -> Just $ ctxt_local_name ctxt "function"
            JSIdentName na _ -> expect_throw (srcloc_jsa "function name" na at') Err_Fun_NamesIllegal
        formals = parseJSFormals at' jsformals
    JSGeneratorExpression _ _ _ _ _ _ _ -> illegal
    JSMemberDot obj a field -> doDot obj a field
    JSMemberExpression rator a rands _ -> doCall rator a $ jscl_flatten rands
    JSMemberNew _ _ _ _ _ -> illegal
    JSMemberSquare arr a idx _ -> doRef arr a idx
    JSNewExpression _ _ -> illegal
    JSObjectLiteral a plist _ -> do
      SLRes olifts (lvl, fenv) <- foldlM f (SLRes mempty (mempty, mempty)) $ jsctl_flatten plist
      return $ SLRes olifts $ (lvl, SLV_Object at' fenv)
      where
        at' = srcloc_jsa "obj" a at
        f (SLRes lifts (lvl, oenv)) pp = keepLifts lifts $ lvlMeetR lvl $ evalPropertyPair ctxt at' env oenv pp
    JSSpreadExpression _ _ -> illegal
    JSTemplateLiteral _ _ _ _ -> illegal
    JSUnaryExpression op ue -> doCallV (unaryToPrim at env op) JSNoAnnot [ue]
    JSVarInitExpression _ _ -> illegal
    JSYieldExpression _ _ -> illegal
    JSYieldFromExpression _ _ _ -> illegal
  where
    illegal = expect_throw at (Err_Eval_IllegalJS e)
    retV v = return $ SLRes mempty $ v
    doCallV ratorv a rands = evalApply ctxt at' env ratorv rands
      where
        at' = srcloc_jsa "application" a at
    doCall rator a rands = do
      let at' = srcloc_jsa "application, rator" a at
      SLRes rlifts (rator_lvl, ratorv) <- evalExpr ctxt at' env rator
      keepLifts rlifts $ lvlMeetR rator_lvl $ doCallV ratorv a rands
    doDot obj a field = do
      let at' = srcloc_jsa "dot" a at
      SLRes olifts (obj_lvl, objv) <- evalExpr ctxt at' env obj
      let fields = (jse_expect_id at') field
      return $ SLRes olifts $ lvlMeet obj_lvl $ evalDot at' objv fields
    doRef arr a idx = do
      let at' = srcloc_jsa "array ref" a at
      SLRes alifts (arr_lvl, arrv) <- evalExpr ctxt at' env arr
      SLRes ilifts (idx_lvl, idxv) <- evalExpr ctxt at' env idx
      let lvl = arr_lvl <> idx_lvl
      let retRef t arr_dla idx_dla = do
            (dv, lifts') <- ctxt_lift_expr ctxt at' (DLVar at' (ctxt_local_name ctxt "ref") t) (DLE_ArrayRef at' arr_dla idx_dla)
            let ansv = SLV_DLVar dv
            return $ SLRes (alifts <> ilifts <> lifts') (lvl, ansv)
      case idxv of
        SLV_Int _ idxi ->
          case arrv of
            SLV_Array _ arrvs ->
              case atMay arrvs idxi of
                Nothing ->
                  expect_throw at' $ Err_Eval_RefOutOfBounds (length arrvs) idxi
                Just ansv ->
                  return $ SLRes (alifts <> ilifts) (lvl, ansv)
            SLV_DLVar adv@(DLVar _ _ (T_Array ts) _) ->
              case atMay ts idxi of
                Nothing ->
                  expect_throw at' $ Err_Eval_RefOutOfBounds (length ts) idxi
                Just t -> retRef t arr_dla idx_dla
                  where
                    arr_dla = DLA_Var adv
                    idx_dla = DLA_Con (DLC_Int idxi)
            _ ->
              expect_throw at' $ Err_Eval_RefNotArray arrv
        SLV_DLVar idxdv@(DLVar _ _ T_UInt256 _) ->
          case arr_ty of
            T_Array ts ->
              retRef elem_ty arr_dla idx_dla
              where
                idx_dla = DLA_Var idxdv
                elem_ty = typeMeets at' $ map (\x -> (at', x)) ts
            _ ->
              expect_throw at' $ Err_Eval_RefNotArray arrv
          where
            (arr_ty, arr_dla) = typeOf at' arrv
        _ ->
          expect_throw at' $ Err_Eval_RefNotInt idxv

evalExprs :: SLCtxt s -> SrcLoc -> SLEnv -> [JSExpression] -> SLComp s [SLSVal]
evalExprs ctxt at env rands =
  case rands of
    [] -> return $ SLRes mempty []
    (rand0 : randN) -> do
      SLRes lifts0 sval0 <- evalExpr ctxt at env rand0
      SLRes liftsN svalN <- evalExprs ctxt at env randN
      return $ SLRes (lifts0 <> liftsN) (sval0:svalN)

evalDecl :: SLCtxt s -> SrcLoc -> SLEnv -> JSExpression -> SLComp s SLEnv
evalDecl ctxt at env decl =
  case decl of
    JSVarInitExpression lhs (JSVarInit va rhs) -> do
      let vat' = srcloc_jsa "var initializer" va at
      (lhs_ns, make_env) <-
        case lhs of
          (JSIdentifier a x) -> do
            let _make_env v = return (mempty, env_insert (srcloc_jsa "id" a at) x v env)
            return ([x], _make_env)
          (JSArrayLiteral a xs _) -> do
            let at' = srcloc_jsa "array" a at
            let ks = map (jse_expect_id at') $ jsa_flatten xs
            let _make_env (lvl, v) = do
                  (vs_lifts, vs) <-
                    case v of
                      SLV_Array _ x -> return (mempty, x)
                      SLV_DLVar dv@(DLVar _ _ (T_Array ts) _) -> do
                        vs_liftsl_and_dvs <- zipWithM mk_ref ts [0 ..]
                        let (vs_liftsl, dvs) = unzip vs_liftsl_and_dvs
                        let vs_lifts = mconcat vs_liftsl
                        return (vs_lifts, dvs)
                        where
                          mk_ref t i = do
                            let e = (DLE_ArrayRef vat' (DLA_Var dv) (DLA_Con (DLC_Int i)))
                            (dvi, i_lifts) <- ctxt_lift_expr ctxt at (DLVar vat' (ctxt_local_name ctxt "array idx") t) e
                            return $ (i_lifts, SLV_DLVar dvi)
                      _ ->
                        expect_throw at' (Err_Decl_NotArray v)
                  let kvs = zipEq at' Err_Decl_WrongArrayLength ks $ map (\x -> (lvl, x)) vs
                  return $ (vs_lifts, foldl' (env_insertp at') env kvs)
            return (ks, _make_env)
          _ ->
            expect_throw at (Err_DeclLHS_IllegalJS lhs)
      let ctxt' = ctxt_local_name_set ctxt lhs_ns
      SLRes rhs_lifts v <- evalExpr ctxt' vat' env rhs
      (lhs_lifts, env') <- make_env v
      traceM $ "evalDecl: defining " ++ (show $ M.keys $ M.difference env' env) ++ " at " ++ show vat'
      return $ SLRes (rhs_lifts <> lhs_lifts) env'
    _ ->
      expect_throw at (Err_Decl_IllegalJS decl)

evalDecls :: SLCtxt s -> SrcLoc -> SLEnv -> (JSCommaList JSExpression) -> SLComp s SLEnv
evalDecls ctxt at env decls =
  --- Note: This makes it so that declarations on the left are visible
  --- on the right, which might be different than JavaScript?
  foldlM f (SLRes mempty env) $ jscl_flatten decls
  where
    f (SLRes lifts env') decl = keepLifts lifts $ evalDecl ctxt at env' decl

evalStmt :: SLCtxt s -> SrcLoc -> SLEnv -> [JSStatement] -> SLComp s SLStmtRes
evalStmt ctxt at env ss =
  case ss of
    [] ->
      case ctxt_must_ret ctxt of
        False -> return $ SLRes mempty $ SLStmtRes env []
        True -> evalStmt ctxt at env $ [(JSReturn JSNoAnnot Nothing JSSemiAuto)]
    ((JSStatementBlock a ss' _ sp) : ks) -> do
      br <- evalStmt ctxt at_in env ss'
      retSeqn br at_after ks
      where
        at_in = srcloc_jsa "block" a at
        at_after = srcloc_after_semi "block" a sp at
    (s@(JSBreak a _ _) : _) -> illegal a s "break"
    (s@(JSLet a _ _) : _) -> illegal a s "let"
    (s@(JSClass a _ _ _ _ _ _) : _) -> illegal a s "class"
    ((JSConstant a decls sp) : ks) -> do
      SLRes lifts env' <- evalDecls ctxt at_in env decls
      keepLifts lifts $ evalStmt ctxt at_after env' ks
      where
        at_after = srcloc_after_semi lab a sp at
        at_in = srcloc_jsa lab a at
        lab = "const"
    ((JSContinue a _ _) : _) ->
      expect_throw (srcloc_jsa "continue" a at) (Err_Block_Continue)
    (s@(JSDoWhile a _ _ _ _ _ _) : _) -> illegal a s "do while"
    (s@(JSFor a _ _ _ _ _ _ _ _) : _) -> illegal a s "for"
    (s@(JSForIn a _ _ _ _ _ _) : _) -> illegal a s "for in"
    (s@(JSForVar a _ _ _ _ _ _ _ _ _) : _) -> illegal a s "for var"
    (s@(JSForVarIn a _ _ _ _ _ _ _) : _) -> illegal a s "for var in"
    (s@(JSForLet a _ _ _ _ _ _ _ _ _) : _) -> illegal a s "for let"
    (s@(JSForLetIn a _ _ _ _ _ _ _) : _) -> illegal a s "for let in"
    (s@(JSForLetOf a _ _ _ _ _ _ _) : _) -> illegal a s "for let of"
    (s@(JSForConst a _ _ _ _ _ _ _ _ _) : _) -> illegal a s "for const"
    (s@(JSForConstIn a _ _ _ _ _ _ _) : _) -> illegal a s "for const in"
    (s@(JSForConstOf a _ _ _ _ _ _ _) : _) -> illegal a s "for const of"
    (s@(JSForOf a _ _ _ _ _ _) : _) -> illegal a s "for of"
    (s@(JSForVarOf a _ _ _ _ _ _ _) : _) -> illegal a s "for var of"
    (s@(JSAsyncFunction a _ _ _ _ _ _ _) : _) -> illegal a s "async function"
    ((JSFunction a name _ jsformals _ body sp) : ks) ->
      evalStmt ctxt at_after env' ks
      where
        clo = SLV_Clo at' (Just f) formals body env
        formals = parseJSFormals at' jsformals
        at' = srcloc_jsa lab a at
        at_after = srcloc_after_semi lab a sp at
        lab = "function def"
        env' = env_insert at f (public clo) env
        f = case name of
          JSIdentNone -> expect_throw at' (Err_TopFun_NoName)
          JSIdentName _ x -> x
    (s@(JSGenerator a _ _ _ _ _ _ _) : _) -> illegal a s "generator"
    ((JSIf a la ce ra ts) : ks) -> do
      evalStmt ctxt at env ((JSIfElse a la ce ra ts ea fs) : ks)
      where
        ea = ra
        fs = (JSEmptyStatement ea)
    ((JSIfElse a _ ce ta ts fa fs) : ks) -> do
      let at' = srcloc_jsa "if" a at
      let t_at' = srcloc_jsa "if > true" ta at'
      let f_at' = srcloc_jsa "if > false" fa t_at'
      SLRes clifts (clvl, cv) <- evalExpr ctxt at' env ce
      tr <- evalStmt ctxt t_at' env [ts]
      fr <- evalStmt ctxt f_at' env [fs]
      keepLifts clifts $
        case cv of
          SLV_Bool _ cb -> do
            retSeqn (if cb then tr else fr) at' ks
          SLV_DLVar cond_dv@(DLVar _ _ T_Bool _) -> do
            let SLRes tlifts (SLStmtRes _ trets) = tr
            let SLRes flifts (SLStmtRes _ frets) = fr
            let lifts' = return $ DLS_If at' (DLA_Var cond_dv) tlifts flifts
            let levelHelp = map (\(r_at, (r_lvl, r_v)) -> (r_at, (clvl <> r_lvl, r_v)))
            let trets' = levelHelp trets
            let frets' = levelHelp frets
            let rets' =
                  case (trets, frets) of
                    ([], []) -> []
                    ([], _) -> [(t_at', (clvl, SLV_Null t_at' "if empty true"))] ++ frets'
                    (_, []) -> trets' ++ [(f_at', (clvl, SLV_Null f_at' "if empty false"))]
                    (_, _) -> trets' ++ frets'
            let ir = SLRes lifts' (SLStmtRes env rets')
            retSeqn ir at' ks
          _ ->
            expect_throw at (Err_Eval_IfCondNotBool cv)
    (s@(JSLabelled _ a _) : _) -> illegal a s "labelled"
    ((JSEmptyStatement a) : ks) -> evalStmt ctxt at' env ks
      where
        at' = srcloc_jsa "empty" a at
    ((JSExpressionStatement e sp) : ks) -> do
      SLRes elifts sev <- evalExpr ctxt at env e
      let (_, ev) = sev
      case (ctxt_mode ctxt, ev) of
        (SLC_Step penvs, SLV_Form (SLForm_Part_OnlyAns only_at who penv' only_v)) ->
          case typeOf at_after only_v of
            (T_Null, _) ->
              keepLifts lifts' $ evalStmt ctxt' at_after env ks
              where
                ctxt' = ctxt {ctxt_mode = SLC_Step $ M.insert who penv' penvs}
                lifts' = return $ DLS_Only only_at who elifts
            _ -> expect_throw at (Err_Block_NotNull ev)
        (SLC_Step penvs, SLV_Form (SLForm_Part_ToConsensus to_at who Nothing mmsg _XXX_mamt _XXX_mtime)) -> do
          let penv = penvs M.! who
          traceM $ "to_consensus from " ++ show who
          (msg_env, tmsg_) <-
            case mmsg of
              Nothing -> return (mempty, [])
              Just msg -> do
                let mk var = do
                      let val =
                             case env_lookup to_at var penv of
                               (Public, x) -> x
                               (Secret, x) ->
                                   expect_throw at $ Err_ExpectedPublic x
                      let (t, da) = typeOf to_at val
                      let m = case da of
                                DLA_Var (DLVar _ v _ _) -> v
                                _ -> "msg"
                      x <- ctxt_alloc ctxt to_at
                      return $ (da, DLVar to_at m t x)
                tvs <- mapM mk msg
                return $ (foldl' (env_insertp at_after) mempty $ zip msg $ map (public . SLV_DLVar) $ map snd tvs, tvs)
          --- We go back to the original env from before the to-consensus step
          let env' = env_merge to_at env msg_env
          let penvs' =
                M.mapWithKey
                  (\p old ->
                     case p == who of
                       True -> old
                       False -> env_merge to_at old msg_env)
                  penvs
          let ctxt_cstep = (ctxt {ctxt_mode = SLC_ConsensusStep penvs'})
          SLRes conlifts cr <- evalStmt ctxt_cstep at_after env' ks
          let lifts' = elifts <> (return $ DLS_ToConsensus to_at who (map fst tmsg_) (map snd tmsg_) conlifts)
          return $ SLRes lifts' cr
        (SLC_ConsensusStep penvs, SLV_Prim SLPrim_committed) -> do
          let ctxt_step = (ctxt {ctxt_mode = SLC_Step penvs})
          SLRes steplifts cr <- evalStmt ctxt_step at_after env ks
          let lifts' = elifts <> (return $ DLS_FromConsensus at steplifts)
          return $ SLRes lifts' cr
        _ ->
          case typeOf at_after ev of
            (T_Null, _) -> keepLifts elifts $ evalStmt ctxt at_after env ks
            _ -> expect_throw at (Err_Block_NotNull ev)
      where
        at_after = srcloc_after_semi "expr stmt" JSNoAnnot sp at
    ((JSAssignStatement _lhs op _rhs _asp) : ks) ->
      case (op, ks) of
        ((JSAssign _), ((JSContinue a _bl sp) : cont_ks)) ->
          expect_empty_tail lab a sp at cont_ks res
          where
            lab = "continue"
            at' = srcloc_jsa lab a at
            res = expect_throw at' (Err_XXX lab)
        _ ->
          expect_throw (srcloc_jsa "assign" JSNoAnnot at) (Err_Block_Assign)
    ((JSMethodCall e a args ra sp) : ks) ->
      evalStmt ctxt at env ss'
      where
        ss' = (JSExpressionStatement e' sp) : ks
        e' = (JSCallExpression e a args ra)
    ((JSReturn a me sp) : ks) -> do
      let lab = "return"
      let at' = srcloc_jsa lab a at
      SLRes elifts sev <-
        case me of
          Nothing -> return $ SLRes mempty $ public $ SLV_Null at' "empty return"
          Just e -> evalExpr ctxt at' env e
      let ret = case ctxt_ret ctxt of
            Just x -> x
            Nothing -> expect_throw at' $ Err_Eval_NoReturn
      let (_, ev) = sev
      let lifts' = return $ DLS_Return at' ret ev
      expect_empty_tail lab a sp at ks $
        return $ SLRes (elifts <> lifts') (SLStmtRes env [(at', sev)])
    (s@(JSSwitch a _ _ _ _ _ _ _) : _) -> illegal a s "switch"
    (s@(JSThrow a _ _) : _) -> illegal a s "throw"
    (s@(JSTry a _ _ _) : _) -> illegal a s "try"
    ((JSVariable a _while_decls _vsp) : ks) ->
      case ks of
        ( (JSMethodCall (JSIdentifier _ "invariant") _ _invariant_args _ _isp)
            : (JSWhile _ _ _while_cond _ _while_body)
            : _while_ks
          ) ->
            expect_throw at' (Err_XXX "while")
        _ ->
          expect_throw at' (Err_Block_Variable)
      where
        at' = (srcloc_jsa "var" a at)
    ((JSWhile a _ _ _ _) : _) ->
      expect_throw (srcloc_jsa "while" a at) (Err_Block_While)
    (s@(JSWith a _ _ _ _ _) : _) -> illegal a s "with"
  where
    illegal a s lab =
      expect_throw (srcloc_jsa lab a at) (Err_Block_IllegalJS s)
    retSeqn sr at' ks = do
      case dropEmptyJSStmts ks of
        [] -> return $ sr
        ks' -> do
          let SLRes lifts0 (SLStmtRes _ rets0) = sr
          let ctxt' =
                case rets0 of
                  [] -> ctxt
                  (_ : _) -> ctxt {ctxt_must_ret = True}
          SLRes lifts1 (SLStmtRes env1 rets1) <- evalStmt ctxt' at' env ks'
          return $ SLRes (lifts0 <> lifts1) (SLStmtRes env1 (rets0 ++ rets1))

expect_empty_tail :: String -> JSAnnot -> JSSemi -> SrcLoc -> [JSStatement] -> a -> a
expect_empty_tail lab a sp at ks res =
  case ks of
    [] -> res
    _ ->
      expect_throw at' (Err_TailNotEmpty ks)
      where
        at' = srcloc_after_semi lab a sp at

evalTopBody :: SLCtxt s -> SrcLoc -> SLLibs -> SLEnv -> SLEnv -> [JSModuleItem] -> SLComp s SLEnv
evalTopBody ctxt at libm env exenv body =
  case body of
    [] -> return $ SLRes mempty exenv
    mi : body' ->
      case mi of
        (JSModuleImportDeclaration _ im) ->
          case im of
            JSImportDeclarationBare a libn sp ->
              evalTopBody ctxt at_after libm env' exenv body'
              where
                at_after = srcloc_after_semi lab a sp at
                at' = srcloc_jsa lab a at
                lab = "import"
                env' = env_merge at' env libex
                libex =
                  case M.lookup (ReachSourceFile libn) libm of
                    Just x -> x
                    Nothing ->
                      impossible $ "dependency not found"
            --- FIXME support more kinds
            _ -> expect_throw at (Err_Import_IllegalJS im)
        (JSModuleExportDeclaration a ed) ->
          case ed of
            JSExport s _ -> doStmt at' True s
            --- FIXME support more kinds
            _ -> expect_throw at' (Err_Export_IllegalJS ed)
          where
            at' = srcloc_jsa "export" a at
        (JSModuleStatementListItem s) -> doStmt at False s
      where
        doStmt at' isExport sm = do
          smr <- evalStmt ctxt at' env [sm]
          case smr of
            SLRes Seq.Empty (SLStmtRes env' []) ->
              let exenv' = case isExport of
                    True ->
                      --- If this is an exporting statement,
                      --- then add to the export environment
                      --- everything that is new.
                      env_merge at' exenv (M.difference env' env)
                    False ->
                      exenv
               in evalTopBody ctxt at' libm env' exenv' body'
            SLRes {} ->
              expect_throw at' $ Err_Module_Return smr

type SLMod = (ReachSource, [JSModuleItem])

type SLLibs = (M.Map ReachSource SLEnv)

evalLib :: SLMod -> SLLibs -> ST s SLLibs
evalLib (src, body) libm = do
  let ctxt_top =
        (SLCtxt
           { ctxt_mode = SLC_Module
           , ctxt_id = Nothing
           , ctxt_must_ret = False
           , ctxt_ret = Nothing
           , ctxt_stack = []
           , ctxt_local_mname = Nothing
           })
  (SLRes flifts exenv) <- evalTopBody ctxt_top prev_at libm stdlib_env mt_env body'
  case flifts == mempty of
    False -> impossible $ "evalLib had lifts"
    True -> return $ M.insert src exenv libm
  where
    stdlib_env =
      case src of
        ReachStdLib -> base_env
        ReachSourceFile _ -> M.union (libm M.! ReachStdLib) base_env
    at = (srcloc_src src)
    (prev_at, body') =
      case body of
        ((JSModuleStatementListItem (JSExpressionStatement (JSStringLiteral a "\'reach 0.1\'") sp)) : j) ->
          ((srcloc_after_semi "header" a sp at), j)
        _ -> expect_throw at (Err_NoHeader body)

evalLibs :: [SLMod] -> ST s SLLibs
evalLibs mods = foldrM evalLib mempty mods

--- Linearizer

type LLRets = M.Map Int DLVar

lin_ss :: (LLRets -> DLStmt -> a -> a) -> LLRets -> DLStmts -> a -> a
lin_ss lin_s rets ss k = foldr (lin_s rets) k ss

--- XXX Too much duplication in these
data LLLocal
  = LLL_LocalStop
  | LLL_Let SrcLoc DLVar DLExpr LLLocal
  | LLL_Var SrcLoc DLVar LLLocal
  | LLL_Set SrcLoc DLVar DLArg LLLocal
  | LLL_Claim SrcLoc [SLCtxtFrame] ClaimType DLArg LLLocal
  | LLL_If SrcLoc DLArg LLLocal LLLocal
  deriving (Eq, Show)

lin_local_s :: LLRets -> DLStmt -> LLLocal -> LLLocal
lin_local_s rets s k =
  case s of
    DLS_Let at dv de -> LLL_Let at dv de k
    DLS_Claim at f ct da -> LLL_Claim at f ct da k
    DLS_If at ca ts fs -> LLL_If at ca t' f'
      where
        t' = iters rets ts LLL_LocalStop
        f' = iters rets fs LLL_LocalStop
    DLS_Transfer {} ->
      impossible $ "local cannot transfer"
    DLS_Return at ret sv ->
      case M.lookup ret rets of
        Nothing -> k
        Just dv -> LLL_Set at dv da k
          where
            (_, da) = typeOf at sv
    DLS_Prompt _ (Left _) ss -> iters rets ss k
    DLS_Prompt at (Right dv@(DLVar _ _ _ ret)) ss ->
      LLL_Var at dv $ iters rets' ss k
      where
        rets' = M.insert ret dv rets
    DLS_Only {} ->
      impossible $ "local cannot only"
    DLS_ToConsensus {} ->
      impossible $ "local cannot consensus"
    DLS_FromConsensus {} ->
      impossible $ "local cannot from consensus"
  where
    iters = lin_ss lin_local_s

lin_local :: DLStmts -> LLLocal
lin_local ss = lin_ss lin_local_s mempty ss LLL_LocalStop

data LLConsensus
  = LLC_ConStop
  | LLC_Let SrcLoc DLVar DLExpr LLConsensus
  | LLC_Var SrcLoc DLVar LLConsensus
  | LLC_Set SrcLoc DLVar DLArg LLConsensus
  | LLC_Claim SrcLoc [SLCtxtFrame] ClaimType DLArg LLConsensus
  | LLC_LocalIf SrcLoc DLArg LLConsensus LLConsensus LLConsensus
  | LLC_If SrcLoc DLArg LLConsensus LLConsensus
  | LLC_Transfer SrcLoc SLPart DLArg LLConsensus
  | LLC_FromConsensus SrcLoc LLStep
  deriving (Eq, Show)

lin_con_s :: (DLStmts -> LLStep) -> LLRets -> DLStmt -> LLConsensus -> LLConsensus
lin_con_s back rets s k =
  case s of
    DLS_Let at dv de -> LLC_Let at dv de k
    DLS_Claim at f ct da -> LLC_Claim at f ct da k
    DLS_If at ca ts fs ->
      case stmt_local s of
        True ->
          LLC_LocalIf at ca t' f' k
          where
            t' = iters rets ts LLC_ConStop
            f' = iters rets fs LLC_ConStop
        False ->
          LLC_If at ca t' f'
          where
            t' = iters rets ts k
            f' = iters rets fs k
    DLS_Transfer at who aa -> LLC_Transfer at who aa k
    DLS_Return at ret sv ->
      case M.lookup ret rets of
        Nothing -> k
        Just dv -> LLC_Set at dv da k
          where
            (_, da) = typeOf at sv
    DLS_Prompt _ (Left _) ss -> iters rets ss k
    DLS_Prompt at (Right dv@(DLVar _ _ _ ret)) ss ->
      LLC_Var at dv $ iters rets' ss k
      where
        rets' = M.insert ret dv rets
    DLS_Only {} -> impossible $ "consensus cannot only"
    DLS_ToConsensus {} -> impossible $ "consensus cannot toconsensus"
    DLS_FromConsensus at cons ->
      case k of
        LLC_ConStop ->
          LLC_FromConsensus at $ back cons
        _ ->
          impossible $ "consensus cannot fromconsensus w/ non-empty k"
  where
    iters = lin_ss (lin_con_s back)

lin_con :: (DLStmts -> LLStep) -> DLStmts -> LLConsensus
lin_con back ss = lin_ss (lin_con_s back) mempty ss LLC_ConStop

data LLStep
  = LLS_Stop DLArg
  | LLS_LocalStop
  | LLS_Let SrcLoc DLVar DLExpr LLStep
  | LLS_Var SrcLoc DLVar LLStep
  | LLS_Set SrcLoc DLVar DLArg LLStep
  | LLS_Claim SrcLoc [SLCtxtFrame] ClaimType DLArg LLStep
  | LLS_LocalIf SrcLoc DLArg LLStep LLStep LLStep
  | LLS_If SrcLoc DLArg LLStep LLStep
  | LLS_Only SrcLoc SLPart LLLocal LLStep
  | LLS_ToConsensus SrcLoc SLPart [DLArg] [DLVar] LLConsensus
  deriving (Eq, Show)

lin_step_s :: LLRets -> DLStmt -> LLStep -> LLStep
lin_step_s rets s k =
  case s of
    DLS_Let at dv de -> LLS_Let at dv de k
    DLS_Claim at f ct da -> LLS_Claim at f ct da k
    DLS_If at ca ts fs ->
      case stmt_local s of
        True ->
          LLS_LocalIf at ca t' f' k
          where
            t' = iters rets ts LLS_LocalStop
            f' = iters rets fs LLS_LocalStop
        False ->
          LLS_If at ca t' f'
          where
            t' = iters rets ts k
            f' = iters rets fs k
    DLS_Transfer {} -> impossible $ "step cannot transfer"
    DLS_Return at ret sv ->
      case M.lookup ret rets of
        Nothing -> k
        Just dv -> LLS_Set at dv da k
          where
            (_, da) = typeOf at sv
    DLS_Prompt _ (Left _) ss -> iters rets ss k
    DLS_Prompt at (Right dv@(DLVar _ _ _ ret)) ss ->
      LLS_Var at dv $ iters rets' ss k
      where
        rets' = M.insert ret dv rets
    DLS_Only at who ss ->
      LLS_Only at who ls k
      where
        ls = lin_local ss
    DLS_ToConsensus at who as ms cons ->
      LLS_ToConsensus at who as ms cons'
      where
        cons' = lin_con back cons
        back more = iters rets more k
    DLS_FromConsensus {} -> impossible $ "step cannot fromconsensus"
  where
    iters = lin_ss lin_step_s

linearize :: DLStmts -> DLArg -> LLStep
linearize ss da = lin_ss lin_step_s mempty ss (LLS_Stop da)

--- Compiler integration
makeInteract :: SrcLoc -> SLEnv -> SLVal
makeInteract at spec = SLV_Object at spec'
  where
    spec' = M.mapWithKey wrap_ty spec
    wrap_ty k (Public, (SLV_Type t)) = secret $ SLV_Prim $ SLPrim_interact at k t
    wrap_ty _ v = expect_throw at $ Err_DApp_InvalidInteract v

type FinalResultXXX = ()

compileDApp :: SLVal -> ST s FinalResultXXX
compileDApp topv =
  case topv of
    SLV_Prim (SLPrim_DApp_Delay at [(SLV_Object _ _opts), (SLV_Array _ parts), clo] top_env) -> do
      --- FIXME look at opts
      idxr <- newSTRef $ 0
      let ctxt_step =
            (SLCtxt
               { ctxt_mode = SLC_Step penvs
               , ctxt_id = Just idxr
               , ctxt_must_ret = False
               , ctxt_ret = Nothing
               , ctxt_stack = []
               , ctxt_local_mname = Nothing
               })
      SLRes final (SLAppRes _ (_, sv)) <- evalApplyVals ctxt_step at' (impossible "DApp_Delay expects clo") clo partvs
      let (_, final_da) = typeOf at sv
      traceM $ ""
      traceM $ show $ render_dls final
      traceM $ ""
      let linear = linearize final final_da
      traceM $ ""
      traceM $ show $ render_step linear
      traceM $ ""
      traceM $ "XXX Finish"
      where
        at' = srcloc_at "compileDApp" Nothing at
        penvs = M.fromList $ map make_penv partvs
        make_penv (Secret, (SLV_Participant _ pn io)) =
          (pn, env_insert at' "interact" (secret io) top_env)
        make_penv _ = impossible "SLPrim_DApp_Delay make_penv"
        partvs = map make_part parts
        make_part v =
          case v of
            SLV_Array p_at [SLV_Bytes _ bs, SLV_Object iat io] ->
                secret $ SLV_Participant p_at bs (makeInteract iat io)
            _ -> expect_throw at' (Err_DApp_InvalidPartSpec v)
    _ ->
      expect_throw srcloc_top (Err_Top_NotDApp topv)

compileBundle :: JSBundle -> SLVar -> FinalResultXXX
compileBundle (JSBundle mods) top = runST $ do
  libm <- evalLibs mods
  let exe_ex = libm M.! exe
  let topv = case M.lookup top exe_ex of
        Just (Public, x) -> x
        Just _ ->
          impossible "private before dapp"
        Nothing ->
          expect_throw srcloc_top (Err_Eval_UnboundId top $ M.keys exe_ex)
  compileDApp topv
  where
    exe = case mods of
      [] -> impossible $ "compileBundle: no files"
      ((x, _) : _) -> x

-- Temporary helper
render_dv :: DLVar -> Doc a
render_dv (DLVar _ s t i) = viaShow s <> ":" <> viaShow t <> ":" <> viaShow i

render_da :: DLArg -> Doc a
render_da a =
  case a of
    DLA_Var v -> render_dv v
    DLA_Con c -> viaShow c
    DLA_Array as -> brackets $ render_das as
    DLA_Obj _env -> "XXX obj"

render_das :: [DLArg] -> Doc a
render_das as = hcat $ punctuate comma $ map render_da as

render_de :: DLExpr -> Doc a
render_de e =
  case e of
    DLE_PrimOp _ o as -> viaShow o <> parens (render_das as)
    DLE_ArrayRef _ a o -> render_da a <> brackets (render_da o)
    DLE_Interact _ m as -> "interact." <> viaShow m <> parens (render_das as)
    DLE_Digest _ as -> "digest" <> parens (render_das as)

render_sp :: SLPart -> Doc a
render_sp p = viaShow p

render_nest :: Doc a -> Doc a
render_nest inner = nest 2 $ braces (hardline <> inner <> " ")

render_dl :: DLStmt -> Doc a
render_dl d =
  case d of
    DLS_Let _ v e ->
      "let" <+> render_dv v <+> "=" <+> render_de e <> semi
    DLS_Claim _ _ ct a ->
      "claim" <> parens (viaShow ct) <> parens (render_da a) <> semi
    DLS_If _ ca ts fs ->
      "if" <+> render_da ca <+> "then"
        <+> ns ts <> hardline <> "else"
        <+> ns fs <> semi
    DLS_Transfer _ who da ->
      "transfer." <> parens (render_da da) <> ".to" <> parens (render_sp who) <> semi
    DLS_Return _ ret sv ->
      "throw" <> parens (viaShow sv) <> ".to" <> parens (viaShow ret) <> semi
    DLS_Prompt _ ret bodys ->
      "prompt" <> parens (viaShow ret) <+> ns bodys <> semi
    DLS_Only _ who onlys ->
      "only" <> parens (render_sp who) <+> ns onlys <> semi
    DLS_ToConsensus _ who as vs cons ->
      "publish" <> parens (render_sp who) <> (cm $ map render_da as) <> (cm $ map render_dv vs) 
        <> ns cons
    DLS_FromConsensus _ more ->
      "commit()" <> semi <> hardline <> render_dls more
  where
    ns x = render_nest $ render_dls x
    cm l = parens (hsep $ punctuate comma $ l)

render_dls :: DLStmts -> Doc a
render_dls ss = concatWith (surround hardline) $ fmap render_dl ss

render_local :: LLLocal -> Doc a
render_local l =
  case l of
    LLL_LocalStop -> "next()" <> semi
    LLL_Let at dv de k -> help (DLS_Let at dv de) k
    LLL_Var _at dv k -> "var" <+> render_dv dv <> semi <> hardline <> render_local k
    LLL_Set _at dv da k -> render_dv dv <+> "=" <+> render_da da <> semi <> hardline <> render_local k
    LLL_Claim at f ct a k -> help (DLS_Claim at f ct a) k
    LLL_If _at ca t f ->
      "if" <+> render_da ca <+> "then"
        <+> ns t <> hardline <> "else"
        <+> ns f <> semi
  where
    help d k = render_dl d <> hardline <> render_local k
    ns x = render_nest $ render_local x

render_con :: LLConsensus -> Doc a
render_con s =
  case s of
    LLC_ConStop -> mempty
    LLC_Let at dv de k -> help (DLS_Let at dv de) k
    LLC_Var _at dv k -> "var" <+> render_dv dv <> semi <> hardline <> render_con k
    LLC_Set _at dv da k -> render_dv dv <+> "=" <+> render_da da <> semi <> hardline <> render_con k
    LLC_Claim at f ct a k -> help (DLS_Claim at f ct a) k
    LLC_LocalIf _at ca t f k -> do_if ca t f <> hardline <> render_con k
    LLC_If _at ca t f -> do_if ca t f
    LLC_Transfer at who da k -> help (DLS_Transfer at who da) k
    LLC_FromConsensus _at k ->
      "commit()" <> semi <> hardline <> render_step k
  where
    help d k = render_dl d <> hardline <> render_con k
    ns = render_nest
    do_if ca t f =
      "if" <+> render_da ca <+> "then"
        <+> ns (render_con t) <> hardline <> "else"
        <+> ns (render_con f) <> semi

render_step :: LLStep -> Doc a
render_step s =
  case s of
    LLS_Stop da -> "exit" <> parens (render_da da) <> semi
    LLS_LocalStop -> mempty
    LLS_Let at dv de k -> help (DLS_Let at dv de) k
    LLS_Var _at dv k -> "var" <+> render_dv dv <> semi <> hardline <> render_step k
    LLS_Set _at dv da k -> render_dv dv <+> "=" <+> render_da da <> semi <> hardline <> render_step k
    LLS_Claim at f ct a k -> help (DLS_Claim at f ct a) k
    LLS_LocalIf _at ca t f k -> do_if ca t f <> hardline <> render_step k
    LLS_If _at ca t f -> do_if ca t f
    LLS_Only _at who onlys k ->
      "only" <> parens (render_sp who) <+> ns (render_local onlys) <> semi <> hardline <> render_step k
    LLS_ToConsensus _at who as vs cons ->
      "publish" <> parens (render_sp who) <> (cm $ map render_da as) <> (cm $ map render_dv vs)
        <> ns (render_con cons)
  where
    help d k = render_dl d <> hardline <> render_step k
    cm l = parens (hsep $ punctuate comma $ l)
    ns = render_nest
    do_if ca t f =
      "if" <+> render_da ca <+> "then"
        <+> ns (render_step t) <> hardline <> "else"
        <+> ns (render_step f) <> semi

-- Main entry point
compileNL :: CompilerOpts -> IO ()
compileNL copts = do
  let out = output copts
  djp <- gatherDeps_top $ source copts
  let nlp = compileBundle djp "main"
  out "nl" $ show nlp
  return ()
