{-# OPTIONS_GHC -fno-warn-orphans #-}
module Reach.CompilerNL where

import Debug.Trace

import Control.Monad
import Control.Monad.ST
import Data.Bits
import Data.FileEmbed
import Data.Foldable
import Data.IORef
import Data.List
import Data.Monoid
import Data.STRef
import GHC.IO.Encoding
import GHC.Stack(HasCallStack)
import Language.JavaScript.Parser
import Language.JavaScript.Parser.AST
import Safe (atMay)
import System.Directory
import System.FilePath
import Text.ParserCombinators.Parsec.Number (numberValue)
import qualified Data.ByteString.Char8 as B
import qualified Data.Graph as G
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq

import Reach.Util
import Reach.Compiler(CompilerOpts, output, source)

-- Helpers
zipEq :: SrcLoc -> (Int -> Int -> CompilerError s) -> [a] -> [b] -> [(a, b)]
zipEq at ce x y =
  if lx == ly then zip x y
  else expect_throw at (ce lx ly)
  where lx = length x
        ly = length y

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

jsStmtToExpr :: JSAnnot -> JSStatement -> JSExpression
jsStmtToExpr a bodys = ce
  where ce = JSCallExpression rator a rands a
        rator = JSArrowExpression aformals a bodys
        aformals = JSParenthesizedArrowParameterList a args a
        args = JSLNil
        rands = JSLNil

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
  --- XXX Add something about whether it's bound?
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
  | SLForm_Part_ToConsensus SLPart (Maybe ToConsensusMode) (Maybe [SLVar]) (Maybe SLVar) (Maybe SLVar)
  | SLForm_Part_OnlyAns SLPart SLEnv SLVal
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

env_insert :: HasCallStack => SrcLoc -> SLVar -> SLVal -> SLEnv -> SLEnv
env_insert at k v env =
  case M.lookup k env of
    Nothing -> M.insert k v env
    Just _ ->
      expect_throw at (Err_Shadowed k)

env_insertp :: HasCallStack => SrcLoc -> SLEnv -> (SLVar, SLVal) -> SLEnv
env_insertp at = flip (uncurry (env_insert at))

env_merge :: HasCallStack => SrcLoc -> SLEnv -> SLEnv -> SLEnv
env_merge at left righte = foldl' (env_insertp at) left $ M.toList righte

env_lookup :: HasCallStack => SrcLoc -> SLVar -> SLEnv -> SLVal
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
  deriving (Eq,Show,Ord)

data DLVar = DLVar SrcLoc String SLType Int
  deriving (Eq,Show,Ord)

data DLArg
  = DLA_Var DLVar
  | DLA_Con DLConstant
  | DLA_Array [DLArg]
  | DLA_Obj (M.Map String DLArg)
  deriving (Eq,Show)

data DLExpr
  = DLE_PrimOp SrcLoc PrimOp [DLArg]
  | DLE_ArrayRef SrcLoc DLArg DLArg
  | DLE_Interact SrcLoc String [DLArg]
  deriving (Eq,Show)

expr_pure :: DLExpr -> Bool
expr_pure e =
  case e of
    DLE_PrimOp _ _ _ -> True
    DLE_ArrayRef _ _ _ -> True
    DLE_Interact _ _ _ -> False

data ClaimType
  = CT_Assert   --- Verified on all paths
  | CT_Assume   --- Always assumed true
  | CT_Require  --- Verified in honest, assumed in dishonest. (This may
                --- sound backwards, but by verifying it in honest
                --- mode, then we are checking that the other
                --- participants fulfill the promise when acting
                --- honestly.)
  | CT_Possible --- Check if an assignment of variables exists to make
                --- this true.
  deriving (Show,Eq,Ord)
           
data DLStmt
  = DLS_Let SrcLoc DLVar DLExpr
  | DLS_Claim SrcLoc [ SLCtxtFrame ] ClaimType DLArg
  | DLS_If SrcLoc DLArg DLStmts DLStmts
  --- XXX These are only allowed in Steps... some sort of dep type?
  | DLS_Only SrcLoc SLPart DLStmts
  | DLS_All SrcLoc DLStmt
  deriving (Eq,Show)

stmt_pure :: DLStmt -> Bool
stmt_pure s =
  case s of
    DLS_Let _ _ e -> expr_pure e
    DLS_Claim _ _ _ _ -> False
    DLS_If _ _ x y -> stmts_pure x && stmts_pure y
    DLS_Only _ _ ss -> stmts_pure ss
    DLS_All _ as -> stmt_pure as

type DLStmts = Seq.Seq DLStmt

stmts_pure :: Foldable f => f DLStmt -> Bool
stmts_pure fs = getAll $ foldMap (All . stmt_pure) fs

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
  = Err_XXX String
  | Err_Apply_ArgCount Int Int
  | Err_Arrow_NoFormals
  | Err_Block_Assign
  | Err_Block_Continue
  | Err_Block_IllegalJS JSStatement
  | Err_Block_NotNull SLVal
  | Err_Block_Variable
  | Err_Block_While
  | Err_DApp_InvalidInteract SLVal
  | Err_DApp_InvalidPartSpec SLVal
  | Err_DeclLHS_IllegalJS JSExpression
  | Err_Decl_IllegalJS JSExpression
  | Err_Decl_NotArray SLVal
  | Err_Decl_WrongArrayLength Int Int
  | Err_Dot_InvalidField SLVal String
  | Err_Eval_IfCondNotBool SLVal
  | Err_Eval_IfNotNull SLVal SLVal
  | Err_Eval_IllegalContext (SLCtxt s) String
  | Err_Eval_IllegalJS JSExpression
  | Err_Eval_IllegalLift (SLCtxt s)
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
  | Err_Module_Return SLVal
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
  | Err_Type_TooFewArguments [SLType]
  | Err_Type_TooManyArguments [SLVal]
  deriving (Eq,Show)

--- XXX Add ctxt frame stack and display
expect_throw :: HasCallStack => SrcLoc -> CompilerError s -> b
expect_throw src ce = error $ "error: " ++ (show src) ++ ": " ++ (take 512 $ show ce)

-- Parser
type BundleMap a b = ((M.Map a [a]), (M.Map a (Maybe b)))
type JSBundleMap = BundleMap ReachSource [JSModuleItem]
data JSBundle = JSBundle [(ReachSource,[JSModuleItem])]
  deriving (Eq,Show)

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

data SLCtxt s = SLCtxt
  { ctxt_mode :: SLCtxtMode
  , ctxt_id :: Maybe (STRef s Int)
  , ctxt_stack :: [ SLCtxtFrame ] }
  deriving (Eq)

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

ctxt_alloc :: SLCtxt s -> SrcLoc -> ST s Int
ctxt_alloc ctxt at = do
  let idr = case ctxt_id ctxt of
              Just x -> x
              Nothing -> expect_throw at $ Err_Eval_IllegalLift ctxt
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

data SLRes = SLRes DLStmts SLEnv SLVal
  deriving (Eq, Show)

type SLKont s = SLRes -> ST s SLRes

kKeepLifts :: DLStmts -> SLKont s -> SLKont s
kKeepLifts lifts k (SLRes lifts' env val) =
  k $ SLRes (lifts <> lifts') env val

type SLCPSd s = SLKont s -> ST s SLRes

ctxt_stack_push :: SLCtxt s -> SLCtxtFrame -> SLCtxt s
ctxt_stack_push ctxt f =
  (ctxt { ctxt_stack = f : (ctxt_stack ctxt) })

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

typeOf :: SrcLoc -> SLVal -> (SLType, DLArg)
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
    SLV_DLVar dv@(DLVar _ _ t _) -> (t, DLA_Var dv)
    SLV_Type _ -> none
    SLV_Participant _ _ _ -> none --- XXX get the address
    SLV_Prim _ -> none --- XXX interacts may work
    SLV_Form _ -> none
  where none = expect_throw at $ Err_Type_None v

type TypeEnv s = M.Map SLVar (STRef s (Maybe SLType))

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
  where (val_ty, res) = typeOf at val
  
typeChecks :: SrcLoc -> TypeEnv s -> [SLType] -> [SLVal] -> ST s [DLArg]
typeChecks at env ts vs =
  case (ts, vs) of
    ([], []) ->
      return []
    ((t:ts'), (v:vs')) -> do
      d <- typeCheck at env t v
      ds' <- typeChecks at env ts' vs'
      return $ d : ds'
    ((_:_), _) ->
      expect_throw at $ Err_Type_TooFewArguments ts
    (_, (_:_)) ->
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
      checkAndConvert_i at env' ft args
    _ -> expect_throw at $ Err_Type_NotApplicable t

checkAndConvert :: SrcLoc -> SLType -> [SLVal] -> (SLType, [DLArg])
checkAndConvert at t args = runST $ checkAndConvert_i at mempty t args

evalDot :: SrcLoc -> SLVal -> String -> SLVal
evalDot at obj field =
  case obj of
    SLV_Object _ env ->
      case M.lookup field env of
        Just v -> v
        Nothing -> illegal_field
    SLV_Participant _ who _ ->
      case field of
        "only" -> SLV_Form (SLForm_Part_Only obj)
        "publish" -> SLV_Form (SLForm_Part_ToConsensus who (Just TCM_Publish) Nothing Nothing Nothing)
        "pay" -> SLV_Form (SLForm_Part_ToConsensus who (Just TCM_Pay) Nothing Nothing Nothing)
        _ -> illegal_field
    SLV_Form (SLForm_Part_ToConsensus who Nothing mpub mpay mtime) ->
      case field of
        "publish" -> SLV_Form (SLForm_Part_ToConsensus who (Just TCM_Publish) mpub mpay mtime)
        "pay" -> SLV_Form (SLForm_Part_ToConsensus who (Just TCM_Pay) mpub mpay mtime)
        "timeout" -> SLV_Form (SLForm_Part_ToConsensus who (Just TCM_Timeout) mpub mpay mtime)
        _ -> illegal_field
    v ->
      expect_throw at (Err_Eval_NotObject v)
  where illegal_field =
          expect_throw at (Err_Dot_InvalidField obj field)

evalForm :: SLCtxt s -> SrcLoc -> SLEnv -> SLForm -> [JSExpression] -> SLCPSd s
evalForm ctxt at env f args k =
  case f of
    SLForm_Part_Only (SLV_Participant _ who _) ->
      case ctxt_mode ctxt of
        SLC_Step penvs -> do
          --- XXX remove do
          let penv = penvs M.! who
          let k' elifts eargs =                
                case eargs of
                  [ thunk ] -> do
                    let ctxt_localstep =
                          (SLCtxt { ctxt_mode = SLC_LocalStep
                                  , ctxt_id = ctxt_id ctxt
                                  , ctxt_stack = ctxt_stack ctxt })
                    let k'' (SLRes alifts penv' ans) = do
                          traceM $ "penv' update = " ++ (show $ M.keys $ M.difference penv' penv)
                          k $ SLRes (elifts <> alifts) env $ SLV_Form $ SLForm_Part_OnlyAns who penv' ans
                    evalApplyVals ctxt_localstep at mt_env thunk [] k''
                  _ -> illegal_args
          let k'_res eargr =
                case eargr of
                  SLRes elifts _ (SLV_Array _ eargs) -> k' elifts eargs
                  _ -> impossible "evalExprs did not return array"
          let ctxt_local =
                (SLCtxt { ctxt_mode = SLC_Local
                        , ctxt_id = ctxt_id ctxt
                        , ctxt_stack = ctxt_stack ctxt })
          evalExprs ctxt_local at penv args k'_res
        _ -> expect_throw at $ Err_Eval_IllegalContext ctxt "part.only"
    SLForm_Part_Only _ -> impossible "SLForm_Part_Only args"
    SLForm_Part_OnlyAns _ _ _ -> impossible "SLForm_Part_OnlyAns"
    SLForm_Part_ToConsensus who mmode mpub mpay mtime ->
      case ctxt_mode ctxt of
        SLC_Step _penvs ->
          case mmode of
            Just TCM_Publish ->
              case mpub of
                Nothing -> retV $ SLV_Form $ SLForm_Part_ToConsensus who Nothing (Just msg) mpay mtime
                  where msg = map (jse_expect_id at) args
                Just _ ->
                  expect_throw at $ Err_ToConsensus_Double TCM_Publish
            Just TCM_Pay ->
              retV $ SLV_Form $ SLForm_Part_ToConsensus who Nothing mpub (Just $ error "XXX TCM_Pay " ++ show args) mtime
            Just TCM_Timeout ->
              retV $ SLV_Form $ SLForm_Part_ToConsensus who Nothing mpub mpay (Just $ error "XXX TCM_Time " ++ show args)
            Nothing ->
              expect_throw at $ Err_Eval_NotApplicable rator
        _ -> expect_throw at $ Err_Eval_IllegalContext ctxt "toConsensus"
  where illegal_args = expect_throw at (Err_Form_InvalidArgs f args)
        rator = SLV_Form f
        retV v = k $ SLRes mempty env v

evalPrimOp :: SLCtxt s -> SrcLoc -> SLEnv -> PrimOp -> [SLVal] -> SLCPSd s
evalPrimOp ctxt at env p args k =
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
    static v = k $ SLRes mempty env $ v
    make_var = do
      let (rng, dargs) = checkAndConvert at (primOpType p) args
      (dv, lifts) <- ctxt_lift_expr ctxt at (DLVar at "prim" rng) (DLE_PrimOp at p dargs)
      k $ SLRes lifts env $ SLV_DLVar dv

evalPrim :: SLCtxt s -> SrcLoc -> SLEnv -> SLPrimitive -> [SLVal] -> SLCPSd s
evalPrim ctxt at env p args k =
  case p of
    SLPrim_op op ->
      evalPrimOp ctxt at env op args k
    SLPrim_Fun ->
      case args of
        [ (SLV_Array _ dom_arr), (SLV_Type rng) ] ->
          retV $ SLV_Type $ T_Fun dom rng
          where dom = map expect_ty dom_arr
        _ -> illegal_args
    SLPrim_Array ->
      retV $ SLV_Type $ T_Array $ map expect_ty args
    SLPrim_makeEnum ->
      case args of
        [ SLV_Int _ i ] ->
          retV $ SLV_Array at' (enum_pred : map (SLV_Int at') [ 0 .. (i-1) ])
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
        SLC_Module ->
          case args of
            [ (SLV_Object _ _), (SLV_Array _ _), (SLV_Clo _ _ _ _ _) ] ->
              retV $ SLV_Prim $ SLPrim_DApp_Delay at args env
            _ -> illegal_args
        _ ->
          expect_throw at (Err_Eval_IllegalContext ctxt "DApp")
    SLPrim_DApp_Delay _ _ _ ->
      expect_throw at (Err_Eval_NotApplicable rator)
    SLPrim_interact _iat m t ->
      case ctxt_mode ctxt of
        SLC_LocalStep -> do
          let (rng, dargs) = checkAndConvert at t args
          (dv, lifts) <- ctxt_lift_expr ctxt at (DLVar at "interact" rng) (DLE_Interact at m dargs)
          k $ SLRes lifts env $ SLV_DLVar dv
        _ ->
          expect_throw at (Err_Eval_IllegalContext ctxt "interact")
    SLPrim_declassify ->
      case args of
        [ val ] ->
          --- XXX do declassify
          retV $ val
        _ -> illegal_args
    SLPrim_commit ->
      case args of
        [ ] -> retV $ SLV_Prim SLPrim_committed
        _ -> illegal_args
    SLPrim_committed -> illegal_args
    SLPrim_assume ->
      case ctxt_mode ctxt of
        SLC_LocalStep ->
          k $ SLRes lifts env $ SLV_Null at
          where darg =
                  case checkAndConvert at (T_Fun [T_Bool] T_Null) args of
                    (_, [x]) -> x
                    _ -> illegal_args
                lifts = return (DLS_Claim at (ctxt_stack ctxt) CT_Assume darg)
        _ -> illegal_args
  where illegal_args = expect_throw at (Err_Prim_InvalidArgs p args)
        retV v = k $ SLRes mempty env v
        rator = SLV_Prim p
        expect_ty v =
          case v of
            SLV_Type t -> t
            _ -> illegal_args

evalApplyVals :: SLCtxt s -> SrcLoc -> SLEnv -> SLVal -> [SLVal] -> SLCPSd s
evalApplyVals ctxt at env rator randvs k =
  case rator of
    SLV_Prim p ->
      evalPrim ctxt at env p randvs k
    SLV_Clo clo_at mname formals (JSBlock body_a body _) clo_env ->
      evalStmt ctxt' body_at env' body k
      where body_at = srcloc_jsa "block" body_a clo_at
            env' = foldl' (env_insertp clo_at) clo_env kvs
            kvs = zipEq clo_at Err_Apply_ArgCount formals randvs
            ctxt' = ctxt_stack_push ctxt (SLC_CloApp at clo_at mname)
    v ->
      expect_throw at (Err_Eval_NotApplicableVals v)

evalApply :: SLCtxt s -> SrcLoc -> SLEnv -> SLVal -> [JSExpression] -> SLCPSd s
evalApply ctxt at env rator rands k =
  case rator of
    SLV_Prim _ -> vals
    SLV_Clo _ _ _ _ _ -> vals
    SLV_Form f -> evalForm ctxt at env f rands k
    v -> expect_throw at (Err_Eval_NotApplicable v)
  where vals = evalExprs ctxt at env rands k'
        k' (SLRes rlifts _ (SLV_Array _ randvs)) = evalApplyVals ctxt at env rator randvs $ kKeepLifts rlifts k
        k' _ = impossible "evalExprs didn't return Array"

evalPropertyName :: SLCtxt s -> SrcLoc -> SLEnv -> JSPropertyName -> SLCPSd s
evalPropertyName ctxt at env pn k =
  case pn of
    JSPropertyIdent _ s -> k_res $ s
    JSPropertyString _ s -> k_res $ trimQuotes s
    JSPropertyNumber an _ ->
      expect_throw at_n (Err_Obj_IllegalField pn)
      where at_n = srcloc_jsa "number" an at
    JSPropertyComputed an e _ ->
      evalExpr ctxt at_n env e k
      where at_n = srcloc_jsa "computed field name" an at
  where k_res s = k $ SLRes mempty env $ SLV_Bytes at $ bpack s

evalPropertyPair :: SLCtxt s -> SrcLoc -> SLEnv -> SLRes -> JSObjectProperty -> SLCPSd s
evalPropertyPair ctxt at env ores@(SLRes olifts _ ov) p k =
  case p of
    JSPropertyNameandValue pn a vs ->
      evalPropertyName ctxt at' env pn k_value
      where at' = srcloc_jsa "property binding" a at
            k_value (SLRes flifts _ fv) =
              case fv of
                SLV_Bytes _ fb ->
                  case vs of
                    [ e ] ->
                      let f = B.unpack fb in
                        evalExpr ctxt at' env e (kKeepLifts flifts $ k_insert flifts f)
                    _ ->
                      expect_throw at' (Err_Obj_IllegalFieldValues vs)
                _ ->
                  expect_throw at' $ Err_Obj_IllegalComputedField fv
            k_insert flifts f (SLRes vlifts _ v) =
              k $ SLRes (olifts <> flifts <> vlifts) env $ SLV_Object at $ env_insert at' f v fenv
    JSPropertyIdentRef a v ->
      evalPropertyPair ctxt at env ores p' k
      where p' = JSPropertyNameandValue pn a vs
            pn = JSPropertyIdent a v
            vs = [ JSIdentifier a v ]
    JSObjectSpread a se ->
      evalExpr ctxt at' env se k'
      where k' (SLRes slifts _ sv) =
              case sv of
                SLV_Object _ senv ->
                  k $ SLRes (olifts <> slifts) env $ SLV_Object at $ env_merge at' fenv senv
                _ -> expect_throw at (Err_Obj_SpreadNotObj sv)
            at' = srcloc_jsa "...obj" a at
    _ ->
      expect_throw at (Err_Obj_IllegalJS p)
  where fenv = case ov of
                 (SLV_Object _ x) -> x
                 _ -> impossible "evalPropertyPair not given SLV_Object"

evalExpr :: SLCtxt s -> SrcLoc -> SLEnv -> JSExpression -> SLCPSd s
evalExpr ctxt at env e k =
  case e of
    JSIdentifier a x -> retV $ env_lookup (srcloc_jsa "id ref" a at) x env
    JSDecimal a ns -> retV $ SLV_Int (srcloc_jsa "decimal" a at) $ numberValue 10 ns
    JSLiteral a l ->
      case l of
        "null" -> retV $ SLV_Null at'
        "true" -> retV $ SLV_Bool at' True
        "false" -> retV $ SLV_Bool at' False
        _ -> expect_throw at' (Err_Parse_IllegalLiteral l)
      where at' = (srcloc_jsa "literal" a at)
    JSHexInteger a ns -> retV $ SLV_Int (srcloc_jsa "hex" a at) $ numberValue 16 ns
    JSOctal a ns -> retV $ SLV_Int (srcloc_jsa "octal" a at) $ numberValue 8 ns
    JSStringLiteral a s -> retV $ SLV_Bytes (srcloc_jsa "string" a at) (bpack (trimQuotes s))
    JSRegEx _ _ -> illegal
    JSArrayLiteral a as _ -> evalExprs ctxt at' env (jsa_flatten as) k
      where at' = (srcloc_jsa "array" a at)
    JSAssignExpression _ _ _ -> illegal
    JSAwaitExpression _ _ -> illegal
    JSCallExpression rator a rands _ -> doCall rator a $ jscl_flatten rands
    JSCallExpressionDot obj a field -> doDot obj a field
    JSCallExpressionSquare arr a idx _ -> doRef arr a idx
    JSClassExpression _ _ _ _ _ _ -> illegal
    JSCommaExpression _ _ _ -> illegal
    JSExpressionBinary lhs op rhs -> doCallV mempty (binaryToPrim at env op) JSNoAnnot [ lhs, rhs ]
    JSExpressionParen a ie _ -> evalExpr ctxt (srcloc_jsa "paren" a at) env ie k
    JSExpressionPostfix _ _ -> illegal
    JSExpressionTernary ce a te _ fe ->
      evalExpr ctxt at' env ce k_c
      where at' = srcloc_jsa "if" a at
            k_c cr = evalExpr ctxt at' env te (k_t cr)
            k_t cr ta = evalExpr ctxt at' env fe (k_fin cr ta)
            k_fin (SLRes clifts _ cv) (SLRes tlifts _ tv) (SLRes flifts _ fv) =
              case cv of
                SLV_Bool _ cb -> do
                  k $ SLRes (clifts <> elifts) env ev
                  where (elifts, ev) = if cb then (tlifts, tv) else (flifts, fv)
                SLV_DLVar dv@(DLVar _ _ T_Bool _) ->
                  case stmts_pure tlifts && stmts_pure flifts of
                    True ->
                      evalPrim ctxt at mempty (SLPrim_op $ CP IF_THEN_ELSE) [ cv, tv, fv ] $ kKeepLifts (clifts <> tlifts <> flifts) k
                    False -> do
                      --- XXX A consensus must duplicate continuation but a local doesn't need to
                      let ilifts = return $ DLS_If at (DLA_Var dv) tlifts flifts
                      let lifts' = clifts <> ilifts
                      --- XXX Don't ignore tv and fv
                      expect_throw at' (Err_XXX $ "impure if " ++ show lifts')
                _ ->
                  expect_throw at' (Err_Eval_IfCondNotBool cv)
    JSArrowExpression aformals a bodys ->
      retV $ SLV_Clo at' fname formals body env
      where at' = srcloc_jsa "arrow" a at
            fname = Nothing --- FIXME syntax-local-infer-name
            body = case bodys of
                     JSStatementBlock ba bodyss aa _ -> JSBlock ba bodyss aa
                     _ -> JSBlock JSNoAnnot [bodys] JSNoAnnot
            formals = parseJSArrowFormals at' aformals
    JSFunctionExpression a name _ jsformals _ body ->
      retV $ SLV_Clo at' fname formals body env
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
      foldlk k (evalPropertyPair ctxt at' env) (SLRes mempty env $ SLV_Object at' mempty) $ jsctl_flatten plist
      where at' = srcloc_jsa "obj" a at
    JSSpreadExpression _ _ -> illegal
    JSTemplateLiteral _ _ _ _ -> illegal
    JSUnaryExpression op ue -> doCallV mempty (unaryToPrim at env op) JSNoAnnot [ ue ]
    JSVarInitExpression _ _ -> illegal
    JSYieldExpression _ _ -> illegal
    JSYieldFromExpression _ _ _ -> illegal
  where illegal = expect_throw at (Err_Eval_IllegalJS e)
        retV v = k $ SLRes mempty env $ v
        doCallV rlifts ratorv a rands = evalApply ctxt at' env ratorv rands $ kKeepLifts rlifts k
          where at' = srcloc_jsa "application" a at
        doCall rator a rands = evalExpr ctxt at' env rator k'
          where k' (SLRes rlifts _ ratorv) = doCallV rlifts ratorv a rands
                at' = srcloc_jsa "application, rator" a at
        doDot obj a field = evalExpr ctxt at' env obj k'
          where k' (SLRes olifts _ objv) = k $ SLRes olifts env $ evalDot at' objv fields
                at' = srcloc_jsa "dot" a at
                fields = (jse_expect_id at') field
        doRef arr a idx = evalExpr ctxt at' env arr k'
          where at' = srcloc_jsa "array ref" a at
                k' arrr =
                  evalExpr ctxt at' env idx (k'' arrr)
                k'' (SLRes alifts _ arrv) (SLRes ilifts _ idxv) =
                  case idxv of
                    SLV_Int _ idxi ->
                      case arrv of
                        SLV_Array _ arrvs ->
                          case atMay arrvs idxi of
                            Nothing ->
                              expect_throw at' $ Err_Eval_RefOutOfBounds (length arrvs) idxi
                            Just ansv ->
                              k $ SLRes (alifts <> ilifts) env ansv
                        SLV_DLVar adv@(DLVar _ _ (T_Array ts) _) ->
                          case atMay ts idxi of
                            Nothing ->
                              expect_throw at' $ Err_Eval_RefOutOfBounds (length ts) idxi
                            Just t -> retRef t arr_dla idx_dla
                              where arr_dla = DLA_Var adv
                                    idx_dla = DLA_Con (DLC_Int idxi)
                        _ ->
                          expect_throw at' $ Err_Eval_RefNotArray arrv
                    SLV_DLVar idxdv@(DLVar _ _ T_UInt256 _) ->
                      case arr_ty of
                        T_Array [] ->
                          expect_throw at' $ Err_Eval_RefEmptyArray 
                        T_Array (t:ts) ->
                          case getAll $ foldMap (All . (t ==)) ts of
                            False ->
                              expect_throw at' $ Err_EvalRefIndirectNotHomogeneous (t:ts)
                            True -> retRef t arr_dla idx_dla
                              where idx_dla = DLA_Var idxdv
                        _ ->
                          expect_throw at' $ Err_Eval_RefNotArray arrv
                      where (arr_ty, arr_dla) = typeOf at' arrv
                    _ ->
                      expect_throw at' $ Err_Eval_RefNotInt idxv
                  where retRef t arr_dla idx_dla = do
                          (dv, lifts') <- ctxt_lift_expr ctxt at' (DLVar at' "ref" t) (DLE_ArrayRef at' arr_dla idx_dla)
                          let ansv = SLV_DLVar dv
                          k $ SLRes (alifts <> ilifts <> lifts') env ansv

evalExprs :: SLCtxt s -> SrcLoc -> SLEnv -> [JSExpression] -> SLCPSd s
evalExprs ctxt at env rands k =
  case rands of
    [] -> k $ SLRes mempty env (SLV_Array at [])
    (rand0:rands') ->
      evalExpr ctxt at env rand0 k'
      where k' (SLRes lifts0 _ val0) =
              evalExprs ctxt at env rands' k''
              where k'' (SLRes liftsN _ valsv) =
                      case valsv of
                        SLV_Array _ vals ->
                          k $ SLRes (lifts0 <> liftsN) env (SLV_Array at $ val0:vals)
                        _ ->
                          impossible "evalExprs did not return array"

evalDecl :: SLCtxt s -> SrcLoc -> SLRes -> JSExpression -> SLCPSd s
evalDecl ctxt at (SLRes lifts env _) decl k =
  case decl of
    JSVarInitExpression lhs (JSVarInit va rhs) ->
      evalExpr ctxt vat' env rhs k'
      where vat' = srcloc_jsa "var initializer" va at
            k' (SLRes rhs_lifts _ v) = do
              (lhs_lifts, env') <-
                case lhs of
                  (JSIdentifier a x) ->
                    return $ (mempty, env_insert (srcloc_jsa "id" a at) x v env)
                  (JSArrayLiteral a xs _) -> do
                    (vs_lifts, vs) <-
                      case v of
                        SLV_Array _ x -> return (mempty, x)
                        SLV_DLVar dv@(DLVar _ _ (T_Array ts) _) -> do
                          vs_liftsl_and_dvs <- zipWithM mk_ref ts [0..]
                          let (vs_liftsl, dvs) = unzip vs_liftsl_and_dvs
                          let vs_lifts = mconcat vs_liftsl
                          return (vs_lifts, dvs)
                          where mk_ref t i = do
                                  let e = (DLE_ArrayRef vat' (DLA_Var dv) (DLA_Con (DLC_Int i)))
                                  (dvi, i_lifts) <- ctxt_lift_expr ctxt at (DLVar vat' "array idx" t) e
                                  return $ (i_lifts, SLV_DLVar dvi)
                        _ ->
                          expect_throw at' (Err_Decl_NotArray v)
                    let kvs = zipEq at' Err_Decl_WrongArrayLength ks vs
                    return $ (vs_lifts, foldl' (env_insertp at') env kvs)
                    where ks = map (jse_expect_id at') $ jsa_flatten xs
                          at' = srcloc_jsa "array" a at
                  _ ->
                    expect_throw at (Err_DeclLHS_IllegalJS lhs)
              traceM $ "evalDecl: defining " ++ (show $ M.keys $ M.difference env' env) ++ " at " ++ show vat'
              k $ SLRes (lifts <> rhs_lifts <> lhs_lifts) env' $ SLV_Null at
    _ ->
      expect_throw at (Err_Decl_IllegalJS decl)

evalDecls :: SLCtxt s -> SrcLoc -> SLEnv -> (JSCommaList JSExpression) -> SLCPSd s
evalDecls ctxt at env decls k =
  --- Note: This makes it so that declarations on the left are visible
  --- on the right, which might be different than JavaScript?
  foldlk k (evalDecl ctxt at) (SLRes mempty env $ SLV_Null at) $ jscl_flatten decls

evalStmt :: SLCtxt s -> SrcLoc -> SLEnv -> [JSStatement] -> SLCPSd s
evalStmt ctxt at env ss k =
  case ss of
    [] -> k $ SLRes mempty env $ SLV_Null at
    ((JSStatementBlock a ss' _ sp):ks) ->
      evalStmt ctxt at_in env ss'
      (\(SLRes lifts _ _XXX_v) ->
          evalStmt ctxt at_after env ks $ kKeepLifts lifts k)
      where at_in = srcloc_jsa "block" a at
            at_after = srcloc_after_semi "block" a sp at
    (s@(JSBreak a _ _):_) -> illegal a s "break"
    (s@(JSLet a _ _):_) -> illegal a s "let"
    (s@(JSClass a _ _ _ _ _ _):_) -> illegal a s "class"
    ((JSConstant a decls sp):ks) ->
      evalDecls ctxt at_in env decls k'
      where at_after = srcloc_after_semi lab a sp at
            at_in = srcloc_jsa lab a at
            lab = "const"
            k' (SLRes lifts env' _) = evalStmt ctxt at_after env' ks $ kKeepLifts lifts k
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
      evalStmt ctxt at_after env' ks k
      where clo = SLV_Clo at' (Just f) formals body env
            formals = parseJSFormals at' jsformals
            at' = srcloc_jsa lab a at
            at_after = srcloc_after_semi lab a sp at
            lab = "function def"
            env' = env_insert at f clo env
            f = case name of
                  JSIdentNone -> expect_throw at' (Err_TopFun_NoName)
                  JSIdentName _ x -> x
    (s@(JSGenerator a _ _ _ _ _ _ _):_) -> illegal a s "generator"
    ((JSIf a la ce ra ts):ks) ->
      evalStmt ctxt at env ((JSIfElse a la ce ra ts ea fs):ks) k
      where ea = ra
            fs = (JSEmptyStatement ea)
    ((JSIfElse a _ ce ta ts fa fs):ks) ->
      evalExpr ctxt at' env e' k'
      where e' = JSExpressionTernary ce a te a fe
            te = jsStmtToExpr ta ts
            fe = jsStmtToExpr fa fs
            k' (SLRes lifts _ _XXX_v) =
              --- XXX Should we ignore v? I don't think so, because we
              --- need to make its return matches ks
              evalStmt ctxt at' env ks $ kKeepLifts lifts k
            at' = srcloc_jsa "if" a at
    (s@(JSLabelled _ a _):_) -> illegal a s "labelled"
    ((JSEmptyStatement a):ks) ->
      evalStmt ctxt at' env ks k
      where at' = srcloc_jsa "empty" a at
    ((JSExpressionStatement e sp):ks) ->
      evalExpr ctxt at env e k'
      where at_after =
              srcloc_after_semi "expr stmt" JSNoAnnot sp at
            k' (SLRes elifts _ ev) =
              case (ctxt_mode ctxt, ev) of
                (SLC_Step penvs, SLV_Form (SLForm_Part_OnlyAns who penv' only_v)) ->
                  case typeOf at_after only_v of
                    (T_Null, _) ->
                      evalStmt ctxt' at_after env ks $ kKeepLifts elifts k
                      where ctxt' = ctxt { ctxt_mode = SLC_Step $ M.insert who penv' penvs }
                    _ -> expect_throw at (Err_Block_NotNull ev) --- XXX rename to expression not null? or ignore?
                (SLC_Step penvs, SLV_Form (SLForm_Part_ToConsensus who Nothing mmsg _XXX_mamt _XXX_mtime)) -> do
                  let penv = penvs M.! who
                  traceM $ "to_consensus from " ++ show who
                  --- XXX at and at_after here might be bad... add to ToConsensus?
                  (msg_env, _XXX_tmsg) <-
                    case mmsg of
                      Nothing -> return (mempty, [])
                      Just msg -> do
                        let mk var = do
                              let val = env_lookup at_after var penv
                              let (t, _) = typeOf at_after val
                              x <- ctxt_alloc ctxt at
                              return $ DLVar at_after "msg" t x
                        tvs <- mapM mk msg
                        return $ (foldl' (env_insertp at_after) mempty $ zip msg $ map SLV_DLVar tvs, tvs)
                  --- We go back to the original env from before the to-consensus step
                  let env' = env_merge at_after env msg_env
                  let penvs' = M.mapWithKey (\p old ->
                                                case p == who of
                                                  True -> old
                                                  False -> env_merge at_after old msg_env) penvs
                  let ctxt_cstep =
                        (SLCtxt { ctxt_mode = SLC_ConsensusStep penvs'
                                , ctxt_id = ctxt_id ctxt
                                , ctxt_stack = ctxt_stack ctxt })
                  --- XXX lift toconsensus
                  evalStmt ctxt_cstep at_after env' ks $ kKeepLifts elifts k
                (SLC_ConsensusStep penvs, SLV_Prim SLPrim_committed) -> do
                  let ctxt_step = (SLCtxt { ctxt_mode = SLC_Step penvs
                                          , ctxt_id = ctxt_id ctxt
                                          , ctxt_stack = ctxt_stack ctxt })
                  evalStmt ctxt_step at_after env ks $ kKeepLifts elifts k
                _ ->
                  case typeOf at_after ev of
                    (T_Null, _) -> evalStmt ctxt at_after env ks $ kKeepLifts elifts k
                    _ -> expect_throw at (Err_Block_NotNull ev) --- XXX rename to expression not null? or ignore?
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
      evalStmt ctxt at env ss' k
      where ss' = (JSExpressionStatement e' sp):ks
            e' = (JSCallExpression e a args ra)
    ((JSReturn a me sp):ks) ->
      expect_empty_tail lab a sp at ks res
      where lab = "return"
            at' = srcloc_jsa lab a at
            retk = expect_throw at' (Err_XXX "retk")
            res = case me of
                    Nothing -> retk $ SLV_Null at'
                    Just e -> evalExpr ctxt at' env e retk
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
  where illegal a s lab =
          expect_throw (srcloc_jsa lab a at) (Err_Block_IllegalJS s)

expect_empty_tail :: String -> JSAnnot -> JSSemi -> SrcLoc -> [JSStatement] -> a -> a
expect_empty_tail lab a sp at ks res =
  case ks of
    [] -> res
    _ ->
      expect_throw at' (Err_TailNotEmpty ks)
      where at' = srcloc_after_semi lab a sp at

evalTopBody :: SLCtxt s -> SrcLoc -> SLLibs -> SLEnv -> SLEnv -> [JSModuleItem] -> SLCPSd s
evalTopBody ctxt at libm env exenv body k =
  case body of
    [] -> k $ SLRes mempty exenv $ SLV_Null at
    mi:body' ->
      case mi of
        (JSModuleImportDeclaration _ im) ->
          case im of
            JSImportDeclarationBare a libn sp ->
              evalTopBody ctxt at_after libm env' exenv body' k
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
              evalStmt ctxt at' env [sm] $
              (\case
                  SLRes Seq.Empty env' (SLV_Null _) ->
                    let exenv' = case isExport of
                                   True ->
                                     --- If this is an exporting statement,
                                     --- then add to the export environment
                                     --- everything that is new.
                                     env_merge at' exenv (M.difference env' env)
                                   False ->
                                     exenv
                    in
                      evalTopBody ctxt at' libm env' exenv' body' k
                  SLRes _ _ v ->
                    expect_throw at' $ Err_Module_Return v)

type SLMod = (ReachSource, [JSModuleItem])
type SLLibs = (M.Map ReachSource SLEnv)

evalLib :: SLMod -> SLLibs -> ST s SLLibs
evalLib (src, body) libm = do
  let ctxt_top =
        (SLCtxt { ctxt_mode = SLC_Module
                , ctxt_id = Nothing
                , ctxt_stack = [] })
  (SLRes flifts exenv _) <- evalTopBody ctxt_top prev_at libm stdlib_env mt_env body' return
  case flifts == mempty of
    False -> impossible $ "evalLib had lifts"
    True -> return $ M.insert src exenv libm
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

evalLibs :: [SLMod] -> ST s SLLibs
evalLibs mods = foldrM evalLib mempty mods

makeInteract :: SrcLoc -> SLEnv -> SLVal
makeInteract at spec = SLV_Object at spec'
  where spec' = M.mapWithKey wrap_ty spec
        wrap_ty k (SLV_Type t) = SLV_Prim $ SLPrim_interact at k t
        wrap_ty _ v = expect_throw at $ Err_DApp_InvalidInteract v

compileDApp :: SLVal -> ST s SLRes
compileDApp topv =
  case topv of
    SLV_Prim (SLPrim_DApp_Delay at [ (SLV_Object _ _opts), (SLV_Array _ parts), clo ] top_env) -> do
      --- xxx look at opts
      idxr <- newSTRef $ 0
      let ctxt_step =
            (SLCtxt { ctxt_mode = SLC_Step penvs
                    , ctxt_id = Just idxr
                    , ctxt_stack = [] })
      evalApplyVals ctxt_step at' mt_env clo partvs k
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

compileBundle :: JSBundle -> SLVar -> SLRes
compileBundle (JSBundle mods) top = runST $ do
  libm <- evalLibs mods
  let exe_ex = libm M.! exe
  --- XXX use env_lookup
  let topv = case M.lookup top exe_ex of
               Just x -> x
               Nothing ->
                 expect_throw srcloc_top (Err_Eval_UnboundId top $ M.keys exe_ex)
  compileDApp topv
  where exe = case mods of
                [] -> impossible $ "compileBundle: no files"
                ((x,_):_) -> x

-- Main entry point
compileNL :: CompilerOpts -> IO ()
compileNL copts = do
  let out = output copts
  djp <- gatherDeps_top $ source copts
  let nlp = compileBundle djp "main"
  out "nl" $ show nlp
  return ()
