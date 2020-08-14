module Reach.Eval (EvalError, compileBundle) where

import Control.Monad
import Control.Monad.ST
import Data.Bits
import qualified Data.ByteString.Char8 as B
import Data.Foldable
import Data.List (intercalate, sortBy)
import qualified Data.Map.Strict as M
import Data.Ord
import qualified Data.Sequence as Seq
import Data.Version (Version (..), showVersion)
import GHC.Stack (HasCallStack)
import Generics.Deriving
import Language.JavaScript.Parser
import Language.JavaScript.Parser.AST
import Paths_reach (version)
import Reach.AST
import Reach.JSUtil
import Reach.Parser
import Reach.STCounter
import Reach.Type
import Reach.Util
import Safe (atMay)
import Text.EditDistance
import Text.ParserCombinators.Parsec.Number (numberValue)
---import Debug.Trace

compatibleVersion :: Version
compatibleVersion = Version (take 2 br) []
  where
    Version br _ = version

versionHeader :: String
versionHeader = "reach " ++ (showVersion compatibleVersion)

zipEq :: Show e => SrcLoc -> (Int -> Int -> e) -> [a] -> [b] -> [(a, b)]
zipEq at ce x y =
  if lx == ly
    then zip x y
    else expect_throw at (ce lx ly)
  where
    lx = length x
    ly = length y

data EvalError
  = Err_Apply_ArgCount SrcLoc Int Int
  | Err_Block_Assign JSAssignOp [JSStatement]
  | Err_Block_IllegalJS JSStatement
  | Err_Block_NotNull SLType SLVal
  | Err_Block_Variable
  | Err_Block_While
  | Err_CannotReturn
  | Err_ToConsensus_TimeoutArgs [JSExpression]
  | Err_App_InvalidInteract SLSVal
  | Err_App_InvalidPartSpec SLVal
  | Err_App_InvalidArgs [JSExpression]
  | Err_DeclLHS_IllegalJS JSExpression
  | Err_Decl_IllegalJS JSExpression
  | Err_Decl_NotRefable SLVal
  | Err_Decl_WrongArrayLength Int Int
  | Err_Dot_InvalidField SLVal [String] String
  | Err_Eval_ContinueNotInWhile
  | Err_Eval_ContinueNotLoopVariable SLVar
  | Err_Eval_IfCondNotBool SLVal
  | Err_Eval_IllegalMode SLMode String
  | Err_Eval_IllegalJS JSExpression
  | Err_Eval_NoReturn
  | Err_Eval_NotApplicable SLVal
  | Err_Eval_NotApplicableVals SLVal
  | Err_Eval_NotObject SLVal
  | Err_Eval_RefNotRefable SLVal
  | Err_Eval_RefNotInt SLVal
  | Err_Eval_IndirectRefNotArray SLVal
  | Err_Eval_RefOutOfBounds Int Integer
  | Err_Eval_UnboundId SLVar [SLVar]
  | Err_ExpectedPrivate SLVal
  | Err_ExpectedPublic SLVal
  | Err_Export_IllegalJS JSExportDeclaration
  | Err_Form_InvalidArgs SLForm Int [JSExpression]
  | Err_Fun_NamesIllegal
  | Err_Import_IllegalJS JSImportDeclaration
  | Err_Module_Return
  | Err_NoHeader [JSModuleItem]
  | Err_Obj_IllegalComputedField SLVal
  | Err_Obj_IllegalFieldValues [JSExpression]
  | Err_Obj_IllegalMethodDefinition JSObjectProperty
  | Err_Obj_IllegalNumberField JSPropertyName
  | Err_Obj_SpreadNotObj SLVal
  | Err_Prim_InvalidArgs SLPrimitive [SLVal]
  | Err_Shadowed SLVar
  | Err_TailNotEmpty [JSStatement]
  | Err_ToConsensus_Double ToConsensusMode
  | Err_TopFun_NoName
  | Err_Top_NotApp SLVal
  | Err_While_IllegalInvariant [JSExpression]
  | Err_Only_NotOneClosure SLVal
  | Err_Each_NotTuple SLVal
  | Err_Each_NotParticipant SLVal
  | Err_Transfer_NotBound SLPart
  | Err_Eval_IncompatibleStates SLState SLState
  deriving (Eq, Generic)

--- FIXME I think most of these things should be in Pretty

displaySlValType :: SLVal -> String
displaySlValType sv =
  case typeOfM (SrcLoc Nothing Nothing Nothing) sv of
    Just (t, _) -> displayTy t
    Nothing -> "<" <> conNameOf sv <> ">"

displayTyList :: [SLType] -> String
displayTyList tys =
  "[" <> (intercalate ", " $ map displayTy tys) <> "]"

displayTy :: SLType -> String
displayTy = \case
  T_Null -> "null"
  T_Bool -> "bool"
  T_UInt256 -> "uint256"
  T_Bytes -> "bytes"
  T_Address -> "address"
  T_Fun _tys _ty -> "function" -- "Fun(" <> displayTyList tys <> ", " <> displayTy ty
  T_Array _ty _sz -> "array" -- <> displayTyList tys
  T_Tuple _tys -> "tuple"
  T_Obj _m -> "object" -- FIXME
  T_Forall x ty {- SLVar SLType -} -> "Forall(" <> x <> ": " <> displayTy ty <> ")"
  T_Var x {- SLVar-} -> x

displaySecurityLevel :: SecurityLevel -> String
displaySecurityLevel Secret = "secret"
displaySecurityLevel Public = "public"

didYouMean :: String -> [String] -> Int -> String
didYouMean invalidStr validOptions maxClosest = case validOptions of
  [] -> ""
  _ -> ". Did you mean: " <> show closest
  where
    closest = take maxClosest $ sortBy (comparing distance) validOptions
    distance = restrictedDamerauLevenshteinDistance defaultEditCosts invalidStr

-- TODO more hints on why invalid syntax is invalid
instance Show EvalError where
  show = \case
    Err_Apply_ArgCount cloAt nFormals nArgs ->
      "Invalid function appication. Expected " <> show nFormals <> " args, got " <> show nArgs <> " for function defined at " <> show cloAt
    Err_Block_Assign _jsop _stmts ->
      "Invalid assignment" -- FIXME explain why
    Err_Block_IllegalJS _stmt ->
      "Invalid statement"
    Err_Block_NotNull ty _slval ->
      -- FIXME explain why null is expected
      "Invalid block result type. Expected Null, got " <> show ty
    Err_Block_Variable ->
      "Invalid `var` syntax. (Double check your syntax for while?)"
    Err_Block_While ->
      "Invalid `while` syntax"
    Err_CannotReturn ->
      "Invalid `return` syntax"
    Err_ToConsensus_TimeoutArgs _jes ->
      "Invalid Participant.timeout args"
    Err_App_InvalidInteract (secLev, val) ->
      "Invalid interact specification. Expected public type, got: "
        <> (displaySecurityLevel secLev <> " " <> displaySlValType val)
    Err_App_InvalidPartSpec _slval ->
      "Invalid participant spec"
    Err_App_InvalidArgs _jes ->
      "Invalid app arguments"
    Err_DeclLHS_IllegalJS _e ->
      "Invalid binding. Expressions cannot appear on the LHS."
    Err_Decl_IllegalJS e ->
      "Invalid Reach declaration: " <> conNameOf e
    Err_Decl_NotRefable slval ->
      "Invalid binding. Expected array or tuple, got: " <> displaySlValType slval
    Err_Decl_WrongArrayLength nIdents nVals ->
      "Invalid array binding. nIdents:" <> show nIdents <> " does not match nVals:" <> show nVals
    Err_Dot_InvalidField _slval ks k ->
      "Invalid field: " <> k <> didYouMean k ks 5
    Err_Eval_ContinueNotInWhile ->
      "Invalid continue. Expected to be inside of a while."
    Err_Eval_ContinueNotLoopVariable var ->
      "Invalid loop variable update. Expected loop variable, got: " <> var
    Err_Eval_IfCondNotBool slval ->
      "Invalid if statement. Expected if condition to be bool, got: " <> displaySlValType slval
    Err_Eval_IllegalMode mode s ->
      "Invalid operation. `" <> s <> "` cannot be used in context: " <> show mode
    Err_Eval_IllegalJS e ->
      "Invalid Reach expression syntax: " <> conNameOf e
    Err_Eval_NoReturn ->
      --- FIXME Is this syntactically possible?
      --- Answer: I think if you put a return at the top-level it will error.
      "Nowhere to return to"
    Err_Eval_NotApplicable slval ->
      "Invalid function application. Cannot apply: " <> displaySlValType slval
    Err_Eval_NotApplicableVals slval ->
      "Invalid function. Cannot apply: " <> displaySlValType slval
    Err_Eval_NotObject slval ->
      "Invalid field access. Expected object, got: " <> displaySlValType slval
    Err_Eval_RefNotRefable slval ->
      "Invalid element reference. Expected array or tuple, got: " <> displaySlValType slval
    Err_Eval_IndirectRefNotArray slval ->
      "Invalid indirect element reference. Expected array, got: " <> displaySlValType slval
    Err_Eval_RefNotInt slval ->
      "Invalid array index. Expected uint256, got: " <> displaySlValType slval
    Err_Eval_RefOutOfBounds maxi ix ->
      "Invalid array index. Expected (0 <= ix < " <> show maxi <> "), got " <> show ix
    Err_Eval_UnboundId slvar slvars ->
      "Invalid unbound identifier: " <> slvar <> didYouMean slvar slvars 5
    Err_ExpectedPrivate slval ->
      "Invalid declassify. Expected to declassify something private, "
        <> ("but this " <> displaySlValType slval <> " is public.")
    Err_ExpectedPublic slval ->
      "Invalid access of secret value (" <> displaySlValType slval <> ")"
    Err_Export_IllegalJS exportDecl ->
      "Invalid Reach export syntax: " <> conNameOf exportDecl
    Err_Form_InvalidArgs _SLForm n es ->
      "Invalid args. Expected " <> show n <> " but got " <> show (length es)
    Err_Fun_NamesIllegal ->
      "Invalid function expression. Anonymous functions must not be named."
    Err_Import_IllegalJS decl ->
      "Invalid Reach import syntax: " <> conNameOf decl
    Err_Module_Return ->
      "Invalid return statement. Cannot return at top level of module."
    Err_NoHeader _mis ->
      "Invalid Reach file. Expected header '" <> versionHeader <> "'; at top of file."
    Err_Obj_IllegalComputedField slval ->
      "Invalid computed field name. Fields must be bytes, but got: " <> displaySlValType slval
    Err_Obj_IllegalFieldValues exprs ->
      -- FIXME Is this syntactically possible?
      "Invalid field values. Expected 1 value, got: " <> show (length exprs)
    Err_Obj_IllegalMethodDefinition _prop ->
      "Invalid function field. Instead of {f() {...}}, write {f: () => {...}}"
    Err_Obj_IllegalNumberField _JSPropertyName ->
      "Invalid field name. Fields must be bytes, but got: uint256"
    Err_Obj_SpreadNotObj slval ->
      "Invalid object spread. Expected object, got: " <> displaySlValType slval
    Err_Prim_InvalidArgs prim slvals ->
      "Invalid args for " <> displayPrim prim <> ". got: "
        <> displayTyList (map (fst . typeOf noSrcLoc) slvals)
      where
        displayPrim = drop (length ("SLPrim_" :: String)) . conNameOf
        noSrcLoc = SrcLoc Nothing Nothing Nothing
    Err_Shadowed n ->
      -- FIXME tell the srcloc of the original binding
      "Invalid name shadowing. Cannot be rebound: " <> n
    Err_TailNotEmpty stmts ->
      "Invalid statement block. Expected empty tail, but found " <> found
      where
        found = show (length stmts) <> " more statements"
    Err_ToConsensus_Double mode -> case mode of
      TCM_Publish -> "Invalid double publish."
      _ -> "Invalid double toConsensus."
    Err_TopFun_NoName ->
      "Invalid function declaration. Top-level functions must be named."
    Err_Top_NotApp slval ->
      "Invalid compilation target. Expected App, but got " <> displaySlValType slval
    Err_While_IllegalInvariant exprs ->
      "Invalid while loop invariant. Expected 1 expr, but got " <> got
      where
        got = show $ length exprs
    Err_Only_NotOneClosure slval ->
      "PART.only not given a single closure as an argument, instead got " <> (displaySlValType slval)
    Err_Each_NotTuple slval ->
      "each not given a tuple as an argument, instead got " <> displaySlValType slval
    Err_Each_NotParticipant slval ->
      "each not given a participant as an argument, instead got " <> displaySlValType slval
    Err_Transfer_NotBound who ->
      "cannot transfer to unbound participant, " <> B.unpack who
    Err_Eval_IncompatibleStates x y ->
      "incompatible states: " <> show x <> " " <> show y

ensure_public :: SrcLoc -> SLSVal -> SLVal
ensure_public at (lvl, v) =
  case lvl of
    Public -> v
    Secret ->
      expect_throw at $ Err_ExpectedPublic v

ensure_publics :: SrcLoc -> [SLSVal] -> [SLVal]
ensure_publics at svs = map (ensure_public at) svs

lvlMeetR :: SecurityLevel -> SLComp s (SecurityLevel, a) -> SLComp s (SecurityLevel, a)
lvlMeetR lvl m = do
  SLRes lifts st v <- m
  return $ SLRes lifts st $ lvlMeet lvl v

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
    , --- Note: This identifier is chosen so that Reach programmers
      --- can't actually use it directly... kind of a hack. :(
      ("__txn.value__", SLV_Prim $ SLPrim_op $ TXN_VALUE)
    , ("balance", SLV_Prim $ SLPrim_op $ BALANCE)
    , ("Null", SLV_Type T_Null)
    , ("Bool", SLV_Type T_Bool)
    , ("UInt256", SLV_Type T_UInt256)
    , ("Bytes", SLV_Type T_Bytes)
    , ("Address", SLV_Type T_Address)
    , ("Array", SLV_Prim SLPrim_Array)
    , ("Tuple", SLV_Prim SLPrim_Tuple)
    , ("Object", SLV_Prim SLPrim_Object)
    , ("Fun", SLV_Prim SLPrim_Fun)
    , ("exit", SLV_Prim SLPrim_exit)
    , ("each", SLV_Form SLForm_each)
    , ( "Reach"
      , (SLV_Object srcloc_top $
           m_fromList_public
             [("App", SLV_Form SLForm_App)])
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

-- General compiler utilities
srcloc_after_semi :: String -> JSAnnot -> JSSemi -> SrcLoc -> SrcLoc
srcloc_after_semi lab a sp at =
  case sp of
    JSSemi x -> srcloc_jsa (alab ++ " semicolon") x at
    _ -> srcloc_jsa alab a at
  where
    alab = "after " ++ lab

checkResType :: SrcLoc -> SLType -> SLComp a SLSVal -> SLComp a DLArg
checkResType at et m = do
  SLRes lifts st (_lvl, v) <- m
  return $ SLRes lifts st $ checkType at et v

-- Compiler
data ReturnStyle
  = RS_ImplicitNull
  | RS_NeedExplicit
  | RS_CannotReturn
  | RS_MayBeEmpty
  deriving (Eq, Show)

--- A scope is local to a single function body (i.e. sequence of
--- statements), so it is reset every function call.
data SLScope = SLScope
  { sco_ret :: Maybe Int
  , sco_must_ret :: ReturnStyle
  , sco_env :: SLEnv
  , sco_while_vars :: Maybe (M.Map SLVar DLVar)
  , --- The participant environments are part of a scope, because
    --- learning something in one scope's "only" doesn't automatically
    --- populate it in another one's
    sco_penvs :: SLPartEnvs
  , --- Same with the consensus environment
    sco_cenv :: SLEnv
  }

sco_to_cloenv :: SLScope -> SLCloEnv
sco_to_cloenv SLScope {..} =
  SLCloEnv sco_env sco_penvs sco_cenv

sco_lookup_penv :: SLCtxt s -> SLScope -> SLPart -> SLEnv
sco_lookup_penv ctxt sco who =
  case M.lookup who $ sco_penvs sco of
    Nothing -> (ctxt_base_penvs ctxt) M.! who
    Just x -> x

penvs_update :: SLCtxt s -> SLScope -> (SLPart -> SLEnv -> SLEnv) -> SLPartEnvs
penvs_update ctxt sco f =
  M.fromList $
    map (\p -> (p, f p $ sco_lookup_penv ctxt sco p)) $
      M.keys $ ctxt_base_penvs ctxt

--- A context has global stuff (the variable counter) and abstracts
--- the control-flow state that leads to the expression, so it
--- inherits in a function call.
data SLCtxt s = SLCtxt
  { ctxt_id :: Maybe (STCounter s)
  , ctxt_stack :: [SLCtxtFrame]
  , ctxt_local_mname :: Maybe [SLVar]
  , ctxt_base_penvs :: SLPartEnvs
  }

instance Show (SLCtxt s) where
  show _ = "<context>"

data SLMode
  = --- The top-level of a module, before the App starts
    SLM_Module
  | --- The app starts in a "step"
    SLM_Step
  | --- An "only" moves from "step" to "local step" and then to "step" again, where x = live
    SLM_LocalStep
  | --- A "toconsensus" moves from "step" to "consensus step" then to "step" again
    SLM_ConsensusStep
  deriving (Eq, Generic, Show)

--- A state represents the state of the protocol, so it is returned
--- out of a function call.
data SLState = SLState
  { --- A function call may modify the mode
    st_mode :: SLMode
  , st_live :: Bool
  , --- A function call may cause a participant to join
    st_pdvs :: SLPartDVars
  }
  deriving (Eq, Show)

stMerge :: HasCallStack => SrcLoc -> SLState -> SLState -> SLState
stMerge at x y =
  case x == y of
    True -> y
    False -> expect_throw at $ Err_Eval_IncompatibleStates x y

stEnsureMode :: SrcLoc -> SLMode -> SLState -> SLState
stEnsureMode at slm st =
  stMerge at st $ st {st_mode = slm}

type SLPartDVars = M.Map SLPart DLVar

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
ctxt_alloc ctxt _at = do
  let idr = case ctxt_id ctxt of
        Just x -> x
        Nothing -> impossible $ "attempt to lift without id"
  incSTCounter idr

ctxt_lift_expr :: SLCtxt s -> SrcLoc -> (Int -> DLVar) -> DLExpr -> ST s (DLVar, DLStmts)
ctxt_lift_expr ctxt at mk_var e = do
  x <- ctxt_alloc ctxt at
  let dv = mk_var x
  let s = DLS_Let at dv e
  return (dv, return s)

data SLRes a = SLRes DLStmts SLState a

keepLifts :: DLStmts -> SLComp s a -> SLComp s a
keepLifts lifts m = do
  SLRes lifts' st r <- m
  return $ SLRes (lifts <> lifts') st r

cannotLift :: String -> SLRes a -> a
cannotLift what (SLRes lifts _ ans) =
  case lifts == mempty of
    False -> impossible $ what <> " had lifts"
    True -> ans

type SLComp s a = ST s (SLRes a)

data SLExits
  = NeverExits
  | AlwaysExits
  | MayExit
  deriving (Eq, Show)

data SLStmtRes = SLStmtRes SLEnv [(SrcLoc, SLSVal)]

data SLAppRes = SLAppRes SLEnv SLSVal

ctxt_stack_push :: SLCtxt s -> SLCtxtFrame -> SLCtxt s
ctxt_stack_push ctxt f =
  (ctxt {ctxt_stack = f : (ctxt_stack ctxt)})

binaryToPrim :: SrcLoc -> SLEnv -> JSBinOp -> SLVal
binaryToPrim at env o =
  case o of
    JSBinOpAnd a -> fun a "and"
    JSBinOpDivide a -> prim a (DIV)
    JSBinOpEq a -> prim a (PEQ)
    JSBinOpGe a -> prim a (PGE)
    JSBinOpGt a -> prim a (PGT)
    JSBinOpLe a -> prim a (PLE)
    JSBinOpLt a -> prim a (PLT)
    JSBinOpMinus a -> prim a (SUB)
    JSBinOpMod a -> prim a (MOD)
    JSBinOpNeq a -> fun a "neq"
    JSBinOpOr a -> fun a "or"
    JSBinOpPlus a -> prim a (ADD)
    JSBinOpStrictEq a -> prim a (BYTES_EQ)
    JSBinOpStrictNeq a -> fun a "bytes_neq"
    JSBinOpTimes a -> prim a (MUL)
    JSBinOpLsh a -> prim a (LSH)
    JSBinOpRsh a -> prim a (RSH)
    JSBinOpBitAnd a -> prim a (BAND)
    JSBinOpBitOr a -> prim a (BIOR)
    JSBinOpBitXor a -> prim a (BXOR)
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

infectWithId :: SLVar -> SLSVal -> SLSVal
infectWithId v (lvl, sv) = (lvl, sv')
  where
    sv' =
      case sv of
        SLV_Participant at who io _ mdv ->
          SLV_Participant at who io (Just v) mdv
        _ -> sv

evalDot :: SLCtxt s -> SrcLoc -> SLState -> SLVal -> String -> SLComp s SLSVal
evalDot ctxt at st obj field =
  case obj of
    SLV_Object _ env ->
      case M.lookup field env of
        Just v -> retV $ v
        Nothing -> illegal_field (M.keys env)
    SLV_DLVar obj_dv@(DLVar _ _ (T_Obj tm) _) ->
      retDLVar tm (DLA_Var obj_dv) Public
    SLV_Prim (SLPrim_interact _ who m it@(T_Obj tm)) ->
      retDLVar tm (DLA_Interact who m it) Secret
    SLV_Participant _ who _ vas _ ->
      case field of
        "only" -> retV $ public $ SLV_Form (SLForm_Part_Only who)
        "publish" -> retV $ public $ SLV_Form (SLForm_Part_ToConsensus at who vas (Just TCM_Publish) Nothing Nothing Nothing)
        "pay" -> retV $ public $ SLV_Form (SLForm_Part_ToConsensus at who vas (Just TCM_Pay) Nothing Nothing Nothing)
        _ -> illegal_field ["only", "publish", "pay"]
    SLV_Form (SLForm_Part_ToConsensus to_at who vas Nothing mpub mpay mtime) ->
      case field of
        "publish" -> retV $ public $ SLV_Form (SLForm_Part_ToConsensus to_at who vas (Just TCM_Publish) mpub mpay mtime)
        "pay" -> retV $ public $ SLV_Form (SLForm_Part_ToConsensus to_at who vas (Just TCM_Pay) mpub mpay mtime)
        "timeout" -> retV $ public $ SLV_Form (SLForm_Part_ToConsensus to_at who vas (Just TCM_Timeout) mpub mpay mtime)
        _ -> illegal_field ["publish", "pay", "timeout"]
    v ->
      expect_throw at (Err_Eval_NotObject v)
  where
    retDLVar tm obj_dla slvl =
      case M.lookup field tm of
        Nothing -> illegal_field (M.keys tm)
        Just t -> do
          (dv, lifts') <- ctxt_lift_expr ctxt at (DLVar at (ctxt_local_name ctxt "object ref") t) (DLE_ObjectRef at obj_dla field)
          let ansv = SLV_DLVar dv
          return $ SLRes lifts' st (slvl, ansv)
    retV sv = return $ SLRes mempty st sv
    illegal_field ks =
      expect_throw at (Err_Dot_InvalidField obj ks field)

evalForm :: SLCtxt s -> SrcLoc -> SLScope -> SLState -> SLForm -> [JSExpression] -> SLComp s SLSVal
evalForm ctxt at sco st f args =
  case f of
    SLForm_App ->
      case args of
        [opte, partse, JSArrowExpression top_formals _ top_s] -> do
          sargs <- cannotLift "App args" <$> evalExprs ctxt at sco st [opte, partse]
          case map snd sargs of
            [(SLV_Object _ opts), (SLV_Tuple _ parts)] ->
              retV $ public $ SLV_Prim $ SLPrim_App_Delay at opts part_vs (jsStmtToBlock top_s) env'
              where
                env = sco_env sco
                env' = foldl' (\env_ (part_var, part_val) -> env_insert at part_var part_val env_) env $ zipEq at (Err_Apply_ArgCount at) top_args part_vs
                top_args = parseJSArrowFormals at top_formals
                part_vs = map make_part parts
                make_part v =
                  case v of
                    SLV_Tuple p_at [SLV_Bytes _ bs, SLV_Object iat io] ->
                      public $ SLV_Participant p_at bs (makeInteract iat bs io) Nothing Nothing
                    _ -> expect_throw at (Err_App_InvalidPartSpec v)
            _ -> expect_throw at (Err_App_InvalidArgs args)
        _ -> expect_throw at (Err_App_InvalidArgs args)
    SLForm_Part_Only who ->
      return $ SLRes mempty st $ public $ SLV_Form $ SLForm_EachAns [who] at (sco_to_cloenv sco) one_arg
    SLForm_Part_ToConsensus to_at who vas mmode mpub mpay mtime ->
      case mmode of
        Just TCM_Publish ->
          case mpub of
            Nothing -> retV $ public $ SLV_Form $ SLForm_Part_ToConsensus to_at who vas Nothing (Just msg) mpay mtime
              where
                msg = map (jse_expect_id at) args
            Just _ ->
              expect_throw at $ Err_ToConsensus_Double TCM_Publish
        Just TCM_Pay ->
          retV $ public $ SLV_Form $ SLForm_Part_ToConsensus to_at who vas Nothing mpub (Just one_arg) mtime
        Just TCM_Timeout ->
          case args of
            [de, JSArrowExpression (JSParenthesizedArrowParameterList _ JSLNil _) _ dt_s] ->
              retV $ public $ SLV_Form $ SLForm_Part_ToConsensus to_at who vas Nothing mpub mpay (Just (at, de, (jsStmtToBlock dt_s)))
            _ -> expect_throw at $ Err_ToConsensus_TimeoutArgs args
        Nothing ->
          expect_throw at $ Err_Eval_NotApplicable rator
    SLForm_each -> do
      let (partse, thunke) = two_args
      SLRes part_lifts part_st (_, parts_v) <- evalExpr ctxt at sco st partse
      case parts_v of
        SLV_Tuple _ part_vs -> do
          let parts =
                map
                  (\case
                     SLV_Participant _ who _ _ _ -> who
                     v -> expect_throw at $ Err_Each_NotParticipant v)
                  part_vs
          return $ SLRes part_lifts part_st $ public $ SLV_Form $ SLForm_EachAns parts at (sco_to_cloenv sco) thunke
        _ ->
          expect_throw at $ Err_Each_NotTuple parts_v
    SLForm_EachAns {} -> impossible "SLForm_Part_OnlyAns"
  where
    illegal_args n = expect_throw at (Err_Form_InvalidArgs f n args)
    rator = SLV_Form f
    retV v = return $ SLRes mempty st v
    one_arg = case args of
      [x] -> x
      _ -> illegal_args 1
    two_args = case args of
      [x, y] -> (x, y)
      _ -> illegal_args 2

evalPrimOp :: SLCtxt s -> SrcLoc -> SLScope -> SLState -> PrimOp -> [SLSVal] -> SLComp s SLSVal
evalPrimOp ctxt at _sco st p sargs =
  case p of
    --- FIXME These should be sensitive to bit widths
    ADD -> nn2n (+)
    SUB -> nn2n (-)
    MUL -> nn2n (*)
    -- FIXME fromIntegral may overflow the Int
    LSH -> nn2n (\a b -> shift a (fromIntegral b))
    RSH -> nn2n (\a b -> shift a (fromIntegral $ b * (-1)))
    BAND -> nn2n (.&.)
    BIOR -> nn2n (.|.)
    BXOR -> nn2n (xor)
    PLT -> nn2b (<)
    PLE -> nn2b (<=)
    PEQ -> nn2b (==)
    PGE -> nn2b (>=)
    PGT -> nn2b (>)
    _ -> make_var
  where
    args = map snd sargs
    lvl = mconcat $ map fst sargs
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
    static v = return $ SLRes mempty st (lvl, v)
    make_var = do
      let (rng, dargs) = checkAndConvert at (primOpType p) args
      (dv, lifts) <- ctxt_lift_expr ctxt at (DLVar at (ctxt_local_name ctxt "prim") rng) (DLE_PrimOp at p dargs)
      return $ SLRes lifts st $ (lvl, SLV_DLVar dv)

evalPrim :: SLCtxt s -> SrcLoc -> SLScope -> SLState -> SLPrimitive -> [SLSVal] -> SLComp s SLSVal
evalPrim ctxt at sco st p sargs =
  case p of
    SLPrim_op op ->
      evalPrimOp ctxt at sco st op sargs
    SLPrim_Fun ->
      case map snd sargs of
        [(SLV_Tuple _ dom_arr), (SLV_Type rng)] ->
          retV $ (lvl, SLV_Type $ T_Fun dom rng)
          where
            lvl = mconcat $ map fst sargs
            dom = map expect_ty dom_arr
        _ -> illegal_args
    SLPrim_Array ->
      case map snd sargs of
        [(SLV_Type ty), (SLV_Int _ sz)] ->
          retV $ (lvl, SLV_Type $ T_Array ty sz)
        _ -> illegal_args
      where
        lvl = mconcat $ map fst sargs
    SLPrim_Tuple ->
      retV $ (lvl, SLV_Type $ T_Tuple $ map expect_ty $ map snd sargs)
      where
        lvl = mconcat $ map fst sargs
    SLPrim_Object ->
      case sargs of
        [(lvl, SLV_Object _ objm)] ->
          retV $ (lvl, SLV_Type $ T_Obj $ M.map (expect_ty . snd) objm)
        _ -> illegal_args
    SLPrim_makeEnum ->
      case sargs of
        [(ilvl, SLV_Int _ i)] ->
          retV $ (ilvl, SLV_Tuple at' (enum_pred : map (SLV_Int at') [0 .. (i -1)]))
          where
            at' = (srcloc_at "makeEnum" Nothing at)
            --- FIXME This sucks... maybe parse an embed string? Would that suck less?... probably want a custom primitive
            --- FIXME also, env is a weird choice here... really want stdlib_env
            enum_pred = SLV_Clo at' fname ["x"] pbody (sco_to_cloenv sco)
            fname = Just $ ctxt_local_name ctxt "makeEnum"
            pbody = JSBlock JSNoAnnot [(JSReturn JSNoAnnot (Just (JSExpressionBinary lhs (JSBinOpAnd JSNoAnnot) rhs)) JSSemiAuto)] JSNoAnnot
            lhs = (JSExpressionBinary (JSDecimal JSNoAnnot "0") (JSBinOpLe JSNoAnnot) (JSIdentifier JSNoAnnot "x"))
            rhs = (JSExpressionBinary (JSIdentifier JSNoAnnot "x") (JSBinOpLt JSNoAnnot) (JSDecimal JSNoAnnot (show i)))
        _ -> illegal_args
    SLPrim_App_Delay {} ->
      expect_throw at (Err_Eval_NotApplicable rator)
    SLPrim_interact _iat who m t ->
      case st_mode st of
        SLM_LocalStep -> do
          let (rng, dargs) = checkAndConvert at t $ map snd sargs
          (dv, lifts) <- ctxt_lift_expr ctxt at (DLVar at (ctxt_local_name ctxt "interact") rng) (DLE_Interact at who m rng dargs)
          return $ SLRes lifts st $ secret $ SLV_DLVar dv
        cm ->
          expect_throw at (Err_Eval_IllegalMode cm "interact")
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
      return $ SLRes lifts st $ (lvl, SLV_DLVar dv)
    SLPrim_claim ct ->
      return $ SLRes lifts st $ public $ SLV_Null at "claim"
      where
        darg = case map snd sargs of
          [arg] -> checkType at T_Bool arg
          _ -> illegal_args
        lifts = return $ DLS_Claim at (ctxt_stack ctxt) ct darg
    SLPrim_transfer ->
      case map (typeOf at) $ ensure_publics at sargs of
        [(T_UInt256, amt_dla)] ->
          return $ SLRes mempty st $ public $ SLV_Object at $ M.fromList [("to", (Public, SLV_Prim (SLPrim_transfer_amt_to amt_dla)))]
        _ -> illegal_args
    SLPrim_transfer_amt_to amt_dla ->
      case st_mode st of
        SLM_ConsensusStep ->
          return $ SLRes lifts st $ public $ SLV_Null at "transfer.to"
          where
            lifts = return $ DLS_Transfer at (ctxt_stack ctxt) who_dla amt_dla
            who_dla =
              case map snd sargs of
                [SLV_Participant _ who _ _ Nothing] ->
                  case M.lookup who $ st_pdvs st of
                    Just dv -> convert $ SLV_DLVar dv
                    Nothing -> expect_throw at $ Err_Transfer_NotBound who
                [one] -> convert one
                _ -> illegal_args
            convert = checkType at T_Address
        cm -> expect_throw at $ Err_Eval_IllegalMode cm "transfer.to"
    SLPrim_exit ->
      case st_mode st of
        SLM_Step ->
          case sargs of
            [] ->
              return $ SLRes lifts st $ public $ SLV_Prim $ SLPrim_exitted
              where
                lifts = return $ DLS_Stop at (ctxt_stack ctxt)
            _ -> illegal_args
        cm -> expect_throw at $ Err_Eval_IllegalMode cm "exit"
    SLPrim_exitted -> illegal_args
  where
    illegal_args = expect_throw at (Err_Prim_InvalidArgs p $ map snd sargs)
    retV v = return $ SLRes mempty st v
    rator = SLV_Prim p
    expect_ty v =
      case v of
        SLV_Type t -> t
        _ -> illegal_args

evalApplyVals :: SLCtxt s -> SrcLoc -> SLScope -> SLState -> SLVal -> [SLSVal] -> SLComp s SLAppRes
evalApplyVals ctxt at sco st rator randvs =
  case rator of
    SLV_Prim p -> do
      SLRes lifts st' val <- evalPrim ctxt at sco st p randvs
      return $ SLRes lifts st' $ SLAppRes (sco_env sco) val
    SLV_Clo clo_at mname formals (JSBlock body_a body _) (SLCloEnv clo_env clo_penvs clo_cenv) -> do
      ret <- ctxt_alloc ctxt at
      let body_at = srcloc_jsa "block" body_a clo_at
      let kvs = zipEq at (Err_Apply_ArgCount clo_at) formals randvs
      let clo_env' = foldl' (env_insertp clo_at) clo_env kvs
      let ctxt' = ctxt_stack_push ctxt (SLC_CloApp at clo_at mname)
      let clo_sco =
            (SLScope
               { sco_ret = Just ret
               , sco_must_ret = RS_ImplicitNull
               , sco_env = clo_env'
               , sco_while_vars = Nothing
               , sco_penvs = clo_penvs
               , sco_cenv = clo_cenv
               })
      SLRes body_lifts body_st (SLStmtRes clo_env'' rs) <-
        evalStmt ctxt' body_at clo_sco st body
      let no_prompt (lvl, v) = do
            let lifts' =
                  case body_lifts of
                    body_lifts' Seq.:|> (DLS_Return _ the_ret_label _the_val)
                      | the_ret_label == ret ->
                        --- We don't check that the_val is v, because
                        --- we're relying on the invariant that there
                        --- was only one Return... this should be
                        --- true, but if something changes in the
                        --- future, this might be a place that an
                        --- error could be introduced.
                        body_lifts'
                    _ ->
                      return $ DLS_Prompt body_at (Left ret) body_lifts
            return $ SLRes lifts' body_st $ SLAppRes clo_env'' $ (lvl, v)
      case rs of
        [] -> no_prompt $ public $ SLV_Null body_at "clo app"
        [(_, x)] -> no_prompt $ x
        _ -> do
          --- FIXME if all the values are actually the same, then we can treat this as a noprompt
          let r_ty = typeMeets body_at $ map (\(r_at, (_r_lvl, r_sv)) -> (r_at, (fst (typeOf r_at r_sv)))) rs
          let lvl = mconcat $ map fst $ map snd rs
          let dv = DLVar body_at (ctxt_local_name ctxt "clo app") r_ty ret
          let lifts' = return $ DLS_Prompt body_at (Right dv) body_lifts
          return $ SLRes lifts' body_st $ SLAppRes clo_env'' (lvl, (SLV_DLVar dv))
    v ->
      expect_throw at (Err_Eval_NotApplicableVals v)

evalApply :: SLCtxt s -> SrcLoc -> SLScope -> SLState -> SLVal -> [JSExpression] -> SLComp s SLSVal
evalApply ctxt at sco st rator rands =
  case rator of
    SLV_Prim _ -> vals
    SLV_Clo _ _ _ _ _ -> vals
    SLV_Form f -> evalForm ctxt at sco st f rands
    v -> expect_throw at (Err_Eval_NotApplicable v)
  where
    vals = do
      SLRes rlifts st_rands randsvs <- evalExprs ctxt at sco st rands
      SLRes alifts st_res (SLAppRes _ r) <-
        evalApplyVals ctxt at sco st_rands rator randsvs
      return $ SLRes (rlifts <> alifts) st_res r

evalPropertyName :: SLCtxt s -> SrcLoc -> SLScope -> SLState -> JSPropertyName -> SLComp s (SecurityLevel, String)
evalPropertyName ctxt at sco st pn =
  case pn of
    JSPropertyIdent _ s -> k_res $ public $ s
    JSPropertyString _ s -> k_res $ public $ trimQuotes s
    JSPropertyNumber an _ ->
      expect_throw at_n (Err_Obj_IllegalNumberField pn)
      where
        at_n = srcloc_jsa "number" an at
    JSPropertyComputed an e _ -> do
      let at_n = srcloc_jsa "computed field name" an at
      SLRes elifts st_e (elvl, ev) <- evalExpr ctxt at_n sco st e
      keepLifts elifts $
        case ev of
          SLV_Bytes _ fb ->
            return $ SLRes mempty st_e $ (elvl, B.unpack fb)
          _ ->
            expect_throw at_n $ Err_Obj_IllegalComputedField ev
  where
    k_res s = return $ SLRes mempty st s

evalPropertyPair :: SLCtxt s -> SrcLoc -> SLScope -> SLState -> SLEnv -> JSObjectProperty -> SLComp s (SecurityLevel, SLEnv)
evalPropertyPair ctxt at sco st fenv p =
  case p of
    JSPropertyNameandValue pn a vs -> do
      let at' = srcloc_jsa "property binding" a at
      SLRes flifts st_name (flvl, f) <- evalPropertyName ctxt at' sco st pn
      keepLifts flifts $
        case vs of
          [e] -> do
            SLRes vlifts st_sv sv <- evalExpr ctxt at' sco st_name e
            return $ SLRes vlifts st_sv $ (flvl, env_insert at' f sv fenv)
          _ -> expect_throw at' (Err_Obj_IllegalFieldValues vs)
    JSPropertyIdentRef a v ->
      evalPropertyPair ctxt at sco st fenv p'
      where
        p' = JSPropertyNameandValue pn a vs
        pn = JSPropertyIdent a v
        vs = [JSIdentifier a v]
    JSObjectSpread a se -> do
      let at' = srcloc_jsa "...obj" a at
      SLRes slifts st_se (slvl, sv) <- evalExpr ctxt at' sco st se
      keepLifts slifts $
        case sv of
          SLV_Object _ senv ->
            return $ SLRes mempty st_se $ (slvl, env_merge at' fenv senv)
          _ -> expect_throw at (Err_Obj_SpreadNotObj sv)
    JSObjectMethod {} ->
      expect_throw at (Err_Obj_IllegalMethodDefinition p)

evalExpr :: SLCtxt s -> SrcLoc -> SLScope -> SLState -> JSExpression -> SLComp s SLSVal
evalExpr ctxt at sco st e = do
  let env = sco_env sco
  case e of
    JSIdentifier a x ->
      retV $ infectWithId x $ env_lookup (srcloc_jsa "id ref" a at) x env
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
      SLRes lifts st' svs <- evalExprs ctxt at' sco st (jsa_flatten as)
      let vs = map snd svs
      let lvl = mconcat $ map fst svs
      return $ SLRes lifts st' $ (lvl, SLV_Tuple at' vs)
      where
        at' = (srcloc_jsa "tuple" a at)
    JSAssignExpression _ _ _ -> illegal
    JSAwaitExpression _ _ -> illegal
    JSCallExpression rator a rands _ -> doCall rator a $ jscl_flatten rands
    JSCallExpressionDot obj a field -> doDot obj a field
    JSCallExpressionSquare arr a idx _ -> doRef arr a idx
    JSClassExpression _ _ _ _ _ _ -> illegal
    JSCommaExpression _ _ _ -> illegal
    JSExpressionBinary lhs op rhs ->
      doCallV st (binaryToPrim at env op) JSNoAnnot [lhs, rhs]
    JSExpressionParen a ie _ ->
      evalExpr ctxt (srcloc_jsa "paren" a at) sco st ie
    JSExpressionPostfix _ _ -> illegal
    JSExpressionTernary ce a te fa fe -> do
      let at' = srcloc_jsa "?:" a at
      let t_at' = srcloc_jsa "?: > true" a at'
      let f_at' = srcloc_jsa "?: > false" fa t_at'
      SLRes clifts st_c csv@(clvl, cv) <- evalExpr ctxt at' sco st ce
      keepLifts clifts $
        case cv of
          SLV_Bool _ cb ->
            lvlMeetR clvl $ evalExpr ctxt n_at' sco st_c ne
            where
              (n_at', ne) = case cb of
                True -> (t_at', te)
                False -> (f_at', fe)
          SLV_DLVar cond_dv@(DLVar _ _ T_Bool _) -> do
            SLRes tlifts st_t tsv@(tlvl, tv) <- evalExpr ctxt t_at' sco st_c te
            SLRes flifts st_f fsv@(flvl, fv) <- evalExpr ctxt f_at' sco st_c fe
            let lvl = clvl <> tlvl <> flvl
            let st_tf = stMerge at st_t st_f
            case stmts_pure tlifts && stmts_pure flifts of
              True ->
                keepLifts (tlifts <> flifts) $
                  lvlMeetR lvl $
                    evalPrim ctxt at sco st_tf (SLPrim_op $ IF_THEN_ELSE) [csv, tsv, fsv]
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
                return $ SLRes lifts' st_tf $ (lvl, SLV_DLVar ans_dv)
          _ ->
            expect_throw at (Err_Eval_IfCondNotBool cv)
    JSArrowExpression aformals a bodys ->
      retV $ public $ SLV_Clo at' fname formals body (sco_to_cloenv sco)
      where
        at' = srcloc_jsa "arrow" a at
        fname = Just $ ctxt_local_name ctxt "arrow"
        body = jsStmtToBlock bodys
        formals = parseJSArrowFormals at' aformals
    JSFunctionExpression a name _ jsformals _ body ->
      retV $ public $ SLV_Clo at' fname formals body (sco_to_cloenv sco)
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
      SLRes olifts st_fin (lvl, fenv) <-
        foldlM f (SLRes mempty st (mempty, mempty)) $ jsctl_flatten plist
      return $ SLRes olifts st_fin $ (lvl, SLV_Object at' fenv)
      where
        at' = srcloc_jsa "obj" a at
        f (SLRes lifts st_f (lvl, oenv)) pp =
          keepLifts lifts $ lvlMeetR lvl $ evalPropertyPair ctxt at' sco st_f oenv pp
    JSSpreadExpression _ _ -> illegal
    JSTemplateLiteral _ _ _ _ -> illegal
    JSUnaryExpression op ue -> doCallV st (unaryToPrim at env op) JSNoAnnot [ue]
    JSVarInitExpression _ _ -> illegal
    JSYieldExpression _ _ -> illegal
    JSYieldFromExpression _ _ _ -> illegal
  where
    illegal = expect_throw at (Err_Eval_IllegalJS e)
    retV v = return $ SLRes mempty st $ v
    doCallV st_rator ratorv a rands =
      evalApply ctxt at' sco st_rator ratorv rands
      where
        at' = srcloc_jsa "application" a at
    doCall rator a rands = do
      let at' = srcloc_jsa "application, rator" a at
      SLRes rlifts st_rator (rator_lvl, ratorv) <-
        evalExpr ctxt at' sco st rator
      keepLifts rlifts $ lvlMeetR rator_lvl $ doCallV st_rator ratorv a rands
    doDot obj a field = do
      let at' = srcloc_jsa "dot" a at
      SLRes olifts obj_st (obj_lvl, objv) <- evalExpr ctxt at' sco st obj
      let fields = (jse_expect_id at') field
      SLRes reflifts ref_st refsv <- evalDot ctxt at' obj_st objv fields
      return $ SLRes (olifts <> reflifts) ref_st $ lvlMeet obj_lvl $ refsv
    doRef arr a idxe = do
      let at' = srcloc_jsa "array ref" a at
      SLRes alifts arr_st (arr_lvl, arrv) <- evalExpr ctxt at' sco st arr
      SLRes ilifts idx_st (idx_lvl, idxv) <- evalExpr ctxt at' sco arr_st idxe
      let lvl = arr_lvl <> idx_lvl
      let retRef t de = do
            (dv, lifts') <- ctxt_lift_expr ctxt at' (DLVar at' (ctxt_local_name ctxt "ref") t) de
            let ansv = SLV_DLVar dv
            return $ SLRes (alifts <> ilifts <> lifts') idx_st (lvl, ansv)
      let retArrayRef t sz arr_dla idx_dla =
            retRef t $ DLE_ArrayRef at' (ctxt_stack ctxt) arr_dla sz idx_dla
      let retTupleRef t arr_dla idx =
            retRef t $ DLE_TupleRef at' arr_dla idx
      let retVal idxi arrvs =
            case fromIntegerMay idxi >>= atMay arrvs of
              Nothing ->
                expect_throw at' $ Err_Eval_RefOutOfBounds (length arrvs) idxi
              Just ansv ->
                return $ SLRes (alifts <> ilifts) idx_st (lvl, ansv)
      case idxv of
        SLV_Int _ idxi ->
          case arrv of
            SLV_Tuple _ tupvs -> retVal idxi tupvs
            SLV_DLVar adv@(DLVar _ _ (T_Tuple ts) _) ->
              case fromIntegerMay idxi >>= atMay ts of
                Nothing ->
                  expect_throw at' $ Err_Eval_RefOutOfBounds (length ts) idxi
                Just t -> retTupleRef t arr_dla idxi
                  where
                    arr_dla = DLA_Var adv
            SLV_DLVar adv@(DLVar _ _ (T_Array t sz) _) ->
              case idxi < sz of
                False ->
                  expect_throw at' $ Err_Eval_RefOutOfBounds (fromIntegral sz) idxi
                True -> retArrayRef t sz arr_dla idx_dla
                  where
                    arr_dla = DLA_Var adv
                    idx_dla = DLA_Con (DLC_Int idxi)
            _ ->
              expect_throw at' $ Err_Eval_RefNotRefable arrv
        SLV_DLVar idxdv@(DLVar _ _ T_UInt256 _) ->
          case arr_ty of
            T_Array elem_ty sz ->
              retArrayRef elem_ty sz arr_dla idx_dla
              where
                idx_dla = DLA_Var idxdv
            _ ->
              expect_throw at' $ Err_Eval_IndirectRefNotArray arrv
          where
            (arr_ty, arr_dla) = typeOf at' arrv
        _ ->
          expect_throw at' $ Err_Eval_RefNotInt idxv

evalExprs :: SLCtxt s -> SrcLoc -> SLScope -> SLState -> [JSExpression] -> SLComp s [SLSVal]
evalExprs ctxt at sco st rands =
  case rands of
    [] -> return $ SLRes mempty st []
    (rand0 : randN) -> do
      SLRes lifts0 st0 sval0 <- evalExpr ctxt at sco st rand0
      SLRes liftsN stN svalN <- evalExprs ctxt at sco st0 randN
      return $ SLRes (lifts0 <> liftsN) stN (sval0 : svalN)

evalDecl :: SLCtxt s -> SrcLoc -> SLState -> SLEnv -> SLScope -> JSExpression -> SLComp s SLEnv
evalDecl ctxt at st lhs_env rhs_sco decl =
  case decl of
    JSVarInitExpression lhs (JSVarInit va rhs) -> do
      let vat' = srcloc_jsa "var initializer" va at
      (lhs_ns, make_env) <-
        case lhs of
          (JSIdentifier a x) -> do
            let _make_env v = return (mempty, env_insert (srcloc_jsa "id" a at) x v lhs_env)
            return ([x], _make_env)
          --- FIXME Support object literal format
          (JSArrayLiteral a xs _) -> do
            let at' = srcloc_jsa "array" a at
            --- FIXME Support spreads in array literals
            let ks = map (jse_expect_id at') $ jsa_flatten xs
            let _make_env (lvl, v) = do
                  (vs_lifts, vs) <-
                    case v of
                      SLV_Tuple _ x -> return (mempty, x)
                      SLV_DLVar dv@(DLVar _ _ (T_Tuple ts) _) -> do
                        vs_liftsl_and_dvs <- zipWithM mk_ref ts [0 ..]
                        let (vs_liftsl, dvs) = unzip vs_liftsl_and_dvs
                        let vs_lifts = mconcat vs_liftsl
                        return (vs_lifts, dvs)
                        where
                          mk_ref t i = do
                            let e = (DLE_TupleRef vat' (DLA_Var dv) i)
                            (dvi, i_lifts) <- ctxt_lift_expr ctxt at (DLVar vat' (ctxt_local_name ctxt "tuple idx") t) e
                            return $ (i_lifts, SLV_DLVar dvi)
                      SLV_DLVar dv@(DLVar _ _ (T_Array t sz) _) -> do
                        vs_liftsl_and_dvs <- mapM mk_ref [0 .. (sz - 1)]
                        let (vs_liftsl, dvs) = unzip vs_liftsl_and_dvs
                        let vs_lifts = mconcat vs_liftsl
                        return (vs_lifts, dvs)
                        where
                          mk_ref i = do
                            let e = (DLE_ArrayRef vat' (ctxt_stack ctxt) (DLA_Var dv) sz (DLA_Con (DLC_Int i)))
                            (dvi, i_lifts) <- ctxt_lift_expr ctxt at (DLVar vat' (ctxt_local_name ctxt "array idx") t) e
                            return $ (i_lifts, SLV_DLVar dvi)
                      _ ->
                        expect_throw at' (Err_Decl_NotRefable v)
                  let kvs = zipEq at' Err_Decl_WrongArrayLength ks $ map (\x -> (lvl, x)) vs
                  return $ (vs_lifts, foldl' (env_insertp at') lhs_env kvs)
            return (ks, _make_env)
          _ ->
            expect_throw at (Err_DeclLHS_IllegalJS lhs)
      let ctxt' = ctxt_local_name_set ctxt lhs_ns
      SLRes rhs_lifts rhs_st v <- evalExpr ctxt' vat' rhs_sco st rhs
      (lhs_lifts, lhs_env') <- make_env v
      return $ SLRes (rhs_lifts <> lhs_lifts) rhs_st lhs_env'
    _ ->
      expect_throw at (Err_Decl_IllegalJS decl)

evalDecls :: SLCtxt s -> SrcLoc -> SLState -> SLScope -> (JSCommaList JSExpression) -> SLComp s SLEnv
evalDecls ctxt at st rhs_sco decls =
  foldlM f (SLRes mempty st mempty) $ jscl_flatten decls
  where
    f (SLRes lifts lhs_st lhs_env) decl =
      keepLifts lifts $ evalDecl ctxt at lhs_st lhs_env rhs_sco decl

doOnly :: SLCtxt s -> SrcLoc -> (DLStmts, SLScope, SLState) -> (SLPart, SrcLoc, SLCloEnv, JSExpression) -> ST s (DLStmts, SLScope, SLState)
doOnly ctxt at (lifts, sco, st) (who, only_at, only_cloenv, only_synarg) = do
  let SLCloEnv only_env only_penvs only_cenv = only_cloenv
  let st_localstep = st {st_mode = SLM_LocalStep}
  let penvs = sco_penvs sco
  let penv = sco_lookup_penv ctxt sco who
  let sco_penv = sco {sco_env = penv}
  SLRes only_lifts _ only_arg <-
    evalExpr ctxt only_at sco_penv st_localstep only_synarg
  case only_arg of
    (_, only_clo@(SLV_Clo _ _ only_formals _ _)) -> do
      let only_vars = map (JSIdentifier JSNoAnnot) only_formals
      let sco_only_env =
            sco
              { sco_env = only_env
              , sco_penvs = only_penvs
              , sco_cenv = only_cenv
              }
      SLRes oarg_lifts _ only_args <-
        evalExprs ctxt only_at sco_only_env st_localstep only_vars
      SLRes alifts _ (SLAppRes penv' (_, only_v)) <-
        evalApplyVals ctxt at (impossible "part_only expects clo") st_localstep only_clo only_args
      case fst $ typeOf only_at only_v of
        T_Null -> do
          let penv'' = foldr' M.delete penv' only_formals
          let penvs' = M.insert who penv'' penvs
          let lifts' = return $ DLS_Only only_at who (only_lifts <> alifts)
          let st' = st {st_mode = SLM_Step}
          let sco' = sco {sco_penvs = penvs'}
          return ((lifts <> oarg_lifts <> lifts'), sco', st')
        ty ->
          expect_throw only_at (Err_Block_NotNull ty only_v)
    _ -> expect_throw at $ Err_Only_NotOneClosure $ snd only_arg

evalStmtTrampoline :: SLCtxt s -> JSSemi -> SrcLoc -> SLScope -> SLState -> SLSVal -> [JSStatement] -> SLComp s SLStmtRes
evalStmtTrampoline ctxt sp at sco st (_, ev) ks =
  case ev of
    SLV_Prim SLPrim_exitted ->
      case (st_mode st, st_live st) of
        (SLM_Step, True) ->
          expect_empty_tail "exit" JSNoAnnot sp at ks $
            return $ SLRes mempty (st {st_live = False}) $ SLStmtRes env []
        _ -> illegal_mode
    SLV_Form (SLForm_EachAns parts only_at only_env only_synarg) ->
      case st_mode st of
        SLM_Step -> do
          (lifts', sco', st') <-
            foldM (doOnly ctxt at) (mempty, sco, st) $
              map (\who -> (who, only_at, only_env, only_synarg)) parts
          keepLifts lifts' $ evalStmt ctxt at sco' st' ks
        _ -> illegal_mode
    SLV_Form (SLForm_Part_ToConsensus to_at who vas Nothing mmsg mamt mtime) ->
      case (st_mode st, st_live st) of
        (SLM_Step, True) -> do
          let st_pure = st {st_mode = SLM_Module}
          let pdvs = st_pdvs st
          let penv = sco_lookup_penv ctxt sco who
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
                return $ (foldl' (env_insertp at) mempty $ zip msg $ map (public . SLV_DLVar) $ map snd tvs, tvs)
          --- We go back to the original env from before the to-consensus step
          (pdvs', fs) <-
            case M.lookup who pdvs of
              Just pdv ->
                return $ (pdvs, FS_Again pdv)
              Nothing -> do
                let whos = B.unpack who
                whon <- ctxt_alloc ctxt to_at
                let whodv = DLVar to_at whos T_Address whon
                return $ ((M.insert who whodv pdvs), FS_Join whodv)
          let add_who_env :: SLEnv -> SLEnv =
                case vas of
                  Nothing -> \x -> x
                  Just whov ->
                    case env_lookup to_at whov env of
                      (lvl_, SLV_Participant at_ who_ io_ as_ _) ->
                        M.insert whov (lvl_, SLV_Participant at_ who_ io_ as_ (Just $ (pdvs' M.! who)))
                      _ ->
                        impossible $ "participant is not participant"
          let env' = add_who_env $ env_merge to_at env msg_env
          let sco_env' = sco {sco_env = env'}
          let penvs' =
                penvs_update
                  ctxt
                  sco
                  (\p old ->
                     case p == who of
                       True -> add_who_env old
                       False -> add_who_env $ env_merge to_at old msg_env)
          (amte, amt_lifts, amt_da) <-
            case mamt of
              Nothing ->
                return $ (amt_e_, mempty, amt_check_da)
                where
                  amt_check_da = DLA_Con $ DLC_Int 0
                  amt_e_ = JSDecimal JSNoAnnot "0"
              Just amte_ -> do
                let penv' = penvs' M.! who
                let sco_penv' =
                      sco
                        { sco_penvs = penvs'
                        , sco_env = penv'
                        }
                SLRes amt_lifts_ _ amt_sv <- evalExpr ctxt at sco_penv' st_pure amte_
                return $ (amte_, amt_lifts_, checkType at T_UInt256 $ ensure_public at amt_sv)
          let amt_compute_lifts = return $ DLS_Only at who amt_lifts
          SLRes amt_check_lifts _ _ <-
            let check_amte = JSCallExpression rator a rands a
                rator = JSIdentifier a "require"
                a = JSNoAnnot
                rands = JSLOne $ JSExpressionBinary amte (JSBinOpEq a) rhs
                rhs = JSCallExpression (JSIdentifier a "__txn.value__") a JSLNil a
             in evalExpr ctxt at sco_env' st_pure check_amte
          (tlifts, mt_st_cr, mtime') <-
            case mtime of
              Nothing -> return $ (mempty, Nothing, Nothing)
              Just (dt_at, de, (JSBlock _ dt_ss _)) -> do
                SLRes de_lifts _ de_sv <- evalExpr ctxt at sco st_pure de
                let de_da = checkType dt_at T_UInt256 $ ensure_public dt_at de_sv
                SLRes dta_lifts dt_st dt_cr <- evalStmt ctxt dt_at sco st dt_ss
                return $ (de_lifts, Just (dt_st, dt_cr), Just (de_da, dta_lifts))
          let st_cstep =
                st
                  { st_mode = SLM_ConsensusStep
                  , st_pdvs = pdvs'
                  }
          let sco' =
                sco
                  { sco_env = env'
                  , sco_cenv = env'
                  , sco_penvs = penvs'
                  }
          SLRes conlifts k_st k_cr <- evalStmt ctxt at sco' st_cstep ks
          let lifts' = tlifts <> amt_compute_lifts <> (return $ DLS_ToConsensus to_at who fs (map fst tmsg_) (map snd tmsg_) amt_da mtime' (amt_check_lifts <> conlifts))
          --- FIXME This might be general logic that applies to any
          --- place where we merge, like IFs and SEQNs, so if one side
          --- of an if dies, then the other side doesn't need to merge
          --- the results.
          case mt_st_cr of
            Nothing ->
              return $ SLRes lifts' k_st k_cr
            Just (t_st, t_cr) ->
              case st_live t_st of
                False ->
                  return $ SLRes lifts' k_st k_cr
                True ->
                  return $ SLRes lifts' (stMerge at t_st k_st) $ combineStmtRes at Public t_cr k_cr
        _ -> illegal_mode
    SLV_Prim SLPrim_committed ->
      case st_mode st of
        SLM_ConsensusStep -> do
          let orig_env = sco_cenv sco
          let addl_env = M.difference env orig_env
          let add_defns p penv = env_merge at penv (M.difference addl_env $ ctxt_base_penvs ctxt M.! p)
          let penvs' = penvs_update ctxt sco add_defns
          let st_step = st { st_mode = SLM_Step }
          let sco' = sco { sco_penvs = penvs'
                         , sco_cenv = env }
          SLRes steplifts k_st cr <- evalStmt ctxt at sco' st_step ks
          let lifts' = (return $ DLS_FromConsensus at steplifts)
          return $ SLRes lifts' k_st cr
        _ -> illegal_mode
    _ ->
      case typeOf at ev of
        (T_Null, _) -> evalStmt ctxt at sco st ks
        (ty, _) -> expect_throw at (Err_Block_NotNull ty ev)
  where
    illegal_mode = expect_throw at $ Err_Eval_IllegalMode (st_mode st) "trampoline"
    env = sco_env sco

evalStmt :: SLCtxt s -> SrcLoc -> SLScope -> SLState -> [JSStatement] -> SLComp s SLStmtRes
evalStmt ctxt at sco st ss =
  case ss of
    [] ->
      case sco_must_ret sco of
        RS_CannotReturn -> ret []
        RS_ImplicitNull -> ret [(at, public $ SLV_Null at "implicit null")]
        RS_NeedExplicit ->
          --- In the presence of `exit()`, it is okay to have a while
          --- that ends in an empty tail, if the empty tail is
          --- dominated by an exit(). Here we really on two properties
          --- of the linearizer and the verifier: first, the
          --- linearizer will completely drop the continuation of
          --- DLS_Continue and DLS_Stop, so if this assert is not
          --- removed, then ti will error.
          keepLifts (return $ DLS_Claim at (ctxt_stack ctxt) CT_Assert (DLA_Con $ DLC_Bool False)) $
            ret []
        RS_MayBeEmpty -> ret []
      where
        ret rs = return $ SLRes mempty st $ SLStmtRes (sco_env sco) rs
    ((JSStatementBlock a ss' _ sp) : ks) -> do
      br <- evalStmt ctxt at_in sco st ss'
      retSeqn br at_after ks
      where
        at_in = srcloc_jsa "block" a at
        at_after = srcloc_after_semi "block" a sp at
    (s@(JSBreak a _ _) : _) -> illegal a s "break"
    (s@(JSLet a _ _) : _) -> illegal a s "let"
    (s@(JSClass a _ _ _ _ _ _) : _) -> illegal a s "class"
    ((JSConstant a decls sp) : ks) -> do
      let env = sco_env sco
      SLRes lifts st_const addl_env <- evalDecls ctxt at_in st sco decls
      let env' = env_merge at_in env addl_env
      let sco' = sco {sco_env = env'}
      keepLifts lifts $ evalStmt ctxt at_after sco' st_const ks
      where
        at_after = srcloc_after_semi lab a sp at
        at_in = srcloc_jsa lab a at
        lab = "const"
    (cont@(JSContinue a _ sp) : cont_ks) ->
      evalStmt ctxt at sco st (assign : cont : cont_ks)
      where
        assign = JSAssignStatement lhs op rhs sp
        lhs = JSArrayLiteral a [] a
        op = JSAssign a
        rhs = lhs
    --- FIXME We could desugar all these to certain while patterns
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
    ((JSFunction a name lp jsformals rp body sp) : ks) ->
      evalStmt ctxt at sco st ss'
      where at' = srcloc_jsa "fun" a at
            ss' = (JSConstant a decls sp) : ks
            decls = JSLOne decl
            decl = JSVarInitExpression lhs (JSVarInit a rhs)
            lhs = JSIdentifier a f
            f = case name of
              JSIdentNone -> expect_throw at' (Err_TopFun_NoName)
              JSIdentName _ x -> x
            rhs = JSFunctionExpression a JSIdentNone lp jsformals rp body
    (s@(JSGenerator a _ _ _ _ _ _ _) : _) -> illegal a s "generator"
    ((JSIf a la ce ra ts) : ks) -> do
      evalStmt ctxt at sco st ((JSIfElse a la ce ra ts ea fs) : ks)
      where
        ea = ra
        fs = (JSEmptyStatement ea)
    ((JSIfElse a _ ce ta ts fa fs) : ks) -> do
      let at' = srcloc_jsa "if" a at
      let t_at' = srcloc_jsa "if > true" ta at'
      let f_at' = srcloc_jsa "if > false" fa t_at'
      SLRes clifts st_c (clvl, cv) <- evalExpr ctxt at' sco st ce
      let ks_ne = dropEmptyJSStmts ks
      let sco' =
            case ks_ne of
              [] -> sco
              _ -> sco {sco_must_ret = RS_MayBeEmpty}
      keepLifts clifts $
        case cv of
          SLV_Bool _ cb -> do
            let (n_at', ns) = case cb of
                  True -> (t_at', ts)
                  False -> (f_at', fs)
            nr <- evalStmt ctxt n_at' sco' st_c [ns]
            retSeqn nr at' ks_ne
          SLV_DLVar cond_dv@(DLVar _ _ T_Bool _) -> do
            SLRes tlifts st_t (SLStmtRes _ trets) <- evalStmt ctxt t_at' sco' st_c [ts]
            SLRes flifts st_f (SLStmtRes _ frets) <- evalStmt ctxt f_at' sco' st_c [fs]
            let st_tf = stMerge at' st_t st_f
            let lifts' = return $ DLS_If at' (DLA_Var cond_dv) tlifts flifts
            let levelHelp = SLStmtRes (sco_env sco) . map (\(r_at, (r_lvl, r_v)) -> (r_at, (clvl <> r_lvl, r_v)))
            let ir = SLRes lifts' st_tf $ combineStmtRes at' clvl (levelHelp trets) (levelHelp frets)
            retSeqn ir at' ks_ne
          _ ->
            expect_throw at (Err_Eval_IfCondNotBool cv)
    (s@(JSLabelled _ a _) : _) ->
      --- FIXME We could allow labels on whiles and have a mapping in
      --- sco_while_vars from a while label to the set of variables
      --- that should be modified, plus a field in sco for the default
      --- (i.e. closest label)
      illegal a s "labelled"
    ((JSEmptyStatement a) : ks) -> evalStmt ctxt at' sco st ks
      where
        at' = srcloc_jsa "empty" a at
    ((JSExpressionStatement e sp) : ks) -> do
      SLRes elifts st_e sev <- evalExpr ctxt at sco st e
      let at_after = srcloc_after_semi "expr stmt" JSNoAnnot sp at
      keepLifts elifts $ evalStmtTrampoline ctxt sp at_after sco st_e sev ks
    ((JSAssignStatement lhs op rhs _asp) : ks) ->
      case (op, ks) of
        ((JSAssign var_a), ((JSContinue cont_a _bl cont_sp) : cont_ks)) ->
          case st_mode st of
            SLM_ConsensusStep -> do
              let cont_at = srcloc_jsa lab cont_a at
              let decl = JSVarInitExpression lhs (JSVarInit var_a rhs)
              let env = sco_env sco
              SLRes decl_lifts st_decl decl_env <-
                evalDecl ctxt var_at st mempty sco decl
              let st_decl' = stEnsureMode at SLM_ConsensusStep st_decl
              let cont_das =
                    DLAssignment $
                      case sco_while_vars sco of
                        Nothing -> expect_throw cont_at $ Err_Eval_ContinueNotInWhile
                        Just whilem -> M.fromList $ map f $ M.toList decl_env
                          where
                            f (v, sv) = (dv, da)
                              where
                                dv = case M.lookup v whilem of
                                  Nothing ->
                                    expect_throw var_at $ Err_Eval_ContinueNotLoopVariable v
                                  Just x -> x
                                val = ensure_public var_at sv
                                da = checkType at et val
                                DLVar _ _ et _ = dv
              let lifts' = decl_lifts <> (return $ DLS_Continue cont_at cont_das)
              expect_empty_tail lab cont_a cont_sp cont_at cont_ks $
                return $ SLRes lifts' st_decl' $ SLStmtRes env []
            cm -> expect_throw var_at $ Err_Eval_IllegalMode cm "continue"
          where
            lab = "continue"
            var_at = srcloc_jsa lab var_a at
        (jsop, stmts) ->
          expect_throw (srcloc_jsa "assign" JSNoAnnot at) (Err_Block_Assign jsop stmts)
    ((JSMethodCall e a args ra sp) : ks) ->
      evalStmt ctxt at sco st ss'
      where
        ss' = (JSExpressionStatement e' sp) : ks
        e' = (JSCallExpression e a args ra)
    ((JSReturn a me sp) : ks) -> do
      let env = sco_env sco
      let lab = "return"
      let at' = srcloc_jsa lab a at
      SLRes elifts st_rt sev <-
        case me of
          Nothing -> return $ SLRes mempty st $ public $ SLV_Null at' "empty return"
          Just e -> evalExpr ctxt at' sco st e
      let ret = case sco_ret sco of
            Just x ->
              case sco_must_ret sco of
                RS_CannotReturn ->
                  expect_throw at $ Err_CannotReturn
                _ -> x
            Nothing -> expect_throw at' $ Err_Eval_NoReturn
      let (_, ev) = sev
      let lifts' = return $ DLS_Return at' ret ev
      expect_empty_tail lab a sp at ks $
        return $ SLRes (elifts <> lifts') st_rt (SLStmtRes env [(at', sev)])
    (s@(JSSwitch a _ _ _ _ _ _ _) : _) -> illegal a s "switch"
    (s@(JSThrow a _ _) : _) -> illegal a s "throw"
    (s@(JSTry a _ _ _) : _) -> illegal a s "try"
    ((JSVariable var_a while_decls _vsp) : var_ks) ->
      case var_ks of
        ( (JSMethodCall (JSIdentifier inv_a "invariant") _ invariant_args _ _isp)
            : (JSWhile while_a cond_a while_cond _ while_body)
            : ks
          ) ->
            case st_mode st of
              SLM_ConsensusStep -> do
                let env = sco_env sco
                SLRes init_lifts st_var vars_env <-
                  evalDecls ctxt var_at st sco while_decls
                let st_var' = stEnsureMode at SLM_ConsensusStep st_var
                let st_pure = st_var' {st_mode = SLM_Module}
                let while_help v sv = do
                      let (_, val) = sv
                      vn <- ctxt_alloc ctxt var_at
                      let (t, da) = typeOf var_at val
                      return $ (DLVar var_at v t vn, da)
                while_helpm <- M.traverseWithKey while_help vars_env
                let unknown_var_env = M.map (public . SLV_DLVar . fst) while_helpm
                let env' = env_merge at env unknown_var_env
                let sco_env' = sco {sco_env = env'}
                SLRes inv_lifts _ inv_da <-
                  case jscl_flatten invariant_args of
                    [invariant_e] ->
                      checkResType inv_at T_Bool $ evalExpr ctxt inv_at sco_env' st_pure invariant_e
                    ial -> expect_throw inv_at $ Err_While_IllegalInvariant ial
                let fs = ctxt_stack ctxt
                let inv_b = DLBlock inv_at fs inv_lifts inv_da
                SLRes cond_lifts _ cond_da <-
                  checkResType cond_at T_Bool $ evalExpr ctxt cond_at sco_env' st_pure while_cond
                let cond_b = DLBlock cond_at fs cond_lifts cond_da
                let while_sco =
                      sco
                        { sco_while_vars = Just $ M.map fst while_helpm
                        , sco_env = env'
                        , sco_must_ret = RS_NeedExplicit
                        }
                SLRes body_lifts body_st (SLStmtRes _ body_rets) <-
                  evalStmt ctxt while_at while_sco st_var' [while_body]
                let while_dam = M.fromList $ M.elems while_helpm
                let the_while =
                      DLS_While var_at (DLAssignment while_dam) inv_b cond_b body_lifts
                let sco' = sco {sco_env = env'}
                let st_post = stMerge at body_st st_var'
                SLRes k_lifts k_st (SLStmtRes k_env' k_rets) <-
                  evalStmt ctxt while_at sco' st_post ks
                let lifts' = init_lifts <> (return $ the_while) <> k_lifts
                let rets' = body_rets <> k_rets
                return $ SLRes lifts' k_st $ SLStmtRes k_env' rets'
              cm -> expect_throw var_at $ Err_Eval_IllegalMode cm "while"
            where
              inv_at = (srcloc_jsa "invariant" inv_a at)
              cond_at = (srcloc_jsa "cond" cond_a at)
              while_at = (srcloc_jsa "while" while_a at)
        _ -> expect_throw var_at $ Err_Block_Variable
      where
        var_at = (srcloc_jsa "var" var_a at)
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
          let SLRes lifts0 st0 (SLStmtRes _ rets0) = sr
          let sco' =
                case rets0 of
                  [] -> sco
                  (_ : _) -> sco {sco_must_ret = RS_ImplicitNull}
          SLRes lifts1 st1 (SLStmtRes env1 rets1) <- evalStmt ctxt at' sco' st0 ks'
          return $ SLRes (lifts0 <> lifts1) st1 (SLStmtRes env1 (rets0 ++ rets1))

combineStmtRes :: SrcLoc -> SecurityLevel -> SLStmtRes -> SLStmtRes -> SLStmtRes
combineStmtRes at' lvl (SLStmtRes _ lrets) (SLStmtRes env rrets) = SLStmtRes env rets
  where
    rets =
      case (lrets, rrets) of
        ([], []) -> []
        ([], _) -> [(at', (lvl, SLV_Null at' "empty left"))] ++ rrets
        (_, []) -> lrets ++ [(at', (lvl, SLV_Null at' "empty right"))]
        (_, _) -> lrets ++ rrets

expect_empty_tail :: String -> JSAnnot -> JSSemi -> SrcLoc -> [JSStatement] -> a -> a
expect_empty_tail lab a sp at ks res =
  case ks of
    [] -> res
    _ ->
      expect_throw at' (Err_TailNotEmpty ks)
      where
        at' = srcloc_after_semi lab a sp at

evalTopBody :: SLCtxt s -> SrcLoc -> SLState -> SLLibs -> SLEnv -> SLEnv -> [JSModuleItem] -> SLComp s SLEnv
evalTopBody ctxt at st libm env exenv body =
  case body of
    [] -> return $ SLRes mempty st exenv
    mi : body' ->
      case mi of
        (JSModuleImportDeclaration _ im) ->
          case im of
            JSImportDeclarationBare a libn sp ->
              evalTopBody ctxt at_after st libm env' exenv body'
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
          let sco =
                (SLScope
                   { sco_ret = Nothing
                   , sco_must_ret = RS_CannotReturn
                   , sco_while_vars = Nothing
                   , sco_env = env
                   , sco_penvs = mempty
                   , sco_cenv = mempty
                   })
          smr <- evalStmt ctxt at' sco st [sm]
          case smr of
            SLRes Seq.Empty _ (SLStmtRes env' []) ->
              let exenv' = case isExport of
                    True ->
                      --- If this is an exporting statement,
                      --- then add to the export environment
                      --- everything that is new.
                      env_merge at' exenv (M.difference env' env)
                    False ->
                      exenv
               in evalTopBody ctxt at' st libm env' exenv' body'
            SLRes {} ->
              expect_throw at' $ Err_Module_Return

type SLMod = (ReachSource, [JSModuleItem])

type SLLibs = (M.Map ReachSource SLEnv)

evalLib :: SLMod -> SLLibs -> ST s SLLibs
evalLib (src, body) libm = do
  let st =
        SLState
          { st_mode = SLM_Module
          , st_live = False
          , st_pdvs = mempty
          }
  let ctxt_top =
        (SLCtxt
           { ctxt_id = Nothing
           , ctxt_stack = []
           , ctxt_local_mname = Nothing
           , ctxt_base_penvs = mempty
           })
  exenv <- cannotLift "evalLibs" <$> evalTopBody ctxt_top prev_at st libm stdlib_env mt_env body'
  return $ M.insert src exenv libm
  where
    stdlib_env =
      case src of
        ReachStdLib -> base_env
        ReachSourceFile _ -> M.union (libm M.! ReachStdLib) base_env
    at = (srcloc_src src)
    (prev_at, body') =
      case body of
        ((JSModuleStatementListItem (JSExpressionStatement (JSStringLiteral a hs) sp)) : j)
          | (trimQuotes hs) == versionHeader ->
            ((srcloc_after_semi "header" a sp at), j)
        _ -> expect_throw at (Err_NoHeader body)

evalLibs :: [SLMod] -> ST s SLLibs
evalLibs mods = foldrM evalLib mempty mods

makeInteract :: SrcLoc -> SLPart -> SLEnv -> SLVal
makeInteract at who spec = SLV_Object at spec'
  where
    spec' = M.mapWithKey wrap_ty spec
    wrap_ty k (Public, (SLV_Type t)) = secret $ SLV_Prim $ SLPrim_interact at who k t
    wrap_ty _ v = expect_throw at $ Err_App_InvalidInteract v

compileDApp :: SLVal -> ST s DLProg
compileDApp topv =
  case topv of
    SLV_Prim (SLPrim_App_Delay at _opts partvs (JSBlock _ top_ss _) top_env) -> do
      --- FIXME look at opts
      idxr <- newSTCounter 0
      let st_step =
            SLState
              { st_mode = SLM_Step
              , st_live = True
              , st_pdvs = mempty
              }
      let ctxt =
            SLCtxt
              { ctxt_id = Just idxr
              , ctxt_stack = []
              , ctxt_local_mname = Nothing
              , ctxt_base_penvs = penvs
              }
      let sco =
            SLScope
              { sco_ret = Nothing
              , sco_must_ret = RS_CannotReturn
              , sco_env = top_env
              , sco_while_vars = Nothing
              , sco_penvs = penvs
              , sco_cenv = mempty
              }
      SLRes final _ _ <- evalStmt ctxt at' sco st_step top_ss
      return $ DLProg at sps final
      where
        at' = srcloc_at "compileDApp" Nothing at
        sps = SLParts $ M.fromList $ map make_sps_entry partvs
        make_sps_entry (Public, (SLV_Participant _ pn (SLV_Object _ io) _ _)) =
          (pn, InteractEnv $ M.map getType io)
          where
            getType (_, (SLV_Prim (SLPrim_interact _ _ _ t))) = t
            getType x = impossible $ "make_sps_entry getType " ++ show x
        make_sps_entry x = impossible $ "make_sps_entry " ++ show x
        penvs = M.fromList $ map make_penv partvs
        make_penv (Public, (SLV_Participant _ pn io _ _)) =
          (pn, env_insert at' "interact" (secret io) top_env)
        make_penv _ = impossible "SLPrim_App_Delay make_penv"
    _ ->
      expect_throw srcloc_top (Err_Top_NotApp topv)

compileBundleST :: JSBundle -> SLVar -> ST s DLProg
compileBundleST (JSBundle mods) top = do
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

compileBundle :: JSBundle -> SLVar -> DLProg
compileBundle jsb top =
  runST $ compileBundleST jsb top
