module Reach.Eval (EvalError, compileBundle) where

import Control.Arrow (second)
import Control.Monad
import Control.Monad.ST
import Control.Monad.Writer
import Data.Bits
import qualified Data.ByteString as B
import Data.Foldable
import Data.List (intercalate, sortBy)
import Data.List.Extra (mconcatMap)
import qualified Data.Map.Strict as M
import Data.Ord (comparing)
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import GHC.Stack (HasCallStack)
import Generics.Deriving
import Language.JavaScript.Parser
import Language.JavaScript.Parser.AST
import Reach.AST
import Reach.JSUtil
import Reach.Parser
import Reach.STCounter
import Reach.Type
import Reach.Util
import Reach.Version
import Safe (atMay)
import Safe.Exact (splitAtExactMay)
import Text.EditDistance (defaultEditCosts, restrictedDamerauLevenshteinDistance)
import Text.ParserCombinators.Parsec.Number (numberValue)

---import Debug.Trace

--- Errors

data EvalError
  = Err_Apply_ArgCount SrcLoc Int Int
  | Err_Block_Assign JSAssignOp [JSStatement]
  | Err_Block_IllegalJS JSStatement
  | Err_Block_NotNull SLType SLVal
  | Err_Block_Variable
  | Err_Block_While
  | Err_CannotReturn
  | Err_ToConsensus_TimeoutArgs [JSExpression]
  | Err_App_Interact_NotFirstOrder SLType
  | Err_App_InvalidOption SLVar [SLVar]
  | Err_App_InvalidOptionValue SLVar String
  | Err_App_InvalidInteract SLSVal
  | Err_App_InvalidPartSpec SLVal
  | Err_App_InvalidArgs [JSExpression]
  | Err_App_PartUnderscore B.ByteString
  | Err_DeclLHS_IllegalJS JSExpression
  | Err_Decl_ObjectSpreadNotLast
  | Err_Decl_ArraySpreadNotLast
  | Err_Decl_NotObject SLVal
  | Err_Decl_IllegalJS JSExpression
  | Err_Decl_NotRefable SLVal
  | Err_Decl_WrongArrayLength Int Int
  | Err_Dot_InvalidField SLVal [String] String
  | Err_Eval_ContinueNotInWhile
  | Err_Eval_IllegalWait DeployMode
  | Err_Eval_ContinueNotLoopVariable SLVar
  | Err_Eval_PartSet_Bound SLPart
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
  | Err_Form_InvalidArgs SLForm Int [JSExpression]
  | Err_Fun_NamesIllegal
  | Err_Import_IllegalJS JSImportClause
  | Err_Module_Return
  | Err_NoHeader [JSModuleItem]
  | Err_Obj_IllegalComputedField SLVal
  | Err_Obj_IllegalFieldValues [JSExpression]
  | Err_Obj_IllegalMethodDefinition JSObjectProperty
  | Err_Obj_IllegalNumberField JSPropertyName
  | Err_Obj_SpreadNotObj SLVal
  | --- FIXME add count
    Err_Prim_InvalidArgs SLPrimitive [SLVal]
  | Err_Shadowed SLVar SLSSVal SLSSVal -- var, alreadyBound, new (invalid)
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
  | Err_Eval_NotSecretIdent SLVar
  | Err_Eval_NotPublicIdent SLVar
  | Err_Eval_LookupUnderscore
  | Err_Eval_NotSpreadable SLVal
  | Err_Unknowable_NotParticipant SLVal
  | Err_Zip_ArraysNotEqualLength Integer Integer
  | Err_Switch_NotData SLVal
  | Err_Switch_DoubleCase SrcLoc SrcLoc (Maybe SLVar)
  | Err_Switch_MissingCases [SLVar]
  | Err_Switch_ExtraCases [SLVar]
  | Err_Expected_Bytes SLVal
  deriving (Eq, Generic)

--- FIXME I think most of these things should be in Pretty

displaySlValType :: SLVal -> String
displaySlValType = \case
  SLV_Participant _ who _ _ _ ->
    "<participant " <> (bunpack who) <> ">"
  SLV_Object _ (Just lab) _ ->
    lab
  sv ->
    case typeOfM (SrcLoc Nothing Nothing Nothing) sv of
      Just (t, _) -> displayTy t
      Nothing -> "<" <> conNameOf sv <> ">"

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
  T_Object _m -> "object" -- FIXME
  T_Data _m -> "data" -- FIXME
  T_Forall x ty {- SLVar SLType -} -> "Forall(" <> x <> ": " <> displayTy ty <> ")"
  T_Var x {- SLVar-} -> x
  T_Type _ -> "type"

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

showVS :: Show a => a -> a -> String
showVS fx fy = show fx <> " vs " <> show fy

showDiff :: Eq b => a -> a -> (a -> b) -> String -> (b -> b -> String) -> String
showDiff x y f lab s =
  let fx = f x
      fy = f y
   in case fx == fy of
        True -> ""
        False -> "\n  " <> lab <> ": " <> s fx fy

showStateDiff :: SLState -> SLState -> String
showStateDiff x y =
  showDiff x y st_mode "Mode" showVS
    <> showDiff x y st_live "Live" showVS
    <> showDiff x y st_after_first "After First Message" showVS
    <> showDiff x y st_pdvs "Participant Definitions" showVS

-- TODO more hints on why invalid syntax is invalid
instance Show EvalError where
  show = \case
    Err_Zip_ArraysNotEqualLength x y ->
      "Zip requires arrays of equal length, but got " <> show x <> " and " <> show y
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
    Err_App_Interact_NotFirstOrder ty ->
      "Invalid interact specification. Expected first-order type, got: "
        <> show ty
    Err_App_InvalidInteract (secLev, val) ->
      "Invalid interact specification. Expected public type, got: "
        <> (displaySecurityLevel secLev <> " " <> displaySlValType val)
    Err_App_InvalidPartSpec _slval ->
      "Invalid participant spec"
    Err_App_InvalidArgs _jes ->
      "Invalid app arguments"
    Err_App_InvalidOption opt opts ->
      opt <> " is not a valid app option" <> didYouMean opt opts 5
    Err_App_InvalidOptionValue opt msg ->
      "Invalid value for app option, " <> opt <> ": " <> msg
    Err_App_PartUnderscore bs ->
      "Invalid participant name. Participant names may not begin with an underscore: "
        <> bunpack bs
    Err_DeclLHS_IllegalJS _e ->
      "Invalid binding. Expressions cannot appear on the LHS."
    Err_Eval_PartSet_Bound who ->
      (bunpack who) <> " is bound and cannot be rebound"
    Err_Eval_IllegalWait dm ->
      "Cannot wait or timeout until after first message in deployMode " <> show dm
    Err_Decl_IllegalJS e ->
      "Invalid Reach declaration: " <> conNameOf e
    Err_Decl_ObjectSpreadNotLast ->
      "Object spread on left-hand side of binding must occur in last position"
    Err_Decl_ArraySpreadNotLast ->
      "Array spread on left-hand side of binding must occur in last position"
    Err_Decl_NotObject slval ->
      "Invalid binding. Expected object, got: " <> displaySlValType slval
    Err_Decl_NotRefable slval ->
      "Invalid binding. Expected array or tuple, got: " <> displaySlValType slval
    Err_Decl_WrongArrayLength nIdents nVals ->
      "Invalid array binding. nIdents:" <> show nIdents <> " does not match nVals:" <> show nVals
    Err_Dot_InvalidField slval ks k ->
      k <> " is not a field of " <> displaySlValType slval <> didYouMean k ks 5
    Err_Eval_ContinueNotInWhile ->
      "Invalid continue. Expected to be inside of a while."
    Err_Eval_ContinueNotLoopVariable var ->
      "Invalid loop variable update. Expected loop variable, got: " <> var
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
      "Invalid computed field name. " <> reason
      where
        reason = case displaySlValType slval of
          "bytes" -> "It must be computable at compile time."
          ty -> "Fields must be bytes, but got: " <> ty
    Err_Obj_IllegalFieldValues exprs ->
      -- FIXME Is this syntactically possible?
      "Invalid field values. Expected 1 value, got: " <> show (length exprs)
    Err_Obj_IllegalMethodDefinition _prop ->
      "Invalid function field. Instead of {f() {...}}, write {f: () => {...}}"
    Err_Obj_IllegalNumberField _JSPropertyName ->
      "Invalid field name. Fields must be bytes, but got: uint256"
    Err_Obj_SpreadNotObj slval ->
      "Invalid object spread. Expected object, got: " <> displaySlValType slval
    Err_Eval_NotSpreadable slval ->
      "Value not spreadable. Expected tuple or array, got: " <> displaySlValType slval
    Err_Prim_InvalidArgs prim slvals ->
      "Invalid args for " <> displayPrim prim <> ". got: "
        <> "["
        <> (intercalate ", " $ map displaySlValType slvals)
        <> "]"
      where
        displayPrim = drop (length ("SLPrim_" :: String)) . conNameOf
    Err_Shadowed n (SLSSVal at0 _ _) (SLSSVal at _ _) ->
      -- FIXME tell the srcloc of the original binding
      "Invalid name shadowing"
        <> (". Identifier '" <> n <> "' is already bound at " <> show at0)
        <> (". It cannot be bound again at " <> show at)
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
      "PART.only not given a single closure, with no arguments, as an argument, instead got " <> (displaySlValType slval)
    Err_Each_NotTuple slval ->
      "each not given a tuple as an argument, instead got " <> displaySlValType slval
    Err_Each_NotParticipant slval ->
      "each not given a participant as an argument, instead got " <> displaySlValType slval
    Err_Unknowable_NotParticipant slval ->
      "unknowable not given a participant as an argument, instead got " <> displaySlValType slval
    Err_Transfer_NotBound who ->
      "cannot transfer to unbound participant, " <> bunpack who
    Err_Eval_IncompatibleStates x y ->
      "incompatible states:" <> showStateDiff x y
    Err_Eval_NotSecretIdent x ->
      ("Invalid binding in PART.only: " <> x <> ".")
        <> " Secret identifiers must be prefixed by _."
        <> " Did you mean to declassify()?"
    Err_Eval_NotPublicIdent x ->
      "Invalid binding: " <> x <> ". Public identifiers must not be prefixed by _"
    Err_Eval_LookupUnderscore ->
      "Invalid identifier reference. The _ identifier may never be read."
    Err_Switch_NotData x ->
      "switch expects data instance, but got " <> displaySlValType x
    Err_Switch_DoubleCase at0 at1 mc ->
      "switch contains duplicate case, " <> (maybe "default" id mc) <> " at " <> show at1 <> "; first defined at " <> show at0
    Err_Switch_MissingCases cs ->
      "switch missing cases: " <> show cs
    Err_Switch_ExtraCases cs ->
      "switch contains extra cases: " <> show cs
    Err_Expected_Bytes v ->
      "expected bytes, got something else: " <> displaySlValType v

--- Utilities
zipEq :: Show e => SrcLoc -> (Int -> Int -> e) -> [a] -> [b] -> [(a, b)]
zipEq at ce x y =
  if lx == ly
    then zip x y
    else expect_throw at (ce lx ly)
  where
    lx = length x
    ly = length y

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

-- | Certain idents are special and bypass the public/private
-- enforced naming convention.
isSpecialIdent :: SLVar -> Bool
isSpecialIdent "interact" = True
isSpecialIdent "__decode_testing__" = True
isSpecialIdent _ = False

-- | Secret idents start with _, but are not _.
isSecretIdent :: SLVar -> Bool
isSecretIdent ('_' : _ : _) = True
isSecretIdent _ = False

internalVar :: SLVar -> SLVar
internalVar v = "." <> v

internalVar_balance :: SLVar
internalVar_balance = internalVar "balance"

data EnvInsertMode
  = AllowShadowing
  | DisallowShadowing

-- | The "_" never actually gets bound;
-- it is therefore only ident that may be "shadowed".
-- Secret idents must start with _.
-- Public idents must not start with _.
-- Special idents "interact" and "__decode_testing__" skip these rules.
env_insert_ :: HasCallStack => EnvInsertMode -> SrcLoc -> SLVar -> SLSSVal -> SLEnv -> SLEnv
env_insert_ _ _ "_" _ env = env
env_insert_ insMode at k v env = case insMode of
  DisallowShadowing ->
    case M.lookup k env of
      Nothing -> go
      Just v0 -> expect_throw at (Err_Shadowed k v0 v)
  AllowShadowing -> go
  where
    go = case v of
      -- Note: secret ident enforcement is limited to doOnly
      (SLSSVal _ Public _)
        | not (isSpecialIdent k) && isSecretIdent k ->
          expect_throw at (Err_Eval_NotPublicIdent k)
      _ -> M.insert k v env

env_insert :: HasCallStack => SrcLoc -> SLVar -> SLSSVal -> SLEnv -> SLEnv
env_insert = env_insert_ DisallowShadowing

env_insertp_ :: HasCallStack => EnvInsertMode -> SrcLoc -> SLEnv -> (SLVar, SLSSVal) -> SLEnv
env_insertp_ imode at = flip (uncurry (env_insert_ imode at))

env_insertp :: HasCallStack => SrcLoc -> SLEnv -> (SLVar, SLSSVal) -> SLEnv
env_insertp = env_insertp_ DisallowShadowing

env_merge_ :: HasCallStack => EnvInsertMode -> SrcLoc -> SLEnv -> SLEnv -> SLEnv
env_merge_ imode at left righte = foldl' (env_insertp_ imode at) left $ M.toList righte

env_merge :: HasCallStack => SrcLoc -> SLEnv -> SLEnv -> SLEnv
env_merge = env_merge_ DisallowShadowing

-- | The "_" ident may never be looked up.
env_lookup :: HasCallStack => SrcLoc -> SLVar -> SLEnv -> SLSSVal
env_lookup at "_" _ = expect_throw at (Err_Eval_LookupUnderscore)
env_lookup at x env =
  case M.lookup x env of
    Just v -> v
    Nothing ->
      expect_throw at (Err_Eval_UnboundId x $ M.keys env)

m_fromList_public_builtin :: [(SLVar, SLVal)] -> SLEnv
m_fromList_public_builtin = m_fromList_public srcloc_builtin

base_env :: SLEnv
base_env =
  m_fromList_public_builtin
    [ ("makeEnum", SLV_Prim SLPrim_makeEnum)
    , ("declassify", SLV_Prim SLPrim_declassify)
    , ("commit", SLV_Prim SLPrim_commit)
    , ("digest", SLV_Prim SLPrim_digest)
    , ("transfer", SLV_Prim SLPrim_transfer)
    , ("assert", SLV_Prim $ SLPrim_claim CT_Assert)
    , ("assume", SLV_Prim $ SLPrim_claim CT_Assume)
    , ("require", SLV_Prim $ SLPrim_claim CT_Require)
    , ("possible", SLV_Prim $ SLPrim_claim CT_Possible)
    , ("unknowable", SLV_Form $ SLForm_unknowable)
    , ("balance", SLV_Prim $ SLPrim_fluid_read $ FV_balance)
    , ("Null", SLV_Type T_Null)
    , ("Bool", SLV_Type T_Bool)
    , ("UInt256", SLV_Type T_UInt256)
    , ("Bytes", SLV_Type T_Bytes)
    , ("Address", SLV_Type T_Address)
    , ("forall", SLV_Prim SLPrim_forall)
    , ("Data", SLV_Prim SLPrim_Data)
    , ("Array", SLV_Prim SLPrim_Array)
    , ("array", SLV_Prim SLPrim_array)
    , ("Tuple", SLV_Prim SLPrim_Tuple)
    , ("Object", SLV_Prim SLPrim_Object)
    , ("Fun", SLV_Prim SLPrim_Fun)
    , ("exit", SLV_Prim SLPrim_exit)
    , ("each", SLV_Form SLForm_each)
    , ("intEq", SLV_Prim $ SLPrim_op PEQ)
    , ("bytesEq", SLV_Prim $ SLPrim_op BYTES_EQ)
    , ("isType", SLV_Prim SLPrim_is_type)
    , ("typeEq", SLV_Prim SLPrim_type_eq)
    , ("typeOf", SLV_Prim SLPrim_typeOf)
    , ("wait", SLV_Prim SLPrim_wait)
    , ( "Participant"
      , (SLV_Object srcloc_builtin (Just $ "Participant") $
           m_fromList_public_builtin
             [("set", SLV_Prim SLPrim_part_set)])
      )
    , ( "Reach"
      , (SLV_Object srcloc_builtin (Just $ "Reach") $
           m_fromList_public_builtin
             [("App", SLV_Form SLForm_App)])
      )
    ]

jsClo :: HasCallStack => SrcLoc -> String -> String -> (M.Map SLVar SLVal) -> SLVal
jsClo at name js env_ = SLV_Clo at (Just name) args body cloenv
  where
    cloenv = SLCloEnv env mempty mempty
    env = M.map (SLSSVal at Public) env_
    (args, body) =
      case readJsExpr js of
        JSArrowExpression aformals _ bodys -> (a_, b_)
          where
            b_ = jsArrowStmtToBlock bodys
            a_ = parseJSArrowFormals at aformals
        _ -> impossible "not arrow"

-- General compiler utilities
checkResType :: SrcLoc -> SLType -> SLComp a SLSVal -> SLComp a DLArg
checkResType at et m = do
  SLRes lifts st (_lvl, v) <- m
  return $ SLRes lifts st $ checkType at et v

-- Compiler
--- A context has global stuff (the variable counter) and abstracts
--- the control-flow state that leads to the expression, so it
--- inherits in a function call.
data SLCtxt s = SLCtxt
  { ctxt_dlo :: DLOpts
  , ctxt_id :: STCounter s
  , ctxt_stack :: [SLCtxtFrame]
  , ctxt_local_mname :: Maybe [SLVar]
  , ctxt_base_penvs :: SLPartEnvs
  }

instance Show (SLCtxt s) where
  show _ = "<context>"

ctxt_alloc :: SLCtxt s -> ST s Int
ctxt_alloc ctxt =
  incSTCounter $ ctxt_id ctxt

ctxt_mkvar :: SLCtxt s -> (Int -> DLVar) -> ST s DLVar
ctxt_mkvar ctxt mkvar =
  mkvar <$> ctxt_alloc ctxt

ctxt_lift_expr :: SLCtxt s -> SrcLoc -> (Int -> DLVar) -> DLExpr -> ST s (DLVar, DLStmts)
ctxt_lift_expr ctxt at mkvar e = do
  dv <- ctxt_mkvar ctxt mkvar
  let s = DLS_Let at (Just dv) e
  return (dv, return s)

ctxt_lift_expr_w :: SLCtxt s -> SrcLoc -> (Int -> DLVar) -> DLExpr -> WriterT DLStmts (ST s) DLVar
ctxt_lift_expr_w ctxt at mk_var e = WriterT $ ctxt_lift_expr ctxt at mk_var e

-- Unused
-- ctxt_lift_arg :: SLCtxt s -> SrcLoc -> SrcLoc -> String -> DLArg -> ST s (DLVar, DLStmts)
-- ctxt_lift_arg ctxt at at' name a =
--   ctxt_lift_expr ctxt at (DLVar at' (ctxt_local_name ctxt name) t) (DLE_Arg at' a)
--   where
--     t = argTypeOf a

ctxt_local_name :: SLCtxt s -> SLVar -> SLVar
ctxt_local_name ctxt def =
  case ctxt_local_mname ctxt of
    Nothing -> def
    Just [x] -> x ++ as
    Just xs -> "one of " ++ show xs ++ as
  where
    as = " (as " ++ def ++ ")"

data SLMode
  = --- The top-level of a module, before the App starts
    SLM_Module
  | --- The app starts in a "step"
    SLM_Step
  | --- An "only" moves from "step" to "local step" and then to "step" again, where x = live
    SLM_LocalStep
  | --- A "toconsensus" moves from "step" to "consensus step" then to "step" again
    SLM_ConsensusStep
  | SLM_ConsensusPure
  deriving (Eq, Generic, Show)

--- A state represents the state of the protocol, so it is returned
--- out of a function call.
data SLState = SLState
  { --- A function call may modify the mode
    st_mode :: SLMode
  , st_live :: Bool
  , st_after_first :: Bool
  , --- A function call may cause a participant to join
    st_pdvs :: SLPartDVars
  }
  deriving (Eq, Show)

allowed_to_wait :: SLCtxt s -> SrcLoc -> SLState -> x -> x
allowed_to_wait ctxt at st =
  case dm of
    DM_constructor -> id
    DM_firstMsg -> after_first
  where
    dm = dlo_deployMode $ ctxt_dlo ctxt
    after_first =
      case st_after_first st of
        True -> id
        False ->
          expect_throw at $ Err_Eval_IllegalWait dm

type SLPartDVars = M.Map SLPart DLVar

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

sco_update_ :: EnvInsertMode -> SLCtxt s -> SrcLoc -> SLScope -> SLState -> SLEnv -> SLScope
sco_update_ imode ctxt at sco st addl_env =
  sco
    { sco_env = do_merge $ sco_env sco
    , sco_penvs = sco_penvs'
    , sco_cenv = sco_cenv'
    }
  where
    sco_penvs' =
      case st_mode st of
        SLM_Step -> updated_penvs
        SLM_ConsensusStep -> updated_penvs
        _ -> sco_penvs sco
    updated_penvs = penvs_update ctxt sco (const do_merge)
    sco_cenv' =
      case st_mode st of
        SLM_ConsensusStep -> do_merge $ sco_cenv sco
        _ -> sco_cenv sco
    do_merge = flip (env_merge_ imode at) addl_env

sco_update :: SLCtxt s -> SrcLoc -> SLScope -> SLState -> SLEnv -> SLScope
sco_update = sco_update_ DisallowShadowing

stMerge :: HasCallStack => SrcLoc -> SLState -> SLState -> SLState
stMerge at x y =
  case x == y of
    True -> y
    False -> expect_throw at $ Err_Eval_IncompatibleStates x y

stEnsureMode :: SrcLoc -> SLMode -> SLState -> SLState
stEnsureMode at slm st =
  stMerge at st $ st {st_mode = slm}

ctxt_local_name_set :: SLCtxt s -> [SLVar] -> SLCtxt s
ctxt_local_name_set ctxt lhs_ns =
  --- FIXME come up with a "reset" mechanism for this and embed in expr some places
  ctxt {ctxt_local_mname = Just lhs_ns}

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

type SLStmtRets = [(SrcLoc, SLSVal)]

data SLStmtRes = SLStmtRes SLEnv SLStmtRets

data SLAppRes = SLAppRes SLEnv SLSVal

ctxt_stack_push :: SLCtxt s -> SLCtxtFrame -> SLCtxt s
ctxt_stack_push ctxt f =
  (ctxt {ctxt_stack = f : (ctxt_stack ctxt)})

binaryToPrim :: SrcLoc -> SLEnv -> JSBinOp -> SLVal
binaryToPrim at env o =
  case o of
    JSBinOpAnd _ -> impossible "and"
    JSBinOpDivide a -> prim a (DIV)
    JSBinOpEq a -> fun a "polyEq"
    JSBinOpGe a -> prim a (PGE)
    JSBinOpGt a -> prim a (PGT)
    JSBinOpLe a -> prim a (PLE)
    JSBinOpLt a -> prim a (PLT)
    JSBinOpMinus a -> prim a (SUB)
    JSBinOpMod a -> prim a (MOD)
    JSBinOpNeq a -> fun a "polyNeq"
    JSBinOpOr _ -> impossible "or"
    JSBinOpPlus a -> prim a (ADD)
    JSBinOpStrictEq a -> fun a "polyEq"
    JSBinOpStrictNeq a -> fun a "polyNeq"
    JSBinOpTimes a -> prim a (MUL)
    JSBinOpLsh a -> prim a (LSH)
    JSBinOpRsh a -> prim a (RSH)
    JSBinOpBitAnd a -> prim a (BAND)
    JSBinOpBitOr a -> prim a (BIOR)
    JSBinOpBitXor a -> prim a (BXOR)
    j -> expect_throw at $ Err_Parse_IllegalBinOp j
  where
    fun a s = sss_val $ env_lookup (srcloc_jsa "binop" a at) s env
    prim _a p = SLV_Prim $ SLPrim_op p

unaryToPrim :: SrcLoc -> SLEnv -> JSUnaryOp -> SLVal
unaryToPrim at env o =
  case o of
    JSUnaryOpMinus a -> fun a "minus"
    JSUnaryOpNot a -> fun a "not"
    JSUnaryOpTypeof a -> fun a "typeOf"
    j -> expect_throw at $ Err_Parse_IllegalUnaOp j
  where
    fun a s = sss_val $ env_lookup (srcloc_jsa "unop" a at) s env

infectWithId :: SLVar -> SLSVal -> SLSVal
infectWithId v (lvl, sv) = (lvl, sv')
  where
    sv' =
      case sv of
        SLV_Participant at who io _ mdv ->
          SLV_Participant at who io (Just v) mdv
        _ -> sv

evalAsEnv :: SrcLoc -> SLVal -> M.Map SLVar (SLCtxt s -> SLScope -> SLState -> SLComp s SLSVal)
evalAsEnv at obj =
  case obj of
    SLV_Object _ _ env ->
      M.map (retV . sss_sls) env
    SLV_DLVar obj_dv@(DLVar _ _ (T_Object tm) _) ->
      retDLVar tm (DLA_Var obj_dv) Public
    SLV_Prim (SLPrim_interact _ who m it@(T_Object tm)) ->
      retDLVar tm (DLA_Interact who m it) Secret
    SLV_Participant _ who _ vas _ ->
      M.fromList
        [ ("only", retV $ public $ SLV_Form (SLForm_Part_Only who))
        , ("publish", retV $ public $ SLV_Form (SLForm_Part_ToConsensus at who vas (Just TCM_Publish) Nothing Nothing Nothing))
        , ("pay", retV $ public $ SLV_Form (SLForm_Part_ToConsensus at who vas (Just TCM_Pay) Nothing Nothing Nothing))
        , ("set", delayCall SLPrim_part_set)
        ]
    SLV_Form (SLForm_Part_ToConsensus to_at who vas Nothing mpub mpay mtime) ->
      M.fromList
        [ ("publish", retV $ public $ SLV_Form (SLForm_Part_ToConsensus to_at who vas (Just TCM_Publish) mpub mpay mtime))
        , ("pay", retV $ public $ SLV_Form (SLForm_Part_ToConsensus to_at who vas (Just TCM_Pay) mpub mpay mtime))
        , ("timeout", retV $ public $ SLV_Form (SLForm_Part_ToConsensus to_at who vas (Just TCM_Timeout) mpub mpay mtime))
        ]
    --- FIXME rewrite the rest to look at the type and go from there
    SLV_Tuple _ _ ->
      M.fromList
        [ ("set", delayCall SLPrim_tuple_set)
        , ("length", doCall SLPrim_tuple_length)
        ]
    SLV_DLVar (DLVar _ _ (T_Tuple _) _) ->
      M.fromList
        [ ("set", delayCall SLPrim_tuple_set)
        , ("length", doCall SLPrim_tuple_length)
        ]
    SLV_Prim SLPrim_Tuple ->
      M.fromList
        [ ("set", retV $ public $ SLV_Prim $ SLPrim_tuple_set)
        , ("length", retV $ public $ SLV_Prim $ SLPrim_tuple_length)
        ]
    SLV_Array _ _ _ ->
      M.fromList
        [ ("set", delayCall SLPrim_array_set)
        , ("length", doCall SLPrim_array_length)
        , ("concat", delayCall SLPrim_array_concat)
        , ("map", delayCall SLPrim_array_map)
        , ("reduce", delayCall SLPrim_array_reduce)
        , ("zip", delayCall SLPrim_array_zip)
        ]
    SLV_DLVar (DLVar _ _ (T_Array _ _) _) ->
      M.fromList
        [ ("set", delayCall SLPrim_array_set)
        , ("length", doCall SLPrim_array_length)
        , ("concat", delayCall SLPrim_array_concat)
        , ("map", delayCall SLPrim_array_map)
        , ("reduce", delayCall SLPrim_array_reduce)
        , ("zip", delayCall SLPrim_array_zip)
        ]
    SLV_Prim SLPrim_Array ->
      M.fromList
        [ ("empty", retStdLib "Array_empty")
        , ("replicate", retStdLib "Array_replicate")
        , ("length", retV $ public $ SLV_Prim $ SLPrim_array_length)
        , ("set", retV $ public $ SLV_Prim $ SLPrim_array_set)
        , ("iota", retV $ public $ SLV_Prim $ SLPrim_Array_iota)
        , ("concat", retV $ public $ SLV_Prim $ SLPrim_array_concat)
        , ("map", retV $ public $ SLV_Prim $ SLPrim_array_map)
        , ("reduce", retV $ public $ SLV_Prim $ SLPrim_array_reduce)
        , ("zip", retV $ public $ SLV_Prim $ SLPrim_array_zip)
        ]
    SLV_Prim SLPrim_Object ->
      M.fromList
        [("set", retStdLib "Object_set")]
    SLV_Type (T_Data varm) ->
      M.mapWithKey (\k t -> retV $ public $ SLV_Prim $ SLPrim_Data_variant varm k t) varm
    v ->
      expect_throw at (Err_Eval_NotObject v)
  where
    delayCall p = retV $ public $ SLV_Prim $ SLPrim_PrimDelay at p [(public obj)] []
    doCall p ctxt sco st = do
      SLRes lifts st' (SLAppRes _ v) <- evalApplyVals ctxt at sco st (SLV_Prim p) [(public obj)]
      return $ SLRes lifts st' v
    retDLVar tm obj_dla slvl =
      M.mapWithKey retk tm
      where
        retk field t ctxt _sco st = do
          (dv, lifts') <- ctxt_lift_expr ctxt at (DLVar at (ctxt_local_name ctxt "object ref") t) (DLE_ObjectRef at obj_dla field)
          let ansv = SLV_DLVar dv
          return $ SLRes lifts' st (slvl, ansv)
    retV sv _ctxt _sco st = return $ SLRes mempty st sv
    retStdLib n ctxt sco st = retV (sss_sls $ env_lookup at n $ sco_env sco) ctxt sco st

evalDot :: SLCtxt s -> SrcLoc -> SLScope -> SLState -> SLVal -> String -> SLComp s SLSVal
evalDot ctxt at sco st obj field = do
  let env = evalAsEnv at obj
  case M.lookup field env of
    Just gv -> gv ctxt sco st
    Nothing -> expect_throw at $ Err_Dot_InvalidField obj (M.keys env) field

evalForm :: SLCtxt s -> SrcLoc -> SLScope -> SLState -> SLForm -> [JSExpression] -> SLComp s SLSVal
evalForm ctxt at sco st f args =
  case f of
    SLForm_App ->
      case args of
        [opte, partse, JSArrowExpression top_formals _ top_s] -> do
          sargs <- cannotLift "App args" <$> evalExprs ctxt at sco st [opte, partse]
          case map snd sargs of
            [(SLV_Object _ _ opts), (SLV_Tuple _ parts)] ->
              retV $ public $ SLV_Prim $ SLPrim_App_Delay at opts part_vs (jsStmtToBlock top_s) env env'
              where
                --- FIXME I think it would be better for env' to be created in compileDApp rather than here
                env = sco_env sco
                env' =
                  foldl' (\env_ (part_var, part_val) -> env_insert at part_var part_val env_) env $
                    map (second (sls_sss at)) $ -- TODO: double check this srcloc
                      zipEq at (Err_Apply_ArgCount at) top_args part_vs
                top_args = parseJSArrowFormals at top_formals
                part_vs = map make_part parts
                make_part v =
                  case v of
                    SLV_Tuple p_at [SLV_Bytes bs_at bs, SLV_Object iat _ io] ->
                      case "_" `B.isPrefixOf` bs of
                        True -> expect_throw bs_at (Err_App_PartUnderscore bs)
                        False -> public $ SLV_Participant p_at bs (makeInteract iat bs io) Nothing Nothing
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
          case allowed_to_wait ctxt at st $ args of
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
    SLForm_unknowable ->
      case st_mode st of
        SLM_Step -> do
          let (notter_e, snd_part, mmsg_e) =
                case args of
                  [x, y] -> (x, y, Nothing)
                  [x, y, z] -> (x, y, Just z)
                  _ -> illegal_args 2
          let (knower_e, whats_e) = jsCallLike at snd_part
          SLRes lifts_m st_m mmsg <-
            case mmsg_e of
              Just x -> do
                SLRes lifts_x st_x msgsv <- evalExpr ctxt at sco st x
                let msgv = ensure_public at msgsv
                return $ SLRes lifts_x st_x $ Just $ mustBeBytes at msgv
              Nothing -> return $ SLRes mempty st Nothing
          SLRes lifts_n st_n (_, v_n) <- evalExpr ctxt at sco st_m notter_e
          let participant_who = \case
                SLV_Participant _ who _ _ _ -> who
                v -> expect_throw at $ Err_Unknowable_NotParticipant v
          let notter = participant_who v_n
          SLRes lifts_kn st_kn (_, v_kn) <- evalExpr ctxt at sco st_n knower_e
          let knower = participant_who v_kn
          let sco_knower = sco {sco_env = sco_lookup_penv ctxt sco knower}
          let st_whats = st {st_mode = SLM_LocalStep}
          SLRes lifts_whats _ whats_sv <- evalExprs ctxt at sco_knower st_whats whats_e
          let whats_v = map snd whats_sv
          let whats_da = map snd $ map (typeOf at) whats_v
          let ct = CT_Unknowable notter
          let lifts' = return $ DLS_Let at Nothing (DLE_Claim at (ctxt_stack ctxt) ct (DLA_Tuple whats_da) mmsg)
          let lifts = lifts_m <> lifts_n <> lifts_kn <> lifts_whats <> lifts'
          return $ SLRes lifts st_kn $ public $ SLV_Null at "unknowable"
        cm ->
          expect_throw at $ Err_Eval_IllegalMode cm $ "unknowable"
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
    DIV -> nn2n (div)
    MOD -> nn2n (mod)
    PLT -> nn2b (<)
    PLE -> nn2b (<=)
    PEQ -> nn2b (==)
    PGE -> nn2b (>=)
    PGT -> nn2b (>)
    IF_THEN_ELSE ->
      case args of
        [SLV_Bool _ b, t, f] -> static $ if b then t else f
        _ -> make_var
    BYTES_EQ ->
      case args of
        [SLV_Bytes _ x, SLV_Bytes _ y] ->
          static $ SLV_Bool at $ x == y
        _ -> make_var
    -- FIXME fromIntegral may overflow the Int
    LSH -> nn2n (\a b -> shift a (fromIntegral b))
    RSH -> nn2n (\a b -> shift a (fromIntegral $ b * (-1)))
    BAND -> nn2n (.&.)
    BIOR -> nn2n (.|.)
    BXOR -> nn2n (xor)
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

explodeTupleLike :: SLCtxt s -> SrcLoc -> String -> SLVal -> ST s (DLStmts, [SLVal])
explodeTupleLike ctxt at lab tuplv =
  case tuplv of
    SLV_Tuple _ vs ->
      return (mempty, vs)
    SLV_Array _ _ vs ->
      return (mempty, vs)
    SLV_DLVar tupdv@(DLVar _ _ (T_Tuple tuptys) _) ->
      mconcatMap (uncurry (flip (mkdv tupdv DLE_TupleRef))) $ zip [0 ..] tuptys
    SLV_DLVar tupdv@(DLVar _ _ (T_Array t sz) _) -> do
      let mkde _ da i = DLE_ArrayRef at da (DLA_Con $ DLC_Int i)
      mconcatMap (mkdv tupdv mkde t) [0 .. sz -1]
    _ ->
      expect_throw at $ Err_Eval_NotSpreadable tuplv
  where
    mkdv tupdv mkde t i = do
      let de = mkde at (DLA_Var tupdv) i
      let mdv = DLVar at (ctxt_local_name ctxt lab) t
      (dv, lifts) <- ctxt_lift_expr ctxt at mdv de
      return $ (lifts, [SLV_DLVar dv])

doFluidRef :: SLCtxt s -> SrcLoc -> SLState -> FluidVar -> SLComp s SLSVal
doFluidRef ctxt at st fv =
  case st_mode st of
    SLM_Module ->
      expect_throw at $ Err_Eval_IllegalMode (st_mode st) "fluid ref"
    _ -> do
      let fvt = fluidVarType fv
      dv <- ctxt_mkvar ctxt (DLVar at (ctxt_local_name ctxt "fluid") fvt)
      let lifts = return $ DLS_FluidRef at dv fv
      return $ SLRes lifts st $ public $ SLV_DLVar dv

doFluidSet :: SrcLoc -> FluidVar -> SLSVal -> DLStmts
doFluidSet at fv ssv = return $ DLS_FluidSet at fv da
  where
    da = checkType at (fluidVarType fv) sv
    sv = ensure_public at ssv

doAssertBalance :: SLCtxt s -> SrcLoc -> SLScope -> SLState -> SLVal -> PrimOp -> ST s DLStmts
doAssertBalance ctxt at sco st lhs op = do
  let cmp_rator = SLV_Prim $ SLPrim_PrimDelay at (SLPrim_op op) [(Public, lhs)] []
  SLRes fr_lifts fr_st balance_v <- doFluidRef ctxt at st FV_balance
  SLRes cmp_lifts _ (SLAppRes _ cmp_v) <- evalApplyVals ctxt at sco fr_st cmp_rator [balance_v]
  let ass_rator = SLV_Prim $ SLPrim_claim CT_Assert
  SLRes res_lifts _ _ <-
    keepLifts (fr_lifts <> cmp_lifts) $
      evalApplyVals ctxt at sco st ass_rator [cmp_v, public $ SLV_Bytes at "balance assertion"]
  return res_lifts

doArrayBoundsCheck :: SLCtxt s -> SrcLoc -> SLScope -> SLState -> Integer -> SLVal -> SLComp s a -> SLComp s a
doArrayBoundsCheck ctxt at sco st sz idxv m = do
  SLRes cmp_lifts _ (SLAppRes _ cmp_v) <-
    evalApplyVals ctxt at sco st (SLV_Prim $ SLPrim_op PLT) [ public idxv, public $ SLV_Int at sz ]
  SLRes check_lifts _ _ <-
    evalApplyVals ctxt at sco st (SLV_Prim $ SLPrim_claim CT_Assert) $
      [ cmp_v, public $ SLV_Bytes at "array bounds check" ]
  let lifts = cmp_lifts <> check_lifts
  keepLifts lifts $ m

doBalanceUpdate :: SLCtxt s -> SrcLoc -> SLScope -> SLState -> PrimOp -> SLVal -> SLComp s ()
doBalanceUpdate ctxt at sco st op rhs = do
  let up_rator = SLV_Prim $ SLPrim_PrimDelay at (SLPrim_op op) [] [(Public, rhs)]
  SLRes fr_lifts fr_st balance_v <- doFluidRef ctxt at st FV_balance
  SLRes lifts st' (SLAppRes _ balance_v') <-
    evalApplyVals ctxt at sco fr_st up_rator [balance_v]
  let fs_lifts = doFluidSet at FV_balance balance_v'
  return $ SLRes (fr_lifts <> lifts <> fs_lifts) st' ()

mustBeBytes :: SrcLoc -> SLVal -> B.ByteString
mustBeBytes at v =
  case v of
    SLV_Bytes _ x -> x
    _ -> expect_throw at $ Err_Expected_Bytes v

evalPrim :: SLCtxt s -> SrcLoc -> SLScope -> SLState -> SLPrimitive -> [SLSVal] -> SLComp s SLSVal
evalPrim ctxt at sco st p sargs =
  case p of
    SLPrim_fluid_read fv ->
      doFluidRef ctxt at st fv
    SLPrim_op op ->
      evalPrimOp ctxt at sco st op sargs
    SLPrim_Fun ->
      case map snd sargs of
        [(SLV_Tuple _ dom_arr), (SLV_Type rng)] ->
          retV $ (lvl, SLV_Type $ T_Fun dom rng)
          where
            dom = map expect_ty dom_arr
        _ -> illegal_args
    SLPrim_is_type ->
      case one_arg of
        SLV_Type _ ->
          retV $ (lvl, SLV_Bool at True)
        _ ->
          retV $ (lvl, SLV_Bool at False)
    SLPrim_type_eq ->
      case map snd sargs of
        [(SLV_Type ty1), (SLV_Type ty2)] ->
          retV $ (lvl, SLV_Bool at (ty1 == ty2))
        _ -> illegal_args
    SLPrim_typeOf ->
      case map snd sargs of
        [(SLV_Type ty)] ->
          retV $ (lvl, SLV_Type (T_Type ty))
        [val] -> retV $ (lvl, SLV_Type ty)
          where
            (ty, _) = typeOf at val
        _ -> illegal_args
    SLPrim_Array ->
      case map snd sargs of
        [(SLV_Type ty), (SLV_Int _ sz)] ->
          retV $ (lvl, SLV_Type $ T_Array ty sz)
        _ -> illegal_args
    SLPrim_tuple_length -> do
      let a = one_arg
      case a of
        SLV_Tuple _ vs ->
          retV $ public $ SLV_Int at $ fromIntegral $ length vs
        SLV_DLVar (DLVar _ _ (T_Tuple ts) _) ->
          retV $ public $ SLV_Int at $ fromIntegral $ length ts
        _ -> illegal_args
    SLPrim_array_length -> do
      let a = one_arg
      case a of
        SLV_Array _ _ vs ->
          retV $ public $ SLV_Int at $ fromIntegral $ length vs
        SLV_DLVar (DLVar _ _ (T_Array _ sz) _) ->
          retV $ public $ SLV_Int at $ fromIntegral $ sz
        _ -> illegal_args
    SLPrim_Array_iota ->
      case map snd sargs of
        [SLV_Int _ sz] ->
          retV $ (lvl, SLV_Array at T_UInt256 $ map (SLV_Int at) [0 .. (sz -1)])
        _ -> illegal_args
    SLPrim_array ->
      case map snd sargs of
        [(SLV_Type elem_ty), elems_v] ->
          case elems_v of
            SLV_Tuple _ elem_vs ->
              retV $ (lvl, SLV_Array at elem_ty elem_vs_checked)
              where
                elem_vs_checked = map check1 elem_vs
                check1 sv = checkType at elem_ty sv `seq` sv
            --- FIXME we could support turning a DL Tuple into an array.
            _ -> illegal_args
        _ -> illegal_args
    SLPrim_array_concat ->
      case map snd sargs of
        [SLV_Array x_at x_ty x_vs, SLV_Array y_at y_ty y_vs] ->
          retV $ (lvl, SLV_Array at (typeMeet at (x_at, x_ty) (y_at, y_ty)) $ x_vs ++ y_vs)
        [x, y] ->
          case (typeOf at x, typeOf at y) of
            ((T_Array x_ty x_sz, xa), (T_Array y_ty y_sz, ya)) -> do
              let t = (T_Array (typeMeet at (at, x_ty) (at, y_ty)) (x_sz + y_sz))
              let mkdv = (DLVar at (ctxt_local_name ctxt "array_concat") t)
              let de = DLE_ArrayConcat at xa ya
              (dv, lifts') <- ctxt_lift_expr ctxt at mkdv de
              return $ SLRes lifts' st (lvl, SLV_DLVar dv)
            _ -> illegal_args
        _ -> illegal_args
    SLPrim_array_zip -> do
      let (x, y) = two_args
      let (xt, x_da) = typeOf at x
      let (x_ty, x_sz) = mustBeArray xt
      let (yt, y_da) = typeOf at y
      let (y_ty, y_sz) = mustBeArray yt
      let ty' = T_Tuple [x_ty, y_ty]
      unless (x_sz == y_sz) $ do
        expect_throw at $ Err_Zip_ArraysNotEqualLength x_sz y_sz
      let sz' = x_sz
      case isLiteralArray x && isLiteralArray y of
        True -> do
          (xlifts', x_vs) <- explodeTupleLike ctxt at "zip" x
          (ylifts', y_vs) <- explodeTupleLike ctxt at "zip" y
          let vs' = zipWith (\xe ye -> SLV_Tuple at [xe, ye]) x_vs y_vs
          return $ SLRes (xlifts' <> ylifts') st (lvl, SLV_Array at ty' vs')
        False -> do
          let t = T_Array ty' sz'
          let mkdv = (DLVar at (ctxt_local_name ctxt "array_zip") t)
          let de = DLE_ArrayZip at x_da y_da
          (dv, lifts') <- ctxt_lift_expr ctxt at mkdv de
          return $ SLRes lifts' st (lvl, SLV_DLVar dv)
    SLPrim_array_map ->
      case args of
        [] -> illegal_args
        [_] -> illegal_args
        [x, f] -> do
          let (xt, x_da) = typeOf at x
          let (x_ty, x_sz) = mustBeArray xt
          let f' a = evalApplyVals ctxt at sco st f [(lvl, a)]
          (a_dv, a_dsv) <- make_dlvar at "map in" x_ty
          SLRes f_lifts f_st (SLAppRes _ (f_lvl, f_v)) <- f' a_dsv
          let (f_ty, f_da) = typeOf at f_v
          let shouldUnroll = not (isPure f_lifts && isLocal f_lifts) || isLiteralArray x
          case shouldUnroll of
            True -> do
              (lifts', x_vs) <- explodeTupleLike ctxt at "map" x
              let evalem (prev_lifts, prev_vs) xv = do
                    SLRes xv_lifts xv_st (SLAppRes _ (_, xv_v')) <- f' xv
                    --- Note: We are artificially restricting maps to
                    --- be parameteric in the state.
                    return $
                      stMerge at f_st xv_st
                        `seq` ((prev_lifts <> xv_lifts), prev_vs ++ [xv_v'])
              (lifts'', vs') <- foldM evalem (mempty, []) x_vs
              return $ SLRes (lifts' <> lifts'') f_st (f_lvl, SLV_Array at f_ty vs')
            False -> do
              let t = T_Array f_ty x_sz
              (ans_dv, ans_dsv) <- make_dlvar at "array_map" t
              let lifts' = return $ DLS_ArrayMap at ans_dv x_da a_dv f_lifts f_da
              return $ SLRes lifts' st (lvl, ans_dsv)
        x : y : args' -> do
          let (f, more) = case reverse args' of
                f_ : rmore -> (f_, reverse rmore)
                _ -> impossible "array_map"
          SLRes xy_lifts xy_st (SLAppRes _ xy_v) <-
            evalApplyVals ctxt at sco st (SLV_Prim $ SLPrim_array_zip) $ map public [x, y]
          let clo_args = concatMap ((",c" <>) . show) [0 .. (length more - 1)]
          let f' = jsClo at "zip" ("(ab" <> clo_args <> ") => f(ab[0], ab[1]" <> clo_args <> ")") (M.fromList [("f", f)])
          SLRes m_lifts m_st (SLAppRes _ m_v) <-
            evalApplyVals ctxt at sco xy_st (SLV_Prim $ SLPrim_array_map) (xy_v : (map public $ more ++ [f']))
          return $ SLRes (xy_lifts <> m_lifts) m_st m_v
    SLPrim_array_reduce ->
      case args of
        [] -> illegal_args
        [_] -> illegal_args
        [_, _] -> illegal_args
        [x, z, f] -> do
          let (xt, x_da) = typeOf at x
          let (x_ty, _) = mustBeArray xt
          let f' b a = evalApplyVals ctxt at sco st f [(lvl, b), (lvl, a)]
          let (z_ty, z_da) = typeOf at z
          (b_dv, b_dsv) <- make_dlvar at "reduce acc" z_ty
          (a_dv, a_dsv) <- make_dlvar at "reduce in" x_ty
          SLRes f_lifts f_st (SLAppRes _ (f_lvl, f_v)) <- f' b_dsv a_dsv
          let (f_ty, f_da) = typeOf at f_v
          let shouldUnroll = not (isPure f_lifts && isLocal f_lifts) || isLiteralArray x
          case shouldUnroll of
            True -> do
              (lifts', x_vs) <- explodeTupleLike ctxt at "reduce" x
              let evalem (prev_lifts, prev_z) xv = do
                    SLRes xv_lifts xv_st (SLAppRes _ (_, xv_v')) <- f' prev_z xv
                    --- Note: We are artificially restricting reduce
                    --- to be parameteric in the state. We also ensure
                    --- that they type is the same as the anonymous
                    --- version.
                    return $
                      stMerge at f_st xv_st
                        `seq` checkType at f_ty xv_v'
                        `seq` ((prev_lifts <> xv_lifts), xv_v')
              (lifts'', z') <- foldM evalem (mempty, z) x_vs
              return $ SLRes (lifts' <> lifts'') f_st (f_lvl, z')
            False -> do
              (ans_dv, ans_dsv) <- make_dlvar at "array_reduce" f_ty
              let lifts' = return $ DLS_ArrayReduce at ans_dv x_da z_da b_dv a_dv f_lifts f_da
              return $ SLRes lifts' st (lvl, ans_dsv)
        x : y : args' -> do
          let (f, z, more) = case reverse args' of
                f_ : z_ : rmore -> (f_, z_, reverse rmore)
                _ -> impossible "array_reduce"
          SLRes xy_lifts xy_st (SLAppRes _ xy_v) <-
            evalApplyVals ctxt at sco st (SLV_Prim $ SLPrim_array_zip) $ map public [x, y]
          let clo_args = concatMap ((",c" <>) . show) [0 .. (length more - 1)]
          let f' = jsClo at "zip" ("(z,ab" <> clo_args <> ") => f(z, ab[0], ab[1]" <> clo_args <> ")") (M.fromList [("f", f)])
          SLRes m_lifts m_st (SLAppRes _ m_v) <-
            evalApplyVals ctxt at sco xy_st (SLV_Prim $ SLPrim_array_reduce) (xy_v : (map public $ more ++ [z, f']))
          return $ SLRes (xy_lifts <> m_lifts) m_st m_v
    SLPrim_array_set ->
      case map snd sargs of
        [arrv, idxv, valv] ->
          case typeOf at idxv of
            (T_UInt256, idxda@(DLA_Con (DLC_Int idxi))) ->
              case arrv of
                SLV_Array _ elem_ty arrvs ->
                  case idxi' < length arrvs of
                    True ->
                      retV $ (lvl, arrv')
                      where
                        arrv' = SLV_Array at elem_ty arrvs'
                        valv_checked = checkType at elem_ty valv `seq` valv
                        arrvs' = take (idxi' - 1) arrvs ++ [valv_checked] ++ drop (idxi' + 1) arrvs
                    False ->
                      expect_throw at $ Err_Eval_RefOutOfBounds (length arrvs) idxi
                SLV_DLVar arrdv@(DLVar _ _ arr_ty@(T_Array elem_ty sz) _) ->
                  case idxi < sz of
                    True ->
                      doArrayBoundsCheck ctxt at sco st sz idxv $
                        retArrDV arr_ty $ DLE_ArraySet at (DLA_Var arrdv) idxda valda
                      where
                        valda = checkType at elem_ty valv
                    False ->
                      expect_throw at $ Err_Eval_RefOutOfBounds (fromIntegral sz) idxi
                _ -> illegal_args
              where
                idxi' = fromIntegral idxi
            (T_UInt256, idxda) ->
              case typeOf at arrv of
                (arr_ty@(T_Array elem_ty sz), arrda) ->
                  doArrayBoundsCheck ctxt at sco st sz idxv $
                    retArrDV arr_ty $ DLE_ArraySet at arrda idxda valda
                  where
                    valda = checkType at elem_ty valv
                _ -> illegal_args
            _ -> illegal_args
        _ -> illegal_args
      where
        retArrDV t de = do
          (dv, lifts') <- ctxt_lift_expr ctxt at (DLVar at (ctxt_local_name ctxt "array_set") t) de
          return $ SLRes lifts' st (lvl, SLV_DLVar dv)
    SLPrim_Tuple ->
      retV $ (lvl, SLV_Type $ T_Tuple $ map expect_ty $ map snd sargs)
    SLPrim_tuple_set ->
      case map snd sargs of
        [tup, (SLV_Int _ idxi), val] -> do
          (lifts, tupvs) <- explodeTupleLike ctxt at "tuple_set" tup
          let go i v = if idxi == i then val else v
          let tupvs' = zipWith go [0 ..] tupvs
          return $ check_idxi tupvs $ SLRes lifts st $ (lvl, SLV_Tuple at tupvs')
          where
            check_idxi l r =
              if idxi < fromIntegral len then r else expect_throw at $ Err_Eval_RefOutOfBounds len idxi
              where
                len = length l
        _ -> illegal_args
    SLPrim_Object ->
      case map snd sargs of
        [(SLV_Object _ _ objm)] ->
          retV $ (lvl, SLV_Type $ T_Object $ M.map (expect_ty . sss_val) objm)
        _ -> illegal_args
    SLPrim_makeEnum ->
      case map snd sargs of
        [iv@(SLV_Int _ i)] ->
          retV $ (lvl, SLV_Tuple at' (enum_pred : map (SLV_Int at') [0 .. (i -1)]))
          where
            enum_pred = jsClo at' (ctxt_local_name ctxt "makeEnum") "(x) => ((0 <= x) && (x < M))" (M.fromList [("M", iv)])
            at' = (srcloc_at "makeEnum" Nothing at)
        _ -> illegal_args
    SLPrim_App_Delay {} ->
      expect_throw at (Err_Eval_NotApplicable rator)
    SLPrim_interact _iat who m t ->
      case st_mode st of
        SLM_LocalStep -> do
          let (rng, dargs) = checkAndConvert at t $ map snd sargs
          (dv, lifts) <- ctxt_lift_expr ctxt at (DLVar at (ctxt_local_name ctxt "interact") rng) (DLE_Interact at (ctxt_stack ctxt) who m rng dargs)
          return $ SLRes lifts st $ secret $ SLV_DLVar dv
        cm ->
          expect_throw at (Err_Eval_IllegalMode cm "interact")
    SLPrim_declassify ->
      case map snd sargs of
        [val] ->
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
      let dargs = map snd $ map ((typeOf at) . snd) sargs
      (dv, lifts) <- ctxt_lift_expr ctxt at (DLVar at (ctxt_local_name ctxt "digest") rng) (DLE_Digest at dargs)
      return $ SLRes lifts st $ (lvl, SLV_DLVar dv)
    SLPrim_claim ct ->
      case (st_mode st, ct) of
        (SLM_LocalStep, CT_Assume) -> good
        (cm, CT_Assume) -> bad cm
        (SLM_ConsensusStep, CT_Require) -> good
        (SLM_ConsensusPure, CT_Require) -> good
        (cm, CT_Require) -> bad cm
        (_, CT_Unknowable {}) -> impossible "unknowable"
        (_, CT_Assert) -> good
        (_, CT_Possible) -> good
      where
        bad cm = expect_throw at $ Err_Eval_IllegalMode cm $ "assert " ++ show ct
        good = return $ SLRes lifts st $ public $ SLV_Null at "claim"
        (darg, mmsg) = case map snd sargs of
          [arg] ->
            (checkType at T_Bool arg, Nothing)
          [arg, marg] ->
            (checkType at T_Bool arg, Just $ mustBeBytes at marg)
          _ -> illegal_args
        lifts = return $ DLS_Let at Nothing $ DLE_Claim at (ctxt_stack ctxt) ct darg mmsg
    SLPrim_transfer ->
      case ensure_publics at sargs of
        [amt_sv] ->
          return . SLRes mempty st . public $
            SLV_Object at (Just "transfer") $
              M.fromList [("to", SLSSVal srcloc_builtin Public transferToPrim)]
          where
            transferToPrim = SLV_Prim (SLPrim_transfer_amt_to amt_sv)
        _ -> illegal_args
    SLPrim_transfer_amt_to amt_sv ->
      case st_mode st of
        SLM_ConsensusStep -> do
          let amt_dla = checkType at T_UInt256 amt_sv
          tbsuff <- doAssertBalance ctxt at sco st amt_sv PLE
          SLRes balup st' () <- doBalanceUpdate ctxt at sco st SUB amt_sv
          let lifts = tbsuff <> (return $ DLS_Let at Nothing $ DLE_Transfer at who_dla amt_dla) <> balup
          return $ SLRes lifts st' $ public $ SLV_Null at "transfer.to"
          where
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
            [] -> do
              let zero = SLV_Int srcloc_builtin 0
              tbzero <- doAssertBalance ctxt at sco st zero PEQ
              let lifts = tbzero <> (return $ DLS_Stop at)
              return $ SLRes lifts st $ public $ SLV_Prim $ SLPrim_exitted
            _ -> illegal_args
        cm -> expect_throw at $ Err_Eval_IllegalMode cm "exit"
    SLPrim_exitted -> illegal_args
    SLPrim_forall {} ->
      case sargs of
        [(olvl, one)] -> do
          let t = expect_ty one
          (dv, lifts) <- ctxt_lift_expr ctxt at (DLVar at (ctxt_local_name ctxt "forall") t) (DLE_Impossible at $ "cannot inspect value from forall")
          return $ SLRes lifts st $ (olvl, SLV_DLVar dv)
        [one, (tlvl, two)] -> do
          SLRes elifts st_e one' <- evalPrim ctxt at sco st SLPrim_forall [one]
          SLRes alifts st_a (SLAppRes _ ans) <-
            evalApplyVals ctxt at sco st_e two [one']
          return $ SLRes (elifts <> alifts) st_a $ lvlMeet tlvl ans
        _ -> illegal_args
    SLPrim_PrimDelay _at dp bargs aargs ->
      evalPrim ctxt at sco st dp $ bargs <> sargs <> aargs
    SLPrim_wait ->
      case allowed_to_wait ctxt at st $ st_mode st of
        SLM_Step ->
          case sargs of
            [amt_sv] -> do
              let amt_da = checkType at T_UInt256 $ ensure_public at amt_sv
              return $ SLRes (return $ DLS_Let at Nothing (DLE_Wait at amt_da)) st $ public $ SLV_Null at "wait"
            _ -> illegal_args
        cm -> expect_throw at $ Err_Eval_IllegalMode cm "wait"
    SLPrim_part_set ->
      case map snd sargs of
        [(SLV_Participant _ who _ _ _), addr] -> do
          let addr_da = checkType at T_Address addr
          retV $ (lvl, (SLV_Prim $ SLPrim_part_setted at who addr_da))
        _ -> illegal_args
    SLPrim_part_setted {} ->
      expect_throw at (Err_Eval_NotApplicable rator)
    SLPrim_Data -> do
      let argm = case args of
            [SLV_Object _ _ m] -> m
            _ -> illegal_args
      let varm = M.map (expect_ty . sss_val) argm
      retV $ (lvl, SLV_Type $ T_Data varm)
    SLPrim_Data_variant t vn vt -> do
      let vv =
            case (vt, args) of
              (T_Null, []) -> SLV_Null at "variant"
              _ -> one_arg
      let vv_da = checkType at vt vv
      retV $ (lvl, SLV_Data at t vn $ vv_da `seq` vv)
  where
    lvl = mconcatMap fst sargs
    args = map snd sargs
    illegal_args = expect_throw at (Err_Prim_InvalidArgs p args)
    retV v = return $ SLRes mempty st v
    rator = SLV_Prim p
    expect_ty v =
      case v of
        SLV_Type t -> t
        _ -> illegal_args
    one_arg = case args of
      [x] -> x
      _ -> illegal_args
    two_args = case args of
      [x, y] -> (x, y)
      _ -> illegal_args
    _three_args = case args of
      [x, y, z] -> (x, y, z)
      _ -> illegal_args
    mustBeArray t =
      case t of
        T_Array ty sz -> (ty, sz)
        _ -> illegal_args
    make_dlvar at' lab ty = do
      dv <- ctxt_mkvar ctxt $ DLVar at' lab ty
      return $ (dv, SLV_DLVar dv)

evalApplyVals :: SLCtxt s -> SrcLoc -> SLScope -> SLState -> SLVal -> [SLSVal] -> SLComp s SLAppRes
evalApplyVals ctxt at sco st rator randvs =
  case rator of
    SLV_Prim p -> do
      SLRes lifts st' val <- evalPrim ctxt at sco st p randvs
      return $ SLRes lifts st' $ SLAppRes (sco_env sco) val
    -- TODO: have formals remember their srclocs
    SLV_Clo clo_at mname formals (JSBlock body_a body _) (SLCloEnv clo_env clo_penvs clo_cenv) -> do
      ret <- ctxt_alloc ctxt
      let body_at = srcloc_jsa "block" body_a clo_at
      let arg_env = M.fromList $ map (second (sls_sss at)) $ zipEq at (Err_Apply_ArgCount clo_at) formals randvs
      let ctxt' = ctxt_stack_push ctxt (SLC_CloApp at clo_at mname)
      let clo_sco =
            (SLScope
               { sco_ret = Just ret
               , sco_must_ret = RS_MayBeEmpty
               , sco_env = clo_env
               , sco_while_vars = Nothing
               , sco_penvs = clo_penvs
               , sco_cenv = clo_cenv
               })
      let clo_sco' = sco_update ctxt clo_at clo_sco st arg_env
      SLRes body_lifts body_st (SLStmtRes clo_env'' rs) <-
        evalStmt ctxt' body_at clo_sco' st body
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
            return $ SLRes mempty st_e $ (elvl, bunpack fb)
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
            let sv' = sls_sss at' sv
            return $ SLRes vlifts st_sv $ (flvl, env_insert_ AllowShadowing at' f sv' fenv)
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
      let mkRes lifts env =
            return $ SLRes lifts st_se $ (slvl, env_merge_ AllowShadowing at' fenv env)
      keepLifts slifts $
        case sv of
          SLV_Object _ _ senv -> mkRes mempty senv
          SLV_DLVar dlv@(DLVar _at _s (T_Object tenv) _i) -> do
            let mkOneEnv k t = do
                  let de = DLE_ObjectRef at (DLA_Var dlv) k
                  let mdv = DLVar at (ctxt_local_name ctxt "obj_ref") t
                  (dv, lifts) <- ctxt_lift_expr ctxt at mdv de
                  return $ (lifts, M.singleton k $ SLSSVal at slvl $ SLV_DLVar dv) -- TODO: double check this srcloc

            -- mconcat over SLEnvs is safe here b/c each is a singleton w/ unique key
            (lifts, env) <- mconcatMap (uncurry mkOneEnv) $ M.toList tenv
            mkRes lifts env
          _ -> expect_throw at (Err_Obj_SpreadNotObj sv)
    JSObjectMethod {} ->
      --- FIXME support these
      expect_throw at (Err_Obj_IllegalMethodDefinition p)

evalExpr :: SLCtxt s -> SrcLoc -> SLScope -> SLState -> JSExpression -> SLComp s SLSVal
evalExpr ctxt at sco st e = do
  let env = sco_env sco
  case e of
    JSIdentifier a x ->
      retV $ infectWithId x $ sss_sls $ env_lookup (srcloc_jsa "id ref" a at) x env
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
      case op of
        JSBinOpAnd a -> tern a True
        JSBinOpOr a -> tern a False
        _ ->
          doCallV st (binaryToPrim at env op) JSNoAnnot [lhs, rhs]
      where
        tern a isAnd = evalExpr ctxt at sco st $ JSExpressionTernary lhs a te a fe
          where
            (te, fe) = case isAnd of
              True -> (rhs, JSLiteral a "false")
              False -> (JSLiteral a "true", rhs)
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
            let sa = (mkAnnot tlifts) <> (mkAnnot flifts)
            case isPure sa of
              True ->
                keepLifts (tlifts <> flifts) $
                  lvlMeetR lvl $
                    evalPrim ctxt at sco st_tf (SLPrim_op $ IF_THEN_ELSE) [csv, tsv, fsv]
              False -> do
                ret <- ctxt_alloc ctxt
                let add_ret e_at' elifts ev = (e_ty, (elifts <> (return $ DLS_Return e_at' ret ev)))
                      where
                        (e_ty, _) = typeOf e_at' ev
                let (t_ty, tlifts') = add_ret t_at' tlifts tv
                let (f_ty, flifts') = add_ret f_at' flifts fv
                let ty = typeMeet at' (t_at', t_ty) (f_at', f_ty)
                let ans_dv = DLVar at' (ctxt_local_name ctxt "clo app") ty ret
                let body_lifts = return $ DLS_If at' (DLA_Var cond_dv) sa tlifts' flifts'
                let lifts' = return $ DLS_Prompt at' (Right ans_dv) body_lifts
                return $ SLRes lifts' st_tf $ (lvl, SLV_DLVar ans_dv)
          _ ->
            lvlMeetR clvl $ evalExpr ctxt t_at' sco st_c te
    JSArrowExpression aformals a bodys ->
      evalExpr ctxt at sco st e'
      where
        e' = JSFunctionExpression a JSIdentNone a fformals a body
        body = jsArrowStmtToBlock bodys
        at' = srcloc_jsa "arrow" a at
        fformals = jsArrowFormalsToFunFormals at' aformals
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
      return $ SLRes olifts st_fin $ (lvl, SLV_Object at' lab fenv)
      where
        lab = case ctxt_local_mname ctxt of
          Just [x] -> Just x
          _ -> Nothing
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
      SLRes reflifts ref_st refsv <- evalDot ctxt at' sco obj_st objv fields
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
            doArrayBoundsCheck ctxt at' sco st sz idxv $
              retRef t $ DLE_ArrayRef at' arr_dla idx_dla
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
            SLV_Array _ _ arrvs -> retVal idxi arrvs
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
    (e0 : randN) -> do
      SLRes lifts0 st0 svals0 <-
        case e0 of
          JSSpreadExpression a rand0 -> do
            let at' = srcloc_jsa "spread" a at
            SLRes l0 s0 (lvl, v0) <- evalExpr ctxt at' sco st rand0
            let addlvl v = (lvl, v)
            (lifts, vs) <- explodeTupleLike ctxt at' "spread" v0
            return $ SLRes (l0 <> lifts) s0 $ map addlvl vs
          rand0 -> do
            SLRes l0 s0 v0 <- evalExpr ctxt at sco st rand0
            return $ SLRes l0 s0 [v0]
      SLRes liftsN stN svalN <- evalExprs ctxt at sco st0 randN
      return $ SLRes (lifts0 <> liftsN) stN (svals0 <> svalN)

evalDeclLHSArray
  :: SrcLoc -> SrcLoc -> SrcLoc -> SLCtxt s -> SLEnv -> [JSArrayElement] -> ([String], SLSVal -> WriterT DLStmts (ST s) SLEnv)
evalDeclLHSArray vat' _at at' ctxt lhs_env xs = (ks', makeEnv)
  where
    ks' = ks <> maybe [] (\a -> [a]) kSpreadMay
    (ks, kSpreadMay) = parseIdentsAndSpread $ jsa_flatten xs
    parseIdentsAndSpread = \case
      [] -> ([], Nothing)
      [(JSSpreadExpression a e0)] ->
        ([], (Just $ jse_expect_id at'' e0))
        where
          at'' = srcloc_jsa "array spread" a at'
      [(JSSpreadExpression a _), _] ->
        expect_throw at'' $ Err_Decl_ArraySpreadNotLast
        where
          at'' = srcloc_jsa "array spread" a at'
      (e0 : eNs) -> ((x0 : xNs), smN)
        where
          (xNs, smN) = parseIdentsAndSpread eNs
          x0 = jse_expect_id at' e0
    makeEnv (lvl, v) = do
      (vs_lifts, vs) <- lift $ explodeTupleLike ctxt vat' "lhs array" v
      tell vs_lifts
      let ks_len = length ks
      case splitAtExactMay ks_len vs of
        Nothing ->
          expect_throw at' $ Err_Decl_WrongArrayLength ks_len (length vs)
        Just (before, after) -> do
          let add_spread =
                case kSpreadMay of
                  Nothing -> id
                  Just sn ->
                    env_insert at' sn $ SLSSVal at' lvl $ SLV_Tuple at' after -- TODO: double check this srcloc
          let kvs = zip ks $ map (\x -> SLSSVal at' lvl x) before
          -- TODO: have ks remember their own srcloc
          return $ foldl' (env_insertp at') (add_spread lhs_env) kvs

-- | const {x, y, ...obj} = ...;
--
-- Returns a tuple of:
-- * boundIdents (e.g. ["x", "y", "obj"])
-- * mkEnv, a function which
--   * accepts an SLSVal (the RHS of the decl)
--   * checks that RHS is an object that has the specified keys
--   * returns the DLStmts and SLEnv produced by this assignment
evalDeclLHSObject :: SrcLoc -> SrcLoc -> SLCtxt s -> SLEnv -> JSObjectPropertyList -> ([String], SLSVal -> WriterT DLStmts (ST s) SLEnv)
evalDeclLHSObject at at' ctxt lhs_env props = (ks', makeEnv)
  where
    ks' = ks <> maybe [] (\a -> [a]) kSpreadMay
    (ks, kSpreadMay) = parseIdentsAndSpread $ jso_flatten props
    ksSet = S.fromList ks
    parseIdentsAndSpread = \case
      [] -> ([], Nothing)
      [(JSObjectSpread a e0)] ->
        ([], (Just $ jse_expect_id at'' e0))
        where
          at'' = srcloc_jsa "object spread" a at'
      [(JSObjectSpread a _), _] ->
        expect_throw at'' $ Err_Decl_ObjectSpreadNotLast
        where
          at'' = srcloc_jsa "object spread" a at'
      (e0 : eNs) -> ((x0 : xNs), smN)
        where
          (xNs, smN) = parseIdentsAndSpread eNs
          x0 = jso_expect_id at' e0
    makeEnv (lvl, val) = case val of
      SLV_DLVar dv@(DLVar _ _ (T_Object tenv) _) -> do
        let mk_ref_ k = do
              let e = (DLE_ObjectRef at' (DLA_Var dv) k)
              let t = case M.lookup k tenv of
                    Nothing -> expect_throw at' $ Err_Dot_InvalidField val (M.keys tenv) k
                    Just x -> x
              ctxt_lift_expr_w ctxt at (DLVar at' (ctxt_local_name ctxt "object ref") t) e
        let mk_ref k = do
              dvi <- mk_ref_ k
              pure (k, SLV_DLVar dvi)
        ks_dvs <- mapM mk_ref ks
        spread_dvs <-
          case kSpreadMay of
            Nothing -> pure mempty
            Just spreadName -> do
              let mkDlArg k _t = do
                    dvi <- mk_ref_ k
                    pure $ DLA_Var dvi
              let tenvWithoutKs = M.withoutKeys tenv ksSet
              objDlEnv <- M.traverseWithKey mkDlArg tenvWithoutKs
              let de = DLE_Arg at' $ DLA_Obj objDlEnv
              let spreadTy = T_Object $ tenvWithoutKs
              let mdv = DLVar at' (ctxt_local_name ctxt "obj") spreadTy
              dlv <- ctxt_lift_expr_w ctxt at mdv de
              pure [(spreadName, SLV_DLVar dlv)]
        let lhs_env'' = foldl' (\lhs_env' (k, v) -> env_insert at' k (SLSSVal at lvl v) lhs_env') lhs_env $ ks_dvs <> spread_dvs
        -- TODO: ^ double check this srcloc
        return lhs_env''
      SLV_Object _ _ env -> pure lhs_env''
        where
          lhs_env'' = case kSpreadMay of
            Just spreadName -> env_insert at spreadName (SLSSVal at' lvl spreadObj) envWithKs
            -- TODO: ^ double check this srcloc
            Nothing -> envWithKs
          envWithKs = foldl' (\lhs_env' k -> env_insert at' k (env_lookup at' k env) lhs_env') lhs_env ks
          envWithoutKs = M.withoutKeys env ksSet
          spreadObj = SLV_Object at' Nothing envWithoutKs
      _ -> expect_throw at' (Err_Decl_NotObject val)

evalDecl :: SLCtxt s -> SrcLoc -> SLState -> SLEnv -> SLScope -> JSExpression -> SLComp s SLEnv
evalDecl ctxt at st lhs_env rhs_sco decl =
  case decl of
    JSVarInitExpression lhs (JSVarInit va rhs) -> do
      let vat' = srcloc_jsa "var initializer" va at
      let (lhs_ns, make_env) =
            case lhs of
              (JSIdentifier a x) -> ([x], _make_env)
                where
                  idAt = srcloc_jsa "id" a at
                  _make_env v = return (env_insert idAt x (sls_sss idAt v) lhs_env)
              (JSArrayLiteral a xs _) ->
                evalDeclLHSArray vat' at at' ctxt lhs_env xs
                where
                  at' = srcloc_jsa "array" a at
              (JSObjectLiteral a props _) ->
                evalDeclLHSObject vat' at' ctxt lhs_env props
                where
                  at' = srcloc_jsa "object" a at
              _ ->
                expect_throw at (Err_DeclLHS_IllegalJS lhs)
      let ctxt' = ctxt_local_name_set ctxt lhs_ns
      SLRes rhs_lifts rhs_st v <- evalExpr ctxt' vat' rhs_sco st rhs
      (lhs_env', lhs_lifts) <- runWriterT $ make_env v
      return $ SLRes (rhs_lifts <> lhs_lifts) rhs_st lhs_env'
    _ ->
      expect_throw at (Err_Decl_IllegalJS decl)

evalDecls :: SLCtxt s -> SrcLoc -> SLState -> SLScope -> (JSCommaList JSExpression) -> SLComp s SLEnv
evalDecls ctxt at st rhs_sco decls =
  foldlM f (SLRes mempty st mempty) $ jscl_flatten decls
  where
    f (SLRes lifts lhs_st lhs_env) decl =
      keepLifts lifts $ evalDecl ctxt at lhs_st lhs_env rhs_sco decl

-- | Make sure all bindings in this SLEnv respect the rule that
-- private vars must be named with a leading underscore.
enforcePrivateUnderscore :: Monad m => SrcLoc -> SLEnv -> m ()
enforcePrivateUnderscore at = mapM_ enf . M.toList
  where
    enf (k, (SLSSVal _ secLev _)) = case secLev of
      Secret
        | not (isSpecialIdent k)
            && not (isSecretIdent k) ->
          expect_throw at (Err_Eval_NotSecretIdent k)
      _ -> return ()

doOnly :: SLCtxt s -> SrcLoc -> (DLStmts, SLScope, SLState) -> (SLPart, SrcLoc, SLCloEnv, JSExpression) -> ST s (DLStmts, SLScope, SLState)
doOnly ctxt at (lifts, sco, st) (who, only_at, only_cloenv, only_synarg) = do
  let SLCloEnv only_env only_penvs only_cenv = only_cloenv
  let st_localstep = st {st_mode = SLM_LocalStep}
  let sco_only_pre =
        sco
          { sco_env = only_env
          , sco_penvs = only_penvs
          , sco_cenv = only_cenv
          }
  let penv = sco_lookup_penv ctxt sco_only_pre who
  let sco_only = sco_only_pre {sco_env = penv}
  SLRes only_lifts _ only_arg <-
    evalExpr ctxt only_at sco_only st_localstep only_synarg
  case only_arg of
    (_, only_clo@(SLV_Clo _ _ [] _ _)) -> do
      SLRes alifts _ (SLAppRes penv' (_, only_v)) <-
        evalApplyVals ctxt at (impossible "part_only expects clo") st_localstep only_clo []
      case fst $ typeOf only_at only_v of
        T_Null -> do
          --- TODO: check less things
          enforcePrivateUnderscore only_at penv'
          let penvs = sco_penvs sco
          let penvs' = M.insert who penv' penvs
          let lifts' = return $ DLS_Only only_at who (only_lifts <> alifts)
          let st' = st {st_mode = SLM_Step}
          let sco' = sco {sco_penvs = penvs'}
          return ((lifts <> lifts'), sco', st')
        ty ->
          expect_throw only_at (Err_Block_NotNull ty only_v)
    _ -> expect_throw at $ Err_Only_NotOneClosure $ snd only_arg

evalStmtTrampoline :: SLCtxt s -> JSSemi -> SrcLoc -> SLScope -> SLState -> SLSVal -> [JSStatement] -> SLComp s SLStmtRes
evalStmtTrampoline ctxt sp at sco st (_, ev) ks =
  case ev of
    SLV_Prim (SLPrim_part_setted at' who addr_da) ->
      case st_mode st of
        SLM_ConsensusStep -> do
          let pdvs = st_pdvs st
          case M.lookup who pdvs of
            Just _ ->
              expect_throw at' $ Err_Eval_PartSet_Bound who
            Nothing -> do
              let who_s = bunpack who
              (whodv, lifts) <- ctxt_lift_expr ctxt at (DLVar at' who_s T_Address) (DLE_PartSet at' who addr_da)
              let pdvs' = M.insert who whodv pdvs
              let st' = st {st_pdvs = pdvs'}
              keepLifts lifts $ evalStmt ctxt at sco st' ks
        _ -> illegal_mode
    SLV_Prim SLPrim_exitted ->
      case (st_mode st, st_live st) of
        (SLM_Step, True) -> do
          let st' = st {st_live = False}
          expect_empty_tail "exit" JSNoAnnot sp at ks $
            return $ SLRes mempty st' $ SLStmtRes env []
        _ -> illegal_mode
    SLV_Form (SLForm_EachAns parts only_at only_cloenv only_synarg) ->
      case st_mode st of
        SLM_Step -> do
          (lifts', sco', st') <-
            foldM (doOnly ctxt at) (mempty, sco, st) $
              map (\who -> (who, only_at, only_cloenv, only_synarg)) parts
          keepLifts lifts' $ evalStmt ctxt at sco' st' ks
        _ -> illegal_mode
    SLV_Form (SLForm_Part_ToConsensus to_at who vas Nothing mmsg mamt mtime) ->
      case (st_mode st, st_live st) of
        (SLM_Step, True) -> do
          let st_pure = st {st_mode = SLM_ConsensusPure}
          let pdvs = st_pdvs st
          let penv = sco_lookup_penv ctxt sco who
          (msg_env, tmsg_) <-
            case mmsg of
              Nothing -> return (mempty, [])
              Just msg -> do
                let mk var = do
                      let val =
                            case env_lookup to_at var penv of
                              (SLSSVal _ Public x) -> x
                              (SLSSVal _ Secret x) ->
                                -- TODO: use binding loc in error
                                expect_throw at $ Err_ExpectedPublic x
                      let (t, da) = typeOf to_at val
                      let m = case da of
                            DLA_Var (DLVar _ v _ _) -> v
                            _ -> "msg"
                      dv <- ctxt_mkvar ctxt $ DLVar to_at m t
                      return $ (da, dv)
                tvs <- mapM mk msg
                return $ (foldl' (env_insertp at) mempty $ zip msg $ map (sls_sss at . public . SLV_DLVar) $ map snd tvs, tvs)
          -- TODO: ^ double check this srcloc
          --- We go back to the original env from before the to-consensus step
          (pdvs', fs) <-
            case M.lookup who pdvs of
              Just pdv ->
                return $ (pdvs, FS_Again pdv)
              Nothing -> do
                let whos = bunpack who
                whodv <- ctxt_mkvar ctxt $ DLVar to_at whos T_Address
                return $ ((M.insert who whodv pdvs), FS_Join whodv)
          let add_who_env :: SLEnv -> SLEnv =
                case vas of
                  Nothing -> \x -> x
                  Just whov ->
                    case env_lookup to_at whov env of
                      (SLSSVal idAt lvl_ (SLV_Participant at_ who_ io_ as_ _)) ->
                        M.insert whov (SLSSVal idAt lvl_ (SLV_Participant at_ who_ io_ as_ (Just $ (pdvs' M.! who))))
                      _ ->
                        impossible $ "participant is not participant"
          --- NOTE we don't use sco_update, because we have to treat the publish specially
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
          amt_dv <- ctxt_mkvar ctxt $ DLVar at "amt" T_UInt256
          SLRes amt_check_lifts _ _ <- do
            --- XXX Merge with doAssertBalance somehow
            let cmp_rator = SLV_Prim $ SLPrim_PrimDelay at (SLPrim_op PEQ) [(Public, SLV_DLVar amt_dv)] []
            SLRes cmp_lifts _ cmp_v <- evalApply ctxt at sco_env' st_pure cmp_rator [amte]
            let req_rator = SLV_Prim $ SLPrim_claim CT_Require
            keepLifts cmp_lifts $
              evalApplyVals ctxt at sco_env' st_pure req_rator [cmp_v, public $ SLV_Bytes at $ "pay amount correct"]
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
                  , st_after_first = True
                  }
          let sco' =
                sco
                  { sco_env = env'
                  , sco_cenv = env'
                  , sco_penvs = penvs'
                  }
          SLRes balup st_cstep' () <- doBalanceUpdate ctxt at sco' st_cstep ADD (SLV_DLVar amt_dv)
          SLRes conlifts k_st k_cr <- evalStmt ctxt at sco' st_cstep' ks
          let lifts' = tlifts <> amt_compute_lifts <> (return $ DLS_ToConsensus to_at who fs (map fst tmsg_) (map snd tmsg_) amt_da amt_dv mtime' (amt_check_lifts <> balup <> conlifts))
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
          let st_step = st {st_mode = SLM_Step}
          SLRes steplifts k_st cr <- evalStmt ctxt at sco st_step ks
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
          keepLifts (return $ DLS_Let at Nothing $ DLE_Claim at (ctxt_stack ctxt) CT_Assert (DLA_Con $ DLC_Bool False) (Just "unreachable")) $
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
      SLRes lifts st_const addl_env <- evalDecls ctxt at_in st sco decls
      let sco' = sco_update ctxt at_in sco st addl_env
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
      where
        at' = srcloc_jsa "fun" a at
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
            let sa = (mkAnnot tlifts) <> (mkAnnot flifts)
            let lifts' = return $ DLS_If at' (DLA_Var cond_dv) sa tlifts flifts
            let levelHelp = SLStmtRes (sco_env sco) . map (\(r_at, (r_lvl, r_v)) -> (r_at, (clvl <> r_lvl, r_v)))
            let ir = SLRes lifts' st_tf $ combineStmtRes at' clvl (levelHelp trets) (levelHelp frets)
            retSeqn ir at' ks_ne
          _ -> do
            nr <- evalStmt ctxt t_at' sco' st_c [ts]
            retSeqn nr at' ks_ne
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
              let whilem =
                    case sco_while_vars sco of
                      Nothing -> expect_throw cont_at $ Err_Eval_ContinueNotInWhile
                      Just x -> x
              let cont_dam =
                    M.fromList $ map f $ M.toList decl_env
                    where
                      f (v, sv) = (dv, da)
                        where
                          dv = case M.lookup v whilem of
                            Nothing ->
                              expect_throw var_at $ Err_Eval_ContinueNotLoopVariable v
                            Just x -> x
                          val = ensure_public var_at $ sss_sls sv
                          da = checkType at et val
                          DLVar _ _ et _ = dv
              SLRes fr_lifts _ balance_v <-
                doFluidRef ctxt cont_at st_decl FV_balance
              let balance_da =
                    checkType cont_at T_UInt256 $
                      ensure_public cont_at balance_v
              let unknown_balance_dv = whilem M.! internalVar_balance
              let cont_dam' =
                    M.insert unknown_balance_dv balance_da cont_dam
              let cont_das = DLAssignment cont_dam'
              let lifts' =
                    decl_lifts <> fr_lifts
                      <> (return $ DLS_Continue cont_at cont_das)
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
    (JSSwitch a _ de _ _ cases _ sp : ks) -> do
      let at' = srcloc_jsa "switch" a at
      let de_v = jse_expect_id at' de
      let env = sco_env sco
      let (de_lvl, de_val) = sss_sls $ env_lookup at' de_v env
      let (de_ty, _) = typeOf at de_val
      let varm = case de_ty of
            T_Data m -> m
            _ -> expect_throw at $ Err_Switch_NotData de_val
      let ks_ne = dropEmptyJSStmts ks
      let sco' =
            case ks_ne of
              [] -> sco
              _ -> sco {sco_must_ret = RS_MayBeEmpty}
      let case_insert k v@(at1, _, _) m =
            case M.lookup k m of
              Nothing -> M.insert k v m
              Just (at0, _, _) -> expect_throw at $ Err_Switch_DoubleCase at0 at1 (Just k)
      let case_minserts cs v m = M.unions $ m : map (flip M.singleton v) cs
      let add_case (seenDefault, casem0) = \case
            JSCase ca ve _ body -> (seenDefault, case_insert vn (at_c, True, body) casem0)
              where
                at_c = srcloc_jsa "case" ca at'
                vn = jse_expect_id at_c ve
            JSDefault ca _ body ->
              case seenDefault of
                Just at_c' -> expect_throw at $ Err_Switch_DoubleCase at_c at_c' Nothing
                Nothing -> ((Just at_c), case_minserts (M.keys varm) (at_c, False, body) casem0)
              where
                at_c = srcloc_jsa "case" ca at'
      let (_, casesm) = foldl' add_case (Nothing, mempty) cases
      let all_cases = M.keysSet varm
      let given_cases = M.keysSet casesm
      let missing_cases = all_cases S.\\ given_cases
      unless (S.null missing_cases) $ do
        expect_throw at' $ Err_Switch_MissingCases $ S.toList missing_cases
      let extra_cases = given_cases S.\\ all_cases
      unless (S.null extra_cases) $ do
        expect_throw at' $ Err_Switch_ExtraCases $ S.toList extra_cases
      let select at_c body mvv = do
            let addl_env = case mvv of
                  Just vv -> M.singleton de_v (sls_sss at_c (de_lvl, vv))
                  Nothing -> mempty
            let sco'' = sco_update_ AllowShadowing ctxt at_c sco' st addl_env
            evalStmt ctxt at_c sco'' st body
      let select_one vn (at_c, shouldBind, body) = do
            (mdv', mvv) <-
              case shouldBind of
                True -> do
                  let vt = varm M.! vn
                  case vt of
                    T_Null ->
                      return (Nothing, Just $ SLV_Null at_c "case")
                    _ -> do
                      dv' <- ctxt_mkvar ctxt $ DLVar at_c ("switch " <> vn) vt
                      return (Just dv', Just $ SLV_DLVar dv')
                False ->
                  return (Nothing, Nothing)
            return $ (mdv', at_c, select at_c body mvv)
      let select_all dv = do
            let casemm = M.mapWithKey select_one casesm
            let cmb (mst', sa', mrets', casemm') (vn, casem) = do
                  (mdv', at_c, casem') <- casem
                  SLRes case_lifts case_st (SLStmtRes _ case_rets) <- casem'
                  let st'' = case mst' of
                        Nothing -> case_st
                        Just st' -> stMerge at_c st' case_st
                  let sa'' = sa' <> mkAnnot case_lifts
                  let rets'' = case mrets' of
                        Nothing -> case_rets
                        Just rets' -> combineStmtRets at_c de_lvl rets' case_rets
                  let casemm'' = M.insert vn (mdv', case_lifts) casemm'
                  return $ (Just st'', sa'', Just rets'', casemm'')
            (mst', sa', mrets', casemm') <- foldM cmb (Nothing, mempty, Nothing, mempty) $ M.toList casemm
            let rets' = maybe mempty id mrets'
            let lifts' = return $ DLS_Switch at dv sa' casemm'
            return $ SLRes lifts' (maybe st id mst') $ SLStmtRes mempty rets'
      fr <-
        case de_val of
          SLV_Data _ _ vn vv ->
            select at_c body mvv
            where
              (at_c, shouldBind, body) = (casesm M.! vn)
              mvv =
                case shouldBind of
                  False -> Nothing
                  True -> Just vv
          SLV_DLVar dv -> select_all dv
          _ -> impossible "switch mvar"
      let at'_after = srcloc_after_semi "switch" a sp at
      retSeqn fr at'_after ks_ne
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
                SLRes init_lifts st_var vars_env_ <-
                  evalDecls ctxt var_at st sco while_decls
                SLRes fr_lifts _ balance_sv <-
                  doFluidRef ctxt var_at st_var FV_balance
                let balance_v = sls_sss var_at balance_sv
                let vars_env =
                      --- XXX This could be broken with multiple loops
                      env_insert var_at internalVar_balance balance_v vars_env_
                let while_help v sv = do
                      let (SLSSVal _ _ val) = sv
                      let (t, da) = typeOf var_at val
                      dv <- ctxt_mkvar ctxt $ DLVar var_at v t
                      return $ (dv, da)
                while_helpm <- M.traverseWithKey while_help vars_env
                let unknown_var_env = M.map (sls_sss var_at . public . SLV_DLVar . fst) while_helpm
                let unknown_bal_v = sss_sls $ unknown_var_env M.! internalVar_balance
                let bal_lifts = doFluidSet at FV_balance unknown_bal_v
                let st_var' = stEnsureMode at SLM_ConsensusStep st_var
                let st_pure = st_var' {st_mode = SLM_ConsensusPure}
                let sco_env' = sco_update ctxt at sco st_var' unknown_var_env
                SLRes inv_lifts_ _ inv_da <-
                  case jscl_flatten invariant_args of
                    [invariant_e] ->
                      checkResType inv_at T_Bool $ evalExpr ctxt inv_at sco_env' st_pure invariant_e
                    ial -> expect_throw inv_at $ Err_While_IllegalInvariant ial
                let inv_lifts = bal_lifts <> inv_lifts_
                let fs = ctxt_stack ctxt
                let inv_b = DLBlock inv_at fs inv_lifts inv_da
                SLRes cond_lifts _ cond_da <-
                  checkResType cond_at T_Bool $ evalExpr ctxt cond_at sco_env' st_pure while_cond
                let cond_b = DLBlock cond_at fs cond_lifts cond_da
                let while_sco =
                      sco_env'
                        { sco_while_vars = Just $ M.map fst while_helpm
                        , sco_must_ret = RS_NeedExplicit
                        }
                SLRes body_lifts_ body_st (SLStmtRes _ body_rets) <-
                  evalStmt ctxt while_at while_sco st_var' [while_body]
                let body_lifts = bal_lifts <> body_lifts_
                let while_dam = M.fromList $ M.elems while_helpm
                let the_while =
                      DLS_While var_at (DLAssignment while_dam) inv_b cond_b body_lifts
                let st_post = stMerge at body_st st_var'
                SLRes k_lifts k_st (SLStmtRes k_env' k_rets) <-
                  evalStmt ctxt while_at sco_env' st_post ks
                let lifts' =
                      init_lifts <> fr_lifts <> (return $ the_while)
                        <> bal_lifts
                        <> k_lifts
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
    (s@(JSWith a _ oe _ body sp) : ks) ->
      case True of
        True -> illegal a s "with"
        False -> do
          --- Because of the inlining-nature of Reach functions, this
          --- exposes too much of a function's definition and breaks
          --- the abstraction.
          let at' = srcloc_jsa "with stmt" a at
          SLRes o_lifts o_st (olvl, ov) <- evalExpr ctxt at' sco st oe
          let mk_o_env' = evalAsEnv at' ov
          let eval1 (SLRes lifts0 st0 env1) (f, getv) = do
                SLRes lifts1 st1 val <- getv ctxt sco st0
                return $ SLRes (lifts0 <> lifts1) st1 $ M.insert f val env1
          SLRes env_lifts env_st o_env' <- foldM eval1 (SLRes mempty o_st mempty) $ M.toList mk_o_env'
          let o_env = M.map (sls_sss at . lvlMeet olvl) o_env'
          let sco' = sco_update ctxt at' sco o_st o_env
          SLRes body_lifts body_st (SLStmtRes _ body_rets) <- evalStmt ctxt at' sco' env_st [body]
          let at_after = srcloc_after_semi "with stmt" a sp at
          SLRes k_lifts k_st (SLStmtRes k_env' k_rets) <- evalStmt ctxt at_after sco body_st ks
          return $ SLRes (o_lifts <> env_lifts <> body_lifts <> k_lifts) k_st $ SLStmtRes k_env' (body_rets <> k_rets)
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
          return $ SLRes (lifts0 <> lifts1) st1 (SLStmtRes env1 (rets0 <> rets1))

combineStmtRes :: SrcLoc -> SecurityLevel -> SLStmtRes -> SLStmtRes -> SLStmtRes
combineStmtRes at' lvl (SLStmtRes _ lrets) (SLStmtRes env rrets) =
  SLStmtRes env $ combineStmtRets at' lvl lrets rrets

combineStmtRets :: SrcLoc -> SecurityLevel -> SLStmtRets -> SLStmtRets -> SLStmtRets
combineStmtRets at' lvl lrets rrets =
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

type SLLibs = (M.Map ReachSource SLEnv)

lookupDep :: ReachSource -> SLLibs -> SLEnv
lookupDep rs libm =
  case M.lookup rs libm of
    Just x -> x
    Nothing -> impossible $ "dependency not found"

evalFromClause :: SLLibs -> JSFromClause -> SLEnv
evalFromClause libm (JSFromClause _ _ libn) =
  lookupDep (ReachSourceFile libn) libm

evalImExportSpecifiers :: SrcLoc -> SLEnv -> (a -> (JSIdent, JSIdent)) -> (JSCommaList a) -> SLEnv
evalImExportSpecifiers at env go cl =
  foldl' (env_insertp at) mempty $ map (uncurry p) $ map go $ jscl_flatten cl
  where
    p f t = p' (parseIdent at f) (parseIdent at t)
    p' (_, f) (_, t) = (t, env_lookup at f env)

evalImportClause :: SrcLoc -> SLEnv -> JSImportClause -> SLEnv
evalImportClause at env im =
  case im of
    JSImportClauseNameSpace (JSImportNameSpace _ _ ji) ->
      M.singleton ns $ (SLSSVal at' Public $ SLV_Object at' (Just $ "module " <> ns) env)
      where
        (at', ns) = parseIdent at ji
    JSImportClauseNamed (JSImportsNamed _ iscl _) ->
      evalImExportSpecifiers at env go iscl
      where
        go = \case
          JSImportSpecifier x -> (x, x)
          JSImportSpecifierAs x _ y -> (x, y)
    JSImportClauseDefault {} -> illegal_import
    JSImportClauseDefaultNameSpace {} -> illegal_import
    JSImportClauseDefaultNamed {} -> illegal_import
  where
    illegal_import = expect_throw at (Err_Import_IllegalJS im)

evalExportClause :: SrcLoc -> SLEnv -> JSExportClause -> SLEnv
evalExportClause at env (JSExportClause _ escl _) =
  evalImExportSpecifiers at env go escl
  where
    go = \case
      JSExportSpecifier x -> (x, x)
      JSExportSpecifierAs x _ y -> (x, y)

evalTopBody :: SLCtxt s -> SrcLoc -> SLState -> SLLibs -> SLEnv -> SLEnv -> [JSModuleItem] -> SLComp s SLEnv
evalTopBody ctxt at st libm env exenv body =
  case body of
    [] -> return $ SLRes mempty st exenv
    mi : body' ->
      case mi of
        (JSModuleImportDeclaration a im) ->
          case im of
            JSImportDeclarationBare _ libn _ ->
              evalTopBody ctxt at' st libm env' exenv body'
              where
                env' = env_merge at' env libex
                libex = lookupDep (ReachSourceFile libn) libm
            JSImportDeclaration ic fc _ ->
              evalTopBody ctxt at' st libm env' exenv body'
              where
                env' = env_merge at' env news
                news = evalImportClause at' ienv ic
                ienv = evalFromClause libm fc
          where
            at' = srcloc_jsa "import" a at
        (JSModuleExportDeclaration a ex) ->
          case ex of
            JSExport s _ -> doStmt at' True s
            JSExportFrom ec fc _ -> go ec (evalFromClause libm fc)
            JSExportLocals ec _ -> go ec env
          where
            at' = srcloc_jsa "export" a at
            go ec eenv =
              evalTopBody ctxt at' st libm env (env_merge at' exenv news) body'
              where
                news = evalExportClause at' eenv ec
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
            SLRes lifts _ (SLStmtRes env' []) ->
              let exenv' = case isExport of
                    True ->
                      --- If this is an exporting statement,
                      --- then add to the export environment
                      --- everything that is new.
                      env_merge at' exenv (M.difference env' env)
                    False ->
                      exenv
               in keepLifts lifts $
                    evalTopBody ctxt at' st libm env' exenv' body'
            SLRes {} ->
              expect_throw at' $ Err_Module_Return

type SLMod = (ReachSource, [JSModuleItem])

evalLib :: STCounter s -> SLMod -> (DLStmts, SLLibs) -> ST s (DLStmts, SLLibs)
evalLib idxr (src, body) (liblifts, libm) = do
  let at = srcloc_src src
  let st =
        SLState
          { st_mode = SLM_Module
          , st_live = False
          , st_pdvs = mempty
          , st_after_first = False
          }
  let ctxt_top =
        (SLCtxt
           { ctxt_dlo = app_default_opts
           , ctxt_id = idxr
           , ctxt_stack = []
           , ctxt_local_mname = Nothing
           , ctxt_base_penvs = mempty
           })
  let stdlib_env =
        case src of
          ReachStdLib -> base_env
          ReachSourceFile _ -> M.union (libm M.! ReachStdLib) base_env
  let (prev_at, body') =
        case body of
          ((JSModuleStatementListItem (JSExpressionStatement (JSStringLiteral a hs) sp)) : j)
            | (trimQuotes hs) == versionHeader ->
              ((srcloc_after_semi "header" a sp at), j)
          _ -> expect_throw at (Err_NoHeader body)
  SLRes more_lifts _ exenv <-
    evalTopBody ctxt_top prev_at st libm stdlib_env mt_env body'
  return $ (liblifts <> more_lifts, M.insert src exenv libm)

evalLibs :: STCounter s -> [SLMod] -> ST s (DLStmts, SLLibs)
evalLibs idxr mods = foldrM (evalLib idxr) (mempty, mempty) mods

makeInteract :: SrcLoc -> SLPart -> SLEnv -> SLVal
makeInteract at who spec = SLV_Object at lab spec'
  where
    lab = Just $ (bunpack who) <> "'s interaction interface"
    spec' = M.mapWithKey wrap_ty spec
    wrap_ty k (SLSSVal idAt Public (SLV_Type t)) = case isFirstOrder t of
      True -> sls_sss idAt $ secret $ SLV_Prim $ SLPrim_interact at who k t
      False -> expect_throw at $ Err_App_Interact_NotFirstOrder t
    -- TODO: add idAt info to the err below?
    wrap_ty _ v = expect_throw at $ Err_App_InvalidInteract $ sss_sls v

app_default_opts :: DLOpts
app_default_opts =
  DLOpts {dlo_deployMode = DM_constructor}

app_options :: M.Map SLVar (DLOpts -> SLVal -> Either String DLOpts)
app_options = M.fromList [("deployMode", opt_deployMode)]
  where
    opt_deployMode opts v =
      case v of
        SLV_Bytes _ "firstMsg" -> up DM_firstMsg
        SLV_Bytes _ "constructor" -> up DM_constructor
        SLV_Bytes _ bs -> Left $ bss <> " is not a deployMode" <> didYouMean bss ["firstMsg", "constructor"] 2
          where
            bss = bunpack bs
        _ -> Left $ "expected bytes"
      where
        up m = Right $ opts {dlo_deployMode = m}

compileDApp :: STCounter s -> DLStmts -> SLVal -> ST s DLProg
compileDApp idxr liblifts topv =
  case topv of
    SLV_Prim (SLPrim_App_Delay at opts partvs (JSBlock _ top_ss _) top_env top_env_wps) -> do
      let dlo = M.foldrWithKey use_opt app_default_opts (M.map sss_val opts)
            where
              use_opt k v acc =
                case M.lookup k app_options of
                  Nothing ->
                    expect_throw at $ Err_App_InvalidOption k (S.toList $ M.keysSet app_options)
                  Just opt ->
                    case opt acc v of
                      Right x -> x
                      Left x ->
                        expect_throw at $ Err_App_InvalidOptionValue k x
      let st_step =
            SLState
              { st_mode = SLM_Step
              , st_live = True
              , st_pdvs = mempty
              , st_after_first = False
              }
      let ctxt =
            SLCtxt
              { ctxt_dlo = dlo
              , ctxt_id = idxr
              , ctxt_stack = []
              , ctxt_local_mname = Nothing
              , ctxt_base_penvs = penvs
              }
      let sco =
            SLScope
              { sco_ret = Nothing
              , sco_must_ret = RS_CannotReturn
              , sco_env = top_env_wps
              , sco_while_vars = Nothing
              , sco_penvs = penvs
              , sco_cenv = mempty
              }
      let bal_lifts = doFluidSet at' FV_balance $ public $ SLV_Int at' 0
      SLRes final st_final _ <- evalStmt ctxt at' sco st_step top_ss
      tbzero <- doAssertBalance ctxt at sco st_final (SLV_Int at' 0) PEQ
      return $ DLProg at dlo sps (liblifts <> bal_lifts <> final <> tbzero)
      where
        at' = srcloc_at "compileDApp" Nothing at
        sps = SLParts $ M.fromList $ map make_sps_entry partvs
        make_sps_entry (Public, (SLV_Participant _ pn (SLV_Object _ _ io) _ _)) =
          (pn, InteractEnv $ M.map getType io)
          where
            getType (SLSSVal _ _ (SLV_Prim (SLPrim_interact _ _ _ t))) = t
            getType x = impossible $ "make_sps_entry getType " ++ show x
        make_sps_entry x = impossible $ "make_sps_entry " ++ show x
        penvs = M.fromList $ map make_penv partvs
        make_penv (Public, (SLV_Participant _ pn io _ _)) =
          (pn, env_insert at' "interact" (sls_sss at' $ secret io) top_env) -- TODO: double check this srcloc
        make_penv _ = impossible "SLPrim_App_Delay make_penv"
    _ ->
      expect_throw srcloc_top (Err_Top_NotApp topv)

compileBundleST :: JSBundle -> SLVar -> ST s DLProg
compileBundleST (JSBundle mods) top = do
  idxr <- newSTCounter 0
  (liblifts, libm) <- evalLibs idxr mods
  let exe_ex = libm M.! exe
  let topv = case M.lookup top exe_ex of
        Just (SLSSVal _ Public x) -> x
        Just _ ->
          impossible "private before dapp"
        Nothing ->
          expect_throw srcloc_top (Err_Eval_UnboundId top $ M.keys exe_ex)
  compileDApp idxr liblifts topv
  where
    exe = case mods of
      [] -> impossible $ "compileBundle: no files"
      ((x, _) : _) -> x

compileBundle :: JSBundle -> SLVar -> DLProg
compileBundle jsb top =
  runST $ compileBundleST jsb top
