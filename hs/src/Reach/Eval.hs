module Reach.Eval (EvalError, compileBundle) where

import Control.Arrow (second)
import Control.Monad
import Control.Monad.ST
import Data.Bits
import qualified Data.ByteString as B
import Data.Foldable
import Data.List (intercalate, sortBy)
import Data.List.Extra (mconcatMap)
import qualified Data.Map.Strict as M
import Data.Ord (comparing)
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Data.Text as T
import GHC.Stack (HasCallStack)
import Generics.Deriving
import Language.JavaScript.Parser
import Language.JavaScript.Parser.AST
import Reach.AST
import Reach.Connector
import Reach.JSUtil
import Reach.Parser
import Reach.STCounter
import Reach.Type
import Reach.Util
import Reach.Version
import Safe (atMay)
import Text.EditDistance (defaultEditCosts, restrictedDamerauLevenshteinDistance)
import Text.ParserCombinators.Parsec.Number (numberValue)

import Reach.Pretty()
-- import Reach.Texty
-- import Debug.Trace

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
  | Err_Decl_NotType String SLVal
  | Err_Decls_IllegalJS (JSCommaList JSExpression)
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
  | Err_Eval_UnboundId LookupCtx SLVar [SLVar]
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
  | Err_NotParticipant SLVal
  | Err_Transfer_NotBound SLPart
  | Err_Eval_IncompatibleStates SLState SLState
  | Err_Eval_NotSecretIdent SLVar
  | Err_Eval_NotPublicIdent SLVar
  | Err_Eval_LookupUnderscore
  | Err_Eval_NotSpreadable SLVal
  | Err_Zip_ArraysNotEqualLength Integer Integer
  | Err_Switch_NotData SLVal
  | Err_Switch_DoubleCase SrcLoc SrcLoc (Maybe SLVar)
  | Err_Switch_MissingCases [SLVar]
  | Err_Switch_ExtraCases [SLVar]
  | Err_Expected_Bytes SLVal
  | Err_RecursionDepthLimit
  | Err_ParallelReduce_Incomplete ParallelReduceMode
  | Err_ParallelReduce_NoInvariant
  | Err_ParallelReduce_InvalidBody String
  | Err_Eval_MustBeLive String
  | Err_Invalid_Statement String
  | Err_Fork_NoCases
  | Err_Fork_IllegalActor SLPart SLPart
  deriving (Eq, Generic)

--- FIXME I think most of these things should be in Pretty

displaySlValType :: SLVal -> String
displaySlValType = \case
  SLV_Participant _ who _ _ ->
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
  T_UInt -> "uint256"
  T_Bytes sz -> "bytes[" <> show sz <> "]"
  T_Digest -> "digest"
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

showDiff :: Eq b => a -> a -> (a -> b) -> (b -> b -> String) -> String
showDiff x y f s =
  let fx = f x
      fy = f y
   in case fx == fy of
        True -> ""
        False -> "\n  * " <> s fx fy

getCorrectGrammer :: Foldable t => t a -> p -> p -> p
getCorrectGrammer xs sing plur = case length xs of
  1 -> sing
  _ -> plur

showStateDiff :: SLState -> SLState -> String
showStateDiff x y =
  "\nThe expected state of the program varies between branches, because:"
  <> showDiff x y st_mode (\ xMode yMode ->
    unwords ["Expected to be in ", show yMode, ", but in ", show xMode <> "."])
  <> showDiff x y st_live (\ xLive yLive ->
      case (xLive, yLive) of
        (False, True) -> "Expected there to be live state."
        (True, False) -> "Expected there to be no live state."
        _             -> impossible "expected st_live to differ."
    )
  <> showDiff x y st_after_first (\ xAfter yAfter ->
      case (xAfter, yAfter) of
        (False, True) -> "Expected a publication to have been made by this point."
        (True, False) -> "Expected no publication to have been made by this point."
        _             -> impossible "expected st_after_first to differ."
    )
  <> showDiff x y st_pdvs (\ xParts yParts ->
      let showParts = intercalate ", " . map (show . fst) . M.toList in
      let actual = case length xParts of
                    0 -> getCorrectGrammer yParts "it hasn't." "they haven't."
                    _ -> unwords ["only", showParts xParts, getCorrectGrammer xParts "has." "have."]
      in
      unwords ["Expected", showParts yParts, "to have published a message or been set, but", actual]
    )
  <> showDiff x y st_fork_body (\ xFB yFB ->
      case (xFB, yFB) of
        (Nothing, Nothing) -> impossible "nothing"
        (Nothing, Just _) -> "Expected there to be a fork case body."
        (Just _, Nothing) -> "Expected there to be no fork case body."
        (Just xWho, Just yWho) -> "Expected " <> show yWho <> " fork case body, but found " <> show xWho
    )

-- TODO more hints on why invalid syntax is invalid
instance Show EvalError where
  show = \case
    Err_Zip_ArraysNotEqualLength x y ->
      "Zip requires arrays of equal length, but got " <> show x <> " and " <> show y
    Err_Apply_ArgCount cloAt nFormals nArgs ->
      "Invalid function application. Expected " <> show nFormals <> " args, got " <> show nArgs <> " for function defined at " <> show cloAt
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
    Err_Decls_IllegalJS _ ->
      "Invalid Reach declaration; expected exactly one declaration"
    Err_Decl_IllegalJS e ->
      "Invalid Reach declaration: " <> conNameOf e
    Err_Decl_ObjectSpreadNotLast ->
      "Object spread on left-hand side of binding must occur in last position"
    Err_Decl_ArraySpreadNotLast ->
      "Array spread on left-hand side of binding must occur in last position"
    Err_Decl_NotType ty slval ->
      "Invalid binding. Expected " <> ty <> ", got: " <> displaySlValType slval
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
    Err_Eval_UnboundId (LC_RefFrom ctx) slvar slvars ->
      "Invalid unbound identifier in " <> ctx <> ": " <> slvar <> didYouMean slvar slvars 5
    Err_Eval_UnboundId LC_CompilerRequired slvar _ ->
      "Expected the following identifier to be declared: " <> show slvar
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
        ty = displaySlValType slval
        reason = case take 5 ty of
          "bytes" -> "It must be computable at compile time."
          _ -> "Fields must be bytes, but got: " <> ty
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
    Err_NotParticipant slval ->
      "expected a participant as an argument, instead got " <> displaySlValType slval
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
    Err_RecursionDepthLimit ->
      "recursion depth limit exceeded, more than " <> show recursionDepthLimit <> " calls; who would need more than that many?"
    Err_ParallelReduce_Incomplete m ->
      "parallel reduce, call incomplete; ending in the midst of " <> show m
    Err_ParallelReduce_NoInvariant ->
      "parallel reduce, missing invariant"
    Err_ParallelReduce_InvalidBody m ->
      "parallel reduce, invalid body: " <> m
    Err_Eval_MustBeLive m ->
      "must be live at " <> m
    Err_Invalid_Statement stmt ->
      "Invalid use of statement: " <> stmt <> ". Did you mean to wrap it in a thunk?"
    Err_Fork_NoCases ->
      "fork had no cases"
    Err_Fork_IllegalActor ewho awho ->
      "fork body must start with " <> show awho <> " action, but instead had " <> show ewho

--- Utilities
zipEq :: Show e => Maybe (SLCtxt s) -> SrcLoc -> (Int -> Int -> e) -> [a] -> [b] -> [(a, b)]
zipEq ctxt at ce x y =
  if lx == ly
    then zip x y
    else expect_throw (ctxt_stack <$> ctxt) at (ce lx ly)
  where
    lx = length x
    ly = length y

ensure_public :: SLCtxt s -> SrcLoc -> SLSVal -> SLVal
ensure_public ctxt at (lvl, v) =
  case lvl of
    Public -> v
    Secret ->
      expect_throw_ctx ctxt at $ Err_ExpectedPublic v

ensure_publics :: SLCtxt s -> SrcLoc -> [SLSVal] -> [SLVal]
ensure_publics ctxt at svs = map (ensure_public ctxt at) svs

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
env_insert_ :: EnvInsertMode -> SLCtxt s -> SrcLoc -> SLVar -> SLSSVal -> SLEnv -> SLEnv
env_insert_ _ _ _ "_" _ env = env
env_insert_ insMode ctxt at k v env = case insMode of
  DisallowShadowing ->
    --- XXX This is a hack
    case k == internalVar_balance of
      True -> go
      False ->
        case M.lookup k env of
          Nothing -> go
          Just v0 -> expect_throw_ctx ctxt at (Err_Shadowed k v0 v)
  AllowShadowing -> go
  where
    go = case v of
      -- Note: secret ident enforcement is limited to doOnly
      (SLSSVal _ Public _)
        | not (isSpecialIdent k) && isSecretIdent k ->
          expect_throw_ctx ctxt at (Err_Eval_NotPublicIdent k)
      _ -> M.insert k v env

env_insert :: SLCtxt s -> SrcLoc -> SLVar -> SLSSVal -> SLEnv -> SLEnv
env_insert = env_insert_ DisallowShadowing

env_insertp_ :: EnvInsertMode -> SLCtxt s -> SrcLoc -> SLEnv -> (SLVar, SLSSVal) -> SLEnv
env_insertp_ imode ctxt at = flip (uncurry (env_insert_ imode ctxt at))

env_insertp :: SLCtxt s -> SrcLoc -> SLEnv -> (SLVar, SLSSVal) -> SLEnv
env_insertp = env_insertp_ DisallowShadowing

env_merge_ :: EnvInsertMode -> SLCtxt s -> SrcLoc -> SLEnv -> SLEnv -> SLEnv
env_merge_ imode ctxt at left righte = foldl' (env_insertp_ imode ctxt at) left $ M.toList righte

env_merge :: SLCtxt s -> SrcLoc -> SLEnv -> SLEnv -> SLEnv
env_merge = env_merge_ DisallowShadowing

data LookupCtx
  -- Signifies the user referencing a variable from a (ctx :: String).
  = LC_RefFrom String
  -- Signifies the compiler expecting a certain id to exist.
  | LC_CompilerRequired
  deriving (Eq, Show)

-- | The "_" ident may never be looked up.
env_lookup :: Maybe (SLCtxt s) -> SrcLoc -> LookupCtx -> SLVar -> SLEnv -> SLSSVal
env_lookup ctxt at _ "_" _ = expect_throw (Just $ maybe [] ctxt_stack ctxt) at Err_Eval_LookupUnderscore
env_lookup ctxt at ctx x env =
  case M.lookup x env of
    Just v -> v
    Nothing -> expect_throw (Just $ maybe [] ctxt_stack ctxt) at $
                Err_Eval_UnboundId ctx x $ M.keys $ M.filter (not . isKwd) env

isKwd :: SLSSVal -> Bool
isKwd (SLSSVal _ _ (SLV_Kwd _)) = True
isKwd _ = False

m_fromList_public_builtin :: [(SLVar, SLVal)] -> SLEnv
m_fromList_public_builtin = m_fromList_public srcloc_builtin

base_env :: SLEnv
base_env =
  m_fromList_public_builtin $
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
    , ("Digest", SLV_Type T_Digest)
    , ("Null", SLV_Type T_Null)
    , ("Bool", SLV_Type T_Bool)
    , ("UInt", SLV_Type T_UInt)
    , ("Bytes", SLV_Prim SLPrim_Bytes)
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
    , --    , ("bytesEq", SLV_Prim $ SLPrim_op BYTES_EQ)
      ("digestEq", SLV_Prim $ SLPrim_op DIGEST_EQ)
    , ("addressEq", SLV_Prim $ SLPrim_op ADDRESS_EQ)
    , ("isType", SLV_Prim SLPrim_is_type)
    , ("typeEq", SLV_Prim SLPrim_type_eq)
    , ("typeOf", SLV_Prim SLPrim_typeOf)
    , ("wait", SLV_Prim SLPrim_wait)
    , ("race", SLV_Prim SLPrim_race)
    , ("fork", SLV_Form SLForm_fork)
    , ("parallel_reduce", SLV_Form SLForm_parallel_reduce)
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
    -- Add language keywords to env to prevent variables from using names.
    <> map (\ t -> (show t, SLV_Kwd t)) allKeywords

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

-- Contexts
--- A context has global stuff (the variable counter) and abstracts
--- the control-flow state that leads to the expression, so it
--- inherits in a function call.
data SLCtxt s = SLCtxt
  { ctxt_dlo :: DLOpts
  , ctxt_id :: STCounter s
  , ctxt_stack :: [SLCtxtFrame]
  , ctxt_base_penvs :: SLPartEnvs
  }

instance Show (SLCtxt s) where
  show _ = "<context>"

expect_throw_ctx :: HasCallStack => Show a => SLCtxt s -> SrcLoc -> a -> b
expect_throw_ctx ctxt = expect_throw (Just $ ctxt_stack ctxt)

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

-- Arg Expr -> Arg
compileArgExpr :: SLCtxt s -> SrcLoc -> DLArgExpr -> ST s (DLStmts, DLArg)
compileArgExpr ctxt at = \case
  DLAE_Arg a -> return $ (mempty, a)
  DLAE_Array t aes -> do
    (ss, as) <- compileArgExprs ctxt at aes
    mk ss $ DLLA_Array t as
  DLAE_Tuple aes -> do
    (ss, as) <- compileArgExprs ctxt at aes
    mk ss $ DLLA_Tuple as
  DLAE_Obj me -> do
    (ss, m) <- compileArgExprMap ctxt at me
    mk ss $ DLLA_Obj m
  DLAE_Data t v ae -> do
    (ss, a) <- compileArgExpr ctxt at ae
    mk ss $ DLLA_Data t v a
  where
    mk ss la = do
      let t = largeArgTypeOf la
      let mkvar = DLVar at "large arg" t
      (dv, ss') <- ctxt_lift_expr ctxt at mkvar (DLE_LArg at la)
      return (ss <> ss', DLA_Var dv)

compileArgExprs :: SLCtxt s -> SrcLoc -> [DLArgExpr] -> ST s (DLStmts, [DLArg])
compileArgExprs ctxt at aes = do
  aes' <- mapM (compileArgExpr ctxt at) aes
  let ss = mconcat $ map fst aes'
  let as = map snd aes'
  return (ss, as)

compileArgExprMap :: SLCtxt s -> SrcLoc -> M.Map a DLArgExpr -> ST s (DLStmts, M.Map a DLArg)
compileArgExprMap ctxt at m = do
  m' <- mapM (compileArgExpr ctxt at) m
  let ss = mconcat $ map fst $ M.elems m'
  let m'' = M.map snd m'
  return (ss, m'')

-- General compiler utilities
slvParticipant_part :: SLCtxt s -> SrcLoc -> SLVal -> SLPart
slvParticipant_part ctxt at = \case
  SLV_Participant _ x _ _ -> x
  x -> expect_throw_ctx ctxt at $ Err_NotParticipant x

compileCheckAndConvert :: SLCtxt s -> SrcLoc -> SLType -> [SLVal] -> ST s (DLStmts, SLType, [DLArg])
compileCheckAndConvert ctxt at t argvs = do
  let (res, arges) = checkAndConvert at t argvs
  (lifts, args) <- compileArgExprs ctxt at arges
  return (lifts, res, args)

compileTypeOf :: SLCtxt s -> SrcLoc -> SLVal -> ST s (DLStmts, SLType, DLArg)
compileTypeOf ctxt at v = do
  let (t, dae) = typeOf at v
  (lifts, da) <- compileArgExpr ctxt at dae
  return (lifts, t, da)

compileTypeOfs :: SLCtxt s -> SrcLoc -> [SLVal] -> ST s (DLStmts, [SLType], [DLArg])
compileTypeOfs ctxt at vs = do
  let (ts, daes) = unzip $ map (typeOf at) vs
  (lifts, das) <- compileArgExprs ctxt at daes
  return (lifts, ts, das)

compileCheckType :: SLCtxt s -> SrcLoc -> SLType -> SLVal -> ST s (DLStmts, DLArg)
compileCheckType ctxt at et v = do
  let ae = checkType at et v
  compileArgExpr ctxt at ae

checkResType :: SLCtxt a -> SrcLoc -> SLType -> SLComp a SLSVal -> SLComp a DLArg
checkResType ctxt at et m = do
  SLRes lifts st (_lvl, v) <- m
  (lifts', a) <- compileCheckType ctxt at et v
  return $ SLRes (lifts <> lifts') st a

-- Modes

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
  , st_fork_body :: Maybe SLPart
  }
  deriving (Eq, Show)

ensure_mode :: SLCtxt s -> SrcLoc -> SLState -> SLMode -> String -> ST s ()
ensure_mode ctxt at st em msg = do
  let am = st_mode st
  case am == em of
    True -> return ()
    False ->
      --- XXX use this function every where and shown em
      expect_throw_ctx ctxt at $ Err_Eval_IllegalMode am msg

ensure_part_can_act :: SLCtxt s -> SrcLoc -> SLState -> SLPart -> ST s ()
ensure_part_can_act ctxt at st who = do
  case st_fork_body st of
    Nothing -> return ()
    Just fb_who ->
      case who == fb_who of
        True -> return ()
        False ->
          expect_throw_ctx ctxt at $ Err_Fork_IllegalActor fb_who who

ensure_live :: SLCtxt s -> SrcLoc -> SLState -> String -> ST s ()
ensure_live ctxt at st msg = do
  case st_live st of
    True -> return ()
    False ->
      expect_throw_ctx ctxt at $ Err_Eval_MustBeLive msg

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
          expect_throw_ctx ctxt at $ Err_Eval_IllegalWait dm

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
  , sco_depth :: Int
  }

recursionDepthLimit :: Int
recursionDepthLimit = 2 ^ (16 :: Int)

sco_depth_update :: SLCtxt s -> SrcLoc -> SLScope -> Int
sco_depth_update ctxt at (SLScope {..}) =
  case x > 0 of
    True -> x
    False -> expect_throw_ctx ctxt at Err_RecursionDepthLimit
  where
    x = sco_depth - 1

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
    do_merge = flip (env_merge_ imode ctxt at) addl_env

sco_update :: SLCtxt s -> SrcLoc -> SLScope -> SLState -> SLEnv -> SLScope
sco_update = sco_update_ DisallowShadowing

stMerge :: SLCtxt s -> SrcLoc -> SLState -> SLState -> SLState
stMerge ctxt at x y =
  case x == y of
    True -> y
    False -> expect_throw_ctx ctxt at $ Err_Eval_IncompatibleStates x y

stMerges :: SLCtxt s -> SrcLoc -> [SLState] -> SLState
stMerges ctxt at = \case
  [] -> impossible $ "stMerges called with empty"
  [x] -> x
  x : ys -> stMerge ctxt at x $ stMerges ctxt at ys

stEnsureMode :: SLCtxt s -> SrcLoc -> SLMode -> SLState -> SLState
stEnsureMode ctxt at slm st =
  stMerge ctxt at st $ st {st_mode = slm}

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

type SLStmtRets = [(SrcLoc, Maybe Int, SLSVal)]

data SLStmtRes = SLStmtRes SLEnv SLStmtRets

data SLAppRes = SLAppRes SLEnv SLSVal

ctxt_stack_push :: SLCtxt s -> SLCtxtFrame -> SLCtxt s
ctxt_stack_push ctxt f =
  (ctxt {ctxt_stack = f : (ctxt_stack ctxt)})

binaryToPrim :: SLCtxt s -> SrcLoc -> SLEnv -> JSBinOp -> SLVal
binaryToPrim ctxt at env o =
  case o of
    JSBinOpAnd _ -> impossible "and"
    JSBinOpDivide a -> prim a (DIV)
    JSBinOpEq a -> fun a "polyEq" "=="
    JSBinOpGe a -> prim a (PGE)
    JSBinOpGt a -> prim a (PGT)
    JSBinOpLe a -> prim a (PLE)
    JSBinOpLt a -> prim a (PLT)
    JSBinOpMinus a -> prim a (SUB)
    JSBinOpMod a -> prim a (MOD)
    JSBinOpNeq a -> fun a "polyNeq" "!="
    JSBinOpOr _ -> impossible "or"
    JSBinOpPlus a -> prim a (ADD)
    JSBinOpStrictEq a -> fun a "polyEq" "==="
    JSBinOpStrictNeq a -> fun a "polyNeq" "!=="
    JSBinOpTimes a -> prim a (MUL)
    JSBinOpLsh a -> prim a (LSH)
    JSBinOpRsh a -> prim a (RSH)
    JSBinOpBitAnd a -> prim a (BAND)
    JSBinOpBitOr a -> prim a (BIOR)
    JSBinOpBitXor a -> prim a (BXOR)
    j -> expect_thrown at $ Err_Parse_IllegalBinOp j
  where
    fun a s ctx = sss_val $ env_lookup (Just ctxt) (srcloc_jsa "binop" a at) (LC_RefFrom ctx) s env
    prim _a p = SLV_Prim $ SLPrim_op p

unaryToPrim :: SLCtxt s -> SrcLoc -> SLEnv -> JSUnaryOp -> SLVal
unaryToPrim ctxt at env o =
  case o of
    JSUnaryOpMinus a -> fun a "minus" "-"
    JSUnaryOpNot a -> fun a "not" "!"
    JSUnaryOpTypeof a -> fun a "typeOf" "typeOf"
    j -> expect_thrown at $ Err_Parse_IllegalUnaOp j
  where
    fun a s ctx = sss_val $ env_lookup (Just ctxt) (srcloc_jsa "unop" a at) (LC_RefFrom ctx) s env

infectWithId :: SLVar -> SLSVal -> SLSVal
infectWithId v (lvl, sv) = (lvl, sv')
  where
    sv' =
      case sv of
        SLV_Participant at who _ mdv ->
          SLV_Participant at who (Just v) mdv
        _ -> sv

type SLObjEnvRHS s = SLCtxt s -> SLScope -> SLState -> SLComp s SLSVal

type SLObjEnv s = M.Map SLVar (SLObjEnvRHS s)

evalObjEnv :: forall s. SLCtxt s -> SrcLoc -> SLScope -> SLState -> SLObjEnv s -> SLComp s SLEnv
evalObjEnv ctxt at sco st0 m =
  foldM go (SLRes mempty st0 mempty) $ M.toList m
  where
    go :: SLRes SLEnv -> (SLVar, SLObjEnvRHS s) -> SLComp s SLEnv
    go (SLRes lifts st env) (f, getv) = do
      SLRes lifts' st' v <- getv ctxt sco st
      let env' = M.insert f (sls_sss at v) env
      return $ SLRes (lifts <> lifts') st' env'

evalAsEnv :: SLCtxt s -> SrcLoc -> SLVal -> SLObjEnv s
evalAsEnv ctx at obj =
  case obj of
    SLV_Object _ _ env ->
      M.map (retV . sss_sls) env
    SLV_DLVar obj_dv@(DLVar _ _ (T_Object tm) _) ->
      retDLVar tm (DLA_Var obj_dv) Public
    SLV_Prim (SLPrim_interact _ who m it@(T_Object tm)) ->
      retDLVar tm (DLA_Interact who m it) Secret
    SLV_Participant _ who vas _ ->
      M.fromList
        [ ("only", retV $ public $ SLV_Form (SLForm_Part_Only who vas))
        , ("publish", retV $ public $ SLV_Form (SLForm_Part_ToConsensus at who vas (Just TCM_Publish) Nothing Nothing Nothing))
        , ("pay", retV $ public $ SLV_Form (SLForm_Part_ToConsensus at who vas (Just TCM_Pay) Nothing Nothing Nothing))
        , ("set", delayCall SLPrim_part_set)
        ]
    SLV_Form (SLForm_Part_ToConsensus to_at who vas Nothing mpub mpay mtime) ->
      M.fromList $
        gom "publish" TCM_Publish mpub <>
        gom "pay" TCM_Pay mpay <>
        gom "timeout" TCM_Timeout mtime
      where
        gom key mode me =
          case me of
            Nothing -> go key mode
            Just _ -> []
        go key mode =
          [ (key, retV $ public $ SLV_Form (SLForm_Part_ToConsensus to_at who vas (Just mode) mpub mpay mtime))]
    SLV_Form (SLForm_fork_partial False caseses) ->
      M.fromList $ [ ("case", retV $ public $ SLV_Form $ SLForm_fork_partial True caseses) ]
    SLV_Form (SLForm_parallel_reduce_partial inite Nothing minve mtimeout muntile casees) ->
      M.fromList $
        gom "invariant" PRM_Invariant minve <>
        gom "timeout" PRM_Timeout mtimeout <>
        gom "until" PRM_Until muntile <>
        go "case" PRM_Case
      where
        gom key mode me =
          case me of
            Nothing -> go key mode
            Just _ -> []
        go key mode =
          [ (key, retV $ public $ SLV_Form $ SLForm_parallel_reduce_partial inite (Just mode) minve mtimeout muntile casees) ]
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
        , ("forEach", doStdlib "Array_forEach1")
        , ("map", delayCall SLPrim_array_map)
        , ("reduce", delayCall SLPrim_array_reduce)
        , ("zip", delayCall SLPrim_array_zip)
        ]
    SLV_DLVar (DLVar _ _ (T_Array _ _) _) ->
      M.fromList
        [ ("set", delayCall SLPrim_array_set)
        , ("length", doCall SLPrim_array_length)
        , ("concat", delayCall SLPrim_array_concat)
        , ("forEach", doStdlib "Array_forEach1")
        , ("map", delayCall SLPrim_array_map)
        , ("reduce", delayCall SLPrim_array_reduce)
        , ("zip", delayCall SLPrim_array_zip)
        ]
    SLV_Data {} ->
      M.fromList
        [
          ("match", delayCall SLPrim_data_match)
        ]
    SLV_DLVar (DLVar _ _ (T_Data _) _) ->
      M.fromList
        [ ("match", delayCall SLPrim_data_match)
        ]
    SLV_Prim SLPrim_Array ->
      M.fromList
        [ ("empty", retStdLib "Array_empty")
        , ("forEach", retStdLib "Array_forEach")
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
    SLV_Type T_UInt ->
      M.fromList
        [("max", retV $ public $ SLV_DLC DLC_UInt_max)]
    SLV_Type (T_Data varm) ->
      M.mapWithKey (\k t -> retV $ public $ SLV_Prim $ SLPrim_Data_variant varm k t) varm
    v ->
      expect_throw_ctx ctx at (Err_Eval_NotObject v)
  where
    delayCall p =
      retV $ public $ SLV_Prim $ SLPrim_PrimDelay at p [(public obj)] []
    doStdlib n ctxt sco st =
      doApply (lookStdlib n sco) ctxt sco st
    lookStdlib n sco = sss_val $ env_lookup (Just ctx) at (LC_RefFrom "stdlib") n $ sco_env sco
    doCall p = doApply $ SLV_Prim p
    doApply f ctxt sco st = do
      SLRes lifts st' (SLAppRes _ v) <- evalApplyVals ctxt at sco st f [(public obj)]
      return $ SLRes lifts st' v
    retDLVar tm obj_dla slvl =
      M.mapWithKey retk tm
      where
        retk field t ctxt _sco st = do
          let mkv = DLVar at "object ref" t
          let e = DLE_ObjectRef at obj_dla field
          (dv, lifts') <- ctxt_lift_expr ctxt at mkv e
          let ansv = SLV_DLVar dv
          return $ SLRes lifts' st (slvl, ansv)
    retV sv _ctxt _sco st =
      return $ SLRes mempty st sv
    retStdLib n ctxt sco st =
      retV (public $ lookStdlib n sco) ctxt sco st

evalDot_ :: SLCtxt s -> SrcLoc -> SLScope -> SLState -> SLVal -> SLObjEnv s -> String -> SLComp s SLSVal
evalDot_ ctxt at sco st obj env field = do
  case M.lookup field env of
    Just gv -> gv ctxt sco st
    Nothing -> expect_throw_ctx ctxt at $ Err_Dot_InvalidField obj (M.keys env) field

evalDot :: SLCtxt s -> SrcLoc -> SLScope -> SLState -> SLVal -> String -> SLComp s SLSVal
evalDot ctxt at sco st obj field = do
  let env = evalAsEnv ctxt at obj
  evalDot_ ctxt at sco st obj env field

evalForm :: SLCtxt s -> SrcLoc -> SLScope -> SLState -> SLForm -> [JSExpression] -> SLComp s SLSVal
evalForm ctxt at sco st f args =
  case f of
    SLForm_App ->
      case args of
        [opte, partse, JSArrowExpression top_formals _ top_s] -> do
          sargs <- cannotLift "App args" <$> evalExprs ctxt at sco st [opte, partse]
          case map snd sargs of
            [(SLV_Object _ _ opts), (SLV_Tuple _ parts)] ->
              retV $ public $ SLV_Prim $ SLPrim_App_Delay at opts parts (parseJSArrowFormals at top_formals) top_s (sco_env sco)
            _ -> expect_throw_ctx ctxt at (Err_App_InvalidArgs args)
        _ -> expect_throw_ctx ctxt at (Err_App_InvalidArgs args)
    SLForm_Part_Only who mv ->
      return $ SLRes mempty st $ public $ SLV_Form $ SLForm_EachAns [(who, mv)] at (sco_to_cloenv sco) one_arg
    SLForm_Part_ToConsensus to_at who vas mmode mpub mpay mtime ->
      case mmode of
        Just TCM_Publish ->
          case mpub of
            Nothing -> retV $ public $ SLV_Form $ SLForm_Part_ToConsensus to_at who vas Nothing (Just msg) mpay mtime
              where
                msg = map (jse_expect_id at) args
            Just _ ->
              expect_throw_ctx ctxt at $ Err_ToConsensus_Double TCM_Publish
        Just TCM_Pay ->
          retV $ public $ SLV_Form $ SLForm_Part_ToConsensus to_at who vas Nothing mpub (Just one_arg) mtime
        Just TCM_Timeout ->
          case allowed_to_wait ctxt at st $ args of
            [de, JSArrowExpression (JSParenthesizedArrowParameterList _ JSLNil _) _ dt_s] ->
              retV $ public $ SLV_Form $ SLForm_Part_ToConsensus to_at who vas Nothing mpub mpay (Just (at, de, (jsStmtToBlock dt_s)))
            _ -> expect_throw_ctx ctxt at $ Err_ToConsensus_TimeoutArgs args
        Nothing ->
          expect_throw_ctx ctxt at $ Err_Eval_NotApplicable rator
    SLForm_each -> do
      let (partse, thunke) = two_args
      SLRes part_lifts part_st (_, parts_v) <- evalExpr ctxt at sco st partse
      case parts_v of
        SLV_Tuple _ part_vs -> do
          let parts =
                map
                  (\case
                     SLV_Participant _ who mv _ -> (who, mv)
                     v -> expect_throw_ctx ctxt at $ Err_NotParticipant v)
                  part_vs
          return $ SLRes part_lifts part_st $ public $ SLV_Form $ SLForm_EachAns parts at (sco_to_cloenv sco) thunke
        _ ->
          expect_throw_ctx ctxt at $ Err_Each_NotTuple parts_v
    -- This case occurs when the "result" of `each` is used as an expression.
    SLForm_EachAns _ e_at _ _ -> expect_throw_ctx ctxt e_at $ Err_Invalid_Statement "each/only"
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
                let msgv = ensure_public ctxt at msgsv
                return $ SLRes lifts_x st_x $ Just $ mustBeBytes ctxt at msgv
              Nothing -> return $ SLRes mempty st Nothing
          SLRes lifts_n st_n (_, v_n) <- evalExpr ctxt at sco st_m notter_e
          let participant_who = \case
                SLV_Participant _ who _ _ -> who
                v -> expect_throw_ctx ctxt at $ Err_NotParticipant v
          let notter = participant_who v_n
          SLRes lifts_kn st_kn (_, v_kn) <- evalExpr ctxt at sco st_n knower_e
          let knower = participant_who v_kn
          let sco_knower = sco {sco_env = sco_lookup_penv ctxt sco knower}
          let st_whats = st {st_mode = SLM_LocalStep}
          SLRes lifts_whats _ whats_sv <-
            evalExprs ctxt at sco_knower st_whats whats_e
          let whats_vs = map snd whats_sv
          (whats_lifts, _, whats_das) <-
            compileTypeOfs ctxt at whats_vs
          let whats_da = DLA_Literal $ DLL_Bool False
          let ct = CT_Unknowable notter whats_das
          let lifts' =
                return $
                  DLS_Let at Nothing $
                    DLE_Claim at (ctxt_stack ctxt) ct whats_da mmsg
          let lifts =
                lifts_m <> lifts_n <> lifts_kn <>
                lifts_whats <> whats_lifts <> lifts'
          return $ SLRes lifts st_kn $ public $ SLV_Null at "unknowable"
        cm ->
          expect_throw_ctx ctxt at $ Err_Eval_IllegalMode cm $ "unknowable"
    SLForm_fork -> do
      zero_args
      retV $ public $ SLV_Form $
        SLForm_fork_partial False []
    SLForm_fork_partial False _ ->
      expect_throw_ctx ctxt at $ Err_Eval_NotApplicable $ SLV_Form f
    SLForm_fork_partial True caseses ->
      retV $ public $ SLV_Form $ SLForm_fork_partial False $ caseses <> [ (at, two_args) ]
    SLForm_parallel_reduce ->
      retV $ public $ SLV_Form $
        SLForm_parallel_reduce_partial one_arg Nothing Nothing Nothing Nothing []
    SLForm_parallel_reduce_partial inite mmode minve mtimeoute muntile casees ->
      case mmode of
        Nothing ->
          expect_throw_ctx ctxt at $ Err_Eval_NotApplicable $ SLV_Form f
        Just PRM_Invariant ->
          retV $ public $ SLV_Form $
            SLForm_parallel_reduce_partial inite Nothing (Just one_arg) mtimeoute muntile casees
        Just PRM_Timeout ->
          retV $ public $ SLV_Form $
            SLForm_parallel_reduce_partial inite Nothing minve (Just one_arg) muntile casees
        Just PRM_Until ->
          retV $ public $ SLV_Form $
            SLForm_parallel_reduce_partial inite Nothing minve mtimeoute (Just one_arg) casees
        Just PRM_Case ->
          retV $ public $ SLV_Form $
            SLForm_parallel_reduce_partial inite Nothing minve mtimeoute muntile $
              casees ++ [(at, two_args)]
  where
    illegal_args n = expect_throw_ctx ctxt at (Err_Form_InvalidArgs f n args)
    rator = SLV_Form f
    retV v = return $ SLRes mempty st v
    zero_args = case args of
      [] -> return ()
      _ -> illegal_args 0
    one_arg = case args of
      [x] -> x
      _ -> illegal_args 1
    two_args = case args of
      [x, y] -> (x, y)
      _ -> illegal_args 2

evalPrimOp :: SLCtxt s -> SrcLoc -> SLScope -> SLState -> PrimOp -> [SLSVal] -> SLComp s SLSVal
evalPrimOp ctxt at _sco st p sargs =
  case p of
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
        [SLV_Bool _ b, t, f] ->
          static $ if b then t else f
        _ -> make_var
    DIGEST_EQ -> make_var
    ADDRESS_EQ -> make_var
    -- FIXME fromIntegral may overflow the Int
    LSH -> nn2n (\a b -> shift a (fromIntegral b))
    RSH -> nn2n (\a b -> shift a (fromIntegral $ b * (-1)))
    BAND -> nn2n (.&.)
    BIOR -> nn2n (.|.)
    BXOR -> nn2n (xor)
    SELF_ADDRESS -> impossible "self address"
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
      (arg_lifts, rng, dargs) <-
        compileCheckAndConvert ctxt at (primOpType p) args
      let doClaim ca msg =
            return $
              DLS_Let at Nothing $
                DLE_Claim at (ctxt_stack ctxt) CT_Assert ca $ Just msg
      let mkvar t = DLVar at "overflow" t
      let doOp t cp cargs = do
            (cv, cl) <- ctxt_lift_expr ctxt at (mkvar t) $ DLE_PrimOp at cp cargs
            return $ (DLA_Var cv, cl)
      let doCmp = doOp T_Bool
      let lim_maxUInt_a = DLA_Constant DLC_UInt_max
      before <-
        case p of
          ADD -> do
            let (a, b) = case dargs of
                  [a_, b_] -> (a_, b_)
                  _ -> impossible "add args"
            (ra, rl) <- doOp T_UInt SUB [lim_maxUInt_a, b]
            (ca, cl) <- doCmp PLE [a, ra]
            return $ rl <> cl <> doClaim ca "add overflow"
          SUB -> do
            (ca, cl) <- doCmp PGE dargs
            return $ cl <> doClaim ca "sub wraparound"
          _ -> return $ mempty
      let mkdv = DLVar at "prim" rng
      (dv, lifts) <- ctxt_lift_expr ctxt at mkdv (DLE_PrimOp at p dargs)
      let da = DLA_Var dv
      after <-
        case p of
          MUL -> do
            (ca, cl) <- doCmp PLE [da, lim_maxUInt_a]
            return $ cl <> doClaim ca "mul overflow"
          _ -> return $ mempty
      let lifts' =
            arg_lifts <>
            case dlo_verifyOverflow $ ctxt_dlo ctxt of
              True -> before <> lifts <> after
              False -> lifts
      return $ SLRes lifts' st $ (lvl, SLV_DLVar dv)

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
      let mkde _ da i = DLE_ArrayRef at da (DLA_Literal $ DLL_Int at i)
      mconcatMap (mkdv tupdv mkde t) [0 .. sz -1]
    _ ->
      expect_throw_ctx ctxt at $ Err_Eval_NotSpreadable tuplv
  where
    mkdv tupdv mkde t i = do
      let de = mkde at (DLA_Var tupdv) i
      let mdv = DLVar at lab t
      (dv, lifts) <- ctxt_lift_expr ctxt at mdv de
      return $ (lifts, [SLV_DLVar dv])

doFluidRef :: SLCtxt s -> SrcLoc -> SLState -> FluidVar -> SLComp s SLSVal
doFluidRef ctxt at st fv =
  case st_mode st of
    SLM_Module ->
      expect_throw_ctx ctxt at $ Err_Eval_IllegalMode (st_mode st) "fluid ref"
    _ -> do
      let fvt = fluidVarType fv
      dv <- ctxt_mkvar ctxt (DLVar at "fluid" fvt)
      let lifts = return $ DLS_FluidRef at dv fv
      return $ SLRes lifts st $ public $ SLV_DLVar dv

doFluidSet :: SLCtxt s -> SrcLoc -> FluidVar -> SLSVal -> ST s DLStmts
doFluidSet ctxt at fv ssv = do
  (lifts, da) <- compileCheckType ctxt at (fluidVarType fv) sv
  return $ lifts <> (return $ DLS_FluidSet at fv da)
  where
    sv = ensure_public ctxt at ssv

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
    evalApplyVals ctxt at sco st (SLV_Prim $ SLPrim_op PLT) [public idxv, public $ SLV_Int at sz]
  SLRes check_lifts _ _ <-
    evalApplyVals ctxt at sco st (SLV_Prim $ SLPrim_claim CT_Assert) $
      [cmp_v, public $ SLV_Bytes at "array bounds check"]
  let lifts = cmp_lifts <> check_lifts
  keepLifts lifts $ m

doBalanceUpdate :: SLCtxt s -> SrcLoc -> SLScope -> SLState -> PrimOp -> SLVal -> SLComp s ()
doBalanceUpdate ctxt at sco st op rhs = do
  let up_rator = SLV_Prim $ SLPrim_PrimDelay at (SLPrim_op op) [] [(Public, rhs)]
  SLRes fr_lifts fr_st balance_v <- doFluidRef ctxt at st FV_balance
  SLRes lifts st' (SLAppRes _ balance_v') <-
    evalApplyVals ctxt at sco fr_st up_rator [balance_v]
  fs_lifts <- doFluidSet ctxt at FV_balance balance_v'
  return $ SLRes (fr_lifts <> lifts <> fs_lifts) st' ()

mustBeBytes :: SLCtxt s -> SrcLoc -> SLVal -> B.ByteString
mustBeBytes ctxt at v =
  case v of
    SLV_Bytes _ x -> x
    _ -> expect_throw_ctx ctxt at $ Err_Expected_Bytes v

evalPrim :: SLCtxt s -> SrcLoc -> SLScope -> SLState -> SLPrimitive -> [SLSVal] -> SLComp s SLSVal
evalPrim ctxt at sco st p sargs =
  case p of
    SLPrim_race ->
      case map snd sargs of
        [(SLV_Tuple _ pargs)] ->
          retV $ (lvl, SLV_RaceParticipant at $ map (slvParticipant_part ctxt at) pargs)
        _ -> illegal_args
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
    SLPrim_Bytes ->
      case map snd sargs of
        [(SLV_Int _ sz)] -> retV $ (lvl, SLV_Type $ T_Bytes sz)
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
          retV $ (lvl, SLV_Array at T_UInt $ map (SLV_Int at) [0 .. (sz -1)])
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
        [x, y] -> do
          (x_lifts, xt, xa) <- compileTypeOf ctxt at x
          (y_lifts, yt, ya) <- compileTypeOf ctxt at y
          keepLifts (x_lifts <> y_lifts) $
            case (xt, yt) of
              (T_Array x_ty x_sz, T_Array y_ty y_sz) -> do
                let t = (T_Array (typeMeet at (at, x_ty) (at, y_ty)) (x_sz + y_sz))
                let mkdv = (DLVar at "array_concat" t)
                let de = DLE_ArrayConcat at xa ya
                (dv, lifts') <- ctxt_lift_expr ctxt at mkdv de
                return $ SLRes lifts' st (lvl, SLV_DLVar dv)
              _ -> illegal_args
        _ -> illegal_args
    SLPrim_array_zip -> do
      let (x, y) = two_args
      (x_lifts, xt, x_da) <- compileTypeOf ctxt at x
      let (x_ty, x_sz) = mustBeArray xt
      (y_lifts, yt, y_da) <- compileTypeOf ctxt at y
      let (y_ty, y_sz) = mustBeArray yt
      let ty' = T_Tuple [x_ty, y_ty]
      unless (x_sz == y_sz) $ do
        expect_throw_ctx ctxt at $ Err_Zip_ArraysNotEqualLength x_sz y_sz
      let sz' = x_sz
      keepLifts (x_lifts <> y_lifts) $
        case isLiteralArray x && isLiteralArray y of
          True -> do
            (xlifts', x_vs) <- explodeTupleLike ctxt at "zip" x
            (ylifts', y_vs) <- explodeTupleLike ctxt at "zip" y
            let vs' = zipWith (\xe ye -> SLV_Tuple at [xe, ye]) x_vs y_vs
            return $ SLRes (xlifts' <> ylifts') st (lvl, SLV_Array at ty' vs')
          False -> do
            let t = T_Array ty' sz'
            let mkdv = (DLVar at "array_zip" t)
            let de = DLE_ArrayZip at x_da y_da
            (dv, lifts') <- ctxt_lift_expr ctxt at mkdv de
            return $ SLRes lifts' st (lvl, SLV_DLVar dv)
    SLPrim_array_map ->
      case args of
        [] -> illegal_args
        [_] -> illegal_args
        [x, f] -> do
          (x_lifts, xt, x_da) <- compileTypeOf ctxt at x
          let (x_ty, x_sz) = mustBeArray xt
          let f' a = evalApplyVals ctxt at sco st f [(lvl, a)]
          (a_dv, a_dsv) <- make_dlvar at "map in" x_ty
          SLRes f_lifts_ f_st (SLAppRes _ (f_lvl, f_v)) <- f' a_dsv
          (f_lifts', f_ty, f_da) <- compileTypeOf ctxt at f_v
          let f_lifts = f_lifts_ <> f_lifts'
          let shouldUnroll = not (isPure f_lifts && isLocal f_lifts) || isLiteralArray x
          keepLifts x_lifts $
            case shouldUnroll of
              True -> do
                (lifts', x_vs) <- explodeTupleLike ctxt at "map" x
                let evalem (prev_lifts, prev_vs) xv = do
                      SLRes xv_lifts xv_st (SLAppRes _ (_, xv_v')) <- f' xv
                      --- Note: We are artificially restricting maps to
                      --- be parameteric in the state.
                      return $
                        stMerge ctxt at f_st xv_st
                          `seq` ((prev_lifts <> xv_lifts), prev_vs ++ [xv_v'])
                (lifts'', vs') <- foldM evalem (mempty, []) x_vs
                return $ SLRes (lifts' <> lifts'') f_st (f_lvl, SLV_Array at f_ty vs')
              False -> do
                let t = T_Array f_ty x_sz
                (ans_dv, ans_dsv) <- make_dlvar at "array_map" t
                let f_bl = DLBlock at [] f_lifts f_da
                let lifts' = return $ DLS_ArrayMap at ans_dv x_da a_dv f_bl
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
          (x_lifts, xt, x_da) <- compileTypeOf ctxt at x
          let (x_ty, _) = mustBeArray xt
          let f' b a = evalApplyVals ctxt at sco st f [(lvl, b), (lvl, a)]
          (z_lifts, z_ty, z_da) <- compileTypeOf ctxt at z
          (b_dv, b_dsv) <- make_dlvar at "reduce acc" z_ty
          (a_dv, a_dsv) <- make_dlvar at "reduce in" x_ty
          SLRes f_lifts_ f_st (SLAppRes _ (f_lvl, f_v)) <- f' b_dsv a_dsv
          (f_lifts', f_ty, f_da) <- compileTypeOf ctxt at f_v
          let f_lifts = f_lifts_ <> f_lifts'
          let shouldUnroll = not (isPure f_lifts && isLocal f_lifts) || isLiteralArray x
          keepLifts (x_lifts <> z_lifts) $
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
                        stMerge ctxt at f_st xv_st
                          `seq` checkType at f_ty xv_v'
                          `seq` ((prev_lifts <> xv_lifts), xv_v')
                (lifts'', z') <- foldM evalem (mempty, z) x_vs
                return $ SLRes (lifts' <> lifts'') f_st (f_lvl, z')
              False -> do
                (ans_dv, ans_dsv) <- make_dlvar at "array_reduce" f_ty
                let f_bl = DLBlock at [] f_lifts f_da
                let lifts' = return $ DLS_ArrayReduce at ans_dv x_da z_da b_dv a_dv f_bl
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
        [arrv, idxv, valv] -> do
          (idx_lifts, idxty, idxda) <- compileTypeOf ctxt at idxv
          keepLifts idx_lifts $
            case (idxty, idxda) of
              (T_UInt, (DLA_Literal (DLL_Int _ idxi))) ->
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
                        expect_throw_ctx ctxt at $ Err_Eval_RefOutOfBounds (length arrvs) idxi
                  SLV_DLVar arrdv@(DLVar _ _ arr_ty@(T_Array elem_ty sz) _) ->
                    case idxi < sz of
                      True -> do
                        (lifts, valda) <- compileCheckType ctxt at elem_ty valv
                        keepLifts lifts $
                          doArrayBoundsCheck ctxt at sco st sz idxv $
                            retArrDV arr_ty $ DLE_ArraySet at (DLA_Var arrdv) idxda valda
                        where
                      False ->
                        expect_throw_ctx ctxt at $ Err_Eval_RefOutOfBounds (fromIntegral sz) idxi
                  _ -> illegal_args
                where
                  idxi' = fromIntegral idxi
              (T_UInt, _) -> do
                (arrv_lifts, arr_ty, arrda) <- compileTypeOf ctxt at arrv
                keepLifts arrv_lifts $
                  case arr_ty of
                    T_Array elem_ty sz -> do
                      (lifts, valda) <- compileCheckType ctxt at elem_ty valv
                      keepLifts lifts $
                        doArrayBoundsCheck ctxt at sco st sz idxv $
                          retArrDV arr_ty $ DLE_ArraySet at arrda idxda valda
                    _ -> illegal_args
              _ -> illegal_args
        _ -> illegal_args
      where
        retArrDV t de = do
          (dv, lifts') <- ctxt_lift_expr ctxt at (DLVar at "array_set" t) de
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
              if idxi < fromIntegral len then r else expect_throw_ctx ctxt at $ Err_Eval_RefOutOfBounds len idxi
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
            enum_pred = jsClo at' "makeEnum" "(x) => ((0 <= x) && (x < M))" (M.fromList [("M", iv)])
            at' = (srcloc_at "makeEnum" Nothing at)
        _ -> illegal_args
    SLPrim_App_Delay {} ->
      expect_throw_ctx ctxt at (Err_Eval_NotApplicable rator)
    SLPrim_interact _iat who m t ->
      case st_mode st of
        SLM_LocalStep -> do
          (lifts', rng, dargs) <- compileCheckAndConvert ctxt at t $ map snd sargs
          (dv, lifts) <- ctxt_lift_expr ctxt at (DLVar at "interact" rng) (DLE_Interact at (ctxt_stack ctxt) who m rng dargs)
          return $ SLRes (lifts' <> lifts) st $ secret $ SLV_DLVar dv
        cm ->
          expect_throw_ctx ctxt at (Err_Eval_IllegalMode cm "interact")
    SLPrim_declassify ->
      case map snd sargs of
        [val] ->
          case lvl of
            Secret -> retV $ public $ val
            Public -> expect_throw_ctx ctxt at $ Err_ExpectedPrivate val
        _ -> illegal_args
    SLPrim_commit ->
      case sargs of
        [] -> retV $ public $ SLV_Prim SLPrim_committed
        _ -> illegal_args
    SLPrim_committed -> illegal_args
    SLPrim_digest -> do
      let rng = T_Digest
      let darges = map snd $ map ((typeOf at) . snd) sargs
      (lifts', dargs) <- compileArgExprs ctxt at darges
      (dv, lifts) <- ctxt_lift_expr ctxt at (DLVar at "digest" rng) (DLE_Digest at dargs)
      return $ SLRes (lifts' <> lifts) st $ (lvl, SLV_DLVar dv)
    SLPrim_claim ct -> do
      let barg = compileCheckType ctxt at T_Bool
      let (dargm, mmsg) = case map snd sargs of
            [arg] ->
              (barg arg, Nothing)
            [arg, marg] ->
              (barg arg, Just $ mustBeBytes ctxt at marg)
            _ -> illegal_args
      (darg_lifts, darg) <- dargm
      let lifts =
            darg_lifts <>
              (return $ DLS_Let at Nothing $
                DLE_Claim at (ctxt_stack ctxt) ct darg mmsg)
      let bad cm =
            expect_throw_ctx ctxt at $ Err_Eval_IllegalMode cm $ "assert " ++ show ct
      let good = return $ SLRes lifts st $ public $ SLV_Null at "claim"
      case (st_mode st, ct) of
        (SLM_LocalStep, CT_Assume) -> good
        (cm, CT_Assume) -> bad cm
        (SLM_ConsensusStep, CT_Require) -> good
        (SLM_ConsensusPure, CT_Require) -> good
        (cm, CT_Require) -> bad cm
        (_, CT_Unknowable {}) -> impossible "unknowable"
        (_, CT_Assert) -> good
        (_, CT_Possible) -> good
    SLPrim_transfer ->
      case ensure_publics ctxt at sargs of
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
          (amt_lifts, amt_dla) <- compileCheckType ctxt at T_UInt amt_sv
          tbsuff <- doAssertBalance ctxt at sco st amt_sv PLE
          SLRes balup st' () <- doBalanceUpdate ctxt at sco st SUB amt_sv
          let convert = compileCheckType ctxt at T_Address
          (who_lifts, who_dla) <-
              case map snd sargs of
                [SLV_Participant _ who _ Nothing] ->
                  case M.lookup who $ st_pdvs st of
                    Just dv -> convert $ SLV_DLVar dv
                    Nothing -> expect_throw_ctx ctxt at $ Err_Transfer_NotBound who
                [one] -> convert one
                _ -> illegal_args
          let lifts = amt_lifts <> tbsuff <> who_lifts <> (return $ DLS_Let at Nothing $ DLE_Transfer at who_dla amt_dla) <> balup
          return $ SLRes lifts st' $ public $ SLV_Null at "transfer.to"
        cm -> expect_throw_ctx ctxt at $ Err_Eval_IllegalMode cm "transfer.to"
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
        cm -> expect_throw_ctx ctxt at $ Err_Eval_IllegalMode cm "exit"
    SLPrim_exitted -> illegal_args
    SLPrim_forall {} ->
      case sargs of
        [(olvl, one)] -> do
          let t = expect_ty one
          (dv, lifts) <- ctxt_lift_expr ctxt at (DLVar at "forall" t) (DLE_Impossible at $ "cannot inspect value from forall")
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
              (lifts, amt_da) <-
                compileCheckType ctxt at T_UInt $ ensure_public ctxt at amt_sv
              keepLifts lifts $
                return $ SLRes (return $ DLS_Let at Nothing (DLE_Wait at amt_da)) st $ public $ SLV_Null at "wait"
            _ -> illegal_args
        cm -> expect_throw_ctx ctxt at $ Err_Eval_IllegalMode cm "wait"
    SLPrim_part_set ->
      case map snd sargs of
        [(SLV_Participant _ who _ _), addr] -> do
          (lifts, addr_da) <- compileCheckType ctxt at T_Address addr
          keepLifts lifts $
            retV $ (lvl, (SLV_Prim $ SLPrim_part_setted at who addr_da))
        _ -> illegal_args
    SLPrim_part_setted {} ->
      expect_throw_ctx ctxt at (Err_Eval_NotApplicable rator)
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
    SLPrim_data_match -> do
      -- Expect two arguments to function
      let (obj, cases) = two_args
      -- Get the key/value pairs for the case object
      let objEnv = case cases of
                    SLV_Object _ _ env -> env
                    ow -> expect_throw_ctx ctxt (getSrcLocOrDefault at ow) $ Err_Decl_NotType "object" ow
      -- Keep a map of type constructor - args, for each case
      let args_x_case = M.map (\ v ->
            case sss_val v of
              SLV_Clo _ _ case_args _ _ -> case_args
              ow -> expect_throw_ctx ctxt (getSrcLocOrDefault at ow) $ Err_Decl_NotType "closure" ow
            ) objEnv
      -- Generate the function to call
      SLRes _ _ (_, fn) <- evalExpr ctxt at sco st $ do
              let ann = JSNoAnnot
              let semi = JSSemiAuto
              let data_param = JSIdentifier ann "data_id"
              let case_param = JSIdentifier ann "cases_id"
              let switch_parts = map (\ (tycon, tycon_args) ->
                    let case_id = JSIdentifier ann tycon in
                    let fn = JSMemberDot case_param ann $ JSIdentifier ann tycon in
                    let js_args = case tycon_args of
                                    [] -> JSLNil
                                    _  -> JSLOne data_param in
                    let ret = JSCallExpression fn ann js_args ann in
                    let case_body = [JSReturn ann (Just ret) semi] in
                    case tycon == "default" of
                      True  -> JSDefault ann ann case_body
                      False -> JSCase ann case_id ann case_body)
                    $ M.toList args_x_case
              let body = JSSwitch ann ann data_param ann ann switch_parts ann semi
              let params = mkArrowParameterList [data_param, case_param]
              JSArrowExpression params ann body
      -- Apply the object and cases to the newly created function
      SLRes a_lifts a_st (SLAppRes _ ans) <-
        evalApplyVals ctxt at sco st fn [public obj, public cases]
      return $ SLRes a_lifts a_st ans
  where
    lvl = mconcatMap fst sargs
    args = map snd sargs
    illegal_args = expect_throw_ctx ctxt at (Err_Prim_InvalidArgs p args)
    retV v = return $ SLRes mempty st v
    rator = SLV_Prim p
    getSrcLocOrDefault def sv =
      let loc = srclocOf sv in
      if loc == srcloc_builtin then def else loc
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
    SLV_Clo clo_at mname formals (JSBlock body_a body _) (SLCloEnv clo_env clo_penvs clo_cenv) -> do
      ret <- ctxt_alloc ctxt
      let body_at = srcloc_jsa "block" body_a clo_at
      SLRes lifts_arge st_arge arg_env <-
        evalDeclLHSs ctxt clo_at sco st mempty $
          zipEq (Just ctxt) at (Err_Apply_ArgCount clo_at) formals randvs
      let ctxt' = ctxt_stack_push ctxt (SLC_CloApp at clo_at mname)
      let clo_sco =
            (SLScope
               { sco_ret = Just ret
               , sco_must_ret = RS_MayBeEmpty
               , sco_env = clo_env
               , sco_while_vars = Nothing
               , sco_penvs = clo_penvs
               , sco_cenv = clo_cenv
               , sco_depth = sco_depth_update ctxt' at sco
               })
      let clo_sco' = sco_update ctxt clo_at clo_sco st arg_env
      SLRes body_lifts body_st (SLStmtRes clo_env'' rs) <-
        keepLifts lifts_arge $
          evalStmt ctxt' body_at clo_sco' st_arge body
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
        [(_, _, x)] -> no_prompt $ x
        (_, _, (xlvl, xv)) : more -> do
          let msvs = map (\(_a, _b, c)->c) more
          let mlvls = map fst msvs
          let mvs = map snd msvs
          let lvl = mconcat $ xlvl : mlvls
          -- Note: This test might be too expensive, so XXX try it
          let all_same = False && all (== xv) mvs
          case all_same of
            True -> no_prompt $ (lvl, xv)
            False -> do
              let go (r_at, rmi, (_, rv)) = do
                    (rlifts, rty, rda) <- compileTypeOf ctxt r_at rv
                    let retsm =
                          case rmi of
                            Nothing -> mempty
                            Just ri -> M.singleton ri (rlifts, rda)
                    return $ (retsm, (r_at, rty))
              (retsms, tys) <- unzip <$> mapM go rs
              let retsm = mconcat retsms
              let r_ty = typeMeets body_at tys
              let dv = DLVar body_at "clo app" r_ty ret
              let lifts' =
                    return $ DLS_Prompt body_at (Right (dv, retsm)) body_lifts
              return $ SLRes lifts' body_st $ SLAppRes clo_env'' (lvl, (SLV_DLVar dv))
    v ->
      expect_throw_ctx ctxt at (Err_Eval_NotApplicableVals v)

evalApply :: SLCtxt s -> SrcLoc -> SLScope -> SLState -> SLVal -> [JSExpression] -> SLComp s SLSVal
evalApply ctxt at sco st rator rands =
  case rator of
    SLV_Prim _ -> vals
    SLV_Clo _ _ _ _ _ -> vals
    SLV_Form f -> evalForm ctxt at sco st f rands
    v -> expect_throw_ctx ctxt (case ctxt_stack ctxt of
                         []  -> at
                         h:_ -> callAt h) (Err_Eval_NotApplicable v)
  where
    vals = do
      SLRes rlifts st_rands randsvs <- evalExprs ctxt at sco st rands
      SLRes alifts st_res (SLAppRes _ r) <-
        evalApplyVals ctxt at sco st_rands rator randsvs
      return $ SLRes (rlifts <> alifts) st_res r

getKwdOrPrim :: Ord k => k -> M.Map k SLSSVal -> Maybe SLSSVal
getKwdOrPrim ident env =
  case M.lookup ident env of
    -- XXX Hack: Allow `default` to be used as object property name for `match`
    -- expr. Keywords are allowed as property names in JS anyway, *shrug*
    Just (SLSSVal _ _ (SLV_Kwd SLK_default)) -> Nothing
    Just s@(SLSSVal _ _ (SLV_Kwd _))  -> Just s
    Just s@(SLSSVal _ _ (SLV_Prim _)) -> Just s
    _ -> Nothing

evalPropertyName :: SLCtxt s -> SrcLoc -> SLScope -> SLState -> JSPropertyName -> SLComp s (SecurityLevel, String)
evalPropertyName ctxt at sco st pn =
  case pn of
    JSPropertyIdent an s ->
      -- Do not allow keywords or primitives to be used as property names
      case getKwdOrPrim s base_env of
        Just s' -> expect_throw_ctx ctxt at_n $ Err_Shadowed s s' dummy_at
        _       -> k_res $ public s
      where
        at_n = srcloc_jsa "field" an at
        dummy_at = SLSSVal at Public $ SLV_Null at ""
    JSPropertyString _ s -> k_res $ public $ trimQuotes s
    JSPropertyNumber an _ ->
      expect_throw_ctx ctxt at_n (Err_Obj_IllegalNumberField pn)
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
            expect_throw_ctx ctxt at_n $ Err_Obj_IllegalComputedField ev
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
            return $ SLRes vlifts st_sv $ (flvl, env_insert_ AllowShadowing ctxt at' f sv' fenv)
          _ -> expect_throw_ctx ctxt at' (Err_Obj_IllegalFieldValues vs)
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
            return $ SLRes lifts st_se $ (slvl, env_merge_ AllowShadowing ctxt at' fenv env)
      keepLifts slifts $
        case sv of
          SLV_Object _ _ senv -> mkRes mempty senv
          SLV_DLVar dlv@(DLVar _at _s (T_Object tenv) _i) -> do
            let mkOneEnv k t = do
                  let de = DLE_ObjectRef at (DLA_Var dlv) k
                  let mdv = DLVar at "obj_ref" t
                  (dv, lifts) <- ctxt_lift_expr ctxt at mdv de
                  return $ (lifts, M.singleton k $ SLSSVal at slvl $ SLV_DLVar dv) -- TODO: double check this srcloc

            -- mconcat over SLEnvs is safe here b/c each is a singleton w/ unique key
            (lifts, env) <- mconcatMap (uncurry mkOneEnv) $ M.toList tenv
            mkRes lifts env
          _ -> expect_throw_ctx ctxt at (Err_Obj_SpreadNotObj sv)
    JSObjectMethod {} ->
      --- FIXME support these
      expect_throw_ctx ctxt at (Err_Obj_IllegalMethodDefinition p)

evalExpr :: SLCtxt s -> SrcLoc -> SLScope -> SLState -> JSExpression -> SLComp s SLSVal
evalExpr ctxt at sco st e = do
  let env = sco_env sco
  case e of
    JSIdentifier a x ->
      retV $ infectWithId x $ sss_sls $ env_lookup (Just ctxt) (srcloc_jsa "id ref" a at) (LC_RefFrom "expression") x env
    JSDecimal a ns -> retV $ public $ SLV_Int (srcloc_jsa "decimal" a at) $ numberValue 10 ns
    JSLiteral a l ->
      case l of
        "null" -> retV $ public $ SLV_Null at' "null"
        "true" -> retV $ public $ SLV_Bool at' True
        "false" -> retV $ public $ SLV_Bool at' False
        _ -> expect_throw_ctx ctxt at' (Err_Parse_IllegalLiteral l)
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
          doCallV st (binaryToPrim ctxt at env op) JSNoAnnot [lhs, rhs]
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
          SLV_DLVar cond_dv@(DLVar _ _ T_Bool _) -> do
            SLRes tlifts st_t tsv@(tlvl, tv) <- evalExpr ctxt t_at' sco st_c te
            SLRes flifts st_f fsv@(flvl, fv) <- evalExpr ctxt f_at' sco st_c fe
            let lvl = clvl <> tlvl <> flvl
            let st_tf = stMerge ctxt at st_t st_f
            let sa = (mkAnnot tlifts) <> (mkAnnot flifts)
            case isPure sa of
              True ->
                keepLifts (tlifts <> flifts) $
                  lvlMeetR lvl $
                    evalPrim ctxt at sco st_tf (SLPrim_op $ IF_THEN_ELSE) [csv, tsv, fsv]
              False -> do
                ret <- ctxt_alloc ctxt
                let add_ret e_at' elifts ev = do
                      (dlifts, e_ty, da) <- compileTypeOf ctxt e_at' ev
                      let elifts' =
                            elifts <> dlifts <>
                              (return $ DLS_Return e_at' ret $ Right da)
                      return $ (e_ty, elifts')
                (t_ty, tlifts') <- add_ret t_at' tlifts tv
                (f_ty, flifts') <- add_ret f_at' flifts fv
                let ty = typeMeet at' (t_at', t_ty) (f_at', f_ty)
                let ans_dv = DLVar at' "clo app" ty ret
                let body_lifts = return $ DLS_If at' (DLA_Var cond_dv) sa tlifts' flifts'
                let lifts' = return $ DLS_Prompt at' (Right (ans_dv, mempty)) body_lifts
                return $ SLRes lifts' st_tf $ (lvl, SLV_DLVar ans_dv)
          _ -> do
            let (n_at', ne) = case cv of
                  SLV_Bool _ False -> (f_at', fe)
                  _ -> (t_at', te)
            lvlMeetR clvl $ evalExpr ctxt n_at' sco st_c ne
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
            JSIdentNone -> Just $ "function"
            JSIdentName na _ -> expect_throw_ctx ctxt (srcloc_jsa "function name" na at') Err_Fun_NamesIllegal
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
        lab = Nothing
        at' = srcloc_jsa "obj" a at
        f (SLRes lifts st_f (lvl, oenv)) pp =
          keepLifts lifts $ lvlMeetR lvl $ evalPropertyPair ctxt at' sco st_f oenv pp
    JSSpreadExpression _ _ -> illegal
    JSTemplateLiteral _ _ _ _ -> illegal
    JSUnaryExpression op ue -> doCallV st (unaryToPrim ctxt at env op) JSNoAnnot [ue]
    JSVarInitExpression _ _ -> illegal
    JSYieldExpression _ _ -> illegal
    JSYieldFromExpression _ _ _ -> illegal
  where
    illegal = expect_throw_ctx ctxt at (Err_Eval_IllegalJS e)
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
      SLRes ilifts idx_st (idx_lvl, idxv) <-
        keepLifts alifts $ evalExpr ctxt at' sco arr_st idxe
      let lvl = arr_lvl <> idx_lvl
      let retRef t de = do
            (dv, lifts') <- ctxt_lift_expr ctxt at' (DLVar at' "ref" t) de
            let ansv = SLV_DLVar dv
            return $ SLRes lifts' idx_st (lvl, ansv)
      let retArrayRef t sz arr_dla idx_dla =
            doArrayBoundsCheck ctxt at' sco st sz idxv $
              retRef t $ DLE_ArrayRef at' arr_dla idx_dla
      let retTupleRef t arr_dla idx =
            retRef t $ DLE_TupleRef at' arr_dla idx
      let retVal idxi arrvs =
            case fromIntegerMay idxi >>= atMay arrvs of
              Nothing ->
                expect_throw_ctx ctxt at' $ Err_Eval_RefOutOfBounds (length arrvs) idxi
              Just ansv ->
                return $ SLRes mempty idx_st (lvl, ansv)
      keepLifts ilifts $
        case idxv of
          SLV_Int _ idxi ->
            case arrv of
              SLV_Array _ _ arrvs -> retVal idxi arrvs
              SLV_Tuple _ tupvs -> retVal idxi tupvs
              SLV_DLVar adv@(DLVar _ _ (T_Tuple ts) _) ->
                case fromIntegerMay idxi >>= atMay ts of
                  Nothing ->
                    expect_throw_ctx ctxt at' $ Err_Eval_RefOutOfBounds (length ts) idxi
                  Just t -> retTupleRef t arr_dla idxi
                    where
                      arr_dla = DLA_Var adv
              SLV_DLVar adv@(DLVar _ _ (T_Array t sz) _) ->
                case idxi < sz of
                  False ->
                    expect_throw_ctx ctxt at' $ Err_Eval_RefOutOfBounds (fromIntegral sz) idxi
                  True -> retArrayRef t sz arr_dla idx_dla
                    where
                      arr_dla = DLA_Var adv
                      idx_dla = DLA_Literal (DLL_Int at idxi)
              _ ->
                expect_throw_ctx ctxt at' $ Err_Eval_RefNotRefable arrv
          SLV_DLVar idxdv@(DLVar _ _ T_UInt _) -> do
            (arr_lifts, arr_ty, arr_dla) <- compileTypeOf ctxt at' arrv
            case arr_ty of
              T_Array elem_ty sz ->
                keepLifts arr_lifts $
                  retArrayRef elem_ty sz arr_dla idx_dla
                where
                  idx_dla = DLA_Var idxdv
              _ ->
                expect_throw_ctx ctxt at' $ Err_Eval_IndirectRefNotArray arrv
          _ ->
            expect_throw_ctx ctxt at' $ Err_Eval_RefNotInt idxv

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

evalDeclLHSArray :: SLCtxt s -> SrcLoc -> SLScope -> SLState -> SecurityLevel -> SLEnv -> [SLVal] -> [JSExpression] -> SLComp s SLEnv
evalDeclLHSArray ctxt at sco st rhs_lvl lhs_env vs es =
  case (vs, es) of
    ([], []) ->
      return $ SLRes mempty st lhs_env
    (_, (JSSpreadExpression a e) : es') -> do
      let at_ = srcloc_jsa "array spread" a at
      let v = SLV_Tuple at_ vs
      case es' of
        [] -> evalDeclLHS ctxt at_ sco st rhs_lvl lhs_env v e
        _ -> expect_throw_ctx ctxt at_ $ Err_Decl_ArraySpreadNotLast
    (v : vs', e : es') -> do
      SLRes lifts' st' lhs_env' <-
        evalDeclLHS ctxt at sco st rhs_lvl lhs_env v e
      keepLifts lifts' $
        evalDeclLHSArray ctxt at sco st' rhs_lvl lhs_env' vs' es'
    (_, _) ->
      expect_throw_ctx ctxt at $ Err_Decl_WrongArrayLength (length es) (length vs)

evalDeclLHSObject :: SLCtxt s -> SrcLoc -> SLScope -> SLState -> SecurityLevel -> SLEnv -> SLVal -> SLObjEnv s -> [JSObjectProperty] -> SLComp s SLEnv
evalDeclLHSObject ctxt at sco st rhs_lvl lhs_env orig_v vm = \case
  [] ->
    return $ SLRes mempty st lhs_env
  (JSObjectSpread a e) : os' -> do
    let at_ = srcloc_jsa "object spread" a at
    case os' of
      [] -> do
        SLRes lifts' st' vom <- evalObjEnv ctxt at sco st vm
        let vo = SLV_Object at_ Nothing vom
        keepLifts lifts' $
          evalDeclLHS ctxt at_ sco st' rhs_lvl lhs_env vo e
      _ -> expect_throw_ctx ctxt at_ $ Err_Decl_ObjectSpreadNotLast
  o : os' -> do
    let go st0 x e = do
          SLRes lifts_v st_v (v_lvl, v) <-
            evalDot_ ctxt at sco st0 orig_v vm x
          let lvl' = rhs_lvl <> v_lvl
          SLRes lifts' st' lhs_env' <-
            keepLifts lifts_v $
              evalDeclLHS ctxt at sco st_v lvl' lhs_env v e
          let vm' = M.delete x vm
          keepLifts lifts' $
            evalDeclLHSObject ctxt at sco st' rhs_lvl lhs_env' orig_v vm' os'
    case o of
      JSPropertyIdentRef a x -> do
        let e = JSIdentifier a x
        go st x e
      JSPropertyNameandValue pn _ [e] -> do
        SLRes lifts_x st_x (_, x) <- evalPropertyName ctxt at sco st pn
        keepLifts lifts_x $ go st_x x e
      _ ->
        expect_throw_ctx ctxt at $ Err_Parse_ExpectIdentifierProp o

evalDeclLHS :: SLCtxt s -> SrcLoc -> SLScope -> SLState -> SecurityLevel -> SLEnv -> SLVal -> JSExpression -> SLComp s SLEnv
evalDeclLHS ctxt at sco st rhs_lvl lhs_env v = \case
  JSIdentifier a x -> do
    let at_ = srcloc_jsa "id" a at
    return $ SLRes mempty st (env_insert ctxt at_ x (SLSSVal at_ rhs_lvl v) lhs_env)
  JSArrayLiteral a xs _ -> do
    let at_ = srcloc_jsa "array" a at
    (vs_lifts, vs) <- explodeTupleLike ctxt at_ "lhs array" v
    keepLifts vs_lifts $
      evalDeclLHSArray ctxt at_ sco st rhs_lvl lhs_env vs (jsa_flatten xs)
  JSObjectLiteral a props _ -> do
    let at_ = srcloc_jsa "object" a at
    let vm = evalAsEnv ctxt at_ v
    evalDeclLHSObject ctxt at_ sco st rhs_lvl lhs_env v vm (jso_flatten props)
  e ->
    expect_throw_ctx ctxt at $ Err_DeclLHS_IllegalJS e

evalDeclLHSs :: SLCtxt s -> SrcLoc -> SLScope -> SLState -> SLEnv -> [(JSExpression, SLSVal)] -> SLComp s SLEnv
evalDeclLHSs ctxt at sco st lhs_env = \case
  [] -> return $ SLRes mempty st lhs_env
  (e, (rhs_lvl, v)) : more -> do
    SLRes lifts st' lhs_env' <- evalDeclLHS ctxt at sco st rhs_lvl lhs_env v e
    keepLifts lifts $ evalDeclLHSs ctxt at sco st' lhs_env' more

evalDecl :: SLCtxt s -> SrcLoc -> SLScope -> SLState -> JSExpression -> JSExpression -> SLComp s SLEnv
evalDecl ctxt at sco st lhs rhs = do
  SLRes rhs_lifts rhs_st (rhs_lvl, rhs_v) <- evalExpr ctxt at sco st rhs
  keepLifts rhs_lifts $ evalDeclLHS ctxt at sco rhs_st rhs_lvl mempty rhs_v lhs

destructDecls :: SLCtxt s -> SrcLoc -> (JSCommaList JSExpression) -> (JSExpression, JSExpression)
destructDecls ctxt at = \case
  (JSLOne (JSVarInitExpression lhs (JSVarInit _ rhs))) -> (lhs, rhs)
  es -> expect_throw_ctx ctxt at $ Err_Decls_IllegalJS es

-- | Make sure all bindings in this SLEnv respect the rule that
-- private vars must be named with a leading underscore.
enforcePrivateUnderscore :: Monad m => SLCtxt s -> SrcLoc -> SLEnv -> m ()
enforcePrivateUnderscore ctxt at = mapM_ enf . M.toList
  where
    enf (k, (SLSSVal _ secLev _)) = case secLev of
      Secret
        | not (isSpecialIdent k)
            && not (isSecretIdent k) ->
          expect_throw_ctx ctxt at (Err_Eval_NotSecretIdent k)
      _ -> return ()

doOnly :: SLCtxt s -> SrcLoc -> (DLStmts, SLScope, SLState) -> ((SLPart, Maybe SLVar), SrcLoc, SLCloEnv, JSExpression) -> ST s (DLStmts, SLScope, SLState)
doOnly ctxt at (lifts, sco, st) ((who, vas), only_at, only_cloenv, only_synarg) = do
  ensure_part_can_act ctxt at st who
  let SLCloEnv only_env only_penvs only_cenv = only_cloenv
  let st_localstep = st {st_mode = SLM_LocalStep}
  let sco_only_pre =
        sco
          { sco_env = only_env
          , sco_penvs = only_penvs
          , sco_cenv = only_cenv
          }
  let penv_ = sco_lookup_penv ctxt sco_only_pre who
  (penv_lifts, penv) <-
    case vas of
      Nothing -> do
        return (mempty, penv_)
      Just v -> do
        (me_dv, me_let_lift) <- doGetSelfAddress ctxt at who
        case M.lookup v penv_ of
          Just (SLSSVal pv_at pv_lvl (SLV_Participant at_ who_ mv_ Nothing)) -> do
            let pv' = SLV_Participant at_ who_ mv_ (Just me_dv)
            let penv_' = M.insert v (SLSSVal pv_at pv_lvl pv') penv_
            return (me_let_lift, penv_')
          Just (SLSSVal _ _ (SLV_Participant _ _ _ (Just _))) ->
            return (mempty, penv_)
          _ -> do
            let pv' = SLV_DLVar me_dv
            let penv_' = M.insert v (SLSSVal only_at Public pv') penv_
            return (me_let_lift, penv_')
  let sco_only = sco_only_pre {sco_env = penv}
  SLRes only_lifts _ only_arg <-
    evalExpr ctxt only_at sco_only st_localstep only_synarg
  case only_arg of
    (_, only_clo@(SLV_Clo _ _ [] _ _)) -> do
      SLRes alifts _ (SLAppRes penv' (_, only_v)) <-
        evalApplyVals ctxt at sco_only st_localstep only_clo []
      case fst $ typeOf only_at only_v of
        T_Null -> do
          --- TODO: check less things
          enforcePrivateUnderscore ctxt only_at penv'
          let penvs = sco_penvs sco
          let penvs' = M.insert who penv' penvs
          let lifts' = return $ DLS_Only only_at who (penv_lifts <> only_lifts <> alifts)
          let st' = st {st_mode = SLM_Step}
          let sco' = sco {sco_penvs = penvs'}
          return ((lifts <> lifts'), sco', st')
        ty ->
          expect_throw_ctx ctxt only_at (Err_Block_NotNull ty only_v)
    _ -> expect_throw_ctx ctxt at $ Err_Only_NotOneClosure $ snd only_arg

doGetSelfAddress :: SLCtxt s -> SrcLoc -> SLPart -> ST s (DLVar, DLStmts)
doGetSelfAddress ctxt at who = do
  let whos = bunpack who
  (dv, lifts) <-
    ctxt_lift_expr ctxt at (DLVar at whos T_Address)
      (DLE_PrimOp at SELF_ADDRESS [DLA_Literal $ DLL_Bytes who])
  return (dv, lifts)

doFork :: SLCtxt s -> SrcLoc -> SLScope -> SLState -> SLForm_ForkCases -> [JSStatement] -> SLComp s SLStmtRes
doFork ctxt at _ _ [] _ =
  expect_throw_ctx ctxt at $ Err_Fork_NoCases
doFork ctxt at sco st_init caseses ks = do
  ensure_mode ctxt at st_init SLM_Step "fork"
  let ks_ne = dropEmptyJSStmts ks
  let sco' =
        case ks_ne of
          [] -> sco
          _ -> sco {sco_must_ret = RS_MayBeEmpty}
  let go (who_at, (whoe, whate)) = do
        -- XXX allow whoe to be an array/tuple and bind to `this`
        let who = jse_expect_id who_at whoe
        let whosv = env_lookup (Just ctxt) who_at (LC_RefFrom "fork case") who (sco_env sco)
        let whov = ensure_public ctxt who_at $ sss_sls whosv
        let whop = slvParticipant_part ctxt who_at whov
        let (JSBlock _ who_ss _) =
              case whate of
                -- XXX allow identifiers listed in aformals to be
                -- implicitly tested for nothing-ness
                JSArrowExpression _XXX_aformals _ s -> jsStmtToBlock s
                _ -> expect_throw_ctx ctxt who_at $ Err_Eval_IllegalJS whate
        -- XXX make this allow ".nack" on to consensus and "cancel" in local
        -- step
        let st_case = st_init { st_fork_body = Just whop }
        SLRes body_lifts body_st (SLStmtRes _ body_rets) <-
          evalStmt ctxt who_at sco' st_case who_ss
        return $ ((body_st, body_rets), (whop, body_lifts))
  (st_and_rets, dcases) <- unzip <$> mapM go caseses
  let (st_cases, rets_cases) = unzip st_and_rets
  let fork_st = stMerges ctxt at st_cases
  let fork_rets = combineStmtRetsl at Public rets_cases
  let fork_dl = DLS_Fork at dcases
  let fr = SLRes (return $ fork_dl) fork_st (SLStmtRes (sco_env sco) fork_rets)
  retSeqn_ ctxt sco' fr at ks_ne

evalStmtTrampoline :: SLCtxt s -> JSSemi -> SrcLoc -> SLScope -> SLState -> SLSVal -> [JSStatement] -> SLComp s SLStmtRes
evalStmtTrampoline ctxt sp at sco st (_, ev) ks =
  case ev of
    SLV_Prim (SLPrim_part_setted at' who addr_da) ->
      case st_mode st of
        SLM_ConsensusStep -> do
          let pdvs = st_pdvs st
          case M.lookup who pdvs of
            Just _ ->
              expect_throw_ctx ctxt at' $ Err_Eval_PartSet_Bound who
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
          expect_empty_tail ctxt "exit" JSNoAnnot sp at ks $
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
          ensure_part_can_act ctxt at st who
          let st_pure = st {st_mode = SLM_ConsensusPure}
          let pdvs = st_pdvs st
          let penv = sco_lookup_penv ctxt sco who
          (msg_lifts, msg_env, tmsg_) <-
            case mmsg of
              Nothing -> return (mempty, mempty, [])
              Just msg -> do
                let mk var = do
                      let val =
                            case env_lookup (Just ctxt) to_at (LC_RefFrom "publish") var penv of
                              (SLSSVal _ Public x) -> x
                              (SLSSVal _ Secret x) ->
                                -- TODO: use binding loc in error
                                expect_throw_ctx ctxt at $ Err_ExpectedPublic x
                      (da_lifts, t, da) <- compileTypeOf ctxt to_at val
                      dv <- ctxt_mkvar ctxt $ DLVar to_at "msg" t
                      return $ (da_lifts, (da, dv))
                tvls <- mapM mk msg
                let tv_lifts = mconcat $ map fst tvls
                let tvs = map snd tvls
                let msg_env_ = foldl' (env_insertp ctxt at) mempty $ zip msg $ map (sls_sss at . public . SLV_DLVar) $ map snd tvs
                return $ (tv_lifts, msg_env_, tvs)
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
                    case env_lookup (Just ctxt) to_at (LC_RefFrom "publish") whov env of
                      (SLSSVal idAt lvl_ (SLV_Participant at_ who_ as_ _)) ->
                        M.insert whov (SLSSVal idAt lvl_ (SLV_Participant at_ who_ as_ (Just $ (pdvs' M.! who))))
                      _ ->
                        impossible $ "participant is not participant"
          --- NOTE we don't use sco_update, because we have to treat the publish specially
          let env' = add_who_env $ env_merge ctxt to_at env msg_env
          let sco_env' = sco {sco_env = env'}
          let penvs' =
                penvs_update
                  ctxt
                  sco
                  (\p old ->
                     case p == who of
                       True -> add_who_env old
                       False -> add_who_env $ env_merge ctxt to_at old msg_env)
          (amte, amt_lifts, amt_da) <-
            case mamt of
              Nothing ->
                return $ (amt_e_, mempty, amt_check_da)
                where
                  amt_check_da = DLA_Literal $ DLL_Int at 0
                  amt_e_ = JSDecimal JSNoAnnot "0"
              Just amte_ -> do
                let penv' = penvs' M.! who
                let sco_penv' =
                      sco
                        { sco_penvs = penvs'
                        , sco_env = penv'
                        }
                SLRes amt_lifts_ _ amt_sv <- evalExpr ctxt at sco_penv' st_pure amte_
                (amt_lifts_', amt_da) <- compileCheckType ctxt at T_UInt $ ensure_public ctxt at amt_sv
                return $ (amte_, amt_lifts_ <> amt_lifts_', amt_da)
          let msg_compute_lifts = return $ DLS_Only at who (msg_lifts <> amt_lifts)
          amt_dv <- ctxt_mkvar ctxt $ DLVar at "amt" T_UInt
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
                (de_lifts', de_da) <- compileCheckType ctxt dt_at T_UInt $ ensure_public ctxt dt_at de_sv
                SLRes dta_lifts dt_st dt_cr <- evalStmt ctxt dt_at sco st dt_ss
                return $ (de_lifts <> de_lifts', Just (dt_st, dt_cr), Just (de_da, dta_lifts))
          let st_cstep =
                st
                  { st_mode = SLM_ConsensusStep
                  , st_pdvs = pdvs'
                  , st_after_first = True
                  , st_fork_body = Nothing
                  }
          let sco' =
                sco
                  { sco_env = env'
                  , sco_cenv = env'
                  , sco_penvs = penvs'
                  }
          SLRes balup st_cstep' () <- doBalanceUpdate ctxt at sco' st_cstep ADD (SLV_DLVar amt_dv)
          SLRes conlifts k_st k_cr <- evalStmt ctxt at sco' st_cstep' ks
          let lifts' = tlifts <> msg_compute_lifts <> (return $ DLS_ToConsensus to_at who fs (map fst tmsg_) (map snd tmsg_) amt_da amt_dv mtime' (amt_check_lifts <> balup <> conlifts))
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
                  return $ SLRes lifts' (stMerge ctxt at t_st k_st) $ combineStmtRes at Public t_cr k_cr
        _ -> illegal_mode
    SLV_Prim SLPrim_committed ->
      case st_mode st of
        SLM_ConsensusStep -> do
          let st_step = st {st_mode = SLM_Step}
          SLRes steplifts k_st cr <- evalStmt ctxt at sco st_step ks
          let lifts' = (return $ DLS_FromConsensus at steplifts)
          return $ SLRes lifts' k_st cr
        _ -> illegal_mode
    SLV_Form (SLForm_fork_partial False caseses) ->
      doFork ctxt at sco st caseses ks
    _ ->
      case typeOf at ev of
        (T_Null, _) -> evalStmt ctxt at sco st ks
        (ty, _) -> expect_throw_ctx ctxt at (Err_Block_NotNull ty ev)
  where
    illegal_mode = expect_throw_ctx ctxt at $ Err_Eval_IllegalMode (st_mode st) "trampoline"
    env = sco_env sco

doWhileLikeInitEval :: SLCtxt s -> SrcLoc -> SLScope -> SLState -> JSExpression -> JSExpression -> ST s (DLStmts, DLStmts, M.Map SLVar DLVar, DLAssignment, SLState, SLScope)
doWhileLikeInitEval ctxt at sco st lhs rhs = do
  SLRes init_lifts st_init vars_env_ <- evalDecl ctxt at sco st lhs rhs
  SLRes fr_lifts _ balance_sv <- doFluidRef ctxt at st_init FV_balance
  let balance_v = sls_sss at balance_sv
  --- XXX This could be broken with multiple loops
  let vars_env = env_insert ctxt at internalVar_balance balance_v vars_env_
  let help v (SLSSVal _ _ val) = do
        let (t, da) = typeOf at val
        dv <- ctxt_mkvar ctxt $ DLVar at v t
        return $ (dv, da)
  helpm <- M.traverseWithKey help vars_env
  let unknown_var_env = M.map (sls_sss at . public . SLV_DLVar . fst) helpm
  let unknown_bal_v = sss_sls $ unknown_var_env M.! internalVar_balance
  bal_lifts <- doFluidSet ctxt at FV_balance unknown_bal_v
  let st_init' = stEnsureMode ctxt at (st_mode st) st_init
  let sco_env' = sco_update ctxt at sco st_init' unknown_var_env
  let init_daem = M.fromList $ M.elems helpm
  let init_vars = M.map fst helpm
  (inite_lifts, init_dam) <- compileArgExprMap ctxt at init_daem
  let init_dl = DLAssignment init_dam
  let pre_lifts = init_lifts <> fr_lifts <> inite_lifts
  let post_lifts = bal_lifts
  return $ (pre_lifts, post_lifts, init_vars, init_dl, st_init', sco_env')

doWhileLikeContinueEval :: SLCtxt s -> SrcLoc -> SLScope -> SLState -> JSExpression -> M.Map SLVar DLVar -> SLSVal -> SLComp s ()
doWhileLikeContinueEval ctxt at sco st lhs whilem (rhs_lvl, rhs_v) = do
  SLRes decl_lifts st_decl decl_env <-
    evalDeclLHS ctxt at sco st rhs_lvl mempty rhs_v lhs
  let st_decl' = stEnsureMode ctxt at SLM_ConsensusStep st_decl
  let cont_daem =
        M.fromList $ map f $ M.toList decl_env
        where
          f (v, sv) = (dv, dae)
            where
              dv = case M.lookup v whilem of
                Nothing ->
                  expect_throw_ctx ctxt at $ Err_Eval_ContinueNotLoopVariable v
                Just x -> x
              val = ensure_public ctxt at $ sss_sls sv
              dae = checkType at et val
              DLVar _ _ et _ = dv
  SLRes fr_lifts _ balance_v <-
    doFluidRef ctxt at st_decl FV_balance
  let balance_dae =
        checkType at T_UInt $
          ensure_public ctxt at balance_v
  let unknown_balance_dv = whilem M.! internalVar_balance
  let cont_daem' =
        M.insert unknown_balance_dv balance_dae cont_daem
  (ae_lifts, cont_dam') <- compileArgExprMap ctxt at cont_daem'
  let cont_das = DLAssignment cont_dam'
  let lifts' =
        decl_lifts <> fr_lifts <> ae_lifts
          <> (return $ DLS_Continue at cont_das)
  return $ SLRes lifts' st_decl' ()

evalPureExprToBlock :: SLCtxt s -> SrcLoc -> SLScope -> SLState -> DLStmts -> JSExpression -> SLType -> ST s DLBlock
evalPureExprToBlock ctxt at sco st klifts e rest = do
  let pure_st = st {st_mode = SLM_ConsensusPure}
  let fs = ctxt_stack ctxt
  SLRes e_lifts _ e_da <-
    keepLifts klifts $
      checkResType ctxt at rest $ evalExpr ctxt at sco pure_st e
  return $ DLBlock at fs e_lifts e_da

checkParallelReduceBody :: SLCtxt s -> SrcLoc -> SLPart -> DLStmts -> ST s ()
checkParallelReduceBody ctxt at who stmts = checkPrompt stmts
  where
    checkPrompt = \case
      ((DLS_Prompt _ _ body) Seq.:<| Seq.Empty) ->
        checkOuter body
      _ -> bad at $ "not surrounded by prompt"
    checkOuter = \case
      Seq.Empty -> bad at $ "empty body"
      ((DLS_Only at' op _) Seq.:<| more) ->
        case who == op of
          True -> checkOuter more
          False -> bad at' $ "only not " <> show who
      ((DLS_ToConsensus at' op _ _ _ _ _ Nothing cons) Seq.:<| Seq.Empty) ->
        case who == op of
          True -> checkInner cons
          False -> bad at' $ "publish not " <> show who
      (s Seq.:<| _) ->
        bad (srclocOf s) $ "body not only or publish, given " <> conNameOf s
    checkInner Seq.Empty = bad at $ "empty consensus step"
    checkInner (s Seq.:<| more) =
      case (s, more) of
        (DLS_Let {}, _) -> cmore
        (DLS_ArrayMap {}, _) -> cmore
        (DLS_ArrayReduce {}, _) -> cmore
        (DLS_If _ _ _ l r, Seq.Empty) ->
          checkInner l >> checkInner r
        (DLS_If at' _ _ _ _, _) ->
          bad at' $ "consensus if not in tail position"
        (DLS_Switch _ _ _ csm, Seq.Empty) ->
          checkInnerCSM csm
        (DLS_Switch at' _ _ _, _) ->
          bad at' $ "consensus switch not in tail position"
        (DLS_Return {}, Seq.Empty) -> return ()
        (DLS_Return at' _ _, _) ->
          bad at' $ "consensus ret not in tail position"
        (DLS_Prompt {}, _) -> cmore
        (DLS_Stop at', _) -> bad at' $ "stop in consensus"
        (DLS_Only {}, _) -> impossible "only in consensus"
        (DLS_ToConsensus {}, _) -> impossible "publish in consensus"
        (DLS_FromConsensus at' _, _) -> bad at' $ "commit in consensus step"
        (DLS_While at' _ _ _ _, _) -> bad at' $ "while in consensus step"
        (DLS_Continue at' _, _) -> bad at' $ "continue in consensus step"
        (DLS_FluidSet {}, _) -> cmore
        (DLS_FluidRef {}, _) -> cmore
        (DLS_ParallelReduce at' _ _ _ _ _, _) -> bad at' $ "parallel reduce in consensus step"
        (DLS_Fork at' _, _) -> bad at' $ "fork in consensus step"
      where
        cmore = checkInner more
    checkInnerCSM =
      mapM_ checkInner . map snd . M.elems
    bad at' m = expect_throw_ctx ctxt at' $ Err_ParallelReduce_InvalidBody m

doParallelReduce :: SLCtxt s -> SrcLoc -> SLScope -> SLState -> JSExpression -> (JSExpression, Maybe ParallelReduceMode, Maybe JSExpression, Maybe JSExpression, Maybe JSExpression, [ (SrcLoc, (JSExpression, JSExpression)) ]) -> SrcLoc -> [JSStatement] -> SLComp s SLStmtRes
doParallelReduce ctxt at _ _ _ (_, Just m, _, _, _, _) _ _ =
  expect_throw_ctx ctxt at $ Err_ParallelReduce_Incomplete m
doParallelReduce ctxt at _ _ _ (_, Nothing, Nothing, _, _, _) _ _ =
  expect_throw_ctx ctxt at $ Err_ParallelReduce_NoInvariant
doParallelReduce ctxt at before_sco st lhs (slfpr_init, Nothing, Just slfpr_inv, slfpr_mtimeout, slfpr_muntil, slfpr_cases) at_after ks = do
  ensure_mode ctxt at st SLM_Step "parallel reduce"
  allowed_to_wait ctxt at st (return ())
  ensure_live ctxt at st "parallel reduce"
  (init_pre_lifts, init_post_lifts, init_vars, init_dl, init_st, after_sco) <-
    doWhileLikeInitEval ctxt at before_sco st lhs slfpr_init
  let mevalPureExprToBlock sco klifts me rest =
        case me of
          Nothing -> return $ Nothing
          Just e ->
            Just <$> evalPureExprToBlock ctxt at sco init_st klifts e rest
  inv_b <- evalPureExprToBlock ctxt at after_sco init_st init_post_lifts slfpr_inv T_Bool
  muntil_b <- mevalPureExprToBlock after_sco init_post_lifts slfpr_muntil T_Bool
  -- XXX timeout should include who should do it
  mtimeout_b <- mevalPureExprToBlock before_sco mempty slfpr_mtimeout T_UInt
  let (timeout_lifts, timeout_mda) =
        case mtimeout_b of
          Nothing -> (mempty, Nothing)
          Just (DLBlock _ _ l d) -> (l, Just d)
  -- XXX if there's no until OR timeout... then we can't end this, so the tail
  -- must be empty and funds might be locked
  let after_st = init_st { st_mode = SLM_ConsensusStep }
  let parse_dcase (who_at, (whoe, whate)) = do
        -- XXX allow whoe to be an array/tuple
        let who = jse_expect_id who_at whoe
        let whosv = env_lookup (Just ctxt) who_at (LC_RefFrom "parallel_reduce case") who (sco_env before_sco)
        let whov = ensure_public ctxt who_at $ sss_sls whosv
        let whop =
              -- FIXME make this a function and use regularly
              case whov of
                SLV_Participant _ x _ _ -> x
                _ -> expect_throw_ctx ctxt who_at $ Err_NotParticipant whov
        let body_e =
              case whate of
                -- XXX allow identifiers listed in aformals to be
                -- implicitly tested for nothing-ness
                JSArrowExpression _XXX_aformals a s ->
                  JSArrowExpression (JSParenthesizedArrowParameterList a JSLNil a) a s
                _ -> expect_throw_ctx ctxt who_at $ Err_Eval_IllegalJS whate
        SLRes _ _ body_clos <- evalExpr ctxt who_at after_sco init_st body_e
        let body_clo = ensure_public ctxt who_at body_clos
        SLRes body_lifts body_st (SLAppRes _ body_res) <-
          evalApplyVals ctxt who_at after_sco init_st body_clo []
        -- traceM $ "dcase " <> show whop
        -- traceM $ show $ pretty body_lifts
        checkParallelReduceBody ctxt who_at whop body_lifts
        SLRes cont_lifts _ () <-
          doWhileLikeContinueEval ctxt at after_sco body_st lhs init_vars body_res
        let lifts' = body_lifts <> cont_lifts
        return (whop, lifts')
  dcases <- mapM parse_dcase slfpr_cases
  let the_pr = DLS_ParallelReduce at init_dl inv_b muntil_b timeout_mda dcases
  let final_lifts =
        init_pre_lifts <> timeout_lifts <> (return $ the_pr) <> init_post_lifts
  keepLifts final_lifts $ evalStmt ctxt at_after after_sco after_st ks

evalStmt :: SLCtxt s -> SrcLoc -> SLScope -> SLState -> [JSStatement] -> SLComp s SLStmtRes
evalStmt ctxt at sco st ss =
  case ss of
    [] ->
      case sco_must_ret sco of
        RS_CannotReturn -> ret []
        RS_ImplicitNull -> ret [(at, Nothing, public $ SLV_Null at "implicit null")]
        RS_NeedExplicit ->
          --- In the presence of `exit()`, it is okay to have a while
          --- that ends in an empty tail, if the empty tail is
          --- dominated by an exit(). Here we really on two properties
          --- of the linearizer and the verifier: first, the
          --- linearizer will completely drop the continuation of
          --- DLS_Continue and DLS_Stop, so if this assert is not
          --- removed, then ti will error.
          keepLifts (return $ DLS_Let at Nothing $ DLE_Claim at (ctxt_stack ctxt) CT_Assert (DLA_Literal $ DLL_Bool False) (Just "unreachable")) $
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
      let lab = "const"
      let at_after = srcloc_after_semi lab a sp at
      let at_in = srcloc_jsa lab a at
      let (lhs, rhs) = destructDecls ctxt at_in decls
      SLRes rhs_lifts rhs_st (rhs_lvl, rhs_v) <- evalExpr ctxt at_in sco st rhs
      keepLifts rhs_lifts $
        case rhs_v of
          SLV_Form (SLForm_parallel_reduce_partial {..}) ->
            doParallelReduce ctxt at_in sco rhs_st lhs (slfpr_init, slfpr_mode, slfpr_minv, slfpr_mtimeout, slfpr_muntil, slfpr_cases) at_after ks
          _ -> do
            SLRes lifts st_const addl_env <-
              evalDeclLHS ctxt at_in sco rhs_st rhs_lvl mempty rhs_v lhs
            let sco' = sco_update ctxt at_in sco st addl_env
            keepLifts lifts $ evalStmt ctxt at_after sco' st_const ks
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
          JSIdentNone -> expect_throw_ctx ctxt at' (Err_TopFun_NoName)
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
          SLV_DLVar cond_dv@(DLVar _ _ T_Bool _) -> do
            SLRes tlifts st_t (SLStmtRes _ trets) <- evalStmt ctxt t_at' sco' st_c [ts]
            SLRes flifts st_f (SLStmtRes _ frets) <- evalStmt ctxt f_at' sco' st_c [fs]
            let st_tf = stMerge ctxt at' st_t st_f
            let sa = (mkAnnot tlifts) <> (mkAnnot flifts)
            let lifts' = return $ DLS_If at' (DLA_Var cond_dv) sa tlifts flifts
            let levelHelp = SLStmtRes (sco_env sco) . map (\(r_at, rmi, (r_lvl, r_v)) -> (r_at, rmi, (clvl <> r_lvl, r_v)))
            let ir = SLRes lifts' st_tf $ combineStmtRes at' clvl (levelHelp trets) (levelHelp frets)
            retSeqn ir at' ks_ne
          _ -> do
            let (n_at', ns) = case cv of
                  SLV_Bool _ False -> (f_at', fs)
                  _ -> (t_at', ts)
            nr <- evalStmt ctxt n_at' sco' st_c [ns]
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
        ((JSAssign var_a), ((JSContinue cont_a _bl cont_sp) : cont_ks)) -> do
          let lab = "continue"
          ensure_mode ctxt at st SLM_ConsensusStep lab
          let var_at = srcloc_jsa lab var_a at
          let cont_at = srcloc_jsa lab cont_a at
          SLRes rhs_lifts rhs_st rhs_sv <-
            evalExpr ctxt var_at sco st rhs
          let !whilem =
                case sco_while_vars sco of
                  Nothing ->
                    expect_throw_ctx ctxt cont_at $ Err_Eval_ContinueNotInWhile
                  Just x -> x
          SLRes cont_lifts st_decl' () <-
            doWhileLikeContinueEval ctxt cont_at sco rhs_st lhs whilem rhs_sv
          let lifts' = rhs_lifts <> cont_lifts
          let env = sco_env sco
          -- XXX We could/should look at sco_must_ret and see if it is
          -- RS_MayBeEmpty which means that the outside scope has an empty
          -- tail?
          expect_empty_tail ctxt lab cont_a cont_sp cont_at cont_ks $
            return $ SLRes lifts' st_decl' $ SLStmtRes env []
        (jsop, stmts) ->
          expect_throw_ctx ctxt (srcloc_jsa "assign" JSNoAnnot at) (Err_Block_Assign jsop stmts)
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
                  expect_throw_ctx ctxt at $ Err_CannotReturn
                _ -> x
            Nothing -> expect_throw_ctx ctxt at' $ Err_Eval_NoReturn
      evi <- ctxt_alloc ctxt
      let lifts' = return $ DLS_Return at' ret (Left evi)
      expect_empty_tail ctxt lab a sp at ks $
        return $ SLRes (elifts <> lifts') st_rt (SLStmtRes env [(at', Just evi, sev)])
    (JSSwitch a _ de _ _ cases _ sp : ks) -> do
      let at' = srcloc_jsa "switch" a at
      let de_v = jse_expect_id at' de
      let env = sco_env sco
      let (de_lvl, de_val) = sss_sls $ env_lookup (Just ctxt) at' (LC_RefFrom "switch statement") de_v env
      let (de_ty, _) = typeOf at de_val
      let varm = case de_ty of
            T_Data m -> m
            _ -> expect_throw_ctx ctxt at $ Err_Switch_NotData de_val
      let ks_ne = dropEmptyJSStmts ks
      let sco' =
            case ks_ne of
              [] -> sco
              _ -> sco {sco_must_ret = RS_MayBeEmpty}
      let case_insert k v@(at1, _, _) m =
            case M.lookup k m of
              Nothing -> M.insert k v m
              Just (at0, _, _) -> expect_throw_ctx ctxt at $ Err_Switch_DoubleCase at0 at1 (Just k)
      let case_minserts cs v m = M.unions $ m : map (flip M.singleton v) cs
      let add_case (seenDefault, casem0) = \case
            JSCase ca ve _ body -> (seenDefault, case_insert vn (at_c, True, body) casem0)
              where
                at_c = srcloc_jsa "case" ca at'
                vn = jse_expect_id at_c ve
            JSDefault ca _ body ->
              case seenDefault of
                Just at_c' -> expect_throw_ctx ctxt at $ Err_Switch_DoubleCase at_c at_c' Nothing
                Nothing -> ((Just at_c), case_minserts (M.keys varm) (at_c, False, body) casem0)
              where
                at_c = srcloc_jsa "case" ca at'
      let (_, casesm) = foldl' add_case (Nothing, mempty) cases
      let all_cases = M.keysSet varm
      let given_cases = M.keysSet casesm
      let missing_cases = all_cases S.\\ given_cases
      unless (S.null missing_cases) $ do
        expect_throw_ctx ctxt at' $ Err_Switch_MissingCases $ S.toList missing_cases
      let extra_cases = given_cases S.\\ all_cases
      unless (S.null extra_cases) $ do
        expect_throw_ctx ctxt at' $ Err_Switch_ExtraCases $ S.toList extra_cases
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
                        Just st' -> stMerge ctxt at_c st' case_st
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
    ((JSVariable var_a while_decls _vsp) : var_ks) -> do
      let var_at = (srcloc_jsa "var" var_a at)
      case var_ks of
        ((JSMethodCall (JSIdentifier inv_a "invariant") _ (JSLOne invariant_e) _ _isp) :
         (JSWhile while_a cond_a while_cond _ while_body) :
         ks) -> do
            ensure_mode ctxt var_at st SLM_ConsensusStep "while"
            let inv_at = (srcloc_jsa "invariant" inv_a at)
            let cond_at = (srcloc_jsa "cond" cond_a at)
            let while_at = (srcloc_jsa "while" while_a at)
            let (while_lhs, while_rhs) = destructDecls ctxt var_at while_decls
            (init_pre_lifts, init_post_lifts, init_vars, init_dl, st_var', sco_env') <-
              doWhileLikeInitEval ctxt var_at sco st while_lhs while_rhs
            inv_b <- evalPureExprToBlock ctxt inv_at sco_env' st_var' init_post_lifts invariant_e T_Bool
            cond_b <- evalPureExprToBlock ctxt cond_at sco_env' st_var' init_post_lifts while_cond T_Bool
            let while_sco =
                  sco_env'
                    { sco_while_vars = Just init_vars
                    , sco_must_ret = RS_NeedExplicit
                    }
            SLRes body_lifts body_st (SLStmtRes _ body_rets) <-
              keepLifts init_post_lifts $
                evalStmt ctxt while_at while_sco st_var' [while_body]
            let the_while = DLS_While var_at init_dl inv_b cond_b body_lifts
            let st_post = stMerge ctxt at body_st st_var'
            SLRes k_lifts k_st (SLStmtRes k_env' k_rets) <-
              keepLifts init_post_lifts $
                evalStmt ctxt while_at sco_env' st_post ks
            let lifts' = init_pre_lifts <> (return $ the_while) <> k_lifts
            let rets' = body_rets <> k_rets
            return $ SLRes lifts' k_st $ SLStmtRes k_env' rets'
        _ -> expect_throw_ctx ctxt var_at $ Err_Block_Variable
    ((JSWhile a _ _ _ _) : _) ->
      expect_throw_ctx ctxt (srcloc_jsa "while" a at) (Err_Block_While)
    (s@(JSWith a _ oe _ body sp) : ks) ->
      case True of
        True -> illegal a s "with"
        False -> do
          --- Because of the inlining-nature of Reach functions, this
          --- exposes too much of a function's definition and breaks
          --- the abstraction.
          let at' = srcloc_jsa "with stmt" a at
          SLRes o_lifts o_st (olvl, ov) <- evalExpr ctxt at' sco st oe
          let mk_o_env' = evalAsEnv ctxt at' ov
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
      expect_throw_ctx ctxt (srcloc_jsa lab a at) (Err_Block_IllegalJS s)
    retSeqn = retSeqn_ ctxt sco

retSeqn_ :: SLCtxt s -> SLScope -> SLRes SLStmtRes -> SrcLoc -> [JSStatement] -> SLComp s SLStmtRes
retSeqn_ ctxt sco sr at' ks = do
  case dropEmptyJSStmts ks of
    [] -> return $ sr
    ks' -> do
      let SLRes lifts0 st0 (SLStmtRes _ rets0) = sr
      let sco' =
            case rets0 of
              [] -> sco
              (_ : _) -> sco {sco_must_ret = RS_ImplicitNull}
      SLRes lifts1 st1 (SLStmtRes env1 rets1) <-
        evalStmt ctxt at' sco' st0 ks'
      return $ SLRes (lifts0 <> lifts1) st1 (SLStmtRes env1 (rets0 <> rets1))

combineStmtRes :: SrcLoc -> SecurityLevel -> SLStmtRes -> SLStmtRes -> SLStmtRes
combineStmtRes at' lvl (SLStmtRes _ lrets) (SLStmtRes env rrets) =
  SLStmtRes env $ combineStmtRets at' lvl lrets rrets

combineStmtRetsl :: SrcLoc -> SecurityLevel -> [SLStmtRets] -> SLStmtRets
combineStmtRetsl at lvl = foldl' (combineStmtRets at lvl) mempty

combineStmtRets :: SrcLoc -> SecurityLevel -> SLStmtRets -> SLStmtRets -> SLStmtRets
combineStmtRets at' lvl lrets rrets =
  case (lrets, rrets) of
    ([], []) -> []
    ([], _) -> [(at', Nothing, (lvl, SLV_Null at' "empty left"))] <> rrets
    (_, []) -> lrets <> [(at', Nothing, (lvl, SLV_Null at' "empty right"))]
    (_, _) -> lrets <> rrets

expect_empty_tail :: SLCtxt s -> String -> JSAnnot -> JSSemi -> SrcLoc -> [JSStatement] -> a -> a
expect_empty_tail ctxt lab a sp at ks res =
  case ks of
    [] -> res
    _ ->
      expect_throw_ctx ctxt at' (Err_TailNotEmpty ks)
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

evalImExportSpecifiers :: SLCtxt s -> SrcLoc -> LookupCtx -> SLEnv -> (a -> (JSIdent, JSIdent)) -> (JSCommaList a) -> SLEnv
evalImExportSpecifiers ctxt at lookupCtx env go cl =
  foldl' (env_insertp ctxt at) mempty $ map (uncurry p) $ map go $ jscl_flatten cl
  where
    p f t = p' (parseIdent at f) (parseIdent at t)
    p' (_, f) (_, t) = (t, env_lookup (Just ctxt) at lookupCtx f env)

evalImportClause :: SLCtxt s -> SrcLoc -> SLEnv -> JSImportClause -> SLEnv
evalImportClause ctxt at env im =
  case im of
    JSImportClauseNameSpace (JSImportNameSpace _ _ ji) ->
      M.singleton ns $ (SLSSVal at' Public $ SLV_Object at' (Just $ "module " <> ns) env)
      where
        (at', ns) = parseIdent at ji
    JSImportClauseNamed (JSImportsNamed _ iscl _) ->
      evalImExportSpecifiers ctxt at (LC_RefFrom "module import") env go iscl
      where
        go = \case
          JSImportSpecifier x -> (x, x)
          JSImportSpecifierAs x _ y -> (x, y)
    JSImportClauseDefault {} -> illegal_import
    JSImportClauseDefaultNameSpace {} -> illegal_import
    JSImportClauseDefaultNamed {} -> illegal_import
  where
    illegal_import = expect_throw_ctx ctxt at (Err_Import_IllegalJS im)

evalExportClause :: SLCtxt s -> SrcLoc -> SLEnv -> JSExportClause -> SLEnv
evalExportClause ctxt at env (JSExportClause _ escl _) =
  evalImExportSpecifiers ctxt at (LC_RefFrom "module export") env go escl
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
                env' = env_merge ctxt at' env libex
                libex = lookupDep (ReachSourceFile libn) libm
            JSImportDeclaration ic fc _ ->
              evalTopBody ctxt at' st libm env' exenv body'
              where
                env' = env_merge ctxt at' env news
                news = evalImportClause ctxt at' ienv ic
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
              evalTopBody ctxt at' st libm env (env_merge ctxt at' exenv news) body'
              where
                news = evalExportClause ctxt at' eenv ec
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
                   , sco_depth = recursionDepthLimit
                   })
          smr <- evalStmt ctxt at' sco st [sm]
          case smr of
            SLRes lifts _ (SLStmtRes env' []) ->
              let exenv' = case isExport of
                    True ->
                      --- If this is an exporting statement,
                      --- then add to the export environment
                      --- everything that is new.
                      env_merge ctxt at' exenv (M.difference env' env)
                    False ->
                      exenv
               in keepLifts lifts $
                    evalTopBody ctxt at' st libm env' exenv' body'
            SLRes {} ->
              expect_throw_ctx ctxt at' $ Err_Module_Return

type SLMod = (ReachSource, [JSModuleItem])

evalLib :: STCounter s -> Connectors -> SLMod -> (DLStmts, SLLibs) -> ST s (DLStmts, SLLibs)
evalLib idxr cns (src, body) (liblifts, libm) = do
  let at = srcloc_src src
  let st =
        SLState
          { st_mode = SLM_Module
          , st_live = False
          , st_pdvs = mempty
          , st_after_first = False
          , st_fork_body = Nothing
          }
  let dlo = app_default_opts $ M.keys cns
  let ctxt_top =
        (SLCtxt
           { ctxt_dlo = dlo
           , ctxt_id = idxr
           , ctxt_stack = []
           , ctxt_base_penvs = mempty
           })
  let base_env' =
        M.union base_env $
          M.mapKeys T.unpack $
            M.mapWithKey
              (\k _ -> SLSSVal srcloc_builtin Public $ SLV_Connector k)
              cns
  let stdlib_env =
        case src of
          ReachStdLib -> base_env'
          ReachSourceFile _ -> M.union (libm M.! ReachStdLib) base_env'
  let (prev_at, body') =
        case body of
          ((JSModuleStatementListItem (JSExpressionStatement (JSStringLiteral a hs) sp)) : j)
            | (trimQuotes hs) == versionHeader ->
              ((srcloc_after_semi "header" a sp at), j)
          _ -> expect_thrown at (Err_NoHeader body)
  SLRes more_lifts _ exenv <-
    evalTopBody ctxt_top prev_at st libm stdlib_env mt_env body'
  return $ (liblifts <> more_lifts, M.insert src exenv libm)

evalLibs :: STCounter s -> Connectors -> [SLMod] -> ST s (DLStmts, SLLibs)
evalLibs idxr cns mods = foldrM (evalLib idxr cns) (mempty, mempty) mods

makeInteract :: SrcLoc -> SLPart -> SLEnv -> SLVal
makeInteract at who spec = SLV_Object at lab spec'
  where
    lab = Just $ (bunpack who) <> "'s interaction interface"
    spec' = M.mapWithKey wrap_ty spec
    wrap_ty k (SLSSVal idAt Public (SLV_Type t)) = case isFirstOrder t of
      True -> sls_sss idAt $ secret $ SLV_Prim $ SLPrim_interact at who k t
      False -> expect_thrown at $ Err_App_Interact_NotFirstOrder t
    -- TODO: add idAt info to the err below?
    wrap_ty _ v = expect_thrown at $ Err_App_InvalidInteract $ sss_sls v

app_default_opts :: [T.Text] -> DLOpts
app_default_opts cns =
  DLOpts
    { dlo_deployMode = DM_constructor
    , dlo_verifyOverflow = False
    , dlo_verifyPerConnector = False
    , dlo_connectors = cns
    }

app_options :: M.Map SLVar (DLOpts -> SLVal -> Either String DLOpts)
app_options =
  M.fromList
    [ ("deployMode", opt_deployMode)
    , ("verifyOverflow", opt_verifyOverflow)
    , ("verifyPerConnector", opt_verifyPerConnector)
    , ("connectors", opt_connectors)
    ]
  where
    opt_verifyPerConnector opts v =
      case v of
        SLV_Bool _ b -> Right $ opts {dlo_verifyPerConnector = b}
        _ -> Left $ "expected boolean"
    opt_verifyOverflow opts v =
      case v of
        SLV_Bool _ b -> Right $ opts {dlo_verifyOverflow = b}
        _ -> Left $ "expected boolean"
    opt_connectors opts v =
      case v of
        SLV_Tuple _ vs ->
          case traverse f vs of
            Left x -> Left x
            Right y -> Right $ opts {dlo_connectors = y}
          where
            f (SLV_Connector cn) = Right $ cn
            f _ = Left $ "expected connector"
        _ -> Left $ "expected tuple"
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

compileDApp :: STCounter s -> DLStmts -> Connectors -> SLVal -> ST s DLProg
compileDApp idxr liblifts cns (SLV_Prim (SLPrim_App_Delay at opts parts top_formals top_s top_env)) = do
  let make_partio v =
        case v of
          SLV_Tuple p_at [SLV_Bytes bs_at bs, SLV_Object iat _ io] ->
            case "_" `B.isPrefixOf` bs of
              True -> expect_thrown bs_at (Err_App_PartUnderscore bs)
              False -> (p_at, bs, iat, (makeInteract iat bs io))
          _ -> expect_thrown at (Err_App_InvalidPartSpec v)
  let part_ios = map make_partio parts
  let make_part (p_at, pn, _, _) =
        public $ SLV_Participant p_at pn Nothing Nothing
  let partvs = map make_part part_ios
  let top_args = map (jse_expect_id at) top_formals
  let top_vargs = zipEq Nothing at (Err_Apply_ArgCount at) top_args partvs
  let top_viargs = map (\(i, pv) -> (i, infectWithId i pv)) top_vargs
  let top_rvargs = map (second $ (sls_sss at)) top_viargs
  let (JSBlock _ top_ss _) = (jsStmtToBlock top_s)
  let use_opt k v acc =
        case M.lookup k app_options of
          Nothing ->
            expect_thrown at $
              Err_App_InvalidOption k (S.toList $ M.keysSet app_options)
          Just opt ->
            case opt acc v of
              Right x -> x
              Left x -> expect_thrown at $ Err_App_InvalidOptionValue k x
  let dlo = M.foldrWithKey use_opt (app_default_opts $ M.keys cns) (M.map sss_val opts)
  let st_step =
        SLState
          { st_mode = SLM_Step
          , st_live = True
          , st_pdvs = mempty
          , st_after_first = False
          , st_fork_body = Nothing
          }
  let at' = srcloc_at "compileDApp" Nothing at
  let ctxt_ =
        SLCtxt
          { ctxt_dlo = dlo
          , ctxt_id = idxr
          , ctxt_stack = []
          , ctxt_base_penvs = mempty
          }
  let top_env_wps = foldl' (env_insertp ctxt_ at) top_env top_rvargs
  let make_penvp (p_at, pn, iat, io) = (pn, env0)
        where env0 = env_insert ctxt_ p_at "interact" (sls_sss iat $ secret io) top_env
  let penvs = M.fromList $ map make_penvp part_ios
  let ctxt = ctxt_ { ctxt_base_penvs = penvs }
  let sco =
        SLScope
          { sco_ret = Nothing
          , sco_must_ret = RS_CannotReturn
          , sco_env = top_env_wps
          , sco_while_vars = Nothing
          , sco_penvs = penvs
          , sco_cenv = mempty
          , sco_depth = recursionDepthLimit
          }
  bal_lifts <- doFluidSet ctxt at' FV_balance $ public $ SLV_Int at' 0
  SLRes final st_final _ <- evalStmt ctxt at' sco st_step top_ss
  ensure_mode ctxt at st_final SLM_Step "program termination"
  tbzero <- doAssertBalance ctxt at sco st_final (SLV_Int at' 0) PEQ
  let make_sps_entry (_p_at, pn, _iat, io) =
        (pn, InteractEnv $ M.map getType iom)
        where
          iom = case io of
            SLV_Object _ _ m -> m
            _ -> impossible $ "make_sps_entry io"
          getType (SLSSVal _ _ (SLV_Prim (SLPrim_interact _ _ _ t))) = t
          getType x = impossible $ "make_sps_entry getType " ++ show x
  let sps = SLParts $ M.fromList $ map make_sps_entry part_ios
  return $ DLProg at dlo sps (liblifts <> bal_lifts <> final <> tbzero)
compileDApp _ _ _ topv =
  expect_thrown srcloc_top (Err_Top_NotApp topv)

compileBundleST :: Connectors -> JSBundle -> SLVar -> ST s DLProg
compileBundleST cns (JSBundle mods) main = do
  idxr <- newSTCounter 0
  (liblifts, libm) <- evalLibs idxr cns mods
  let exe_ex = libm M.! exe
  let topv = case env_lookup Nothing srcloc_top LC_CompilerRequired main exe_ex of
        SLSSVal _ Public x -> x
        _ -> impossible "private before dapp"
  compileDApp idxr liblifts cns topv
  where
    exe = case mods of
      [] -> impossible $ "compileBundle: no files"
      ((x, _) : _) -> x

compileBundle :: Connectors -> JSBundle -> SLVar -> DLProg
compileBundle cns jsb main =
  runST $ compileBundleST cns jsb main
