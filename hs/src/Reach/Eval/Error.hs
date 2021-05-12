module Reach.Eval.Error
  ( EvalError (..)
  , LookupCtx (..)
  , didYouMean
  )
where

import qualified Data.ByteString as B
import Data.List (intercalate, sortBy)
import qualified Data.Map.Strict as M
import Data.Ord (comparing)
import Generics.Deriving
import Language.JavaScript.Parser
import Language.JavaScript.Parser.AST
import Reach.AST.Base
import Reach.AST.DLBase
import Reach.AST.SL
import Reach.Eval.Types
import Reach.Texty (pretty)
import Reach.Util
import Reach.Version
import Text.EditDistance (defaultEditCosts, restrictedDamerauLevenshteinDistance)

data LookupCtx
  = -- Signifies the user referencing a variable from a ctxt (:: String).
    LC_RefFrom String
  | -- Signifies the compiler expecting a certain id to exist.
    LC_CompilerRequired
  deriving (Eq, Show)

data EvalError
  = Err_Apply_ArgCount SrcLoc Int Int
  | Err_Block_Assign JSAssignOp [JSStatement]
  | Err_Block_IllegalJS JSStatement
  | Err_Block_NotNull DLType SLVal
  | Err_Block_Variable
  | Err_Block_While
  | Err_CannotReturn
  | Err_ToConsensus_TimeoutArgs [JSExpression]
  | Err_ToConsensus_NoTimeoutBlock
  | Err_Transfer_DoubleNetworkToken TransferType
  | Err_Transfer_DoubleToken TransferType
  | Err_Transfer_Type TransferType SLValTy
  | Err_App_Interact_NotFirstOrder SLType
  | Err_App_InvalidOption SLVar [SLVar]
  | Err_App_InvalidOptionValue SLVar String
  | Err_App_InvalidInteract SLValTy
  | Err_App_InvalidPartSpec SLVal
  | Err_App_InvalidArgs [JSExpression]
  | Err_App_PartUnderscore B.ByteString
  | Err_DeclLHS_IllegalJS JSExpression
  | Err_Decl_ObjectSpreadNotLast
  | Err_Decl_ArraySpreadNotLast
  | Err_Decl_NotType String SLValTy
  | Err_Decls_IllegalJS (JSCommaList JSExpression)
  | Err_Decl_IllegalJS JSExpression
  | Err_Decl_NotRefable SLValTy
  | Err_Decl_WrongArrayLength Int Int
  | Err_Dot_InvalidField SLValTy [String] String
  | Err_Eval_ContinueNotInWhile
  | Err_Eval_IllegalWait DeployMode
  | Err_Eval_ContinueNotLoopVariable SLVar
  | Err_Eval_PartSet_Class SLPart
  | Err_Eval_PartSet_Bound SLPart
  | Err_Eval_IllegalMode SLMode String [SLMode]
  | Err_LValue_IllegalJS JSExpression
  | Err_Eval_IllegalJS JSExpression
  | Err_Eval_NoReturn
  | Err_Eval_NotApplicable SLValTy
  | Err_Eval_NotApplicableVals SLValTy
  | Err_Eval_NotObject SLValTy
  | Err_Eval_RefNotRefable SLValTy
  | Err_Eval_RefNotInt SLValTy
  | Err_Eval_IndirectRefNotArray SLValTy
  | Err_Eval_RefOutOfBounds Int Integer
  | Err_Eval_UnboundId LookupCtx SLVar [SLVar]
  | Err_ExpectedLevel SecurityLevel
  | Err_Form_InvalidArgs SLForm Int [JSExpression]
  | Err_Fun_NamesIllegal
  | Err_Import_IllegalJS JSImportClause
  | Err_Module_Return
  | Err_NoHeader [JSModuleItem]
  | Err_Obj_IllegalComputedField SLValTy
  | Err_Obj_IllegalFieldValues [JSExpression]
  | Err_Obj_IllegalMethodDefinition JSObjectProperty
  | Err_Obj_IllegalNumberField JSPropertyName
  | Err_Obj_SpreadNotObj SLVal
  | Err_Prim_InvalidArg_Dynamic SLPrimitive
  | Err_Prim_InvalidArgs SLPrimitive [SLValTy]
  | Err_Shadowed SLVar SLSSVal SLSSVal -- var, alreadyBound, new (invalid)
  | Err_TailNotEmpty [JSStatement]
  | Err_ToConsensus_Double ToConsensusMode
  | Err_TopFun_NoName
  | Err_While_IllegalInvariant [JSExpression]
  | Err_Only_NotOneClosure SLValTy
  | Err_Each_NotTuple SLValTy
  | Err_NotParticipant SLValTy
  | Err_Transfer_NotBound SLPart
  | Err_Eval_IncompatibleStates SLState SLState
  | Err_Eval_NotSecretIdent SLVar
  | Err_Eval_NotPublicIdent SLVar
  | Err_Eval_LookupUnderscore
  | Err_Eval_NotSpreadable String SLVal
  | Err_Zip_ArraysNotEqualLength Integer Integer
  | Err_Switch_NotData DLType
  | Err_Switch_DoubleCase SrcLoc SrcLoc (Maybe SLVar)
  | Err_Switch_MissingCases [SLVar]
  | Err_Switch_ExtraCases [SLVar]
  | Err_Expected String SLValTy
  | Err_RecursionDepthLimit
  | Err_Eval_MustBeLive String
  | Err_Invalid_Statement String
  | Err_ToConsensus_WhenNoTimeout Bool
  | Err_Fork_ResultNotObject DLType
  | Err_Fork_ConsensusBadArrow JSExpression
  | Err_ParallelReduceIncomplete String
  | Err_ParallelReduceBranchArgs String Int [JSExpression]
  | Err_Type_None SLVal
  | Err_Type_NotDT SLType
  | Err_Type_NotApplicable SLType
  | Err_TypeMeets_dMismatch DLType DLType
  | Err_Eval_MustBeInWhileInvariant String
  | Err_Expected_Map SLValTy
  | Err_Prim_Foldable
  | Err_Default_Arg_Position
  | Err_IllegalEffPosition SLValTy
  | Err_Unused_Variables [(SrcLoc, SLVar)]
  | Err_Remote_NotFun SLVar SLType
  | Err_Struct_Key_Invalid String
  | Err_Struct_Key_Not_Unique [String] String
  | Err_InvalidPartName String
  | Err_Strict_Conditional SLVal
  | Err_Try_Type_Mismatch DLType DLType
  | Err_Throw_No_Catch
  | Err_Token_OnCtor
  | Err_Token_InWhile
  | Err_Token_DynamicRef
  | Err_WithBill_Type DLType
  | Err_View_DuplicateView SLPart
  deriving (Eq, Generic)

--- FIXME I think most of these things should be in Pretty

show_sv :: SLValTy -> String
show_sv = \case
  (SLV_Participant _ who _ _ _, _) ->
    "<participant " <> (bunpack who) <> ">"
  (SLV_Object _ (Just lab) _, _) ->
    lab
  (sv, Nothing) -> "<" <> conNameOf sv <> ">"
  (_, Just t) -> show t

getErrorSuggestions :: String -> [String] -> Int -> [String]
getErrorSuggestions invalidStr validOptions maxClosest =
  take maxClosest $ sortBy (comparing distance) validOptions
  where
    distance = restrictedDamerauLevenshteinDistance defaultEditCosts invalidStr

didYouMean :: [Char] -> [String] -> Int -> String
didYouMean invalidStr validOptions maxClosest =
  case validOptions of
    [] -> ""
    _ -> ". Did you mean: " <> show options
  where
    options = getErrorSuggestions invalidStr validOptions maxClosest

showDiff :: Eq b => a -> a -> (a -> b) -> (b -> b -> String) -> String
showDiff x y f s =
  let fx = f x
      fy = f y
   in case fx == fy of
        True -> ""
        False -> "\n  * " <> s fx fy

showStateDiff :: SLState -> SLState -> String
showStateDiff x y =
  "\nThe expected state of the program varies between branches because:"
    <> showDiff
      x
      y
      st_mode
      (\xMode yMode ->
         unwords ["Expected to be in", show yMode, ", but in", show xMode <> "."])
    <> showDiff
      x
      y
      st_live
      (\xLive yLive ->
         case (xLive, yLive) of
           (False, True) -> "Expected there to be live state."
           (True, False) -> "Expected there to be no live state."
           _ -> impossible "expected st_live to differ.")
    <> showDiff
      x
      y
      st_after_ctor
      (\xAfter yAfter ->
         case (xAfter, yAfter) of
           (False, True) -> "Expected a constructing publication to have been made by this point."
           (True, False) -> "Expected no constructing publication to have been made by this point."
           _ -> impossible "expected st_after_ctor to differ.")
    <> showDiff
      x
      y
      st_after_first
      (\xAfter yAfter ->
         case (xAfter, yAfter) of
           (False, True) -> "Expected a publication to have been made by this point."
           (True, False) -> "Expected no publication to have been made by this point."
           _ -> impossible "expected st_after_first to differ.")
    <> showDiff
      x
      y
      st_pdvs
      (\xParts yParts ->
         let showParts = intercalate ", " . map (show . fst) . M.toList
          in "The number of active participants vary between states: " <> showParts xParts <> " vs " <> showParts yParts
               <> ". Ensure all needed participants have been set before the branch. Perhaps move the first `publish` of the missing participant before the branch?")

-- XXX tokens

instance ErrorMessageForJson EvalError where
  errorMessageForJson = \case
    Err_App_InvalidOption opt _ ->
      opt
        <> " is not a valid app option"
    Err_Dot_InvalidField slval _ k ->
      k
        <> " is not a field of "
        <> show_sv slval
    Err_Eval_UnboundId (LC_RefFrom ctxt) slvar _ ->
      "Invalid unbound identifier in " <> ctxt <> ": " <> slvar
    ow -> show ow

getIllegalModeSuggestion :: SLMode -> [SLMode] -> String
getIllegalModeSuggestion _ [] = impossible "getIllegalModeSuggestion: No expected mode"
getIllegalModeSuggestion mode (m : _) = get (mode, m)
  where
    get = \case
      (SLM_Module, _) -> "create a `React.App`"
      (SLM_AppInit, _) -> "`deploy`"
      (s, SLM_Step)
        | isConsensusStep s -> "`commit`"
        | isLocalStep s -> "exit `only` or `each`"
      (SLM_Step, s)
        | isConsensusStep s -> "`publish`, `pay`, or `fork`"
        | isLocalStep s -> "`only` or `each`"
      (s1, s2)
        | isConsensusStep s1 && isLocalStep s2 -> "`only` or `each`"
        | isLocalStep s1 && isConsensusStep s2 -> "exit `only`, `each` or `case`"
      _ -> impossible "getIllegalModeSuggestion"

instance ErrorSuggestions EvalError where
  errorSuggestions = \case
    Err_App_InvalidOption opt opts -> (Just opt, getErrorSuggestions opt opts 5)
    Err_Dot_InvalidField _ ks k -> (Just k, getErrorSuggestions k ks 5)
    Err_Eval_UnboundId _ slvar slvars -> (Just slvar, getErrorSuggestions slvar slvars 5)
    _ -> (Nothing, [])

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
      "Invalid `var` syntax. (Double check your `while` syntax)"
    Err_Block_While ->
      "Invalid `while` syntax"
    Err_CannotReturn ->
      "Invalid `return` syntax"
    Err_ToConsensus_NoTimeoutBlock ->
      "Timeout delay given without timeout block"
    Err_ToConsensus_TimeoutArgs jes ->
      "Invalid Participant.timeout args"
        <> case jes of
          [_, y] ->
            case y of
              JSArrowExpression aformals _ _ ->
                case aformals of
                  JSParenthesizedArrowParameterList _ JSLNil _ ->
                    impossible $ "nothing wrong with timeout args!"
                  _ -> ": the second argument should have no arguments"
              _ -> ": the second argument should be an arrow, but got something else"
          _ -> ": expected one argument or two arguments where the second is a syntactic thunk; got " <> (show $ length jes) <> " args"
    Err_Transfer_DoubleToken tt ->
      show tt <> " amount contains multiple non-network token amounts for the same token, but only one is allowed"
    Err_Transfer_DoubleNetworkToken tt ->
      show tt <> " amount contains multiple network token amounts, but only one is allowed"
    Err_Transfer_Type tt sv ->
      show tt <> " amount type invalid: expected for network token or tuple of UInt for network token or tuple of UInt and token for non-network token, i.e., Union(UInt, Tuple([Union(UInt, Tuple([UInt, Token])), ...])) but got: " <> show_sv sv
    Err_App_Interact_NotFirstOrder ty ->
      "Invalid interact specification. Expected first-order type, got: "
        <> show ty
    Err_App_InvalidInteract val ->
      "Invalid interact specification. Expected public type, got: "
        <> show_sv val
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
    Err_Eval_PartSet_Class who ->
      (bunpack who) <> " is a class and cannot be bound"
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
      "Invalid binding. Expected " <> ty <> ", got: " <> show_sv slval
    Err_Decl_NotRefable slval ->
      "Invalid binding. Expected array or tuple, got: " <> show_sv slval
    Err_Decl_WrongArrayLength nIdents nVals ->
      "Invalid array binding. nIdents:" <> show nIdents <> " does not match nVals:" <> show nVals
    Err_Dot_InvalidField slval ks k ->
      k <> " is not a field of " <> show_sv slval <> didYouMean k ks 5
    Err_Eval_ContinueNotInWhile ->
      "Invalid continue. Expected to be inside of a while."
    Err_Eval_ContinueNotLoopVariable var ->
      "Invalid loop variable update. Expected loop variable, got: " <> var
    Err_Eval_IllegalMode mode s ok_modes ->
      "Invalid operation. `" <> s <> "` cannot be used in context: " <> show mode <> ", must be in " <> intercalate " or " (map show ok_modes)
        <> ". You must "
        <> getIllegalModeSuggestion mode ok_modes
        <> " first."
    Err_LValue_IllegalJS e ->
      "Invalid Reach l-value syntax: " <> conNameOf e
    Err_Eval_IllegalJS e ->
      "Invalid Reach expression syntax: " <> conNameOf e
    Err_Eval_NoReturn ->
      --- FIXME Is this syntactically possible?
      --- Answer: I think if you put a return at the top-level it will error.
      "Nowhere to return to"
    Err_Eval_NotApplicable slval ->
      "Invalid function application. Cannot apply: " <> show_sv slval
    Err_Eval_NotApplicableVals slval ->
      "Invalid function. Cannot apply: " <> show_sv slval
    Err_Eval_NotObject slval ->
      "Invalid field access. Expected object, got: " <> show_sv slval
    Err_Eval_RefNotRefable slval ->
      "Invalid element reference. Expected array or tuple, got: " <> show_sv slval
    Err_Eval_IndirectRefNotArray slval ->
      "Invalid indirect element reference. Expected array, got: " <> show_sv slval
    Err_Eval_RefNotInt slval ->
      "Invalid array index. Expected uint256, got: " <> show_sv slval
    Err_Eval_RefOutOfBounds maxi ix ->
      "Invalid array index. Expected (0 <= ix < " <> show maxi <> "), got " <> show ix
    Err_Eval_UnboundId (LC_RefFrom ctxt) slvar slvars ->
      "Invalid unbound identifier in " <> ctxt <> ": " <> slvar <> didYouMean slvar slvars 5
    Err_Eval_UnboundId LC_CompilerRequired slvar _ ->
      "Expected the following identifier to be declared: " <> show slvar
    Err_ExpectedLevel want ->
      case want of
        Public ->
          "Invalid access of secret value"
        Secret ->
          "Invalid declassify. Expected to declassify something private"
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
        ty = show_sv slval
        reason = case take 5 ty of
          "Bytes" -> "It must be computable at compile time."
          _ -> "Fields must be bytes, but got: " <> ty
    Err_Obj_IllegalFieldValues exprs ->
      -- FIXME Is this syntactically possible?
      "Invalid field values. Expected 1 value, got: " <> show (length exprs)
    Err_Obj_IllegalMethodDefinition _prop ->
      "Invalid function field. Instead of {f() {...}}, write {f: () => {...}}"
    Err_Obj_IllegalNumberField _JSPropertyName ->
      "Invalid field name. Fields must be bytes, but got: uint256"
    Err_Obj_SpreadNotObj slval ->
      "Invalid object spread. Expected object, got: " <> (show $ pretty slval)
    Err_Eval_NotSpreadable lab slval ->
      "Value not spreadable in " <> lab <> " context. Expected tuple or array, got: " <> (show $ pretty slval)
    Err_Prim_InvalidArgs prim slvals ->
      "Invalid args for " <> displayPrim prim <> ". got: "
        <> "["
        <> (intercalate ", " $ map show_sv slvals)
        <> "]"
    Err_Prim_InvalidArg_Dynamic prim ->
      "Invalid arg for " <> displayPrim prim
        <> ". The argument must be computable at compile time"
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
    Err_While_IllegalInvariant exprs ->
      "Invalid while loop invariant. Expected 1 expr, but got " <> got
      where
        got = show $ length exprs
    Err_Only_NotOneClosure slval ->
      "PART.only not given a single closure, with no arguments, as an argument, instead got " <> (show_sv slval)
    Err_Each_NotTuple slval ->
      "each not given a tuple as an argument, instead got " <> show_sv slval
    Err_NotParticipant slval ->
      "expected a participant as an argument, instead got " <> show_sv slval
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
      "switch expects data instance, but got " <> show x
    Err_Switch_DoubleCase at0 at1 mc ->
      "switch contains duplicate case, " <> (maybe "default" id mc) <> " at " <> show at1 <> "; first defined at " <> show at0
    Err_Switch_MissingCases cs ->
      "switch missing cases: " <> show cs
    Err_Switch_ExtraCases cs ->
      "switch contains extra cases: " <> show cs
    Err_Expected t v ->
      "expected " <> t <> ", got something else: " <> show_sv v
    Err_RecursionDepthLimit ->
      "recursion depth limit exceeded, more than " <> show recursionDepthLimit <> " calls; who would need more than that many?"
    Err_Eval_MustBeLive m ->
      "must be live at " <> m
    Err_Eval_MustBeInWhileInvariant m ->
      "must be in while invariant at " <> m
    Err_Invalid_Statement stmt ->
      "Invalid use of statement: " <> stmt <> ". Did you mean to wrap it in a thunk?"
    Err_ToConsensus_WhenNoTimeout noMatterWhat ->
      case noMatterWhat of
        True -> "Cannot ignore timeout requirement, unless at least one participant always races or one class might race"
        False -> "Cannot optionally transition to consensus or have an empty race without timeout."
    Err_Fork_ResultNotObject t ->
      "fork local result must be object with fields `msg` or `when`, but got " <> show t
    Err_Fork_ConsensusBadArrow _ ->
      "fork consensus block should be arrow with zero or one parameters, but got something else"
    Err_ParallelReduceIncomplete lab ->
      "parallel reduce incomplete: " <> lab
    Err_ParallelReduceBranchArgs b n args ->
      let numArgs = length args
       in let arguments = if n == 1 then "argument" else "arguments"
           in "The `" <> b <> "` branch of `parallelReduce` expects " <> show n <> " " <> arguments <> ", but received " <> show numArgs
    Err_Type_None val ->
      "Value cannot exist at runtime: " <> show (pretty val)
    Err_Type_NotDT t ->
      "Value of this type cannot exist at runtime: " <> show (pretty t)
    Err_Type_NotApplicable ty ->
      "Cannot apply this like a function: " <> show ty
    Err_TypeMeets_dMismatch t1 t2 ->
      "These types are mismatched: " <> (show t1 <> " vs " <> show t2)
    Err_Expected_Map v ->
      "Expected map, got: " <> show_sv v
    Err_Prim_Foldable ->
      "Instances of Foldable cannot be created. Did you mean to call a function provided by `Foldable`?"
    Err_Default_Arg_Position ->
      "Parameters with default arguments must come after all other function arguments."
    Err_IllegalEffPosition v ->
      "Effects cannot be bound, got: " <> show_sv v
    Err_Unused_Variables vars ->
      intercalate "\n" $ map (\(at, v) -> "unused variable: " <> v <> " at " <> show at) vars
    Err_Remote_NotFun k t ->
      "Remote type not a function, " <> show k <> " has type " <> show t
    Err_Struct_Key_Invalid s ->
      "Struct key `" <> s <> "` is of the wrong format. A struct key should be of the format: [_a-zA-Z][_a-zA-Z0-9]*"
    Err_Struct_Key_Not_Unique sk k ->
      "All Struct keys must be unique, but `" <> k <> "` is not. This Struct already has keys: " <> intercalate ", " sk
    Err_InvalidPartName n ->
      "Invalid participant name: `" <> n <> "`. Reach exports this identifier in the backend."
    Err_Strict_Conditional v ->
      "Strict mode expects conditional values to be of type `Bool`, but received: " <> show (pretty v)
    Err_Try_Type_Mismatch expect actual ->
      "Every expression thrown within a `try` block must be of the same type. Expected " <> show (pretty expect) <> ", but received: " <> show (pretty actual)
    Err_Throw_No_Catch ->
      "`throw` statements may only occur inside of `try/catch` blocks."
    Err_Token_OnCtor ->
      "Token paid on constructor, which is not possible, because contract does not yet exist and therefore cannot receive tokens."
    Err_Token_InWhile ->
      "Token published within while, which Reach cannot track, yet."
    Err_Token_DynamicRef ->
      "Token reference based on dynamic computation, which Reach cannot track, yet."
    Err_WithBill_Type ty ->
      "`withBill` expects no arguments or a Tuple of Tokens, but received: " <> show (pretty ty)
    Err_View_DuplicateView n ->
      "Duplicated view name: " <> show n
    where
      displayPrim = drop (length ("SLPrim_" :: String)) . conNameOf
