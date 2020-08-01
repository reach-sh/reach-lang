module Reach.NL_Eval where

import Control.Monad
import Control.Monad.ST
import Data.Bits
import qualified Data.ByteString.Char8 as B
import Data.Foldable
import qualified Data.Map.Strict as M
import Data.STRef
import qualified Data.Sequence as Seq
import GHC.Stack (HasCallStack)
import Language.JavaScript.Parser
import Language.JavaScript.Parser.AST
import Reach.JSUtil
import Reach.NL_AST
import Reach.NL_Parser
import Reach.NL_Type
import Reach.Util
import Safe (atMay)
import Text.ParserCombinators.Parsec.Number (numberValue)
---import Debug.Trace

zipEq :: Show e => SrcLoc -> (Int -> Int -> e) -> [a] -> [b] -> [(a, b)]
zipEq at ce x y =
  if lx == ly
    then zip x y
    else expect_throw at (ce lx ly)
  where
    lx = length x
    ly = length y

--- FIXME implement a custom show that is useful
data EvalError s
  = Err_Apply_ArgCount Int Int
  | Err_Block_Assign
  | Err_Block_Continue
  | Err_Block_IllegalJS JSStatement
  | Err_Block_NotNull SLVal
  | Err_Block_Variable
  | Err_Block_While
  | Err_CannotReturn
  | Err_DApp_InvalidInteract SLSVal
  | Err_DApp_InvalidPartSpec SLVal
  | Err_DeclLHS_IllegalJS JSExpression
  | Err_Decl_IllegalJS JSExpression
  | Err_Decl_NotArray SLVal
  | Err_Decl_WrongArrayLength Int Int
  | Err_Dot_InvalidField SLVal String
  | Err_EvalRefIndirectNotHomogeneous [SLType]
  | Err_Eval_ContinueNotInWhile
  | Err_Eval_ContinueNotLoopVariable SLVar
  | Err_Eval_IfCondNotBool SLVal
  | Err_Eval_IfNotNull SLVal SLVal
  | Err_Eval_IllegalContext SLCtxtMode String
  | Err_Eval_IllegalJS JSExpression
  | Err_Eval_IllegalLift SLCtxtMode
  | Err_Eval_NoReturn
  | Err_Eval_NotApplicable SLVal
  | Err_Eval_NotApplicableVals SLVal
  | Err_Eval_NotObject SLVal
  | Err_Eval_RefEmptyArray
  | Err_Eval_RefNotArray SLVal
  | Err_Eval_RefNotInt SLVal
  | Err_Eval_RefOutOfBounds Int Int
  | Err_Eval_ReturnsDifferentTypes [SLType]
  | Err_Eval_UnboundId SLVar [SLVar]
  | Err_ExpectedPrivate SLVal
  | Err_ExpectedPublic SLVal
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
  | Err_Prim_InvalidArgs SLPrimitive [SLVal]
  | Err_Shadowed SLVar
  | Err_TailEmpty
  | Err_TailNotEmpty [JSStatement]
  | Err_ToConsensus_Double ToConsensusMode
  | Err_TopFun_NoName
  | Err_Top_IllegalJS JSStatement
  | Err_Top_NotDApp SLVal
  | Err_While_IllegalInvariant [JSExpression]
  deriving (Eq, Show)

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
  SLRes lifts v <- m
  return $ SLRes lifts $ lvlMeet lvl v

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
    , ("balance", SLV_Prim $ SLPrim_op $ CP BALANCE)
    , ("Null", SLV_Type T_Null)
    , ("Bool", SLV_Type T_Bool)
    , ("UInt256", SLV_Type T_UInt256)
    , ("Bytes", SLV_Type T_Bytes)
    , ("Address", SLV_Type T_Address)
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

-- General compiler utilities
srcloc_after_semi :: String -> JSAnnot -> JSSemi -> SrcLoc -> SrcLoc
srcloc_after_semi lab a sp at =
  case sp of
    JSSemi x -> srcloc_jsa (alab ++ " semicolon") x at
    _ -> srcloc_jsa alab a at
  where
    alab = "after " ++ lab

checkType :: SrcLoc -> SLType -> SLVal -> DLArg
checkType at et v =
  case et == t of
    True -> da
    False -> expect_throw at $ Err_Type_Mismatch et t v
  where
    (t, da) = typeOf at v

checkResType :: SrcLoc -> SLType -> SLComp a SLSVal -> SLComp a DLArg
checkResType at et m = do
  SLRes lifts (_lvl, v) <- m
  return $ SLRes lifts $ checkType at et v

-- Compiler
data ReturnStyle
  = RS_ImplicitNull
  | RS_NeedExplicit
  | RS_CannotReturn
  deriving (Eq, Show)

data SLScope = SLScope
  { sco_ret :: Maybe Int
  , sco_must_ret :: ReturnStyle
  , sco_env :: SLEnv
  , sco_while_vars :: Maybe (M.Map SLVar DLVar)
  }
  deriving (Eq, Show)

data SLCtxt s = SLCtxt
  { ctxt_mode :: SLCtxtMode
  , ctxt_id :: Maybe (STRef s Int)
  , ctxt_stack :: [SLCtxtFrame]
  , ctxt_local_mname :: Maybe [SLVar]
  }
  deriving ()

instance Show (SLCtxt s) where
  show ctxt = show $ ctxt_mode ctxt

type SLPartEnvs = M.Map SLPart SLEnv
type SLPartDVars = M.Map SLPart DLVar

data SLCtxtMode
  = SLC_Module
  | SLC_Step SLPartDVars SLPartEnvs
  | SLC_Local
  | SLC_LocalStep
  | SLC_ConsensusStep SLEnv SLPartDVars SLPartEnvs
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

ctxt_stack_push :: SLCtxt s -> SLCtxtFrame -> SLCtxt s
ctxt_stack_push ctxt f =
  (ctxt {ctxt_stack = f : (ctxt_stack ctxt)})

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

infectWithId :: SLVar -> SLSVal -> SLSVal
infectWithId v (lvl, sv) = (lvl, sv')
  where sv' =
          case sv of
            SLV_Participant at who io _ mdv ->
              SLV_Participant at who io (Just v) mdv
            _ -> sv

evalDot :: SrcLoc -> SLVal -> String -> SLSVal
evalDot at obj field =
  case obj of
    SLV_Object _ env ->
      case M.lookup field env of
        Just v -> v
        Nothing -> illegal_field
    SLV_Participant _ who _ vas _ ->
      case field of
        "only" -> public $ SLV_Form (SLForm_Part_Only obj)
        "publish" -> public $ SLV_Form (SLForm_Part_ToConsensus at who vas (Just TCM_Publish) Nothing Nothing Nothing)
        "pay" -> public $ SLV_Form (SLForm_Part_ToConsensus at who vas (Just TCM_Pay) Nothing Nothing Nothing)
        _ -> illegal_field
    SLV_Form (SLForm_Part_ToConsensus to_at who vas Nothing mpub mpay mtime) ->
      case field of
        "publish" -> public $ SLV_Form (SLForm_Part_ToConsensus to_at who vas (Just TCM_Publish) mpub mpay mtime)
        "pay" -> public $ SLV_Form (SLForm_Part_ToConsensus to_at who vas (Just TCM_Pay) mpub mpay mtime)
        "timeout" -> public $ SLV_Form (SLForm_Part_ToConsensus to_at who vas (Just TCM_Timeout) mpub mpay mtime)
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
    SLForm_Part_Only (SLV_Participant _ who _ _ _) ->
      case ctxt_mode ctxt of
        SLC_Step _pdvs penvs -> do
          let penv = penvs M.! who
          let ctxt_local = (ctxt {ctxt_mode = SLC_Local})
          SLRes elifts eargs <- evalExprs ctxt_local at penv args
          case eargs of
            [(_, thunk)] -> do
              let ctxt_localstep = (ctxt {ctxt_mode = SLC_LocalStep})
              SLRes alifts (SLAppRes penv' (_, ans)) <- evalApplyVals ctxt_localstep at (impossible "Part_only expects clo") thunk []
              return $ SLRes (elifts <> alifts) $ public $ SLV_Form $ SLForm_Part_OnlyAns at who penv' ans
            _ -> illegal_args
        cm -> expect_throw at $ Err_Eval_IllegalContext cm "part.only"
    SLForm_Part_Only _ -> impossible "SLForm_Part_Only args"
    SLForm_Part_OnlyAns {} -> impossible "SLForm_Part_OnlyAns"
    SLForm_Part_ToConsensus to_at who vas mmode mpub mpay mtime ->
      case ctxt_mode ctxt of
        SLC_Step _pdvs _penvs ->
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
              retV $ public $ SLV_Form $ SLForm_Part_ToConsensus to_at who vas Nothing mpub mpay (Just two_args)
            Nothing ->
              expect_throw at $ Err_Eval_NotApplicable rator
        cm -> expect_throw at $ Err_Eval_IllegalContext cm "toConsensus"
  where
    illegal_args = expect_throw at (Err_Form_InvalidArgs f args)
    rator = SLV_Form f
    retV v = return $ SLRes mempty v
    one_arg = case args of
      [x] -> x
      _ -> illegal_args
    two_args = case args of
      [x, y] -> (x, y)
      _ -> illegal_args

evalPrimOp :: SLCtxt s -> SrcLoc -> SLEnv -> PrimOp -> [SLSVal] -> SLComp s SLSVal
evalPrimOp ctxt at _env p sargs =
  case p of
    --- FIXME These should be sensitive to bit widths
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
          where
            lvl = mconcat $ map fst sargs
            dom = map expect_ty dom_arr
        _ -> illegal_args
    SLPrim_Array ->
      retV $ (lvl, SLV_Type $ T_Array $ map expect_ty $ map snd sargs)
      where
        lvl = mconcat $ map fst sargs
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
          where
            args = map snd sargs
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
            _ -> impossible "claim"
        lifts = return $ DLS_Claim at (ctxt_stack ctxt) ct darg
    SLPrim_transfer ->
      case ctxt_mode ctxt of
        SLC_ConsensusStep {} ->
          case map (typeOf at) $ ensure_publics at sargs of
            [(T_UInt256, amt_dla)] ->
              return $ SLRes mempty $ public $ SLV_Prim $ SLPrim_transfer_amt amt_dla
            _ -> illegal_args
        cm -> expect_throw at $ Err_Eval_IllegalContext cm "transfer"
    SLPrim_transfer_amt _ -> not_app
    SLPrim_transfer_amt_to amt_dla ->
      case ctxt_mode ctxt of
        SLC_ConsensusStep {} ->
          case sargs of
            [(_, SLV_Participant _ who _ _ _)] ->
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
      let ctxt' = ctxt_stack_push ctxt (SLC_CloApp at clo_at mname)
      let clo_sco =
            (SLScope
               { sco_ret = Just ret
               , sco_must_ret = RS_ImplicitNull
               , sco_env = clo_env'
               , sco_while_vars = Nothing
               })
      SLRes body_lifts (SLStmtRes clo_env'' rs) <- evalStmt ctxt' body_at clo_sco body
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
      return $ SLRes (lifts0 <> liftsN) (sval0 : svalN)

evalDecl :: SLCtxt s -> SrcLoc -> SLEnv -> SLEnv -> JSExpression -> SLComp s SLEnv
evalDecl ctxt at lhs_env rhs_env decl =
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
                  return $ (vs_lifts, foldl' (env_insertp at') lhs_env kvs)
            return (ks, _make_env)
          _ ->
            expect_throw at (Err_DeclLHS_IllegalJS lhs)
      let ctxt' = ctxt_local_name_set ctxt lhs_ns
      SLRes rhs_lifts v <- evalExpr ctxt' vat' rhs_env rhs
      (lhs_lifts, lhs_env') <- make_env v
      return $ SLRes (rhs_lifts <> lhs_lifts) lhs_env'
    _ ->
      expect_throw at (Err_Decl_IllegalJS decl)

evalDecls :: SLCtxt s -> SrcLoc -> SLEnv -> (JSCommaList JSExpression) -> SLComp s SLEnv
evalDecls ctxt at rhs_env decls =
  foldlM f (SLRes mempty mempty) $ jscl_flatten decls
  where
    f (SLRes lifts lhs_env) decl =
      keepLifts lifts $ evalDecl ctxt at lhs_env rhs_env decl

evalStmt :: SLCtxt s -> SrcLoc -> SLScope -> [JSStatement] -> SLComp s SLStmtRes
evalStmt ctxt at sco ss =
  case ss of
    [] ->
      case sco_must_ret sco of
        RS_CannotReturn ->
          return $ SLRes mempty $ SLStmtRes (sco_env sco) []
        RS_ImplicitNull ->
          evalStmt ctxt at sco $ [(JSReturn JSNoAnnot Nothing JSSemiAuto)]
        RS_NeedExplicit ->
          expect_throw at $ Err_TailEmpty
    ((JSStatementBlock a ss' _ sp) : ks) -> do
      br <- evalStmt ctxt at_in sco ss'
      retSeqn br at_after ks
      where
        at_in = srcloc_jsa "block" a at
        at_after = srcloc_after_semi "block" a sp at
    (s@(JSBreak a _ _) : _) -> illegal a s "break"
    (s@(JSLet a _ _) : _) -> illegal a s "let"
    (s@(JSClass a _ _ _ _ _ _) : _) -> illegal a s "class"
    ((JSConstant a decls sp) : ks) -> do
      let env = sco_env sco
      SLRes lifts addl_env <- evalDecls ctxt at_in env decls
      let env' = env_merge at_in env addl_env
      let sco' = sco {sco_env = env'}
      keepLifts lifts $ evalStmt ctxt at_after sco' ks
      where
        at_after = srcloc_after_semi lab a sp at
        at_in = srcloc_jsa lab a at
        lab = "const"
    (cont@(JSContinue a _ sp) : cont_ks) ->
      evalStmt ctxt at sco (assign : cont : cont_ks)
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
    ((JSFunction a name _ jsformals _ body sp) : ks) ->
      evalStmt ctxt at_after sco' ks
      where
        env = sco_env sco
        sco' = sco {sco_env = env'}
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
      evalStmt ctxt at sco ((JSIfElse a la ce ra ts ea fs) : ks)
      where
        ea = ra
        fs = (JSEmptyStatement ea)
    ((JSIfElse a _ ce ta ts fa fs) : ks) -> do
      let env = sco_env sco
      let at' = srcloc_jsa "if" a at
      let t_at' = srcloc_jsa "if > true" ta at'
      let f_at' = srcloc_jsa "if > false" fa t_at'
      SLRes clifts (clvl, cv) <- evalExpr ctxt at' env ce
      tr <- evalStmt ctxt t_at' sco [ts]
      fr <- evalStmt ctxt f_at' sco [fs]
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
    (s@(JSLabelled _ a _) : _) ->
      --- FIXME We could allow labels on whiles and have a mapping in
      --- sco_while_vars from a while label to the set of variables
      --- that should be modified, plus a field in sco for the default
      --- (i.e. closest label)
      illegal a s "labelled"
    ((JSEmptyStatement a) : ks) -> evalStmt ctxt at' sco ks
      where
        at' = srcloc_jsa "empty" a at
    ((JSExpressionStatement e sp) : ks) -> do
      let env = sco_env sco
      SLRes elifts sev <- evalExpr ctxt at env e
      let (_, ev) = sev
      case (ctxt_mode ctxt, ev) of
        (SLC_Step pdvs penvs, SLV_Form (SLForm_Part_OnlyAns only_at who penv' only_v)) ->
          case typeOf at_after only_v of
            (T_Null, _) ->
              keepLifts lifts' $ evalStmt ctxt' at_after sco ks
              where
                ctxt' = ctxt {ctxt_mode = SLC_Step pdvs $ M.insert who penv' penvs}
                lifts' = return $ DLS_Only only_at who elifts
            _ -> expect_throw at (Err_Block_NotNull ev)
        (SLC_Step pdvs penvs, SLV_Form (SLForm_Part_ToConsensus to_at who vas Nothing mmsg mamt mtime)) -> do
          let penv = penvs M.! who
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
          let penvs' =
                M.mapWithKey
                  (\p old ->
                     case p == who of
                       True -> add_who_env old
                       False -> add_who_env $ env_merge to_at old msg_env)
                  penvs
          amt' <-
            case mamt of
              Nothing ->
                return $ DLBlock at mempty (DLA_Con $ DLC_Int 0)
              Just amte -> do
                SLRes amt_lifts amt_sv <- evalExpr ctxt at env' amte
                --- FIXME The pattern should be a function
                let amt_v = ensure_public at amt_sv
                let (amt_ty, amt_da) = typeOf at amt_v
                case amt_ty of
                  T_UInt256 ->
                    return $ DLBlock at amt_lifts amt_da
                  _ ->
                    expect_throw at $ Err_Type_Mismatch T_UInt256 amt_ty amt_v
          (tlifts, mtime') <-
            case mtime of
              Nothing -> return $ (mempty, Nothing)
              Just (de, dt) -> do
                SLRes de_lifts de_sv <- evalExpr ctxt at env de
                let de_v = ensure_public at de_sv
                let (de_ty, de_da) = typeOf at de_v
                SLRes dt_lifts dt_sv <- evalExpr ctxt at env dt
                let dt_thunk = ensure_public at dt_sv
                SLRes dta_lifts (SLAppRes _ dt_fins) <-
                  evalApplyVals ctxt at (impossible "timeout expects clo") dt_thunk []
                let dt_fin = ensure_public at dt_fins
                let (_dt_fin_ty, dt_fin_da) = typeOf at dt_fin
                --- FIXME: We check the type of dt_fin_ty against the
                --- rest of the program in the linearize... There
                --- should be a better way.
                let dp = DLBlock at dta_lifts dt_fin_da
                case de_ty of
                  T_UInt256 ->
                    return $ (de_lifts <> dt_lifts, Just (de_da, dp))
                  _ ->
                    expect_throw at $ Err_Type_Mismatch T_UInt256 de_ty de_v
          let ctxt_cstep = (ctxt {ctxt_mode = SLC_ConsensusStep env' pdvs' penvs'})
          let sco' = sco {sco_env = env'}
          SLRes conlifts cr <- evalStmt ctxt_cstep at_after sco' ks
          let lifts' = elifts <> tlifts <> (return $ DLS_ToConsensus to_at who fs (map fst tmsg_) (map snd tmsg_) amt' mtime' conlifts)
          return $ SLRes lifts' cr
        (SLC_ConsensusStep orig_env pdvs penvs, SLV_Prim SLPrim_committed) -> do
          let addl_env = M.difference env orig_env
          let add_defns penv = env_merge at_after penv addl_env
          let penvs' = M.map add_defns penvs
          let ctxt_step = (ctxt {ctxt_mode = SLC_Step pdvs penvs'})
          SLRes steplifts cr <- evalStmt ctxt_step at_after sco ks
          let lifts' = elifts <> (return $ DLS_FromConsensus at steplifts)
          return $ SLRes lifts' cr
        _ ->
          case typeOf at_after ev of
            (T_Null, _) -> keepLifts elifts $ evalStmt ctxt at_after sco ks
            _ -> expect_throw at (Err_Block_NotNull ev)
      where
        at_after = srcloc_after_semi "expr stmt" JSNoAnnot sp at
    ((JSAssignStatement lhs op rhs _asp) : ks) ->
      case (op, ks) of
        ((JSAssign var_a), ((JSContinue cont_a _bl cont_sp) : cont_ks)) ->
          case ctxt_mode ctxt of
            SLC_ConsensusStep {} -> do
              let cont_at = srcloc_jsa lab cont_a at
              let decl = JSVarInitExpression lhs (JSVarInit var_a rhs)
              let env = sco_env sco
              SLRes decl_lifts decl_env <- evalDecl ctxt var_at mempty env decl
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
                return $ SLRes lifts' $ SLStmtRes env []
            cm -> expect_throw var_at $ Err_Eval_IllegalContext cm "continue"
          where
            lab = "continue"
            var_at = srcloc_jsa lab var_a at
        _ ->
          expect_throw (srcloc_jsa "assign" JSNoAnnot at) (Err_Block_Assign)
    ((JSMethodCall e a args ra sp) : ks) ->
      evalStmt ctxt at sco ss'
      where
        ss' = (JSExpressionStatement e' sp) : ks
        e' = (JSCallExpression e a args ra)
    ((JSReturn a me sp) : ks) -> do
      let env = sco_env sco
      let lab = "return"
      let at' = srcloc_jsa lab a at
      SLRes elifts sev <-
        case me of
          Nothing -> return $ SLRes mempty $ public $ SLV_Null at' "empty return"
          Just e -> evalExpr ctxt at' env e
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
        return $ SLRes (elifts <> lifts') (SLStmtRes env [(at', sev)])
    (s@(JSSwitch a _ _ _ _ _ _ _) : _) -> illegal a s "switch"
    (s@(JSThrow a _ _) : _) -> illegal a s "throw"
    (s@(JSTry a _ _ _) : _) -> illegal a s "try"
    ((JSVariable var_a while_decls _vsp) : var_ks) ->
      case var_ks of
        ( (JSMethodCall (JSIdentifier inv_a "invariant") _ invariant_args _ _isp)
            : (JSWhile while_a cond_a while_cond _ while_body)
            : ks
          ) ->
            case ctxt_mode ctxt of
              SLC_ConsensusStep {} -> do
                let env = sco_env sco
                SLRes init_lifts vars_env <- evalDecls ctxt var_at env while_decls
                let while_help v sv = do
                      let (_, val) = sv
                      vn <- ctxt_alloc ctxt var_at
                      let (t, da) = typeOf var_at val
                      return $ (DLVar var_at v t vn, da)
                while_helpm <- M.traverseWithKey while_help vars_env
                let unknown_var_env = M.map (public . SLV_DLVar . fst) while_helpm
                let env' = env_merge at env unknown_var_env
                SLRes inv_lifts inv_da <-
                  case jscl_flatten invariant_args of
                    [invariant_e] ->
                      checkResType inv_at T_Bool $ evalExpr ctxt inv_at env' invariant_e
                    ial -> expect_throw inv_at $ Err_While_IllegalInvariant ial
                let inv_b = DLBlock inv_at inv_lifts inv_da
                SLRes cond_lifts cond_da <-
                  checkResType cond_at T_Bool $ evalExpr ctxt cond_at env' while_cond
                let cond_b = DLBlock cond_at cond_lifts cond_da
                let while_sco =
                      sco
                        { sco_while_vars = Just $ M.map fst while_helpm
                        , sco_env = env'
                        , sco_must_ret = RS_NeedExplicit
                        }
                SLRes body_lifts (SLStmtRes _ body_rets) <-
                  evalStmt ctxt while_at while_sco [while_body]
                let while_dam = M.fromList $ M.elems while_helpm
                let the_while =
                      DLS_While var_at (DLAssignment while_dam) inv_b cond_b body_lifts
                let sco' = sco {sco_env = env'}
                SLRes k_lifts (SLStmtRes k_env' k_rets) <-
                  evalStmt ctxt while_at sco' ks
                let lifts' = init_lifts <> (return $ the_while) <> k_lifts
                let rets' = body_rets <> k_rets
                return $ SLRes lifts' $ SLStmtRes k_env' rets'
              cm -> expect_throw var_at $ Err_Eval_IllegalContext cm "while"
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
          let SLRes lifts0 (SLStmtRes _ rets0) = sr
          let sco' =
                case rets0 of
                  [] -> sco
                  (_ : _) -> sco {sco_must_ret = RS_ImplicitNull}
          SLRes lifts1 (SLStmtRes env1 rets1) <- evalStmt ctxt at' sco' ks'
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
          let sco =
                (SLScope
                   { sco_ret = Nothing
                   , sco_must_ret = RS_CannotReturn
                   , sco_while_vars = Nothing
                   , sco_env = env
                   })
          smr <- evalStmt ctxt at' sco [sm]
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

makeInteract :: SrcLoc -> SLEnv -> SLVal
makeInteract at spec = SLV_Object at spec'
  where
    spec' = M.mapWithKey wrap_ty spec
    wrap_ty k (Public, (SLV_Type t)) = secret $ SLV_Prim $ SLPrim_interact at k t
    wrap_ty _ v = expect_throw at $ Err_DApp_InvalidInteract v

compileDApp :: SLVal -> ST s DLProg
compileDApp topv =
  case topv of
    SLV_Prim (SLPrim_DApp_Delay at [(SLV_Object _ _opts), (SLV_Array _ parts), clo] top_env) -> do
      --- FIXME look at opts
      idxr <- newSTRef $ 0
      let ctxt_step =
            (SLCtxt
               { ctxt_mode = SLC_Step mempty penvs
               , ctxt_id = Just idxr
               , ctxt_stack = []
               , ctxt_local_mname = Nothing
               })
      SLRes final (SLAppRes _ (_, sv)) <-
        evalApplyVals ctxt_step at' (impossible "DApp_Delay expects clo") clo partvs
      let (_, final_da) = typeOf at sv
      return $ DLProg at sps $ DLBlock at final final_da
      where
        at' = srcloc_at "compileDApp" Nothing at
        sps = SLParts $ M.fromList $ map make_sps_entry partvs
        make_sps_entry (Secret, (SLV_Participant _ pn (SLV_Object _ io) _ _)) =
          (pn, InteractEnv $ M.map getType io)
          where
            getType (_, (SLV_Prim (SLPrim_interact _ _ t))) = t
            getType x = impossible $ "make_sps_entry getType " ++ show x
        make_sps_entry x = impossible $ "make_sps_entry " ++ show x
        penvs = M.fromList $ map make_penv partvs
        make_penv (Secret, (SLV_Participant _ pn io _ _)) =
          (pn, env_insert at' "interact" (secret io) top_env)
        make_penv _ = impossible "SLPrim_DApp_Delay make_penv"
        partvs = map make_part parts
        make_part v =
          case v of
            SLV_Array p_at [SLV_Bytes _ bs, SLV_Object iat io] ->
              secret $ SLV_Participant p_at bs (makeInteract iat io) Nothing Nothing
            _ -> expect_throw at' (Err_DApp_InvalidPartSpec v)
    _ ->
      expect_throw srcloc_top (Err_Top_NotDApp topv)

compileBundle :: JSBundle -> SLVar -> DLProg
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
