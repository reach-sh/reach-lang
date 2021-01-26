module Reach.Backend.JS (backend_js) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as B
import qualified Data.Foldable as Foldable
import qualified Data.HashMap.Strict as HM
import Data.List (elemIndex, foldl')
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Scientific as Sci
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as LTIO
import Reach.AST.Base
import Reach.AST.DLBase
import Reach.AST.PL
import Reach.Backend
import Reach.Connector
import Reach.Texty
import Reach.Type
import Reach.UnsafeUtil
import Reach.Util
import Reach.Version

import Debug.Trace

--- Pretty helpers

sb :: SrcLoc
sb = srcloc_builtin

vsep_with_blank :: [Doc] -> Doc
vsep_with_blank l = vsep $ punctuate emptyDoc l

--- JS Helpers

jsString :: String -> Doc
jsString s = squotes $ pretty s

jsArray :: [Doc] -> Doc
jsArray elems = brackets $ hcat $ punctuate (comma <> space) elems

jsApply :: Doc -> [Doc] -> Doc
jsApply f args = f <> parens (hcat $ punctuate (comma <> space) args)

jsFunction :: Doc -> [Doc] -> Doc -> Doc
jsFunction name args body =
  "async function" <+> jsApply name args <+> jsBraces body

jsWhile :: Doc -> Doc -> Doc
jsWhile cond body = "while" <+> parens cond <+> jsBraces body

jsReturn :: Doc -> Doc
jsReturn a = "return" <+> a <> semi

jsIf :: Doc -> Doc -> Doc -> Doc
jsIf cap ttp ftp = "if" <+> parens cap <+> jsBraces ttp <> hardline <> "else" <+> jsBraces ftp

jsBraces :: Doc -> Doc
jsBraces body = braces (nest 2 $ hardline <> body <> space)

jsObject :: Pretty k => M.Map k (Doc) -> Doc
jsObject m = jsBraces $ vsep $ punctuate comma $ map jsObjField $ M.toList m
  where
    jsObjField (k, v) = pretty k <> ":" <+> v

jsBacktickText :: T.Text -> Doc
jsBacktickText x = "`" <> pretty x <> "`"

--- Compiler
--
data JSCtxt = JSCtxt
  { ctxt_who :: SLPart
  , ctxt_txn :: Int
  , ctxt_simulate :: Bool
  , ctxt_while :: Maybe (Maybe DLVar, PLBlock, ETail, ETail)
  , ctxt_timev :: Maybe DLVar
  }

jsTxn :: JSCtxt -> Doc
jsTxn ctxt = "txn" <> pretty (ctxt_txn ctxt)

jsTimeoutFlag :: JSCtxt -> Doc
jsTimeoutFlag ctxt = jsTxn ctxt <> ".didTimeout"

--- FIXME use Haskell state to keep track of which contracts have been
--- constructed and then use a JS backend map to store these so we
--- don't create them ovre and over... or do something like the sol
--- and smt backends and collect the set of types and define them all
--- up front.
jsContract :: DLType -> Doc
jsContract = \case
  T_Null -> "stdlib.T_Null"
  T_Bool -> "stdlib.T_Bool"
  T_UInt -> "stdlib.T_UInt"
  T_Bytes sz -> jsApply "stdlib.T_Bytes" [jsCon $ DLL_Int sb sz]
  T_Digest -> "stdlib.T_Digest"
  T_Address -> "stdlib.T_Address"
  T_Fun {} -> impossible "fun dl"
  T_Array t sz -> jsApply ("stdlib.T_Array") $ [jsContract t, jsCon (DLL_Int sb sz)]
  T_Tuple as -> jsApply ("stdlib.T_Tuple") $ [jsArray $ map jsContract as]
  T_Object m -> jsApply ("stdlib.T_Object") [jsObject $ M.map jsContract m]
  T_Data m -> jsApply ("stdlib.T_Data") [jsObject $ M.map jsContract m]
  T_Forall {} -> impossible "forall dl"
  T_Var {} -> impossible "var dl"
  T_Type {} -> impossible "type dl"

jsProtect :: Doc -> DLType -> Doc -> Doc
jsProtect ai how what =
  jsApply "stdlib.protect" $ [jsContract how, what, ai]

jsAt :: SrcLoc -> Doc
jsAt at = jsString $ unsafeRedactAbsStr $ show at

jsAssertInfo :: JSCtxt -> SrcLoc -> [SLCtxtFrame] -> Maybe B.ByteString -> Doc
jsAssertInfo ctxt at fs mmsg =
  jsObject $
    M.fromList
      [ ("who" :: String, who_p)
      , ("msg", msg_p)
      , ("at", jsAt at)
      , ("fs", fs_p)
      ]
  where
    msg_p = case mmsg of
      Nothing -> "null"
      Just b -> jsString $ bunpack b
    who_p = jsCon $ DLL_Bytes $ ctxt_who ctxt
    fs_p = jsArray $ map (jsString . unsafeRedactAbsStr . show) fs

jsVar :: DLVar -> Doc
jsVar (DLVar _ _ _ n) = "v" <> pretty n

jsContinueVar :: DLVar -> Doc
jsContinueVar dv = "c" <> jsVar dv

jsCon :: DLLiteral -> Doc
jsCon = \case
  DLL_Null -> "null"
  DLL_Bool True -> "true"
  DLL_Bool False -> "false"
  DLL_Int at i ->
    jsApply "stdlib.checkedBigNumberify" [jsAt at, jsArg (DLA_Constant $ DLC_UInt_max), pretty i]
  DLL_Bytes b -> jsString $ bunpack b

jsArg :: DLArg -> Doc
jsArg = \case
  DLA_Var v -> jsVar v
  DLA_Constant c ->
    case c of
      DLC_UInt_max ->
        "stdlib.UInt_max"
  DLA_Literal c -> jsCon c
  DLA_Interact _ m t ->
    jsProtect "null" t $ "interact." <> pretty m

jsLargeArg :: DLLargeArg -> Doc
jsLargeArg = \case
  DLLA_Array _ as ->
    jsLargeArg $ DLLA_Tuple as
  DLLA_Tuple as ->
    jsArray $ map jsArg as
  DLLA_Obj m ->
    jsObject $ M.map jsArg m
  DLLA_Data _ vn vv ->
    jsArray [jsString vn, jsArg vv]

jsDigest :: [DLArg] -> Doc
jsDigest as =
  jsApply
    "stdlib.digest"
    [ jsContract (T_Tuple $ map argTypeOf as)
    , jsLargeArg (DLLA_Tuple as)
    ]

jsPrimApply :: JSCtxt -> PrimOp -> [Doc] -> Doc
jsPrimApply _ctxt = \case
  SELF_ADDRESS -> jsApply "ctc.selfAddress"
  ADD -> jsApply "stdlib.add"
  SUB -> jsApply "stdlib.sub"
  MUL -> jsApply "stdlib.mul"
  DIV -> jsApply "stdlib.div"
  MOD -> jsApply "stdlib.mod"
  PLT -> jsApply "stdlib.lt"
  PLE -> jsApply "stdlib.le"
  PEQ -> jsApply "stdlib.eq"
  PGE -> jsApply "stdlib.ge"
  PGT -> jsApply "stdlib.gt"
  LSH -> jsApply "stdlib.lsh"
  RSH -> jsApply "stdlib.rsh"
  BAND -> jsApply "stdlib.band"
  BIOR -> jsApply "stdlib.bior"
  BXOR -> jsApply "stdlib.bxor"
  IF_THEN_ELSE -> \args -> case args of
    [c, t, f] -> c <+> "?" <+> t <+> ":" <+> f
    _ -> impossible $ "emitJS: ITE called with wrong number of arguments"
  --  BYTES_EQ -> jsApply "stdlib.bytesEq"
  DIGEST_EQ -> jsApply "stdlib.digestEq"
  ADDRESS_EQ -> jsApply "stdlib.addressEq"

jsExpr :: JSCtxt -> DLExpr -> Doc
jsExpr ctxt = \case
  DLE_Arg _ a ->
    jsArg a
  DLE_LArg _ la ->
    jsLargeArg la
  DLE_Impossible at msg ->
    expect_thrown at $ Err_Impossible msg
  DLE_PrimOp _ p as ->
    jsPrimApply ctxt p $ map jsArg as
  DLE_ArrayRef _ aa ia ->
    jsArg aa <> brackets (jsArg ia)
  DLE_ArraySet _ aa ia va ->
    jsApply "stdlib.Array_set" $ map jsArg [aa, ia, va]
  DLE_ArrayConcat _ x y ->
    jsArg x <> "." <> jsApply "concat" [jsArg y]
  DLE_ArrayZip _ x y ->
    jsApply "stdlib.Array_zip" $ map jsArg [x, y]
  DLE_TupleRef at aa i ->
    jsArg aa <> brackets (jsCon $ DLL_Int at i)
  DLE_ObjectRef _ oa f ->
    jsArg oa <> "." <> pretty f
  DLE_Interact at fs _ m t as ->
    jsProtect (jsAssertInfo ctxt at fs (Just $ bpack m)) t $ "await" <+> (jsApply ("interact." <> pretty m) $ map jsArg as)
  DLE_Digest _ as -> jsDigest as
  DLE_Claim at fs ct a mmsg ->
    check
    where
      check = case ct of
        CT_Assert -> impossible "assert"
        CT_Assume -> require
        CT_Require -> require
        CT_Possible -> impossible "possible"
        CT_Unknowable {} -> impossible "unknowable"
      require =
        jsApply "stdlib.assert" $ [jsArg a, jsAssertInfo ctxt at fs mmsg]
  DLE_Transfer _ who amt ->
    case ctxt_simulate ctxt of
      False -> emptyDoc
      True ->
        jsApply
          "sim_r.txns.push"
          [ jsObject $
              M.fromList $
                [ ("to" :: String, jsArg who)
                , ("amt" :: String, jsArg amt)
                ]
          ]
  DLE_Wait _ amt ->
    case ctxt_simulate ctxt of
      True -> jsApply "void" [jsArg amt]
      False -> "await" <+> jsApply "ctc.wait" [jsArg amt]
  DLE_PartSet _ who what ->
    case ctxt_who ctxt == who of
      True ->
        jsApply "ctc.iam" [jsArg what]
      False ->
        jsArg what

jsEmitSwitch :: (JSCtxt -> k -> Doc) -> JSCtxt -> SrcLoc -> DLVar -> SwitchCases k -> Doc
jsEmitSwitch iter ctxt _at ov csm = "switch" <+> parens (jsVar ov <> "[0]") <+> jsBraces (vsep $ map cm1 $ M.toAscList csm)
  where
    cm1 (vn, (mov', body)) = "case" <+> jsString vn <> ":" <+> jsBraces set_and_body'
      where
        set_and_body' = vsep [set', iter ctxt body, "break;"]
        set' = case mov' of
          Just ov' -> "const" <+> jsVar ov' <+> "=" <+> jsVar ov <> "[1]" <> semi
          Nothing -> emptyDoc

jsCom :: JSCtxt -> PLCommon -> Doc
jsCom ctxt = \case
  DL_Nop _ -> mempty
  DL_Let _ (PV_Let _ dv) de ->
    "const" <+> jsVar dv <+> "=" <+> jsExpr ctxt de <> semi
  DL_Let _ PV_Eff de ->
    jsExpr ctxt de <> semi
  DL_Var _ dv ->
    "let" <+> jsVar dv <> semi
  DL_Set _ dv da ->
    jsVar dv <+> "=" <+> jsArg da <> semi
  DL_LocalIf _ c t f ->
    jsIf (jsArg c) (jsPLTail ctxt t) (jsPLTail ctxt f)
  DL_LocalSwitch at ov csm ->
    jsEmitSwitch jsPLTail ctxt at ov csm
  DL_ArrayMap _ ans x a (DLinBlock _ _ f r) ->
    "const" <+> jsVar ans <+> "=" <+> jsArg x <> "." <> jsApply "map" [(jsApply "" [jsArg $ DLA_Var a] <+> "=>" <+> jsBraces (jsPLTail ctxt f <> hardline <> jsReturn (jsArg r)))]
  DL_ArrayReduce _ ans x z b a (DLinBlock _ _ f r) ->
    "const" <+> jsVar ans <+> "=" <+> jsArg x <> "." <> jsApply "reduce" [(jsApply "" (map (jsArg . DLA_Var) [b, a]) <+> "=>" <+> jsBraces (jsPLTail ctxt f <> hardline <> jsReturn (jsArg r))), jsArg z]

jsPLTail :: JSCtxt -> PLTail -> Doc
jsPLTail ctxt = \case
  DT_Return {} -> emptyDoc
  DT_Com m k -> jsCom ctxt m <> hardline <> jsPLTail ctxt k

jsNewScope :: Doc -> Doc
jsNewScope body =
  jsApply (parens (parens emptyDoc <+> "=>" <+> jsBraces body)) []

jsBlock :: JSCtxt -> PLBlock -> Doc
jsBlock ctxt (DLinBlock _ _ t a) = jsNewScope body
  where
    body = jsPLTail ctxt t <> hardline <> jsReturn (jsArg a)

data AsnMode
  = AM_While
  | AM_WhileSim
  | AM_ContinueOuter
  | AM_ContinueInner
  | AM_ContinueInnerSim

jsAsn :: JSCtxt -> AsnMode -> DLAssignment -> Doc
jsAsn _ctxt mode asn =
  case mode of
    AM_While -> def "let " v a
    AM_WhileSim -> def "const " v a
    AM_ContinueOuter -> def "const " cv a
    AM_ContinueInner -> def emptyDoc v cv
    AM_ContinueInnerSim -> def "const " v cv
  where
    def decl lhs rhs =
      vsep $ map (mk1 decl lhs rhs) $ M.toList asnm
    DLAssignment asnm = asn
    mk1 decl lhs rhs row = decl <> lhs row <+> "=" <+> rhs row <> semi
    v (v_, _) = jsVar v_
    cv (v_, _) = jsContinueVar v_
    a (_, a_) = jsArg a_

jsFromSpec :: JSCtxt -> DLVar -> Doc
jsFromSpec ctxt v =
  "const" <+> jsVar v <+> "=" <+> jsTxn ctxt <> ".from" <> semi <> hardline

jsETail :: JSCtxt -> ETail -> Doc
jsETail ctxt = \case
  ET_Com m k -> jsCom ctxt m <> hardline <> jsETail ctxt k
  ET_Stop _ ->
    case ctxt_simulate ctxt of
      False -> "return" <> semi
      True -> emptyDoc
  ET_If _ c t f -> jsIf (jsArg c) (jsETail ctxt t) (jsETail ctxt f)
  ET_Switch at ov csm -> jsEmitSwitch jsETail ctxt at ov csm
  ET_FromConsensus at which msvs k ->
    case ctxt_simulate ctxt of
      False -> kp
      True ->
        vsep
          [ "sim_r.nextSt =" <+> nextSt' <> semi
          , "sim_r.nextSt_noTime =" <+> nextSt_noTime' <> semi
          , "sim_r.isHalt =" <+> isHalt' <> semi
          ]
    where
      kp = jsETail ctxt k
      (nextSt', nextSt_noTime', isHalt') =
        case msvs of
          Nothing ->
            ( jsDigest [] --- XXX This is only used by Algorand and it should really be zero bytes, but the fakery with numbers and byte lengths is getting me
            , jsDigest []
            , jsCon $ DLL_Bool True
            )
          Just svs ->
            ( mkStDigest svs
            , (trace ("fc" <> show which <> " removing " <> (show $ pretty timev) <> " from " <> (show $ pretty svs) <> " to get " <> (show $ pretty svs')) (mkStDigest svs'))
            , jsCon $ DLL_Bool False
            )
            where
              timev = fromMaybe (impossible "no timev") (ctxt_timev ctxt)
              svs' = dvdeletep timev svs
      mkStDigest svs_ = jsDigest (DLA_Literal (DLL_Int at $ fromIntegral which) : (map snd svs_))
  ET_ToConsensus at fs_ok prev last_timemv which from_me msg amtv timev mto k_ok -> tp
    where
      tp = vsep [defp, k_p]
      (delayp, k_p) =
        case mto of
          Nothing -> ("false", k_okp)
          Just (delays, k_to) -> (jsSum delays, jsIf (jsTimeoutFlag ctxt') k_top k_okp)
            where
              jsSum [] = impossible "no delay"
              jsSum [x] = jsArg x
              jsSum (x : xs) = jsApply "stdlib.add" [jsArg x, jsSum xs]
              k_top = jsETail ctxt' k_to
      msg_vs = map jsVar msg
      msg_vs_defp = "const" <+> jsArray msg_vs <+> "=" <+> (jsTxn ctxt') <> ".data" <> semi <> hardline
      amt_defp = "const" <+> jsVar amtv <+> "=" <+> (jsTxn ctxt') <> ".value" <> semi <> hardline
      time_defp = "const" <+> jsVar timev <+> "=" <+> (jsTxn ctxt') <> ".time" <> semi <> hardline
      k_defp =
        msg_vs_defp
          <> amt_defp
          <> time_defp
          <> jsFromSpec ctxt' fs_ok
      k_okp = k_defp <> jsETail ctxt' k_ok
      ctxt' =
        ctxt
          { ctxt_txn = (ctxt_txn ctxt) + 1
          , ctxt_timev = Just timev
          }
      whop = jsCon $ DLL_Bytes $ ctxt_who ctxt
      defp = "const" <+> jsTxn ctxt' <+> "=" <+> "await" <+> parens callp <> semi
      callp =
        case from_me of
          Just (args, amt, whena, svs, soloSend) -> sendp
            where
              sendp =
                jsApply
                  "ctc.sendrecv"
                  [ whop
                  , pretty which
                  , pretty (length msg)
                  , jsCon (maybe (DLL_Bool False) (DLL_Int at . fromIntegral) (last_timemv >>= flip elemIndex svs))
                  , jsArray $ map (jsContract . argTypeOf) $ svs_as ++ args
                  , vs
                  , amtp
                  , jsArray $ map (jsContract . argTypeOf) $ map DLA_Var msg
                  , jsArg whena
                  , jsCon (DLL_Bool soloSend)
                  , delayp
                  , parens $ "(" <> jsTxn ctxt' <> ") => " <> jsBraces sim_body
                  ]
              svs_as = map DLA_Var svs
              amtp = jsArg amt
              sim_body =
                vsep
                  [ "const sim_r = { txns: [] };"
                  , "sim_r.prevSt =" <+> mkStDigest svs <> semi
                  , "sim_r.prevSt_noPrevTime =" <+> mkStDigest svs_noPrevTime <> semi
                  , k_defp
                  , sim_body_core
                  , "return sim_r;"
                  ]
              svs_noPrevTime_ = dvdeletem last_timemv svs
              svs_noPrevTime = (trace ("tc" <> show which <> " removing " <> (show $ pretty last_timemv) <> " from " <> (show $ pretty svs) <> " to get " <> (show $ pretty svs_noPrevTime_)) svs_noPrevTime_)
              mkStDigest svs_ = jsDigest (DLA_Literal (DLL_Int at $ fromIntegral prev) : (map DLA_Var svs_))
              sim_body_core = jsETail ctxt'_sim k_ok
              ctxt'_sim = ctxt' {ctxt_simulate = True}
              vs = jsArray $ (map jsVar svs) ++ (map jsArg args)
          Nothing -> recvp
      recvp =
        jsApply
          "ctc.recv"
          [ whop
          , pretty which
          , pretty (length msg)
          , jsArray $ map (jsContract . argTypeOf) $ map DLA_Var msg
          , "false"
          , delayp
          ]
  ET_While _ asn cond body k ->
    case ctxt_simulate ctxt of
      False ->
        jsAsn ctxt AM_While asn
          <> hardline
          <> jsWhile (jsBlock ctxt cond) (jsETail ctxt' body)
          <> hardline
          <> jsETail ctxt_tv' k
      True ->
        jsAsn ctxt AM_WhileSim asn
          <> hardline
          <> jsIf (jsBlock ctxt cond) (jsETail ctxt' body) (jsETail ctxt_tv' k)
    where
      mtimev' =
        case ctxt_timev ctxt of
          Nothing -> Just Nothing
          Just timev -> foldl' go Nothing (M.toList asnm)
            where
              go mtv (v, a) =
                case a == DLA_Var timev of
                  True -> Just $ Just v
                  False -> mtv
              DLAssignment asnm = asn
      timev' =
        case mtimev' of
          Nothing -> impossible "no timev in while"
          Just x -> x
      ctxt_tv' = ctxt {ctxt_timev = timev'}
      ctxt' = ctxt_tv' {ctxt_while = Just (timev', cond, body, k)}
  ET_Continue _ asn ->
    jsAsn ctxt AM_ContinueOuter asn <> hardline
      <> case ctxt_simulate ctxt of
        False ->
          jsAsn ctxt AM_ContinueInner asn
            <> hardline
            <> "continue"
            <> semi
        True ->
          case ctxt_while ctxt of
            Nothing -> impossible "continue not in while"
            Just (wtimev', wcond, wbody, wk) ->
              (jsNewScope $
                 jsAsn ctxt AM_ContinueInnerSim asn
                   <> hardline
                   <> jsIf (jsBlock ctxt wcond) (jsETail ctxt' wbody) (jsETail ctxt' wk))
                <> semi
              where
                ctxt' = ctxt {ctxt_timev = wtimev'}
  ET_ConsensusOnly _at l k ->
    case ctxt_simulate ctxt of
      True -> kp
      False -> vsep [lp, kp]
    where
      kp = jsETail ctxt k
      lp = jsPLTail ctxt l

jsPart :: DLInit -> SLPart -> EPProg -> Doc
jsPart dli p (EPProg _ _ et) =
  "export" <+> jsFunction (pretty $ bunpack p) ["ctc", "interact"] bodyp'
  where
    ctxt =
      JSCtxt
        { ctxt_who = p
        , ctxt_txn = 0
        , ctxt_simulate = False
        , ctxt_while = Nothing
        , ctxt_timev = Nothing
        }
    DLInit ctimem = dli
    ctimem' =
      case ctimem of
        Nothing -> mempty
        Just v -> "const" <+> jsVar v <+> "=" <+> "await ctc.creationTime();"
    bodyp' =
      vsep
        [ "const stdlib = ctc.stdlib;"
        , ctimem'
        , jsETail ctxt et
        ]

jsConnInfo :: ConnectorInfo -> Doc
jsConnInfo = \case
  Aeson.Null -> jsCon DLL_Null
  Aeson.Bool b -> jsCon $ DLL_Bool b
  -- Note: only integers are supported.
  -- TODO: throw error if non-integer detected,
  -- or alernatively, support non-integers
  Aeson.Number i -> pretty $ Sci.formatScientific Sci.Fixed (Just 0) i
  Aeson.String t -> jsBacktickText t
  Aeson.Array a -> jsArray $ map jsConnInfo $ Foldable.toList a
  Aeson.Object m -> jsObject $ M.map jsConnInfo $ M.fromList $ HM.toList $ m

jsCnp :: T.Text -> ConnectorInfo -> Doc
jsCnp name cnp = "const" <+> "_" <> pretty name <+> "=" <+> (jsConnInfo cnp) <> semi

jsConnsExp :: [T.Text] -> Doc
jsConnsExp names = "export const _Connectors" <+> "=" <+> jsObject connMap <> semi
  where
    connMap = M.fromList [(name, "_" <> pretty name) | name <- names]

jsPLProg :: ConnectorResult -> PLProg -> Doc
jsPLProg cr (PLProg _ (PLOpts {..}) dli (EPPs pm) _) = modp
  where
    modp = vsep_with_blank $ preamble : emptyDoc : partsp ++ emptyDoc : cnpsp ++ [emptyDoc, connsExp, emptyDoc]
    preamble =
      vsep
        [ pretty $ "// Automatically generated with Reach " ++ versionStr
        , "/* eslint-disable no-unused-vars, no-empty-pattern, no-useless-escape, no-loop-func */"
        , "export const _version =" <+> jsString versionStr <> semi
        ]
    partsp = map (uncurry (jsPart dli)) $ M.toList pm
    cnpsp = map (uncurry jsCnp) $ HM.toList cr
    connsExp = jsConnsExp (HM.keys cr)

backend_js :: Backend
backend_js outn crs pl = do
  let jsf = outn "mjs"
  LTIO.writeFile jsf $ render $ jsPLProg crs pl
