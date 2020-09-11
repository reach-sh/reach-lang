module Reach.Backend.JS (backend_js) where

import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import Reach.AST
import Reach.Backend
import Reach.Connector
import Reach.UnsafeUtil
import Reach.Util
import Reach.Version

--- Pretty helpers

vsep_with_blank :: [Doc a] -> Doc a
vsep_with_blank l = vsep $ punctuate emptyDoc l

--- JS Helpers

jsString :: String -> Doc a
jsString s = squotes $ pretty s

jsArray :: [Doc a] -> Doc a
jsArray elems = brackets $ hcat $ punctuate (comma <> space) elems

jsApply :: String -> [Doc a] -> Doc a
jsApply f args = pretty f <> parens (hcat $ punctuate (comma <> space) args)

jsFunction :: String -> [Doc a] -> Doc a -> Doc a
jsFunction name args body =
  "async function" <+> jsApply name args <+> jsBraces body

jsWhile :: Doc a -> Doc a -> Doc a
jsWhile cond body = "while" <+> parens cond <+> jsBraces body

jsReturn :: Doc a -> Doc a
jsReturn a = "return" <+> a <> semi

jsIf :: Doc a -> Doc a -> Doc a -> Doc a
jsIf cap ttp ftp = "if" <+> parens cap <+> jsBraces ttp <> hardline <> "else" <+> jsBraces ftp

jsBraces :: Doc a -> Doc a
jsBraces body = braces (nest 2 $ hardline <> body <> space)

jsObject :: Pretty k => M.Map k (Doc a) -> Doc a
jsObject m = jsBraces $ vsep $ punctuate comma $ map jsObjField $ M.toList m
  where
    jsObjField (k, v) = pretty k <> ":" <+> v

jsBacktickText :: T.Text -> Doc a
jsBacktickText x = "`" <> pretty x <> "`"

--- Compiler

data JSCtxt = JSCtxt
  { ctxt_who :: SLPart
  , ctxt_txn :: Int
  , ctxt_simulate :: Bool
  }

jsTxn :: JSCtxt -> Doc a
jsTxn ctxt = "txn" <> pretty (ctxt_txn ctxt)

jsTimeoutFlag :: JSCtxt -> Doc a
jsTimeoutFlag ctxt = jsTxn ctxt <> ".didTimeout"

jsContract :: SLType -> Doc a
jsContract = \case
  T_Null -> "stdlib.T_Null"
  T_Bool -> "stdlib.T_Bool"
  T_UInt256 -> "stdlib.T_UInt256"
  T_Bytes -> "stdlib.T_Bytes"
  T_Address -> "stdlib.T_Address"
  T_Fun {} -> impossible "fun dl"
  T_Array t sz -> jsApply ("stdlib.T_Array") $ [jsContract t, jsCon (DLC_Int sz)]
  T_Tuple as -> jsApply ("stdlib.T_Tuple") $ [jsArray $ map jsContract as]
  T_Object m -> jsApply ("stdlib.T_Object") [jsObject $ M.map jsContract m]
  T_Forall {} -> impossible "forall dl"
  T_Var {} -> impossible "var dl"
  T_Type {} -> impossible "type dl"

jsProtect :: Doc a -> SLType -> Doc a -> Doc a
jsProtect ai how what =
  jsApply "stdlib.protect" $ [jsContract how, what, ai]

jsAssertInfo :: JSCtxt -> SrcLoc -> [SLCtxtFrame] -> Doc a
jsAssertInfo ctxt at fs =
  jsObject $ M.fromList [("who" :: String, who_p), ("at", at_p), ("fs", fs_p)]
  where
    who_p = jsCon $ DLC_Bytes $ ctxt_who ctxt
    at_p = jsString $ unsafeRedactAbsStr $ show at
    fs_p = jsArray $ map (jsString . unsafeRedactAbsStr . show) fs

jsVar :: DLVar -> Doc a
jsVar (DLVar _ _ _ n) = "v" <> pretty n

jsCon :: DLConstant -> Doc a
jsCon = \case
  DLC_Null -> "null"
  DLC_Bool True -> "true"
  DLC_Bool False -> "false"
  DLC_Int i -> pretty i
  DLC_Bytes b -> jsString $ B.unpack b

jsArg :: DLArg -> Doc a
jsArg = \case
  DLA_Var v -> jsVar v
  DLA_Con c -> jsCon c
  DLA_Array _ as -> jsArg $ DLA_Tuple as
  DLA_Tuple as -> jsArray $ map jsArg as
  DLA_Obj m -> jsObject $ M.map jsArg m
  DLA_Interact _ m t ->
    jsProtect "null" t $ "interact." <> pretty m

jsPrimApply :: JSCtxt -> PrimOp -> [Doc a] -> Doc a
jsPrimApply ctxt = \case
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
  BYTES_EQ -> jsApply "stdlib.bytesEq"
  BALANCE -> \_ -> jsTxn ctxt <> ".balance"
  TXN_VALUE -> \_ -> jsTxn ctxt <> ".value"

jsExpr :: JSCtxt -> DLExpr -> Doc a
jsExpr ctxt = \case
  DLE_Arg _ a ->
    jsArg a
  DLE_Impossible at msg ->
    expect_throw at msg
  DLE_PrimOp _ p as ->
    jsPrimApply ctxt p $ map jsArg as
  DLE_ArrayRef _ _ aa _ ia ->
    jsArg aa <> brackets (jsArg ia)
  DLE_ArraySet _ _ aa _ ia va ->
    jsApply "stdlib.Array_set" $ map jsArg [aa, ia, va]
  DLE_ArrayConcat _ x y ->
    jsArg x <> "." <> jsApply "concat" [ jsArg y ]
  DLE_TupleRef _ aa i ->
    jsArg aa <> brackets (jsCon $ DLC_Int i)
  DLE_ObjectRef _ oa f ->
    jsArg oa <> "." <> pretty f
  DLE_Interact at fs _ m t as ->
    jsProtect (jsAssertInfo ctxt at fs) t $ "await" <+> (jsApply ("interact." <> m) $ map jsArg as)
  DLE_Digest _ as ->
    jsApply "stdlib.keccak256" $ map jsArg as
  DLE_Claim at fs ct a ->
    check
    where
      check = case ct of
        CT_Assert -> impossible "assert"
        CT_Assume -> require
        CT_Require -> require
        CT_Possible -> impossible "possible"
        CT_Unknowable {} -> impossible "unknowable"
      require =
        jsApply "stdlib.assert" $ [jsArg a, jsAssertInfo ctxt at fs]
  DLE_Transfer _ _ who amt ->
    "//" <+> (jsApply "stdlib.transfer" $ map jsArg [who, amt])
  DLE_Wait _ amt ->
    "await" <+> jsApply "ctc.wait" [jsArg amt]
  DLE_PartSet _ who what ->
    case ctxt_who ctxt == who of
      True ->
        jsApply "ctc.iam" [jsArg what]
      False ->
        jsArg what

jsCom :: (JSCtxt -> k -> Doc a) -> JSCtxt -> PLCommon k -> Doc a
jsCom iter ctxt = \case
  PL_Return {} -> emptyDoc
  PL_Let _ _ dv de k ->
    "const" <+> jsVar dv <+> "=" <+> jsExpr ctxt de <> semi <> hardline
      <> iter ctxt k
  PL_Eff _ de k ->
    jsExpr ctxt de <> semi <> hardline
      <> iter ctxt k
  PL_Var _ dv k ->
    "let" <+> jsVar dv <> semi <> hardline
      <> iter ctxt k
  PL_Set _ dv da k ->
    jsVar dv <+> "=" <+> jsArg da <> semi <> hardline
      <> iter ctxt k
  PL_LocalIf _ c t f k ->
    vsep
      [ jsIf (jsArg c) (jsPLTail ctxt t) (jsPLTail ctxt f)
      , iter ctxt k
      ]
  PL_ArrayMap _ ans x a f r k ->
    "const" <+> jsVar ans <+> "=" <+> jsArg x <> "." <> jsApply "map" [ (jsApply "" [ jsArg $ DLA_Var a ] <+> "=>" <+> jsBraces ( jsPLTail ctxt f <> hardline <> jsReturn (jsArg r) )) ] <>
    hardline <> iter ctxt k
  PL_ArrayReduce _ ans x z b a f r k ->
    "const" <+> jsVar ans <+> "=" <+> jsArg x <> "." <> jsApply "reduce" [ (jsApply "" ( map (jsArg . DLA_Var) [ b, a ] ) <+> "=>" <+> jsBraces ( jsPLTail ctxt f <> hardline <> jsReturn (jsArg r) )), jsArg z ] <>
    hardline <> iter ctxt k

jsPLTail :: JSCtxt -> PLTail -> Doc a
jsPLTail ctxt (PLTail m) = jsCom jsPLTail ctxt m

jsBlock :: JSCtxt -> PLBlock -> Doc a
jsBlock ctxt (PLBlock _ t a) =
  parens (parens emptyDoc <+> "=>" <+> jsBraces body) <> parens emptyDoc
  where
    body = jsPLTail ctxt t <> hardline <> jsReturn (jsArg a)

jsAsn :: JSCtxt -> Bool -> DLAssignment -> Doc a
jsAsn _ctxt isDefn asn = vsep $ map (uncurry mk1) $ M.toList asnm
  where
    DLAssignment asnm = asn
    mk1 v a = mdecl <> jsVar v <+> "=" <+> jsArg a <> semi
    mdecl = case isDefn of
      True -> "let "
      False -> emptyDoc

jsFromSpec :: JSCtxt -> FromSpec -> Doc a
jsFromSpec ctxt = \case
  FS_Join v -> "const" <+> jsVar v <+> "=" <+> jsTxn ctxt <> ".from" <> semi <> hardline
  FS_Again _ -> emptyDoc

jsETail :: JSCtxt -> ETail -> Doc a
jsETail ctxt = \case
  ET_Com m -> jsCom jsETail ctxt m
  ET_Seqn _ f s ->
    vsep
      [ jsPLTail ctxt f
      , jsETail ctxt s
      ]
  ET_Stop _ -> "return" <> semi
  ET_If _ c t f -> jsIf (jsArg c) (jsETail ctxt t) (jsETail ctxt f)
  ET_ToConsensus _ fs_ok which from_me msg mto k_ok -> tp
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
      k_okp =
        "const" <+> jsArray msg_vs <+> "=" <+> (jsTxn ctxt') <> ".data" <> semi
          <> hardline
          <> jsFromSpec ctxt' fs_ok
          <> jsETail ctxt' k_ok
      ctxt' = ctxt {ctxt_txn = (ctxt_txn ctxt) + 1}
      whop = jsCon $ DLC_Bytes $ ctxt_who ctxt
      defp = "const" <+> jsTxn ctxt' <+> "=" <+> "await" <+> callp <> semi
      callp =
        case from_me of
          Just (args, amt, svs) ->
            jsApply
              "ctc.sendrecv"
              [ whop
              , jsCon (DLC_Int $ fromIntegral which)
              , jsCon (DLC_Int $ fromIntegral $ length msg)
              , vs
              , amtp
              , delayp
              , "null" --- XXX implement simulation to discover transfer in EPP, not here.
              ]
            where
              amtp = jsArg amt
              --- ok_sim_p = jsETail ctxt'_sim k_ok
              --- ctxt'_sim = ctxt' { ctxt_simulate = True }
              vs = jsArray $ (map jsVar svs) ++ (map jsArg args)
          Nothing ->
            jsApply
              "ctc.recv"
              [ whop
              , jsCon (DLC_Int $ fromIntegral which)
              , jsCon (DLC_Int $ fromIntegral $ length msg)
              , delayp
              ]
  ET_While _ asn cond body k ->
    jsAsn ctxt True asn
      <> hardline
      <> jsWhile (jsBlock ctxt cond) (jsETail ctxt body)
      <> hardline
      <> jsETail ctxt k
  ET_Continue _ asn ->
    jsAsn ctxt False asn
      <> hardline
      <> "continue"
      <> semi

jsPart :: SLPart -> EPProg -> Doc a
jsPart p (EPProg _ _ et) =
  "export" <+> jsFunction (B.unpack p) (["stdlib", "ctc", "interact"]) bodyp'
  where
    ctxt =
      JSCtxt
        { ctxt_who = p
        , ctxt_txn = 0
        , ctxt_simulate = False
        }
    bodyp' =
      vsep
        [ "const" <+> jsTxn ctxt <+> "= { balance: 0, value: 0 }" <> semi
        , jsETail ctxt et
        ]

jsCnp :: String -> M.Map String T.Text -> Doc a
jsCnp name cnp = "const" <+> "_" <> pretty name <+> "=" <+> jsObject (M.map jsBacktickText cnp) <> semi

jsConnsExp :: [String] -> Doc a
jsConnsExp names = "export const _Connectors" <+> "=" <+> jsObject connMap <> semi
  where
    connMap = M.fromList [(name, "_" <> pretty name) | name <- names]

jsPLProg :: ConnectorResult -> PLProg -> Doc a
jsPLProg cr (PLProg _ (PLOpts {..}) (EPPs pm) _) = modp
  where
    modp = vsep_with_blank $ preamble : emptyDoc : partsp ++ emptyDoc : cnpsp ++ [emptyDoc, connsExp, emptyDoc]
    preamble = pretty $ "// Automatically generated with Reach " ++ versionStr
    partsp = map (uncurry jsPart) $ M.toList pm
    cnpsp = map (uncurry jsCnp) $ M.toList cr
    connsExp = jsConnsExp (M.keys cr)

backend_js :: Backend
backend_js outn crs pl = do
  let jsf = outn "mjs"
  writeFile jsf $ show $ jsPLProg crs pl
