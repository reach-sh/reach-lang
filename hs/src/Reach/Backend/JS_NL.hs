module Reach.Backend.JS_NL (backend_js) where

import Data.Text.Prettyprint.Doc
import Data.Version (showVersion)
import Paths_reach (version)
import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Reach.NL_AST
import Reach.Connector
import Reach.Backend
import Reach.Util

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

jsLambda :: [Doc a] -> Doc a -> Doc a
jsLambda args body = "async" <+> jsApply "" args <+> "=>" <+> jsBraces body

jsWhile :: Doc a -> Doc a -> Doc a
jsWhile cond body = "while" <+> parens cond <+> jsBraces body

jsReturn :: Doc a -> Doc a
jsReturn a = "return" <+> a <> semi

jsBinOp :: String -> Doc a -> Doc a -> Doc a
jsBinOp o l r = l <+> pretty o <+> r

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

data JSCtxt =
  JSCtxt { ctxt_who :: SLPart
         , ctxt_txn :: Int
         , ctxt_simulate :: Bool
         }

jsTxn :: JSCtxt -> Doc a
jsTxn ctxt = "txn" <> pretty (ctxt_txn ctxt)

jsTimeoutFlag :: JSCtxt -> Doc a
jsTimeoutFlag ctxt = jsTxn ctxt <> ".didTimeout" 

jsAssert :: Doc a -> Doc a
--- FIXME Add srcloc and context frames
jsAssert a = jsApply "stdlib.assert" [a] <> semi

jsContract :: SLType -> Doc a
jsContract = \case
  T_Null -> "stdlib.Null"
  T_Bool -> "stdlib.Bool"
  T_UInt256 -> "stdlib.UInt256"
  T_Bytes -> "stdlib.Bytes"
  T_Address -> "stdlib.Address"
  T_Fun {} -> impossible "fun dl"
  T_Array as -> jsApply ("stdlib.Array") $ map jsContract as
  T_Obj m -> jsApply ("stdlib.Object") [ jsObject $ M.map jsContract m ]
  T_Forall {} -> impossible "forall dl"
  T_Var {} -> impossible "var dl"

jsProtect :: SLType -> Doc a -> Doc a
jsProtect how what = jsApply "stdlib.protect" [ jsContract how, what ]

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
  DLA_Array as -> jsArray $ map jsArg as
  DLA_Obj m -> jsObject $ M.map jsArg m
  DLA_Interact _ m t ->
    jsProtect t $ "interact." <> pretty m

jsPrimApply :: JSCtxt -> PrimOp -> [Doc a] -> Doc a
jsPrimApply ctxt = \case
  CP ADD -> jsApply "stdlib.add"
  CP SUB -> jsApply "stdlib.sub"
  CP MUL -> jsApply "stdlib.mul"
  CP DIV -> jsApply "stdlib.div"
  CP MOD -> jsApply "stdlib.mod"
  CP PLT -> jsApply "stdlib.lt"
  CP PLE -> jsApply "stdlib.le"
  CP PEQ -> jsApply "stdlib.eq"
  CP PGE -> jsApply "stdlib.ge"
  CP PGT -> jsApply "stdlib.gt"
  CP LSH -> jsApply "stdlib.lsh"
  CP RSH -> jsApply "stdlib.rsh"
  CP BAND -> jsApply "stdlib.band"
  CP BIOR -> jsApply "stdlib.bior"
  CP BXOR -> jsApply "stdlib.bxor"
  CP IF_THEN_ELSE -> \args -> case args of
    [c, t, f] -> c <+> "?" <+> t <+> ":" <+> f
    _ -> impossible $ "emitJS: ITE called with wrong number of arguments"
  CP BYTES_EQ -> jsApply "stdlib.bytes_eq"
  CP BALANCE -> \_ -> jsTxn ctxt <> ".balance"
  CP TXN_VALUE -> \_ -> jsTxn ctxt <> ".value"
  RANDOM -> jsApply "stdlib.random_uint256"

jsExpr :: JSCtxt -> DLExpr -> Doc a
jsExpr ctxt = \case
  DLE_PrimOp _ p as ->
    jsPrimApply ctxt p $ map jsArg as
  DLE_ArrayRef _ aa ia ->
    jsArg aa <> brackets (jsArg ia)
  DLE_ObjectRef _ oa f ->
    jsArg oa <> "." <> pretty f
  DLE_Interact _ _ m t as ->
    jsProtect t $ "await" <+> (jsApply ("interact." <> m) $ map jsArg as)
  DLE_Digest _ as ->
    jsApply "stdlib.keccak256" $ map jsArg as

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
  PL_Claim _ _ ct a k ->
    check <> iter ctxt k
    where check = case ct of
            CT_Assert -> emptyDoc
            CT_Assume -> require
            CT_Require -> require
            CT_Possible -> emptyDoc
          require = (jsAssert $ jsArg a) <> hardline
  PL_LocalIf _ c t f k ->
    vsep [ jsIf (jsArg c) (jsPLTail ctxt t) (jsPLTail ctxt f)
         , iter ctxt k ]
    
jsPLTail :: JSCtxt -> PLTail -> Doc a
jsPLTail ctxt (PLTail m) = jsCom jsPLTail ctxt m

jsBlock :: JSCtxt -> PLBlock -> Doc a
jsBlock ctxt (PLBlock _ t a) =
  parens (parens emptyDoc <+> "=>" <+> jsBraces body) <> parens emptyDoc
  where body = jsPLTail ctxt t <> hardline <> jsReturn (jsArg a)

jsAsn :: JSCtxt -> Bool -> DLAssignment -> Doc a
jsAsn _ctxt isDefn asn = vsep $ map (uncurry mk1) $ M.toList asnm
  where DLAssignment asnm = asn
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
  ET_Seqn _ f s -> vsep [ jsPLTail ctxt f
                        , jsETail ctxt s ]
  ET_Stop _ a -> "return" <+> jsArg a
  ET_If _ c t f -> jsIf (jsArg c) (jsETail ctxt t) (jsETail ctxt f)
  ET_ToConsensus _ fs_ok which from_me msg mto k_ok -> tp
    where
      tp = vsep [defp, k_p]
      (delayp, k_p) =
        case mto of
          Nothing -> ("false", k_okp)
          Just (delay, k_to) -> (jsArg delay, jsIf (jsTimeoutFlag ctxt') k_top k_okp)
              where k_top = jsETail ctxt' k_to
      msg_vs = map jsVar msg
      k_okp =
        "const" <+> jsArray msg_vs <+> "=" <+> (jsTxn ctxt') <> ".data" <> semi
        <> hardline <> jsFromSpec ctxt' fs_ok
        <> jsETail ctxt' k_ok
      ctxt' = ctxt { ctxt_txn = (ctxt_txn ctxt) + 1 }
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
            where amtp = jsArg amt
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
    <> "continue" <> semi

jsPart :: SLPart -> EPProg -> Doc a
jsPart p (EPProg _ _ et) =
    "export" <+> jsFunction (B.unpack p) (["stdlib", "ctc", "interact"]) bodyp'
  where
    ctxt = JSCtxt
      { ctxt_who = p
      , ctxt_txn = 0
      , ctxt_simulate = False }
    bodyp' =
      vsep
        [ "const" <+> jsTxn ctxt <+> "= { balance: 0, value: 0 }" <> semi
        , jsETail ctxt et
        ]

jsCnp :: String -> M.Map String T.Text -> Doc a
jsCnp name cnp = "export const" <+> pretty name <+> "=" <+> jsObject (M.map jsBacktickText cnp) <> semi

jsPLProg :: ConnectorResult -> PLProg -> Doc a
jsPLProg cr (PLProg _ (EPPs pm) _) = modp
  where
    modp = vsep_with_blank $ preamble : emptyDoc : partsp ++ emptyDoc : cnpsp
    preamble = pretty $ "// Automatically generated with Reach " ++ showVersion version
    partsp = map (uncurry jsPart) $ M.toList pm
    cnpsp = map (uncurry jsCnp) $ M.toList cr

backend_js :: Backend
backend_js outn crs pl = do
  let jsf = outn "mjs"
  writeFile jsf $ show $ jsPLProg crs pl
