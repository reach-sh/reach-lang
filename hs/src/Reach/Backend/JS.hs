module Reach.Backend.JS (backend_js) where

import Control.Monad.Reader
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as B
import qualified Data.Foldable as Foldable
import qualified Data.HashMap.Strict as HM
import Data.IORef
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
import Reach.Counter
import Reach.Texty
import Reach.UnsafeUtil
import Reach.Util
import Reach.Version

-- import Debug.Trace

--- Pretty helpers

sb :: SrcLoc
sb = srcloc_builtin

vsep_with_blank :: [Doc] -> Doc
vsep_with_blank l = vsep $ punctuate emptyDoc l

--- JS Helpers

jsMapIdx :: DLMVar -> Doc
jsMapIdx (DLMVar i) = pretty i

jsMapVar :: DLMVar -> Doc
jsMapVar mpv = pretty mpv

jsMapVarCtc :: DLMVar -> Doc
jsMapVarCtc mpv = jsMapVar mpv <> "_ctc"

jsString :: String -> Doc
jsString s = squotes $ pretty $ escape s
  where
    escape = concatMap escapec
    escapec = \case
      '\'' -> "\\'"
      c -> [c]

jsArray :: [Doc] -> Doc
jsArray elems = brackets $ hcat $ punctuate (comma <> space) elems

jsApply :: Doc -> [Doc] -> Doc
jsApply f args = f <> parens (hcat $ punctuate (comma <> space) args)

jsApplyKws :: Doc -> M.Map String Doc -> Doc
jsApplyKws f m = jsApply f [ jsObject m ]

jsFunction_ :: Doc -> [Doc] -> Doc -> Doc
jsFunction_ name args body =
  "function" <+> jsApply name args <+> jsBraces body <> semi

jsFunction :: Doc -> [Doc] -> Doc -> Doc
jsFunction name args body = "async" <+> jsFunction_ name args body

jsWhile :: Doc -> Doc -> Doc
jsWhile cond body = "while" <+> parens cond <+> jsBraces body

jsReturn :: Doc -> Doc
jsReturn a = "return" <+> a <> semi

jsWhen :: Doc -> Doc -> Doc
jsWhen cap ttp = "if" <+> parens cap <+> jsBraces ttp

jsIf :: Doc -> Doc -> Doc -> Doc
jsIf cap ttp ftp = jsWhen cap ttp <> hardline <> "else" <+> jsBraces ftp

jsBraces :: Doc -> Doc
jsBraces body = braces (nest $ hardline <> body)

jsObject :: Pretty k => M.Map k Doc -> Doc
jsObject m = jsBraces $ vsep $ punctuate comma $ map jsObjField $ M.toList m
  where
    jsObjField (k, v) = pretty k <> ":" <+> v

jsBacktickText :: T.Text -> Doc
jsBacktickText x = "`" <> pretty x <> "`"

-- Compiler

data JSContracts = JSContracts
  { jsc_idx :: Counter
  , jsc_t2i :: IORef (M.Map DLType Doc)
  , jsc_i2t :: IORef (M.Map Int Doc)
  }

data JSCtxtWhile
  = JWhile_None
  | JWhile_Diverge
  | JWhile_Some JSCtxtWhile DLBlock ETail ETail

data JSMode
  = JM_Simulate
  | JM_View
  | JM_Backend
  deriving (Eq)

data JSCtxt = JSCtxt
  { ctxt_who :: SLPart
  , ctxt_txn :: Int
  , ctxt_mode :: JSMode
  , ctxt_while :: JSCtxtWhile
  , ctxt_ctcs :: Maybe JSContracts
  , ctxt_maps :: M.Map DLMVar DLMapInfo
  }

type App = ReaderT JSCtxt IO

type AppT a = a -> App Doc

instance Semigroup a => Semigroup (App a) where
  mx <> my = (<>) <$> mx <*> my

instance Monoid a => Monoid (App a) where
  mempty = return mempty

jsTxn :: App Doc
jsTxn = ("txn" <>) . pretty . ctxt_txn <$> ask

jsTimeoutFlag :: App Doc
jsTimeoutFlag = (<> ".didTimeout") <$> jsTxn

jsContract_ :: DLType -> App Doc
jsContract_ = \case
  T_Null -> return "stdlib.T_Null"
  T_Bool -> return "stdlib.T_Bool"
  T_UInt -> return "stdlib.T_UInt"
  T_Bytes sz -> do
    sz' <- jsCon $ DLL_Int sb sz
    return $ jsApply "stdlib.T_Bytes" [sz']
  T_Digest -> return $ "stdlib.T_Digest"
  T_Address -> return $ "stdlib.T_Address"
  T_Token -> return $ "stdlib.T_Token"
  T_Array t sz -> do
    t' <- jsContract t
    sz' <- jsCon (DLL_Int sb sz)
    return $ jsApply ("stdlib.T_Array") $ [t', sz']
  T_Tuple as -> do
    as' <- mapM jsContract as
    return $ jsApply ("stdlib.T_Tuple") $ [jsArray as']
  T_Object m -> do
    m' <- mapM jsContract m
    return $ jsApply ("stdlib.T_Object") [jsObject m']
  T_Data m -> do
    m' <- mapM jsContract m
    return $ jsApply ("stdlib.T_Data") [jsObject m']
  T_Struct as -> do
    let go (k, t) = do
          t' <- jsContract t
          return $ jsArray [jsString k, t']
    as' <- mapM go as
    return $ jsApply ("stdlib.T_Struct") $ [jsArray as']

jsContract :: DLType -> App Doc
jsContract t = do
  (ctxt_ctcs <$> ask) >>= \case
    Nothing -> jsContract_ t
    Just (JSContracts {..}) -> do
      t2i <- liftIO $ readIORef jsc_t2i
      case M.lookup t t2i of
        Just d -> return d
        Nothing -> do
          def_rhs <- jsContract_ t
          ti <- liftIO $ incCounter jsc_idx
          let d = "ctc" <> pretty ti
          let def = "const" <+> d <+> "=" <+> def_rhs <> semi
          liftIO $ modifyIORef jsc_t2i $ M.insert t d
          liftIO $ modifyIORef jsc_i2t $ M.insert ti def
          return d

jsProtect_ :: Doc -> Doc -> Doc -> Doc
jsProtect_ ai how what =
  jsApply "stdlib.protect" $ [how, what, ai]

jsProtect :: Doc -> DLType -> Doc -> App Doc
jsProtect ai how what =
  flip (jsProtect_ ai) what <$> jsContract how

jsAt :: SrcLoc -> Doc
jsAt at = jsString $ unsafeRedactAbsStr $ show at

jsAssertInfo :: SrcLoc -> [SLCtxtFrame] -> Maybe B.ByteString -> App Doc
jsAssertInfo at fs mmsg = do
  let msg_p = case mmsg of
        Nothing -> "null"
        Just b -> jsString $ bunpack b
  who <- ctxt_who <$> ask
  who_p <- jsCon $ DLL_Bytes $ who
  let fs_p = jsArray $ map (jsString . unsafeRedactAbsStr . show) fs
  return $
    jsObject $
      M.fromList
        [ ("who" :: String, who_p)
        , ("msg", msg_p)
        , ("at", jsAt at)
        , ("fs", fs_p)
        ]

jsVar :: AppT DLVar
jsVar (DLVar _ _ _ n) = return $ "v" <> pretty n

jsContinueVar :: AppT DLVar
jsContinueVar dv = ("c" <>) <$> jsVar dv

jsCon :: AppT DLLiteral
jsCon = \case
  DLL_Null -> return "null"
  DLL_Bool True -> return "true"
  DLL_Bool False -> return "false"
  DLL_Int at i -> do
    uim <- jsArg (DLA_Constant $ DLC_UInt_max)
    return $ jsApply "stdlib.checkedBigNumberify" [jsAt at, uim, pretty i]
  DLL_Bytes b -> return $ jsString $ bunpack b

jsArg :: AppT DLArg
jsArg = \case
  DLA_Var v -> jsVar v
  DLA_Constant c ->
    case c of
      DLC_UInt_max ->
        return "stdlib.UInt_max"
  DLA_Literal c -> jsCon c
  DLA_Interact who m t ->
    jsProtect (jsString $ "for " <> bunpack who <> "'s interact field " <> m) t $ "interact." <> pretty m

jsLargeArg :: AppT DLLargeArg
jsLargeArg = \case
  DLLA_Array _ as ->
    jsLargeArg $ DLLA_Tuple as
  DLLA_Tuple as ->
    jsArray <$> mapM jsArg as
  DLLA_Obj m ->
    jsObject <$> mapM jsArg m
  DLLA_Data _ vn vv -> do
    vv' <- jsArg vv
    return $ jsArray [jsString vn, vv']
  DLLA_Struct kvs ->
    jsLargeArg $ DLLA_Obj $ M.fromList kvs

jsContractAndVals :: [DLArg] -> App [Doc]
jsContractAndVals as = do
  let la = DLLA_Tuple as
  ctc <- jsContract $ largeArgTypeOf la
  as' <- jsLargeArg la
  return $ [ctc, as']

jsDigest :: AppT [DLArg]
jsDigest as = jsApply "stdlib.digest" <$> jsContractAndVals as

jsPrimApply :: PrimOp -> [Doc] -> Doc
jsPrimApply = \case
  SELF_ADDRESS -> jsApply "ctc.selfAddress"
  ADD -> jsApply "stdlib.add"
  SUB -> jsApply "stdlib.sub"
  MUL -> jsApply "stdlib.mul"
  DIV -> jsApply "stdlib.div"
  MUL_DIV -> jsApply "stdlib.muldiv"
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
  TOKEN_EQ -> jsApply "stdlib.tokenEq"
  BYTES_CONCAT -> jsApply "stdlib.bytesConcat"

jsArg_m :: AppT (Maybe DLArg)
jsArg_m = \case
  Nothing -> return $ "undefined"
  Just a -> jsArg a

jsExpr :: AppT DLExpr
jsExpr = \case
  DLE_Arg _ a ->
    jsArg a
  DLE_LArg _ la ->
    jsLargeArg la
  DLE_Impossible at _ err ->
    expect_thrown at err
  DLE_PrimOp _ p as ->
    jsPrimApply p <$> mapM jsArg as
  DLE_ArrayRef _ aa ia -> do
    aa' <- jsArg aa
    ia' <- jsArg ia
    return $ aa' <> brackets ia'
  DLE_ArraySet _ aa ia va -> do
    as' <- mapM jsArg [aa, ia, va]
    return $ jsApply "stdlib.Array_set" as'
  DLE_ArrayConcat _ x y -> do
    x' <- jsArg x
    y' <- jsArg y
    return $ x' <> "." <> jsApply "concat" [y']
  DLE_ArrayZip _ x y -> do
    as' <- mapM jsArg [x, y]
    return $ jsApply "stdlib.Array_zip" as'
  DLE_TupleRef at aa i -> do
    aa' <- jsArg aa
    i' <- jsCon $ DLL_Int at i
    return $ aa' <> brackets i'
  DLE_ObjectRef _ oa f -> do
    oa' <- jsArg oa
    return $ oa' <> "." <> pretty f
  DLE_Interact at fs _ m t as -> do
    ai' <- jsAssertInfo at fs (Just $ bpack m)
    as' <- mapM jsArg as
    jsProtect ai' t $ "await" <+> (jsApply ("interact." <> pretty m) as')
  DLE_Digest _ as -> jsDigest as
  DLE_Claim at fs ct a mmsg ->
    check
    where
      check = case ct of
        CT_Assert -> impossible "assert"
        CT_Assume _ -> require
        CT_Require -> require
        CT_Possible -> impossible "possible"
        CT_Unknowable {} -> impossible "unknowable"
      require = do
        a' <- jsArg a
        ai' <- jsAssertInfo at fs mmsg
        return $ jsApply "stdlib.assert" $ [a', ai']
  DLE_Transfer _ who amt mtok ->
    (ctxt_mode <$> ask) >>= \case
      JM_Backend -> mempty
      JM_View -> impossible "view transfer"
      JM_Simulate -> do
        who' <- jsArg who
        amt' <- jsArg amt
        mtok' <- jsArg_m mtok
        return $
          jsSimTxn "from" $
            [ ("to", who')
            , ("amt", amt')
            , ("tok", mtok')
            ]
  DLE_TokenInit _ tok -> do
    (ctxt_mode <$> ask) >>= \case
      JM_Backend -> mempty
      JM_View -> impossible "view tokeninit"
      JM_Simulate -> do
        zero' <- jsCon $ DLL_Int sb 0
        tok' <- jsArg tok
        return $
          jsSimTxn "init" $
            [ ("amt", zero')
            , ("tok", tok')
            ]
  DLE_CheckPay _ _ amt mtok -> do
    (ctxt_mode <$> ask) >>= \case
      JM_Backend -> mempty
      JM_View -> impossible "view checkpay"
      JM_Simulate -> do
        amt' <- jsArg amt
        mtok' <- jsArg_m mtok
        return $
          jsSimTxn "to" $
            [ ("amt", amt')
            , ("tok", mtok')
            ]
  DLE_Wait _ amtt -> do
    let (which, amt) = case amtt of
                         Left t -> ("waitTime", t)
                         Right t -> ("waitSecs", t)
    amt' <- jsArg amt
    (ctxt_mode <$> ask) >>= \case
      JM_Simulate -> return $ jsApply "void" [amt']
      JM_Backend -> return $ "await" <+> jsApply ("ctc." <> which) [amt']
      JM_View -> impossible "view wait"
  DLE_PartSet _ who what -> do
    rwho <- ctxt_who <$> ask
    case rwho == who of
      True -> do
        what' <- jsArg what
        return $ jsApply "ctc.iam" [what']
      False ->
        jsArg what
  DLE_MapRef _ mpv fa -> do
    let ctc = jsMapVarCtc mpv
    fa' <- jsArg fa
    (f, args) <-
      (ctxt_mode <$> ask) >>= \case
        JM_Simulate -> return $ ("stdlib.simMapRef", ["sim_r", jsMapIdx mpv])
        JM_Backend -> return $ ("stdlib.mapRef", [jsMapVar mpv])
        JM_View -> return $ ("await viewlib.viewMapRef", [jsMapIdx mpv])
    return $ jsProtect_ "null" ctc $ jsApply f $ args <> [fa']
  DLE_MapSet _ mpv fa mna -> do
    fa' <- jsArg fa
    na' <- case mna of
      Just na -> jsArg na
      Nothing -> return "undefined"
    (ctxt_mode <$> ask) >>= \case
      JM_Simulate ->
        return $ jsApply "stdlib.simMapSet" ["sim_r", jsMapIdx mpv, fa', na']
      JM_Backend ->
        return $ jsMapVar mpv <> brackets fa' <+> "=" <+> na'
      JM_View -> impossible "view mapset"
  DLE_Remote {} -> impossible "remote"
  DLE_TokenNew {} -> impossible "tokennew"
  DLE_TokenBurn _ ta aa ->
    (ctxt_mode <$> ask) >>= \case
      JM_Simulate -> do
        ta' <- jsArg ta
        aa' <- jsArg aa
        return $ jsApply "stdlib.simTokenBurn" ["sim_r", ta', aa']
      JM_Backend -> return "undefined"
      JM_View -> impossible "token.burn"
  DLE_TokenDestroy _ ta ->
    (ctxt_mode <$> ask) >>= \case
      JM_Simulate -> do
        ta' <- jsArg ta
        return $ jsApply "stdlib.simTokenDestroy" ["sim_r", ta']
      JM_Backend -> return "undefined"
      JM_View -> impossible "token.burn"
  DLE_TimeOrder {} -> impossible "timeorder"

jsEmitSwitch :: AppT k -> SrcLoc -> DLVar -> SwitchCases k -> App Doc
jsEmitSwitch iter _at ov csm = do
  ov' <- jsVar ov
  let cm1 (vn, (ov2, _, body)) = do
        body' <- iter body
        ov2' <- jsVar ov2
        let set' = "const" <+> ov2' <+> "=" <+> ov' <> "[1]" <> semi
        let set_and_body' = vsep [set', body', "break;"]
        return $ "case" <+> jsString vn <> ":" <+> jsBraces set_and_body'
  csm' <- mapM cm1 $ M.toAscList csm
  return $ "switch" <+> parens (ov' <> "[0]") <+> jsBraces (vsep csm')

doGetOutput :: String -> AppT DLVar
doGetOutput mode dv = do
  dv' <- jsVar dv
  txn' <- jsTxn
  dvt' <- jsContract $ varType dv
  jsConstDefn dv $ "await" <+> txn' <> "." <> jsApply "getOutput" [squotes (pretty mode), squotes dv', dvt']

jsConstDefn :: DLVar -> Doc -> App Doc
jsConstDefn dv rhs = do
  dv' <- jsVar dv
  return $ "const" <+> dv' <+> "=" <+> rhs <> semi

jsCom :: AppT DLStmt
jsCom = \case
  DL_Nop _ -> mempty
  DL_Let _ DLV_Eff (DLE_Remote {}) -> mempty
  DL_Let _ (DLV_Let _ dv) (DLE_Remote at fs _av _f _pa _as _ba) ->
    (ctxt_mode <$> ask) >>= \case
      JM_Backend -> doGetOutput "remote" dv
      JM_View -> impossible "view remote"
      JM_Simulate -> do
        jsConstDefn dv =<< jsExpr (DLE_Claim at fs CT_Require (DLA_Literal $ DLL_Bool False) (Just $ "cannot simulate remote objects"))
  DL_Let _ DLV_Eff (DLE_TokenNew {}) -> mempty
  DL_Let _ (DLV_Let _ dv) (DLE_TokenNew _ tns) -> do
    (ctxt_mode <$> ask) >>= \case
      JM_Backend -> doGetOutput "tokenNew" dv
      JM_View -> impossible "view output"
      JM_Simulate -> do
        let DLTokenNew {..} = tns
        n' <- jsArg dtn_name
        s' <- jsArg dtn_sym
        u' <- jsArg dtn_url
        m' <- jsArg dtn_metadata
        p' <- jsArg dtn_supply
        jsConstDefn dv $ jsApply "stdlib.simTokenNew" [ "sim_r", n', s', u', m', p' ]
  DL_Let _ (DLV_Let _ dv) de -> do
    jsConstDefn dv =<< jsExpr de
  DL_Let _ DLV_Eff de -> do
    de' <- jsExpr de
    return $ de' <> semi
  DL_Var _ dv -> do
    dv' <- jsVar dv
    return $ "let" <+> dv' <> semi
  DL_Set _ dv da -> do
    dv' <- jsVar dv
    da' <- jsArg da
    return $ dv' <+> "=" <+> da' <> semi
  DL_LocalIf _ c t f -> do
    c' <- jsArg c
    t' <- jsPLTail t
    f' <- jsPLTail f
    return $ jsIf c' t' f'
  DL_LocalSwitch at ov csm ->
    jsEmitSwitch jsPLTail at ov csm
  DL_ArrayMap _ ans x a (DLBlock _ _ f r) -> do
    ans' <- jsVar ans
    x' <- jsArg x
    a' <- jsArg $ DLA_Var a
    f' <- jsPLTail f
    r' <- jsArg r
    return $ "const" <+> ans' <+> "=" <+> x' <> "." <> jsApply "map" [(jsApply "" [a'] <+> "=>" <+> jsBraces (f' <> hardline <> jsReturn r'))]
  DL_ArrayReduce _ ans x z b a (DLBlock _ _ f r) -> do
    ans' <- jsVar ans
    x' <- jsArg x
    z' <- jsArg z
    a' <- jsArg $ DLA_Var a
    b' <- jsArg $ DLA_Var b
    f' <- jsPLTail f
    r' <- jsArg r
    return $ "const" <+> ans' <+> "=" <+> x' <> "." <> jsApply "reduce" [(jsApply "" [b', a']) <+> "=>" <+> jsBraces (f' <> hardline <> jsReturn r'), z']
  DL_MapReduce {} ->
    impossible $ "cannot inspect maps at runtime"
  DL_Only _at (Right c) l -> do
    m <- ctxt_mode <$> ask
    let sim = m == JM_Simulate
    case (not c || (c && not sim)) of
      True -> jsPLTail l
      False -> mempty
  DL_Only {} -> impossible $ "left only after EPP"
  DL_LocalDo _ t -> jsPLTail t

jsPLTail :: AppT DLTail
jsPLTail = \case
  DT_Return {} -> mempty
  DT_Com m k -> jsCom m <> pure hardline <> jsPLTail k

jsNewScope :: Doc -> Doc
jsNewScope body =
  jsApply (parens (parens emptyDoc <+> "=>" <+> jsBraces body)) []

jsBlockNewScope :: AppT DLBlock
jsBlockNewScope b = do
  (t', a') <- jsBlock b
  return $ jsNewScope $ t' <> hardline <> jsReturn a'

jsBlock :: DLBlock -> App (Doc, Doc)
jsBlock (DLBlock _ _ t a) = do
  t' <- jsPLTail t
  a' <- jsArg a
  return (t', a')

data AsnMode
  = AM_While
  | AM_WhileSim
  | AM_ContinueOuter
  | AM_ContinueInner
  | AM_ContinueInnerSim

jsAsn :: AsnMode -> DLAssignment -> App Doc
jsAsn mode asn =
  case mode of
    AM_While -> def "let " v a
    AM_WhileSim -> def "const " v a
    AM_ContinueOuter -> def "const " cv a
    AM_ContinueInner -> def emptyDoc v cv
    AM_ContinueInnerSim -> def "const " v cv
  where
    def decl lhs rhs =
      vsep <$> (mapM (mk1 decl lhs rhs) $ M.toList asnm)
    DLAssignment asnm = asn
    mk1 decl lhs rhs row = do
      lhs' <- lhs row
      rhs' <- rhs row
      return $ decl <> lhs' <+> "=" <+> rhs' <> semi
    v (v_, _) = jsVar v_
    cv (v_, _) = jsContinueVar v_
    a (_, a_) = jsArg a_

jsFromSpec :: AppT DLVar
jsFromSpec v = do
  v' <- jsVar v
  txn <- jsTxn
  return $ "const" <+> v' <+> "=" <+> txn <> ".from" <> semi <> hardline

jsPayAmt :: AppT DLPayAmt
jsPayAmt (DLPayAmt {..}) = do
  net' <- jsArg pa_net
  let go a t = do
        a' <- jsArg a
        t' <- jsArg t
        return $ jsArray [a', t']
  ks' <- jsArray <$> (mapM (uncurry go) pa_ks)
  return $ jsArray $ [net', ks']

jsSimTxn :: String -> [(String, Doc)] -> Doc
jsSimTxn kind kvs =
  jsApply "sim_r.txns.push" $
    [jsObject $ M.fromList $ [("kind", jsString kind)] <> kvs]

jsTimeArg :: AppT DLTimeArg
jsTimeArg ta = do
  let (lab, a) = case ta of
                   Left x -> ("time", x)
                   Right x -> ("secs", x)
  a' <- jsArg a
  return $ jsArray $ [ jsString lab, a' ]

jsETail :: AppT ETail
jsETail = \case
  ET_Com m k -> jsCom m <> return hardline <> jsETail k
  ET_Stop _ ->
    (ctxt_mode <$> ask) >>= \case
      JM_Simulate -> mempty
      _ -> return $ "return" <> semi
  ET_If _ c t f -> jsIf <$> jsArg c <*> jsETail t <*> jsETail f
  ET_Switch at ov csm -> jsEmitSwitch jsETail at ov csm
  ET_FromConsensus _at _which msvs k ->
    (ctxt_mode <$> ask) >>= \case
      JM_Backend -> jsETail k
      JM_View -> impossible "view from"
      JM_Simulate -> do
        let common extra isHalt' =
              vsep $
                extra
                  <> [ "sim_r.isHalt =" <+> isHalt' <> semi
                     ]
        case msvs of
          FI_Halt toks -> do
            --- XXX This is only used by Algorand and it should really be zero bytes, but the fakery with numbers and byte lengths is getting me
            let close mtok = do
                  mtok' <- jsArg_m mtok
                  return $ jsSimTxn "halt" [("tok", mtok')]
            let close_escrow = close Nothing
            let close_asset = close . Just
            closes <- (<>) <$> forM toks close_asset <*> ((\x -> [x]) <$> close_escrow)
            common closes <$> (jsCon $ DLL_Bool True)
          FI_Continue _vis _svs -> do
            common [] <$> (jsCon $ DLL_Bool False)
  ET_ToConsensus _at fs_ok _prev which from_me msg_vs _out timev secsv mto k_ok -> do
    msg_ctcs <- mapM (jsContract . argTypeOf) $ map DLA_Var msg_vs
    msg_vs' <- mapM jsVar msg_vs
    let withCtxt = local (\e -> e { ctxt_txn = (ctxt_txn e) + 1 })
    txn <- withCtxt jsTxn
    let msg_vs_defp = "const" <+> jsArray msg_vs' <+> "=" <+> txn <> ".data" <> semi <> hardline
    timev' <- jsVar timev
    let time_defp = "const" <+> timev' <+> "=" <+> txn <> ".time" <> semi <> hardline
    secsv' <- jsVar secsv
    let secs_defp = "const" <+> secsv' <+> "=" <+> txn <> ".secs" <> semi <> hardline
    fs_ok' <- withCtxt $ jsFromSpec fs_ok
    let k_defp = msg_vs_defp <> time_defp <> secs_defp <> fs_ok'
    k_ok' <- withCtxt $ jsETail k_ok
    let k_okp = k_defp <> k_ok'
    (delayp, k_p) <-
      case mto of
        Nothing -> return ("undefined", k_okp)
        Just (delays, k_to) -> do
          k_top <- withCtxt $ jsETail k_to
          delays' <- case delays of
                      Nothing -> return "undefined"
                      Just x -> jsTimeArg x
          timef <- withCtxt $ jsTimeoutFlag
          return (delays', jsIf timef k_top k_okp)
    let a_funcNum = pretty which
    let a_evt_cnt = pretty $ length msg_vs
    let a_out_tys = jsArray msg_ctcs
    let a_timeout_delay = delayp
    let recvp = jsApplyKws "ctc.recv" $ M.fromList $
          [ ("funcNum", a_funcNum)
          , ("evt_cnt", a_evt_cnt)
          , ("out_tys", a_out_tys)
          , ("waitIfNotPresent", "false")
          , ("timeoutAt", a_timeout_delay) ]
    callp <-
      withCtxt $
        case from_me of
          Just (args, amt, whena, svs, soloSend) -> do
            let svs_as = map DLA_Var svs
            amtp <- jsPayAmt amt
            let withSim = local (\e -> e {ctxt_mode = JM_Simulate})
            sim_body_core <- withSim $ jsETail k_ok
            let dupeMap (mpv, _) = do
                  return $
                    (jsApply "stdlib.simMapDupe" $
                       ["sim_r", jsMapIdx mpv, jsMapVar mpv])
                      <> semi
            dupeMaps <- mapM dupeMap =<< ((M.toAscList . ctxt_maps) <$> ask)
            let sim_body =
                  vsep
                    [ "const sim_r = { txns: [], mapRefs: [], mapsPrev: [], mapsNext: [] };"
                    , vsep dupeMaps
                    , k_defp
                    , sim_body_core
                    , "return sim_r;"
                    ]
            vs <- jsArray <$> ((++) <$> mapM jsVar svs <*> mapM jsArg args)
            whena' <- jsArg whena
            soloSend' <- jsCon (DLL_Bool soloSend)
            msgts <- mapM (jsContract . argTypeOf) $ svs_as ++ args
            let a_sim_p = parens $ "async" <+> "(" <> txn <> ") => " <> jsBraces sim_body
            let sendp = jsApplyKws "ctc.sendrecv" $ M.fromList $
                  [ ("funcNum", a_funcNum)
                  , ("evt_cnt", a_evt_cnt)
                  , ("tys", jsArray msgts)
                  , ("args", vs)
                  , ("pay", amtp)
                  , ("out_tys", a_out_tys)
                  , ("onlyIf", whena')
                  , ("soloSend", soloSend')
                  , ("waitIfNotPresent", "false")
                  , ("timeoutAt", a_timeout_delay)
                  , ("sim_p", a_sim_p) ]
            return sendp
          Nothing ->
            return recvp
    let defp = "const" <+> txn <+> "=" <+> "await" <+> parens callp <> semi
    return $ vsep [defp, k_p]
  ET_While at asn cond body k -> do
    oldWhile <- ctxt_while <$> ask
    let newWhile = JWhile_Some oldWhile cond body k
    let newCtxt' = local (\e -> e {ctxt_while = newWhile})
    cond' <- jsBlockNewScope cond
    body' <- newCtxt' $ jsETail body
    k' <- jsETail k
    (ctxt_mode <$> ask) >>= \case
      JM_Backend -> do
        asn' <- jsAsn AM_While asn
        return $ asn' <> hardline <> jsWhile cond' body' <> hardline <> k'
      JM_Simulate -> do
        asn' <- jsAsn AM_WhileSim asn
        return $ asn' <> hardline <> jsIf cond' body' k'
      JM_View -> impossible $ "view while " <> show at
  ET_Continue at asn -> do
    asn'o <- jsAsn AM_ContinueOuter asn
    asn'i <-
      (ctxt_mode <$> ask) >>= \case
        JM_View -> impossible "view continue"
        JM_Backend -> do
          asn_ <- jsAsn AM_ContinueInner asn
          return $ asn_ <> hardline <> "continue" <> semi
        JM_Simulate ->
          (ctxt_while <$> ask) >>= \case
            JWhile_None -> impossible $ "continue not in while: " <> show at
            JWhile_Diverge -> impossible $ "diverging while " <> show at
            JWhile_Some woldWhile wcond wbody wk -> do
              let newCtxt_noWhile = local (\e -> e {ctxt_while = JWhile_Diverge})
              let newCtxt_oldWhile = local (\e -> e {ctxt_while = woldWhile})
              asn_ <- jsAsn AM_ContinueInnerSim asn
              wcond' <- jsBlockNewScope wcond
              wbody' <- newCtxt_noWhile $ jsETail wbody
              wk' <- newCtxt_oldWhile $ jsETail wk
              return $ (jsNewScope $ asn_ <> hardline <> jsIf wcond' wbody' wk') <> semi
    return $ asn'o <> hardline <> asn'i

newJsContract :: App JSContracts
newJsContract = do
  jsc_idx <- liftIO $ newCounter 0
  jsc_t2i <- liftIO $ newIORef mempty
  jsc_i2t <- liftIO $ newIORef mempty
  return $ JSContracts {..}

jsMapDefns :: Bool -> App Doc
jsMapDefns varsHuh = do
  vsep <$> (mapM go . M.toAscList =<< (ctxt_maps <$> ask))
  where
    go (mpv, mi) = do
      ctc <- jsContract $ dlmi_tym mi
      return $ vsep $
        (if varsHuh then [ "const" <+> jsMapVar mpv <+> "=" <+> "{};" ] else [])
        <> [ "const" <+> jsMapVarCtc mpv <+> "=" <+> ctc <> ";" ]

jsError :: Doc -> Doc
jsError err = "new Error(" <> err <> ")"

jsPart :: DLInit -> SLPart -> EPProg -> App Doc
jsPart dli p (EPProg _ _ et) = do
  jsc@(JSContracts {..}) <- newJsContract
  let ctxt_ctcs = Just jsc
  let ctxt_who = p
  let ctxt_txn = 0
  let ctxt_while = JWhile_None
  let ctxt_mode = JM_Backend
  let ctxt_maps = dli_maps dli
  local (const JSCtxt {..}) $ do
    maps_defn <- jsMapDefns True
    et' <- jsETail et
    i2t' <- liftIO $ readIORef jsc_i2t
    let ctcs = vsep $ map snd $ M.toAscList i2t'
    let who = pretty $ bunpack p
    let iExpect this nth = "`The backend for" <+> who <+> "expects to receive" <+> this <+>
                            "as its" <+> nth <+> "argument.`"
    let rejectIf cond err = jsWhen cond $ jsReturn $ jsApply "Promise.reject" [jsError err]
    let ctcChk = rejectIf "typeof(ctc) !== 'object' || ctc.sendrecv === undefined" $ iExpect "a contract" "first"
    let interactChk = rejectIf "typeof(interact) !== 'object'" $ iExpect "an interact object" "second"
    let bodyp' =
          vsep
            [ ctcChk
            , interactChk
            , "const stdlib = ctc.stdlib;"
            , ctcs
            , maps_defn
            , et'
            ]
    return $ "export" <+> jsFunction who ["ctc", "interact"] bodyp'

jsConnInfo :: ConnectorInfo -> App Doc
jsConnInfo = \case
  Aeson.Null -> jsCon DLL_Null
  Aeson.Bool b -> jsCon $ DLL_Bool b
  -- Note: only integers are supported.
  -- TODO: throw error if non-integer detected,
  -- or alernatively, support non-integers
  Aeson.Number i -> return $ pretty $ Sci.formatScientific Sci.Fixed (Just 0) i
  Aeson.String t -> return $ jsBacktickText t
  Aeson.Array a -> jsArray <$> (mapM jsConnInfo $ Foldable.toList a)
  Aeson.Object m -> jsObject <$> (mapM jsConnInfo $ M.fromList $ HM.toList $ m)

jsCnp :: T.Text -> ConnectorInfo -> App Doc
jsCnp name cnp = do
  cnp' <- jsConnInfo cnp
  return $ "const" <+> "_" <> pretty name <+> "=" <+> cnp' <> semi

jsConnsExp :: [T.Text] -> Doc
jsConnsExp names = "export const _Connectors" <+> "=" <+> jsObject connMap <> semi
  where
    connMap = M.fromList [(name, "_" <> pretty name) | name <- names]

jsExportBlock :: Bool -> DLExportBlock -> App Doc
jsExportBlock isAsync (DLinExportBlock _ margs b) = do
  (tl, ret) <- jsBlock b
  let args = fromMaybe [] margs
  (argDefs, tmps) <-
    unzip
      <$> mapM
        (\arg -> do
           arg' <- jsVar arg
           let tmp = "_" <> arg'
           protected <- jsProtect "null" (varType arg) tmp
           let def = "  const" <+> arg' <+> "=" <+> protected <> semi <> hardline
           return (def, tmp))
        args
  let body = hcat argDefs <> hardline <> tl <> hardline <> jsReturn ret
  let argList = parens $ hsep $ punctuate comma tmps
  let mAsync = if isAsync then "async " else ""
  let fun = parens $ mAsync <> argList <+> "=>" <+> jsBraces body
  let call x = jsApply x []
  let wrap = maybe call (const id) margs
  return $ wrap fun

jsFunctionWStdlib :: Doc -> [Doc] -> App Doc -> App Doc
jsFunctionWStdlib name moreargs mbody = do
  jsc <- newJsContract
  local (\c -> c {ctxt_ctcs = Just jsc}) $ do
    body <- mbody
    i2t' <- liftIO $ readIORef $ jsc_i2t jsc
    let ctcs = map snd $ M.toAscList i2t'
    return $
      (<+>) "export" $
        jsFunction_ name (["s"] <> moreargs) $
          vsep $
            ["const stdlib = s.reachStdlib" <> semi]
              <> ctcs
              <> [body]

jsExports :: DLExports -> App Doc
jsExports exports =
  jsFunctionWStdlib "getExports" [] $ do
    exportM <- mapM (jsExportBlock False) exports
    return $ jsReturn $ jsObject exportM

jsViews :: Maybe (CPViews, ViewInfos) -> App Doc
jsViews mcv = do
  let menv e = e { ctxt_mode = JM_View }
  jsFunctionWStdlib "_getViews" ["viewlib"] $ local menv $ do
    let cvs = fromMaybe mempty $ fmap fst mcv
    let vis = fromMaybe mempty $ fmap snd mcv
    let toObj fv o = jsObject <$> mapWithKeyM fv o
    let enView _ (ViewInfo vs _) =
          jsArray <$> (mapM jsContract $ map varType vs)
    views <- toObj enView vis
    let illegal = jsApply "stdlib.assert" ["false", jsString "illegal view"]
    let enDecode v k vi (ViewInfo vs vim) = do
          vs' <- mapM jsVar vs
          vi' <- jsCon $ DLL_Int sb $ fromIntegral vi
          let c = jsPrimApply PEQ ["i", vi']
          let let' = "const" <+> jsArray vs' <+> "=" <+> "svs" <> semi
          ret' <-
            case M.lookup k (fromMaybe mempty $ M.lookup v vim) of
              Just eb -> do
                eb' <- jsExportBlock True $ dlebEnsureFun eb
                let eb'call = jsApply ("await" <+> parens eb') ["...args"]
                return $ jsReturn $ parens eb'call
              Nothing -> return $ illegal
          return $ jsWhen c $ vsep [let', ret']
    let enInfo' :: SLPart -> SLVar -> IType -> App Doc
        enInfo' v k vt = do
          let (_, rng) = itype2arr vt
          rng' <- jsContract rng
          body <- (vsep . M.elems) <$> mapWithKeyM (enDecode v k) vis
          let body' = vsep [body, illegal]
          let decode' = jsApply "async " ["i", "svs", "args"] <+> "=>" <+> jsBraces body'
          return $
            jsObject $
              M.fromList $
                [ ("ty" :: String, rng')
                , ("decode", decode')
                ]
    let enInfo v = toObj (enInfo' v)
    infos <- toObj enInfo cvs
    maps_defn <- jsMapDefns False
    return $ vsep $
      [ maps_defn
      , jsReturn $
          jsObject $
            M.fromList $
              [ ("views" :: String, views)
              , ("infos", infos)
              ]
      ]

-- XXX copied from ALGO.hs
mapDataTy :: DLMapInfos -> DLType
mapDataTy m = T_Tuple $ map (dlmi_tym . snd) $ M.toAscList m

jsMaps :: DLMapInfos -> App Doc
jsMaps ms = do
  jsFunctionWStdlib "_getMaps" [] $ do
    mapDataTy' <- jsContract $ mapDataTy ms
    return $
      jsReturn $
        jsObject $
          M.fromList $
            [("mapDataTy" :: String, mapDataTy')]

reachBackendVersion :: Int
reachBackendVersion = 2

jsPIProg :: ConnectorResult -> PLProg -> App Doc
jsPIProg cr (PLProg _ (PLOpts {}) dli dexports (EPPs pm) (CPProg _ vi _)) = do
  let DLInit {..} = dli
  let preamble =
        vsep
          [ pretty $ "// Automatically generated with Reach " ++ versionStr
          , "/* eslint-disable */"
          , "export const _version =" <+> jsString versionStr <> semi
          , "export const _backendVersion =" <+> pretty reachBackendVersion <> semi
          ]
  partsp <- mapM (uncurry (jsPart dli)) $ M.toAscList pm
  cnpsp <- mapM (uncurry jsCnp) $ HM.toList cr
  let connsExp = jsConnsExp $ HM.keys cr
  exportsp <- jsExports dexports
  viewsp <-
    local (\e -> e {ctxt_maps = dli_maps}) $
      jsViews vi
  mapsp <- jsMaps dli_maps
  return $ vsep_with_blank $ preamble : emptyDoc : exportsp : emptyDoc : viewsp : emptyDoc : mapsp : emptyDoc : partsp ++ emptyDoc : cnpsp ++ [emptyDoc, connsExp, emptyDoc]

backend_js :: Backend
backend_js outn crs pl = do
  let jsf = outn "mjs"
  let ctxt_who = "Module"
  let ctxt_txn = 0
  let ctxt_mode = JM_Backend
  let ctxt_while = JWhile_None
  let ctxt_ctcs = Nothing
  let ctxt_maps = mempty
  d <-
    flip runReaderT (JSCtxt {..}) $
      jsPIProg crs pl
  LTIO.writeFile jsf $ render d
