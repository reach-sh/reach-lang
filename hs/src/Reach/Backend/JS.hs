module Reach.Backend.JS (backend_js) where

import Control.Monad.Reader
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as B
import qualified Data.Foldable as Foldable
import qualified Data.HashMap.Strict as HM
import Data.IORef
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

jsMapVar :: DLMVar -> Doc
jsMapVar mpv = pretty mpv

jsMapVarCtc :: DLMVar -> Doc
jsMapVarCtc mpv = jsMapVar mpv <> "_ctc"

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

-- Compiler

data JSContracts = JSContracts
  { jsc_idx :: Counter
  , jsc_t2i :: IORef (M.Map DLType Doc)
  , jsc_i2t :: IORef (M.Map Int Doc)
  }

data JSCtxt = JSCtxt
  { ctxt_who :: SLPart
  , ctxt_txn :: Int
  , ctxt_simulate :: Bool
  , ctxt_while :: Maybe (Maybe DLVar, PILBlock, EITail, EITail)
  , ctxt_timev :: Maybe DLVar
  , ctxt_ctcs :: Maybe JSContracts
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
          return $ jsArray [ jsString k, t' ]
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
  DLA_Interact _ m t ->
    jsProtect "null" t $ "interact." <> pretty m

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

jsDigest :: AppT [DLArg]
jsDigest as = do
  ctc <- jsContract (T_Tuple $ map argTypeOf as)
  as' <- jsLargeArg (DLLA_Tuple as)
  return $ jsApply "stdlib.digest" [ctc, as']

jsPrimApply :: PrimOp -> [Doc] -> Doc
jsPrimApply = \case
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

jsExpr :: AppT DLExpr
jsExpr = \case
  DLE_Arg _ a ->
    jsArg a
  DLE_LArg _ la ->
    jsLargeArg la
  DLE_Impossible at msg ->
    expect_thrown at $ Err_Impossible msg
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
  DLE_Transfer _ who amt ->
    (ctxt_simulate <$> ask) >>= \case
      False -> mempty
      True -> do
        who' <- jsArg who
        amt' <- jsArg amt
        return $
          jsApply
            "sim_r.txns.push"
            [ jsObject $
                M.fromList $
                  [ ("to" :: String, who')
                  , ("amt" :: String, amt')
                  ]
            ]
  DLE_Wait _ amt -> do
    amt' <- jsArg amt
    (ctxt_simulate <$> ask) >>= \case
      True -> return $ jsApply "void" [amt']
      False -> return $ "await" <+> jsApply "ctc.wait" [amt']
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
    return $ jsProtect_ "null" ctc $ jsApply "stdlib.mapRef" [jsMapVar mpv, fa']
  DLE_MapSet _ mpv fa na -> do
    -- XXX something really bad is going to happen during the simulation of
    -- this
    fa' <- jsArg fa
    na' <- jsArg na
    return $ jsMapVar mpv <> brackets fa' <+> "=" <+> na'
  DLE_MapDel _ mpv fa -> do
    fa' <- jsArg fa
    return $ jsMapVar mpv <> brackets fa' <+> "=" <+> "undefined"
  DLE_Remote {} ->
    impossible "jsExpr remote"

jsEmitSwitch :: AppT k -> SrcLoc -> DLVar -> SwitchCases k -> App Doc
jsEmitSwitch iter _at ov csm = do
  ov' <- jsVar ov
  let cm1 (vn, (mov', body)) = do
        body' <- iter body
        set' <- case mov' of
          Just ov2 -> do
            ov2' <- jsVar ov2
            return $ "const" <+> ov2' <+> "=" <+> ov' <> "[1]" <> semi
          Nothing -> mempty
        let set_and_body' = vsep [set', body', "break;"]
        return $ "case" <+> jsString vn <> ":" <+> jsBraces set_and_body'
  csm' <- mapM cm1 $ M.toAscList csm
  return $ "switch" <+> parens (ov' <> "[0]") <+> jsBraces (vsep csm')

jsCom :: AppT PILCommon
jsCom = \case
  DL_Nop _ -> mempty
  DL_Let _ pv (DLE_Remote {}) ->
    case pv of
      Nothing -> mempty
      Just dv -> do
        dv' <- jsVar dv
        txn' <- jsTxn
        dvt' <- jsContract $ varType dv
        return $ "const" <+> dv' <+> "=" <+> "await" <+> txn' <> "." <> jsApply "getOutput" [ squotes dv', dvt' ]
  DL_Let _ (Just dv) de -> do
    dv' <- jsVar dv
    de' <- jsExpr de
    return $ "const" <+> dv' <+> "=" <+> de' <> semi
  DL_Let _ Nothing de -> do
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
  DL_ArrayMap _ ans x a (DLinBlock _ _ f r) -> do
    ans' <- jsVar ans
    x' <- jsArg x
    a' <- jsArg $ DLA_Var a
    f' <- jsPLTail f
    r' <- jsArg r
    return $ "const" <+> ans' <+> "=" <+> x' <> "." <> jsApply "map" [(jsApply "" [a'] <+> "=>" <+> jsBraces (f' <> hardline <> jsReturn r'))]
  DL_ArrayReduce _ ans x z b a (DLinBlock _ _ f r) -> do
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

jsPLTail :: AppT PILTail
jsPLTail = \case
  DT_Return {} -> mempty
  DT_Com m k -> jsCom m <> pure hardline <> jsPLTail k

jsNewScope :: Doc -> Doc
jsNewScope body =
  jsApply (parens (parens emptyDoc <+> "=>" <+> jsBraces body)) []

jsBlockNewScope :: AppT PILBlock
jsBlockNewScope b = do
  (t', a') <- jsBlock b
  return $ jsNewScope $ t' <> hardline <> jsReturn a'

jsBlock :: DLinBlock PILVar -> App (Doc, Doc)
jsBlock (DLinBlock _ _ t a) = do
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

jsETail :: AppT EITail
jsETail = \case
  ET_Com m k -> jsCom m <> return hardline <> jsETail k
  ET_Stop _ ->
    (ctxt_simulate <$> ask) >>= \case
      False -> return $ "return" <> semi
      True -> mempty
  ET_If _ c t f -> jsIf <$> jsArg c <*> jsETail t <*> jsETail f
  ET_Switch at ov csm -> jsEmitSwitch jsETail at ov csm
  ET_FromConsensus at which msvs k ->
    (ctxt_simulate <$> ask) >>= \case
      False -> jsETail k
      True -> do
        let mkStDigest svs_ = jsDigest (DLA_Literal (DLL_Int at $ fromIntegral which) : (map snd svs_))
        (nextSt', nextSt_noTime', isHalt') <-
          case msvs of
            Nothing ->
              --- XXX This is only used by Algorand and it should really be zero bytes, but the fakery with numbers and byte lengths is getting me
              (,,) <$> jsDigest [] <*> jsDigest [] <*> (jsCon $ DLL_Bool True)
            Just svs -> do
              timev <- (fromMaybe (impossible "no timev") . ctxt_timev) <$> ask
              let svs' = dvdeletep timev svs
              (,,) <$> mkStDigest svs <*> mkStDigest svs' <*> (jsCon $ DLL_Bool False)
        return $
          vsep
            [ "sim_r.nextSt =" <+> nextSt' <> semi
            , "sim_r.nextSt_noTime =" <+> nextSt_noTime' <> semi
            , "sim_r.isHalt =" <+> isHalt' <> semi
            ]
  ET_ToConsensus at fs_ok prev last_timemv which from_me msg_vs _out amtv timev mto k_ok -> do
    msg_ctcs <- mapM (jsContract . argTypeOf) $ map DLA_Var msg_vs
    msg_vs' <- mapM jsVar msg_vs
    let withCtxt =
          local
            (\e ->
               e
                 { ctxt_txn = (ctxt_txn e) + 1
                 , ctxt_timev = Just timev
                 })
    txn <- withCtxt jsTxn
    let msg_vs_defp = "const" <+> jsArray msg_vs' <+> "=" <+> txn <> ".data" <> semi <> hardline
    amtv' <- jsVar amtv
    let amt_defp = "const" <+> amtv' <+> "=" <+> txn <> ".value" <> semi <> hardline
    timev' <- jsVar timev
    let time_defp = "const" <+> timev' <+> "=" <+> txn <> ".time" <> semi <> hardline
    fs_ok' <- withCtxt $ jsFromSpec fs_ok
    let k_defp = msg_vs_defp <> amt_defp <> time_defp <> fs_ok'
    whop <- jsCon =<< ((DLL_Bytes . ctxt_who) <$> ask)
    k_ok' <- withCtxt $ jsETail k_ok
    let k_okp = k_defp <> k_ok'
    (delayp, k_p) <-
      case mto of
        Nothing -> return ("false", k_okp)
        Just (delays, k_to) -> do
          let jsSum [] = impossible "no delay"
              jsSum [x] = jsArg x
              jsSum (x : xs) = do
                x' <- jsArg x
                xs' <- jsSum xs
                return $ jsApply "stdlib.add" [x', xs']
          k_top <- withCtxt $ jsETail k_to
          delays' <- jsSum delays
          timef <- withCtxt $ jsTimeoutFlag
          return (delays', jsIf timef k_top k_okp)
    let recvp =
          jsApply
            "ctc.recv"
            [ whop
            , pretty which
            , pretty $ length msg_vs
            , jsArray msg_ctcs
            , "false"
            , delayp
            ]
    callp <-
      withCtxt $
        case from_me of
          Just (args, amt, whena, svs, soloSend) -> do
            let svs_as = map DLA_Var svs
            amtp <- jsArg amt
            let svs_noPrevTime = dvdeletem last_timemv svs
            let mkStDigest svs_ = jsDigest (DLA_Literal (DLL_Int at $ fromIntegral prev) : (map DLA_Var svs_))
            let withSim = local (\e -> e {ctxt_simulate = True})
            sim_body_core <- withSim $ jsETail k_ok
            svs_d <- mkStDigest svs
            svs_nptd <- mkStDigest svs_noPrevTime
            let sim_body =
                  vsep
                    [ "const sim_r = { txns: [] };"
                    , "sim_r.prevSt =" <+> svs_d <> semi
                    , "sim_r.prevSt_noPrevTime =" <+> svs_nptd <> semi
                    , k_defp
                    , sim_body_core
                    , "return sim_r;"
                    ]
            vs <- jsArray <$> ((++) <$> mapM jsVar svs <*> mapM jsArg args)
            whena' <- jsArg whena
            soloSend' <- jsCon (DLL_Bool soloSend)
            msgts <- mapM (jsContract . argTypeOf) $ svs_as ++ args
            last_timev' <- jsCon (maybe (DLL_Bool False) (DLL_Int at . fromIntegral) (last_timemv >>= flip elemIndex svs))
            let sendp =
                  jsApply
                    "ctc.sendrecv"
                    [ whop
                    , pretty which
                    , pretty (length msg_vs)
                    , last_timev'
                    , jsArray msgts
                    , vs
                    , amtp
                    , jsArray msg_ctcs
                    , whena'
                    , soloSend'
                    , delayp
                    , parens $ "async" <+> "(" <> txn <> ") => " <> jsBraces sim_body
                    ]
            return sendp
          Nothing ->
            return recvp
    let defp = "const" <+> txn <+> "=" <+> "await" <+> parens callp <> semi
    return $ vsep [defp, k_p]
  ET_While _ asn cond body k -> do
    timev_ <- ctxt_timev <$> ask
    let mtimev' =
          case timev_ of
            Nothing -> Just Nothing
            Just timev -> foldl' go Nothing (M.toList asnm)
              where
                go mtv (v, a) =
                  case a == DLA_Var timev of
                    True -> Just $ Just v
                    False -> mtv
                DLAssignment asnm = asn
    let timev' =
          case mtimev' of
            Nothing -> impossible "no timev in while"
            Just x -> x
    let newCtxt_tv = local (\e -> e {ctxt_timev = timev'})
    let newCtxt' = newCtxt_tv . local (\e -> e {ctxt_while = Just (timev', cond, body, k)})
    cond' <- jsBlockNewScope cond
    body' <- newCtxt' $ jsETail body
    k' <- newCtxt_tv $ jsETail k
    (ctxt_simulate <$> ask) >>= \case
      False -> do
        asn' <- jsAsn AM_While asn
        return $ asn' <> hardline <> jsWhile cond' body' <> hardline <> k'
      True -> do
        asn' <- jsAsn AM_WhileSim asn
        return $ asn' <> hardline <> jsIf cond' body' k'
  ET_Continue _ asn -> do
    asn'o <- jsAsn AM_ContinueOuter asn
    asn'i <-
      (ctxt_simulate <$> ask) >>= \case
        False -> do
          asn_ <- jsAsn AM_ContinueInner asn
          return $ asn_ <> hardline <> "continue" <> semi
        True ->
          (ctxt_while <$> ask) >>= \case
            Nothing -> impossible "continue not in while"
            Just (wtimev', wcond, wbody, wk) -> do
              let newCtxt = local (\e -> e {ctxt_timev = wtimev'})
              asn_ <- jsAsn AM_ContinueInnerSim asn
              wcond' <- jsBlockNewScope wcond
              wbody' <- newCtxt $ jsETail wbody
              wk' <- newCtxt $ jsETail wk
              return $ (jsNewScope $ asn_ <> hardline <> jsIf wcond' wbody' wk') <> semi
    return $ asn'o <> hardline <> asn'i
  ET_ConsensusOnly _at l k ->
    (ctxt_simulate <$> ask) >>= \case
      True -> kp
      False -> vsep <$> mapM id [lp, kp]
    where
      kp = jsETail k
      lp = jsPLTail l

newJsContract :: App JSContracts
newJsContract = do
  jsc_idx <- liftIO $ newCounter 0
  jsc_t2i <- liftIO $ newIORef mempty
  jsc_i2t <- liftIO $ newIORef mempty
  return $ JSContracts {..}

jsPart :: DLInit -> SLPart -> EIProg -> App Doc
jsPart dli p (EPProg _ _ et) = do
  JSContracts {..} <- newJsContract
  let ctxt_ctcs = Just $ JSContracts {..}
  let ctxt_who = p
  let ctxt_txn = 0
  let ctxt_while = Nothing
  let ctxt_simulate = False
  let ctxt_timev = Nothing
  local (const JSCtxt {..}) $ do
    let DLInit {..} = dli
    ctimem' <-
      case dli_ctimem of
        Nothing -> mempty
        Just v -> do
          v' <- jsVar v
          return $ "const" <+> v' <+> "=" <+> "await ctc.creationTime();"
    let map_defn (mpv, DLMapInfo {..}) = do
          ctc <- jsContract (maybeT dlmi_ty)
          return $
            vsep
              [ "const" <+> jsMapVar mpv <+> "=" <+> "{};"
              , "const" <+> jsMapVarCtc mpv <+> "=" <+> ctc <> ";"
              ]
    maps_defn <- vsep <$> (mapM map_defn $ M.toList dli_maps)
    et' <- jsETail et
    i2t' <- liftIO $ readIORef jsc_i2t
    let ctcs = vsep $ map snd $ M.toAscList i2t'
    let bodyp' =
          vsep
            [ "const stdlib = ctc.stdlib;"
            , ctcs
            , maps_defn
            , ctimem'
            , et'
            ]
    return $ "export" <+> jsFunction (pretty $ bunpack p) ["ctc", "interact"] bodyp'

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

jsExportValue :: DLinExportVal PILBlock -> App Doc
jsExportValue = \case
  DLEV_Arg a  -> do
    let t = argTypeOf a
    a' <- jsArg a
    jsProtect "null" t a'
  DLEV_LArg a -> do
    let t = largeArgTypeOf a
    a' <- jsLargeArg a
    jsProtect "null" t a'
  DLEV_Fun args b -> do
    (tl, ret) <- jsBlock b
    (argDefs, tmps) <-
      unzip <$> mapM (\ arg -> do
        arg' <- jsVar arg
        let tmp = "_" <> arg'
        protected <- jsProtect "null" (varType arg) tmp
        let def = "  const" <+> arg' <+> "=" <+> protected <> semi <> hardline
        return (def, tmp)) args
    let body = hcat argDefs <> hardline <> tl <> hardline <> jsReturn ret
    let argList = parens $ hsep $ punctuate comma tmps
    return $ argList <+> "=>" <+> jsBraces body

jsExports :: CCExports PILVar -> App Doc
jsExports exports = do
  jsc <- newJsContract
  local (\ c -> c { ctxt_ctcs = Just jsc}) $ do
    exportProps <- mapM (\ (k, v) -> do
          vs <- jsExportValue v
          return $ "    " <> pretty k <> " : " <> vs <> ",\n") $ M.toList exports
    i2t' <- liftIO $ readIORef $ jsc_i2t jsc
    let ctcs = vsep $ map snd $ M.toAscList i2t'
    let body =
          vsep
            [ "const stdlib = s.reachStdlib;"
            , ctcs
            , " return " <> parens (braces $ hcat exportProps)]
    return $ "export const getExports = (s) => {\n  " <> body <> " \n};"


jsPIProg :: ConnectorResult -> PIProg -> App Doc
jsPIProg cr (PLProg _ (PLOpts {}) dli (EPPs pm) (CPProg _ dexports _)) = do
  let preamble =
        vsep
          [ pretty $ "// Automatically generated with Reach " ++ versionStr
          , "/* eslint-disable */"
          , "export const _version =" <+> jsString versionStr <> semi
          ]
  partsp <- mapM (uncurry (jsPart dli)) $ M.toAscList pm
  cnpsp <- mapM (uncurry jsCnp) $ HM.toList cr
  let connsExp = jsConnsExp $ HM.keys cr
  exportsp <- jsExports dexports
  return $ vsep_with_blank $ preamble : emptyDoc : exportsp : emptyDoc : partsp ++ emptyDoc : cnpsp ++ [emptyDoc, connsExp, emptyDoc]

backend_js :: Backend
backend_js outn crs pl = do
  let jsf = outn "mjs"
  let ctxt_who = "Module"
  let ctxt_txn = 0
  let ctxt_simulate = False
  let ctxt_while = Nothing
  let ctxt_timev = Nothing
  let ctxt_ctcs = Nothing
  d <-
    flip runReaderT (JSCtxt {..}) $
      jsPIProg crs pl
  LTIO.writeFile jsf $ render d
