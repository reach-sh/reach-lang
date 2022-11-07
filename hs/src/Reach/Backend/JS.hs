module Reach.Backend.JS (backend_js) where

import Control.Monad.Reader
import qualified Data.Aeson as AS
import qualified Data.ByteString.Char8 as B
import qualified Data.Foldable as Foldable
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Scientific as Sci
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as LTIO
import Reach.AST.Base
import Reach.AST.DLBase
import Reach.AST.EP
import Reach.Backend
import Reach.Connector
import Reach.Counter
import Reach.Texty
import Reach.UnsafeUtil
import Reach.Util
import Reach.Version
import Reach.BigOpt
import qualified Data.Text.Encoding as BS
import qualified Data.ByteString.Base16 as B16

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
jsApplyKws f m = jsApply f [jsObject m]

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
  | JWhile_Some JSCtxtWhile DLBlock ETail ETail Doc

data JSMode
  = JM_Simulate
  | JM_View
  | JM_Backend
  deriving (Eq)

data JSCtxt = JSCtxt
  { ctxt_who :: SLPart
  , ctxt_isAPI :: Bool
  , ctxt_txn :: Int
  , ctxt_mode :: JSMode
  , ctxt_while :: JSCtxtWhile
  , ctxt_ctcs :: Maybe JSContracts
  , ctxt_maps :: M.Map DLMVar DLMapInfo
  , ctxt_ctr :: Counter
  }

type App = ReaderT JSCtxt IO

type AppT a = a -> App Doc

instance Semigroup a => Semigroup (App a) where
  mx <> my = (<>) <$> mx <*> my

instance Monoid a => Monoid (App a) where
  mempty = return mempty

jsTxn :: App Doc
jsTxn = ("txn" <>) . pretty . ctxt_txn <$> ask
incTxn :: App a -> App a
incTxn = local (\e -> e {ctxt_txn = (ctxt_txn e) + 1})

jsContract_ :: DLType -> App Doc
jsContract_ = \case
  T_Null -> return "stdlib.T_Null"
  T_Bool -> return "stdlib.T_Bool"
  T_UInt UI_Word -> return "stdlib.T_UInt"
  T_UInt UI_256 -> return "stdlib.T_UInt256"
  T_Bytes sz -> do
    sz' <- jsCon $ DLL_Int sb UI_Word sz
    return $ jsApply "stdlib.T_Bytes" [sz']
  T_BytesDyn -> return $ "stdlib.T_BytesDyn"
  T_StringDyn -> return $ "stdlib.T_StringDyn"
  T_Digest -> return $ "stdlib.T_Digest"
  T_Address -> return $ "stdlib.T_Address"
  T_Contract -> return $ "stdlib.T_Contract"
  T_Token -> return $ "stdlib.T_Token"
  T_Array t sz -> do
    t' <- jsContract t
    sz' <- jsCon (DLL_Int sb UI_Word sz)
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
        Just b -> jsBytes b
  who <- ctxt_who <$> ask
  let who_p = jsBytes who
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

jsBool :: Bool -> Doc
jsBool = \case
  True -> "true"
  False -> "false"

jsNum :: Integer -> Doc
jsNum = jsString . show

jsChkBigNum :: SrcLoc -> UIntTy -> Doc -> App Doc
jsChkBigNum at uit i = do
  uim <- case uit of
            UI_Word -> jsArg (DLA_Constant $ DLC_UInt_max)
            UI_256 -> return $ jsNum $ uint256_Max
  return $ jsApply "stdlib.checkedBigNumberify" [jsAt at, uim, i]

jsCon :: AppT DLLiteral
jsCon = \case
  DLL_Null -> return "null"
  DLL_Bool b -> return $ jsBool b
  DLL_Int at uit i -> jsChkBigNum at uit $ jsNum i
  DLL_TokenZero ->
    return "stdlib.Token_zero"

jsArg :: AppT DLArg
jsArg = \case
  DLA_Var v -> jsVar v
  DLA_Constant c ->
    case c of
      DLC_UInt_max ->
        return "stdlib.UInt_max"
      DLC_Token_zero ->
        return "stdlib.Token_zero"
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
  DLLA_Bytes b -> return $ jsBytes b
  DLLA_BytesDyn b -> return $ jsBytes b
  DLLA_StringDyn t -> return $ jsString $ T.unpack t

jsBytes :: B.ByteString -> Doc
jsBytes b =
  case BS.decodeUtf8' b of
    Left _ -> jsApply "stdlib.bytesFromHex" [dquotes $ "0x" <> pretty (B16.encodeBase16 b)]
    Right _ -> jsString . bunpack $ b

jsContractsAndVals :: [DLArg] -> App [Doc]
jsContractsAndVals as = do
  let la = DLLA_Tuple as
  ctcs <- jsArray <$> (mapM jsContract $ map argTypeOf as)
  as' <- jsLargeArg la
  return $ [ctcs, as']

jsDigest :: AppT [DLArg]
jsDigest as = jsApply "stdlib.digest" <$> jsContractsAndVals as

jsUIntTy :: UIntTy -> Doc
jsUIntTy t = if t == UI_Word then "\"UInt\"" else "\"UInt256\""

jsPrimApply :: PrimOp -> [Doc] -> App Doc
jsPrimApply = \case
  SELF_ADDRESS {} -> r $ jsApply "ctc.selfAddress"
  ADD t PV_Safe -> r $ jsApply_ui t "stdlib.safeAdd"
  ADD t _ -> r $ jsApply_ui t "stdlib.add"
  SUB t PV_Safe -> r $ jsApply_ui t "stdlib.safeSub"
  SUB t _ -> r $ jsApply_ui t "stdlib.sub"
  MUL t PV_Safe -> r $ jsApply_ui t "stdlib.safeMul"
  MUL t _ -> r $ jsApply_ui t "stdlib.mul"
  DIV t PV_Safe -> r $ jsApply_ui t "stdlib.safeDiv"
  DIV t _ -> r $ jsApply_ui t "stdlib.div"
  MOD t PV_Safe -> r $ jsApply_ui t "stdlib.safeMod"
  MOD t _ -> r $ jsApply_ui t "stdlib.mod"
  PLT t -> r $ jsApply_ui t "stdlib.lt"
  PLE t -> r $ jsApply_ui t "stdlib.le"
  PEQ t -> r $ jsApply_ui t "stdlib.eq"
  PGE t -> r $ jsApply_ui t "stdlib.ge"
  PGT t -> r $ jsApply_ui t "stdlib.gt"
  SQRT t -> r $ jsApply_ui t "stdlib.sqrt"
  UCAST dom rng trunc PV_Safe -> \a -> return $ jsApply "stdlib.cast" $ [ jsUIntTy dom, jsUIntTy rng ] <> a <> [ jsBool trunc, "true" ]
  UCAST dom rng trunc _ -> \a -> return $ jsApply "stdlib.cast" $ [ jsUIntTy dom, jsUIntTy rng ] <> a <> [ jsBool trunc, "false" ]
  LSH -> r $ jsApply "stdlib.lsh"
  RSH -> r $ jsApply "stdlib.rsh"
  MUL_DIV PV_Safe -> r $ jsApply "stdlib.safeMuldiv"
  MUL_DIV _ -> r $ jsApply "stdlib.muldiv"
  BAND t -> r $ jsApply_ui t "stdlib.band"
  BIOR t -> r $ jsApply_ui t "stdlib.bior"
  BXOR t -> r $ jsApply_ui t "stdlib.bxor"
  DIGEST_XOR -> r $ jsApply "stdlib.digest_xor"
  BYTES_XOR -> r $ jsApply "stdlib.bytes_xor"
  IF_THEN_ELSE -> \args -> case args of
    [c, t, f] -> return $ c <+> "?" <+> t <+> ":" <+> f
    _ -> impossible $ "emitJS: ITE called with wrong number of arguments"
  --  BYTES_EQ -> jsApply "stdlib.bytesEq"
  DIGEST_EQ -> r $ jsApply "stdlib.digestEq"
  ADDRESS_EQ -> r $ jsApply "stdlib.addressEq"
  TOKEN_EQ -> r $ jsApply "stdlib.tokenEq"
  BYTES_ZPAD xtra -> \args -> return $ jsApply "stdlib.bytesConcat" (args <> [jsBytes $ bytesZero xtra])
  STRINGDYN_CONCAT -> r $ jsApply "stdlib.stringDynConcat"
  UINT_TO_STRINGDYN t -> r $ jsApply_ui t "stdlib.uintToStringDyn"
  BTOI_LAST8 _ -> r $ jsApply "stdlib.btoiLast8"
  CTC_ADDR_EQ -> r $ jsApply "stdlib.ctcAddrEq"
  GET_CONTRACT -> const $ do
    isInitial <- (==) 0 <$> asks ctxt_txn
    asks ctxt_mode >>= \case
      JM_Simulate
        | isInitial -> return $ jsApply "stdlib.emptyContractInfo" []
      _ -> return $ "await" <+> jsApply "ctc.getContractInfo" []
  GET_ADDRESS -> const $
    return $ "await" <+> jsApply "ctc.getContractAddress" []
  GET_COMPANION -> const $ do
    return $ "await" <+> jsApply "ctc.getContractCompanion" [ ]
  where
    r f = return . f
    jsApply_ui t f = jsApply $ f <> (if (t == UI_256) then "256" else "")

jsMaybe :: AppT a -> AppT (Maybe a)
jsMaybe f = \case
  Nothing -> return $ "undefined /* Nothing */"
  Just a -> f a

jsArg_m :: AppT (Maybe DLArg)
jsArg_m = jsMaybe jsArg

shouldHashMapKey :: DLType -> Bool
shouldHashMapKey = \case
  T_Null -> False
  T_Bool -> False
  T_UInt {} -> False
  T_Digest -> False
  T_Address -> False
  T_Contract -> False
  T_Token -> False
  T_Bytes {} -> True
  T_BytesDyn -> True
  T_StringDyn -> True
  T_Array {} -> True
  T_Data {} -> True
  T_Tuple {} -> True
  T_Object {} -> True
  T_Struct {} -> True

jsMapKey :: DLArg -> App Doc
jsMapKey k =
  case shouldHashMapKey $ argTypeOf k of
    False -> jsArg k
    True  -> jsDigest [k]

jsRemote :: SrcLoc -> DLRemote -> App Doc
jsRemote at (DLRemote _rm (DLPayAmt pay_net pay_ks) as (DLWithBill nRecv nnRecv _nnZero) malgo) = do
  let DLRemoteALGO r_fees r_accounts r_assets _r_addr2acc r_apps _r_oc r_strictPay _r_rawCall
                   _r_simNetRecv _r_simTokRecv _r_simRetVal = malgo
  fees' <- jsArg r_fees
  let notStaticZero = if r_strictPay then const True else not . staticZero
  let pay_ks_nz = filter (notStaticZero . fst) pay_ks
  let l2n x = jsCon $ DLL_Int at UI_Word $ fromIntegral $ length $ x
  pays' <- l2n $ filter notStaticZero $ pay_net : map fst pay_ks_nz
  let nRecvCount = if nRecv then [r_fees] else []
  bills' <- l2n $ nRecvCount <> nnRecv
  toks' <- mapM jsArg $ nnRecv <> map snd pay_ks_nz <> r_assets
  let isAddress = (==) T_Address . argTypeOf
  accs' <- mapM jsArg $ (filter isAddress as) <> r_accounts
  apps' <- mapM jsArg r_apps
  return $ parens $ jsObject $ M.fromList $
    [ (("pays"::String), pays')
    , ("bills", bills')
    , ("toks", jsArray toks')
    , ("accs", jsArray accs')
    , ("apps", jsArray apps')
    , ("fees", fees')
    ]

jsExpr :: AppT DLExpr
jsExpr = \case
  DLE_Arg _ a ->
    jsArg a
  DLE_LArg _ la ->
    jsLargeArg la
  DLE_Impossible at _ (Err_Impossible_Case f)-> do
    ai <- jsAssertInfo at [] (Just $ bpack f)
    return $ "Error(" <> ai <> ")"
  DLE_Impossible at _ err ->
    expect_thrown at err
  DLE_VerifyMuldiv at _ _ _ err ->
    expect_thrown at err
  DLE_PrimOp _ p as ->
    jsPrimApply p =<< mapM jsArg as
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
  DLE_BytesDynCast _ x -> jsArg x
  DLE_TupleRef at aa i -> do
    aa' <- jsArg aa
    i' <- jsCon $ DLL_Int at UI_Word i
    return $ aa' <> brackets i'
  DLE_TupleSet _ t i v -> do
    t' <- jsArg t
    v' <- jsArg v
    return $ jsApply "stdlib.Array_set" [t', jsNum i, v']
  DLE_ObjectRef _ oa f -> do
    oa' <- jsArg oa
    return $ oa' <> "." <> pretty f
  DLE_ObjectSet _ o k v -> do
    o' <- jsArg o
    v' <- jsArg v
    let k' = jsString k
    return $ jsBraces $ "..." <> o' <> ", " <> k' <> ": " <> v'
  DLE_Interact at fs _ m t as -> do
    ai' <- jsAssertInfo at fs (Just $ bpack m)
    as' <- mapM jsArg as
    jsProtect ai' t $ "await" <+> (jsApply ("interact." <> pretty m) as')
  DLE_Digest _ as -> jsDigest as
  DLE_Claim at fs ct a mmsg ->
    (ctxt_mode <$> ask) >>= \case
      JM_Backend -> check
      JM_View -> check
      JM_Simulate -> mempty
    where
      check = case ct of
        CT_Assert -> impossible "assert"
        CT_Enforce -> require
        CT_Assume -> require
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
      JM_Simulate ->
        case staticZero amt of
          True -> mempty
          False -> do
            who' <- jsArg who
            mtok' <- jsArg_m mtok
            return $
              jsSimTxn "from" $
                [ ("to", who')
                , ("tok", mtok')
                ]
  DLE_TokenInit _ tok -> do
    (ctxt_mode <$> ask) >>= \case
      JM_Backend -> mempty
      JM_View -> impossible "view tokeninit"
      JM_Simulate -> do
        zero' <- jsCon $ DLL_Int sb UI_Word 0
        tok' <- jsArg tok
        return $
          jsSimTxn "init" $
            [ ("amt", zero')
            , ("tok", tok')
            ]
  DLE_TokenAccepted _ addr tok -> do
    (ctxt_mode <$> ask) >>= \case
      JM_Backend -> return "true"
      JM_View -> impossible "view canReceive"
      JM_Simulate -> do
        addr' <- jsArg addr
        tok' <- jsArg tok
        return $ jsApply "ctc.simTokenAccepted" ["sim_r", addr', tok']
  DLE_CheckPay _ _ amt mtok -> do
    (ctxt_mode <$> ask) >>= \case
      JM_Backend -> mempty
      JM_View -> impossible "view checkpay"
      JM_Simulate ->
        case staticZero amt of
          True -> mempty
          False -> do
            amt' <- jsArg amt
            mtok' <- jsArg_m mtok
            return $
              jsSimTxn "to" $
                [ ("amt", amt')
                , ("tok", mtok')
                ]
  DLE_Wait _ amtt -> do
    let (which, amt) = case amtt of
          Left t -> ("waitUntilTime", t)
          Right t -> ("waitUntilSecs", t)
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
    fa' <- jsMapKey fa
    (f, args) <-
      (ctxt_mode <$> ask) >>= \case
        JM_Simulate -> return $ ("await stdlib.simMapRef", ["sim_r", jsMapIdx mpv])
        JM_Backend -> return $ ("await stdlib.mapRef", [jsMapVar mpv])
        JM_View -> return $ ("await viewlib.viewMapRef", [jsMapIdx mpv])
    return $ jsProtect_ "null" ctc $ jsApply f $ args <> [fa']
  DLE_MapSet _ mpv fa mna -> do
    fa' <- jsMapKey fa
    na' <- jsArg_m mna
    (ctxt_mode <$> ask) >>= \case
      JM_Simulate ->
        return $ jsApply "await stdlib.simMapSet" ["sim_r", jsMapIdx mpv, fa', na']
      JM_Backend ->
        return $ jsApply "await stdlib.mapSet" [jsMapVar mpv, fa', na']
      JM_View -> impossible "view mapset"
  DLE_Remote at _fs ro rng_ty dr -> do
    (ctxt_mode <$> ask) >>= \case
      JM_Backend -> return "undefined /* Remote */"
      JM_View -> impossible "view Remote"
      JM_Simulate -> do
        dr' <- jsRemote at dr
        obj' <- jsArg ro
        let simTxn = jsSimTxn "remote" $
              [ ("obj", obj')
              , ("remote", dr')
              ]
        let DLRemoteALGO { ralgo_simNetRecv, ralgo_simTokensRecv, ralgo_simReturnVal } = dr_ralgo dr
        netRecv' <- jsArg ralgo_simNetRecv
        tokensRecv' <- jsArg $ case ralgo_simTokensRecv of
          RA_Tuple t -> t
          _ -> impossible "expected RA_Tuple"
        returnVal' <-
          case ralgo_simReturnVal of
            Just a -> jsArg a
            Nothing -> do
              c <- jsContract rng_ty
              return $ c <> ".defaultValue"
        let nnRecv = dwb_tok_billed $ dr_bills dr
        let mTR = if length nnRecv == 0 then [] else [ tokensRecv' <> " /* simTokensRecv */" ]
        let arr = jsArray $
              [ netRecv'    <> " /* simNetRecv */" ]
              <> mTR
              <> [ returnVal'  <> " /* simReturnVal */" ]
        return $ jsNewScope $ simTxn <> hardline <> jsReturn arr
  DLE_TokenNew _ tns -> do
    (ctxt_mode <$> ask) >>= \case
      JM_Backend -> return "undefined /* TokenNew */"
      JM_View -> impossible "view TokenNew"
      JM_Simulate -> do
        let DLTokenNew {..} = tns
        n' <- jsArg dtn_name
        s' <- jsArg dtn_sym
        u' <- jsArg dtn_url
        m' <- jsArg dtn_metadata
        p' <- jsArg dtn_supply
        d' <- jsArg_m dtn_decimals
        return $ jsApply "stdlib.simTokenNew" ["sim_r", n', s', u', m', p', d', "getSimTokCtr()"]
  DLE_TokenBurn _ ta aa ->
    (ctxt_mode <$> ask) >>= \case
      JM_Simulate -> do
        ta' <- jsArg ta
        aa' <- jsArg aa
        return $ jsApply "stdlib.simTokenBurn" ["sim_r", ta', aa']
      JM_Backend -> return "undefined /* TokenBurn */"
      JM_View -> impossible "token.burn"
  DLE_TokenDestroy _ ta ->
    (ctxt_mode <$> ask) >>= \case
      JM_Simulate -> do
        ta' <- jsArg ta
        return $ jsApply "stdlib.simTokenDestroy" ["sim_r", ta']
      JM_Backend -> return "undefined /* TokenDestroy */"
      JM_View -> impossible "token.burn"
  DLE_TimeOrder {} -> impossible "timeorder"
  DLE_EmitLog _at kind dvs -> do
    let go :: String -> DLVar -> App Doc
        go mode dv = do
          dv' <- jsVar dv
          txn' <- jsTxn
          dvt' <- jsContract $ varType dv
          return $ "await" <+> txn' <> "." <> jsApply "getOutput" [squotes (pretty mode), squotes dv', dvt', dv']
    case (kind, dvs) of
      (L_Internal, [dv]) -> go "internal" dv
      (L_Api p, [dv]) -> go (bunpack p) dv
      (_, _) -> return $ "null"
  DLE_setApiDetails _ who _ _ _ -> do
    let who' = viaShow who
    asks ctxt_mode >>= \case
      JM_Simulate -> return $ jsSimTxn "api" [("who", who')]
      _ -> return "undefined /* setApiDetails */"
  DLE_GetUntrackedFunds at mtok tb -> do
    tok <- maybe (return "") jsArg mtok
    tb' <- jsArg tb
    zero <- jsArg $ DLA_Literal $ DLL_Int at UI_Word 0
    let bal = "await" <+> jsApply "ctc.getBalance" [tok]
    c' <- jsPrimApply (PLE UI_Word) [bal, tb']
    f' <- jsPrimApply (SUB UI_Word PV_Safe) [bal, tb']
    rhs <- jsPrimApply IF_THEN_ELSE [ c', zero, f' ]
    ctm <- asks ctxt_mode
    let infoSim = case ctm == JM_Simulate && isJust mtok of
          True -> jsSimTxn "info" [("tok", tok)] <> ","
          _ -> ""
    return $ parens $ infoSim <> rhs
  DLE_DataTag _at d -> do
    let o = jsObject $ M.map (DText . s2lt . show) $ dataTagMap $ argTypeOf d
    d' <- jsArg d
    return $ o <> brackets (d' <> "[0]")
  DLE_FromSome _at mo da -> do
    mo' <- jsArg mo
    da' <- jsArg da
    return $ jsApply "stdlib.fromSome" [mo', da']
  DLE_ContractFromAddress _at _addr -> do
    return $ jsArray [jsString "None", "null"]
  DLE_ContractNew at cns dr -> do
    (ctxt_mode <$> ask) >>= \case
      JM_Backend -> return "undefined /* ContractNew */"
      JM_View -> impossible "view ContractNew"
      JM_Simulate -> do
        dr' <- jsRemote at dr
        cns' <- forM cns $ \DLContractNew {..} -> do
          c' <- jsJSON dcn_code
          o' <- jsJSON dcn_opts
          return $ jsObject $ M.fromList
            [ ("code"::String, c')
            , ("opts", o')
            ]
        return $ jsApply "stdlib.simContractNew" ["sim_r", jsObject cns', dr', "getSimTokCtr()"]

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

jsCom :: AppT DLStmt
jsCom = \case
  DL_Nop _ -> mempty
  DL_Let _ (DLV_Let _ dv) de -> do
    dv' <- jsVar dv
    rhs <- jsExpr de
    return $ "const" <+> dv' <+> "=" <+> rhs <> semi
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
  DL_LocalIf _ _ c t f -> do
    c' <- jsArg c
    t' <- jsPLTail t
    f' <- jsPLTail f
    return $ jsIf c' t' f'
  DL_LocalSwitch at ov csm ->
    jsEmitSwitch jsPLTail at ov csm
  DL_ArrayMap _ ans xs as i (DLBlock _ _ f r) -> do
    ans' <- jsVar ans
    xs' <- mapM jsArg xs
    as' <- mapM jsArg $ map DLA_Var as
    i' <- jsArg $ DLA_Var i
    f' <- jsPLTail f
    r' <- jsArg r
    return $ "const" <+> ans' <+> "=" <+> "await" <+> jsApply "stdlib.Array_asyncMap" [jsArray xs', (jsApply "async" ([jsArray as', i']) <+> "=>" <+> jsBraces (f' <> hardline <> jsReturn r'))]
  DL_ArrayReduce _ ans xs z b as i (DLBlock _ _ f r) -> do
    ans' <- jsVar ans
    xs' <- mapM jsArg xs
    z' <- jsArg z
    as' <- mapM jsArg $ map DLA_Var as
    b' <- jsArg $ DLA_Var b
    i' <- jsArg $ DLA_Var i
    f' <- jsPLTail f
    r' <- jsArg r
    return $ "const" <+> ans' <+> "=" <+> "await" <+> jsApply "stdlib.Array_asyncReduce" ([jsArray xs', z', (jsApply "async" $ [jsArray as', b', i']) <+> "=>" <+> jsBraces (f' <> hardline <> jsReturn r')])
  DL_MapReduce {} ->
    impossible $ "cannot inspect maps at runtime"
  DL_Only _at (Right c) l -> do
    m <- ctxt_mode <$> ask
    let sim = m == JM_Simulate
    case (not c || (c && not sim)) of
      True -> jsPLTail l
      False -> mempty
  DL_Only {} -> impossible $ "left only after EPP"
  DL_LocalDo _ _ t -> jsPLTail t

jsPLTail :: AppT DLTail
jsPLTail = \case
  DT_Return {} -> mempty
  DT_Com m k -> jsCom m <> pure hardline <> jsPLTail k

jsNewScope :: Doc -> Doc
jsNewScope body =
  "await" <+> (jsApply (parens ("async" <+> parens emptyDoc <+> "=>" <+> jsBraces body)) [])

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
  return $ jsArray $ [jsString lab, a']

jsETail :: AppT ETail
jsETail = \case
  ET_Com m k -> jsCom m <> return hardline <> jsETail k
  ET_Stop _ ->
    (ctxt_mode <$> ask) >>= \case
      JM_Simulate -> mempty
      _ -> return $ "return" <> semi
  ET_If _ c t f -> jsIf <$> jsArg c <*> jsETail t <*> jsETail f
  ET_Switch at ov csm -> jsEmitSwitch jsETail at ov csm
  ET_FromConsensus _at which msvs k ->
    (ctxt_mode <$> ask) >>= \case
      JM_Backend -> do
        more <-
          (ctxt_isAPI <$> ask) >>= \case
            False -> return []
            True ->
              case msvs of
                FI_Halt _ -> return []
                FI_Continue svs -> do
                  let vs = map fst svs
                  vs' <- mapM jsVar vs
                  ctcs <- jsArray <$> (mapM jsContract $ map varType vs)
                  w' <- jsCon $ DLL_Int sb UI_Word $ fromIntegral which
                  let getState = jsApply ("await ctc.getState") [w', ctcs]
                  return ["const" <+> jsArray vs' <+> "=" <+> getState <> semi]
        k' <- local (\e -> e {ctxt_isAPI = False}) $ jsETail k
        return $ vsep $ more <> [k']
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
          FI_Continue _svs -> do
            common [] <$> (jsCon $ DLL_Bool False)
  ET_ToConsensus _at fs_ok _prev lct_v which from_me msg_vs _out timev secsv didSendv mto k_ok -> do
    msg_ctcs <- mapM (jsContract . argTypeOf) $ map DLA_Var msg_vs
    msg_vs' <- mapM jsVar msg_vs
    let withCtxt = incTxn
    txn <- withCtxt jsTxn
    timev' <- jsVar timev
    secsv' <- jsVar secsv
    didSendv' <- jsVar didSendv
    fromv' <- jsVar fs_ok
    let txn_defs =
          [ "data:" <+> jsArray msg_vs'
          , "secs:" <+> secsv'
          , "time:" <+> timev'
          , "didSend:" <+> didSendv'
          , "from:" <+> fromv'
          ]
    let k_defp = "const" <+> braces (hsep $ punctuate comma txn_defs) <+> "=" <+> txn <> semi <> hardline
    k_ok' <- withCtxt $ jsETail k_ok
    let k_okp = k_defp <> k_ok'
    (delayp, k_p) <-
      case mto of
        Nothing -> return ("undefined /* mto */", k_okp)
        Just (delays, k_to) -> do
          k_top <- withCtxt $ jsETail k_to
          delays' <- case delays of
            Nothing -> return "undefined /* delays */"
            Just x -> jsTimeArg x
          timef <- withCtxt $ (<> ".didTimeout") <$> jsTxn
          return (delays', jsIf timef k_top k_okp)
    let a_funcNum = pretty which
    let a_evt_cnt = pretty $ length msg_vs
    let a_out_tys = jsArray msg_ctcs
    let a_timeout_delay = delayp
    let recvp =
          jsApplyKws "ctc.recv" $
            M.fromList $
              [ ("funcNum", a_funcNum)
              , ("evt_cnt", a_evt_cnt)
              , ("out_tys", a_out_tys)
              , ("didSend", "false")
              , ("waitIfNotPresent", "false")
              , ("timeoutAt", a_timeout_delay)
              ]
    callp <-
      withCtxt $
        case from_me of
          Just (args, amt, whena, svs, soloSend) -> do
            let svs_as = map DLA_Var svs
            amtp <- jsPayAmt amt
            let withSim = local (\e -> e {ctxt_mode = JM_Simulate})
            k_ok_sim <- liftIO . flip bigopt_sim k_ok =<< asks ctxt_ctr
            sim_body_core <- withSim $ jsETail k_ok_sim
            let dupeMap (mpv, _) = do
                  return $
                    (jsApply "stdlib.simMapDupe" $
                       ["sim_r", jsMapIdx mpv, jsMapVar mpv])
                      <> semi
            dupeMaps <- mapM dupeMap =<< ((M.toAscList . ctxt_maps) <$> ask)
            let tokCtr = vsep
                        [ "let sim_txn_ctr = stdlib.UInt_max;"
                        , "const getSimTokCtr = () => { sim_txn_ctr = sim_txn_ctr.sub(1); return sim_txn_ctr; };" ]
            let sim_body =
                  vsep
                    [ "const sim_r = { txns: [], mapRefs: [], maps: [] };"
                    , tokCtr
                    , vsep dupeMaps
                    , k_defp
                    , sim_body_core
                    , "return sim_r;"
                    ]
            vs <- jsArray <$> ((++) <$> mapM jsVar svs <*> mapM jsArg args)
            lct_v' <- case lct_v of
              Just x -> jsArg x
              Nothing -> jsCon $ DLL_Int sb UI_Word 0
            whena' <- jsArg whena
            soloSend' <- jsCon (DLL_Bool soloSend)
            msgts <- mapM (jsContract . argTypeOf) $ svs_as ++ args
            let a_sim_p = parens $ "async" <+> "(" <> txn <> ") => " <> jsBraces sim_body
            let sendp =
                  jsApplyKws "ctc.sendrecv" $
                    M.fromList $
                      [ ("funcNum", a_funcNum)
                      , ("lct", lct_v')
                      , ("evt_cnt", a_evt_cnt)
                      , ("tys", jsArray msgts)
                      , ("args", vs)
                      , ("pay", amtp)
                      , ("out_tys", a_out_tys)
                      , ("onlyIf", whena')
                      , ("soloSend", soloSend')
                      , ("waitIfNotPresent", "false")
                      , ("timeoutAt", a_timeout_delay)
                      , ("sim_p", a_sim_p)
                      ]
            return sendp
          Nothing ->
            return recvp
    let defp = "const" <+> txn <+> "=" <+> "await" <+> parens callp <> semi
    return $ vsep [defp, k_p]
  ET_While at asn cond body k -> do
    oldWhile <- ctxt_while <$> ask
    oldTxn <- jsTxn
    let withNewTxn = incTxn
    newTxn <- withNewTxn $ jsTxn
    let newTxnDef = "let" <+> newTxn <+> "=" <+> oldTxn <> semi <> hardline
    let newWhile = JWhile_Some oldWhile cond body k newTxn
    let newCtxt' = withNewTxn . local (\e -> e {ctxt_while = newWhile})
    cond' <- withNewTxn $ jsBlockNewScope cond
    body' <- newCtxt' $ jsETail body
    k' <- withNewTxn $ jsETail k
    (ctxt_mode <$> ask) >>= \case
      JM_Backend -> do
        asn' <- jsAsn AM_While asn
        return $ asn' <> hardline <> newTxnDef <> jsWhile cond' body' <> hardline <> k'
      JM_Simulate -> do
        asn' <- jsAsn AM_WhileSim asn
        return $ asn' <> hardline <> jsIf cond' body' k'
      JM_View -> impossible $ "view while " <> show at
  ET_Continue at asn -> do
    (ctxt_while <$> ask) >>= \case
      JWhile_None -> impossible $ "continue not in while: " <> show at
      JWhile_Diverge -> impossible $ "diverging while " <> show at
      JWhile_Some woldWhile wcond wbody wk wtxn -> do
        asn'o <- jsAsn AM_ContinueOuter asn
        asn'i <-
          (ctxt_mode <$> ask) >>= \case
            JM_View -> impossible "view continue"
            JM_Backend -> do
              asn_ <- jsAsn AM_ContinueInner asn
              ttxn <- jsTxn
              let asnWTxn = wtxn <+> "=" <+> ttxn <> semi <> hardline
              return $ asn_ <> hardline <> asnWTxn <> "continue" <> semi
            JM_Simulate -> do
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
      ia <- ctxt_isAPI <$> ask
      return $
        vsep $
          ["const" <+> jsMapVarCtc mpv <+> "=" <+> ctc <> ";"]
            <> (if varsHuh then ["const" <+> jsMapVar mpv <+> "=" <+> jsApplyKws "stdlib.newMap" (M.fromList $ [("idx", jsMapIdx mpv), ("ctc", "ctc"), ("ty", jsMapVarCtc mpv), ("isAPI", jsBool ia)]) <> ";"] else [])

jsError :: Doc -> Doc
jsError err = "new Error(" <> err <> ")"

setupPart :: Doc -> [Doc]
setupPart who = [
  ctcTopChk who
  , interactChk who
  , "const ctc = ctcTop._initialize();"
  , "const stdlib = ctc.stdlib;" ]

jsApiWrapper :: B.ByteString -> [Int] -> App Doc
jsApiWrapper p whichs = do
  let who = pretty $ bunpack p
  let aux = jsChkBigNum srcloc_builtin UI_Word
  allowed <- pretty <$> mapM (aux . jsNum . fromIntegral) whichs
  let jmps = map (\ which -> do
          let inst = "_" <> who <> pretty which
          "if" <+> parens ("step" <+> "==" <+> pretty which) <+> braces ("return " <> inst <> parens "ctcTop, interact" <> semi)
        ) whichs
  curStep <- aux "step"
  let body = vsep $
        setupPart who
        <> [ "const step = await ctc.getCurrentStep()" ]
        <> jmps
        <> [ "throw stdlib.apiStateMismatchError({ _stateSourceMap }, " <> allowed <> ", " <> curStep <> ")" ]
  return $ "export" <+> jsFunction who ["ctcTop", "interact"] body

iExpect :: Doc -> Doc -> Doc -> Doc
iExpect who this nth = "`The backend for" <+> who <+> "expects to receive" <+> this <+> "as its" <+> nth <+> "argument.`"

rejectIf :: Doc -> Doc -> Doc
rejectIf cond err = jsWhen cond $ jsReturn $ jsApply "Promise.reject" [jsError err]

ctcTopChk :: Doc -> Doc
ctcTopChk who = rejectIf "typeof(ctcTop) !== 'object' || ctcTop._initialize === undefined" $ iExpect who "a contract" "first"

interactChk :: Doc -> Doc
interactChk who = rejectIf "typeof(interact) !== 'object'" $ iExpect who "an interact object" "second"

jsPart :: DLInit -> (SLPart, Maybe Int) -> EPart -> App Doc
jsPart dli (p, m_api_which) (EPart {..}) = do
  let ctxt_isAPI = ep_isApi
  jsc@(JSContracts {..}) <- newJsContract
  let ctxt_ctcs = Just jsc
  let ctxt_who = p
  let ctxt_txn = 0
  let ctxt_while = JWhile_None
  let ctxt_mode = JM_Backend
  let ctxt_maps = dli_maps dli
  ctxt_ctr <- asks ctxt_ctr
  local (const JSCtxt {..}) $ do
    maps_defn <- jsMapDefns True
    et' <- jsETail ep_tail
    i2t' <- liftIO $ readIORef jsc_i2t
    let ctcs = vsep $ map snd $ M.toAscList i2t'
    let who = adjustApiName (bunpack p) (fromMaybe 0 m_api_which) (isJust m_api_which)
    let bodyp' =
          vsep $
            setupPart (pretty who)
            <> [ ctcs
            , maps_defn
            , et'
            ]
    return $ "export" <+> jsFunction (pretty who) ["ctcTop", "interact"] bodyp'

jsJSON :: AS.Value -> App Doc
jsJSON = \case
  AS.Null -> jsCon DLL_Null
  AS.Bool b -> jsCon $ DLL_Bool b
  -- Note: only integers are supported.
  -- TODO: throw error if non-integer detected,
  -- or alernatively, support non-integers
  AS.Number i -> return $ pretty $ Sci.formatScientific Sci.Fixed (Just 0) i
  AS.String t -> return $ jsBacktickText t
  AS.Array a -> jsArray <$> (mapM jsJSON $ Foldable.toList a)
  AS.Object m -> jsObject <$> (mapM jsJSON $ kmToM m)

jsCnp :: T.Text -> ConnectorInfo -> App Doc
jsCnp name cnp = do
  cnp' <- jsJSON cnp
  return $ "const" <+> "_" <> pretty name <+> "=" <+> cnp' <> semi

jsObjectDef :: Pretty k => Doc -> M.Map k Doc -> Doc
jsObjectDef v m = "export const" <+> v <+> "=" <+> jsObject m <> semi

jsExportBlock :: Bool -> DLExportBlock -> App Doc
jsExportBlock isAsync (DLinExportBlock _ margs b) = do
  (tl, ret) <- jsBlock b
  let argls = fromMaybe [] margs
  let args = map varLetVar argls
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

jsEvents :: DLEvents -> App Doc
jsEvents events = do
  jsFunctionWStdlib "_getEvents" [] $ do
    devts <-
      foldM
        (\acc (mk, m) -> do
           dv <- mapM (\ts -> jsArray <$> mapM jsContract ts) m
           case mk of
             Just k -> do
               return $ M.insert (bunpack k) (jsObject dv) acc
             Nothing -> return $ M.union acc dv)
        mempty
        $ M.toList events
    return $ jsReturn $ jsObject devts

jsViews :: DLViewsX -> App Doc
jsViews (DLViewsX cvs vis) = do
  let menv e = e {ctxt_mode = JM_View}
  jsFunctionWStdlib "_getViews" ["viewlib"] $
    local menv $ do
      let toObj fv o = jsObject <$> mapWithKeyM fv o
      let enView _ (ViewInfo vs _) =
            jsArray <$> (mapM jsContract $ map varType vs)
      views <- toObj enView vis
      let illegal = jsApply "stdlib.assert" ["false", jsString "illegal view"]
      let enDecode v k vi (ViewInfo vs vim) = do
            vs' <- mapM jsVar vs
            vi' <- jsCon $ DLL_Int sb UI_Word $ fromIntegral vi
            c <- jsPrimApply (PEQ UI_Word) ["i", vi']
            let let' = "const" <+> jsArray vs' <+> "=" <+> "svs" <> semi
            ret' <-
              case M.lookup k (fromMaybe mempty $ M.lookup v vim) of
                Just eb -> do
                  eb' <- jsExportBlock True $ dlebEnsureFun eb
                  let eb'call = jsApply ("await" <+> parens eb') ["...args"]
                  return $ jsReturn $ parens eb'call
                Nothing -> return $ illegal
            return $ jsWhen c $ vsep [let', ret']
      let enInfo' :: Maybe SLPart -> (SLVar, DLView) -> App ([(SLVar, Doc)], Doc)
          enInfo' v (k, (DLView _at vt aliases)) = do
            let (dom, rng) = itype2arr vt
            dom' <- jsArray <$> mapM jsContract dom
            rng' <- jsContract rng
            body <- (vsep . M.elems) <$> mapWithKeyM (enDecode v k) vis
            let body' = vsep [body, illegal]
            let decode' = jsApply "async " ["i", "svs", "args"] <+> "=>" <+> jsBraces body'
            let name = maybe "" (pretty . bunpack) v <> "_" <> pretty k
            let view_asn = "const " <> name <> " = " <> decode' <> ";"
            let val = jsObject $
                        M.fromList $
                          [ ("dom" :: String, dom')
                          , ("rng", rng')
                          , ("decode", name)
                          ]
            return $ (,view_asn) $ map (, val) $ k : map bunpack aliases
      let enInfo k v = do
            (m, view_asn) <- unzip <$> mapM (enInfo' k) (M.toList v)
            return (view_asn, M.fromList . concat $ m)
      infos' <- mapWithKeyM enInfo cvs
      let view_asns = concatMap fst $ M.elems infos'
      let infos'' = M.map snd infos'
      -- Lift untagged views to same level as tagged views
      let infos =
            jsObject $
              M.foldrWithKey
                (\mk ->
                   case mk of
                     Just k -> M.insert (bunpack k) . jsObject
                     Nothing -> M.union)
                mempty
                infos''
      maps_defn <- jsMapDefns False
      return $
        vsep $
          maps_defn :
          view_asns <>
          [ jsReturn $
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
reachBackendVersion = 26

jsEPProg :: ConnectorObject -> EPProg -> App Doc
jsEPProg cr (EPProg {..}) = do
  let DLInit {..} = epp_init
  let preamble =
        vsep
          [ pretty $ "// Automatically generated with Reach " ++ versionHashStr
          , "/* eslint-disable */"
          , -- XXX make these a `_metadata` object (cleaner on TS side)
            "export const _version =" <+> jsString versionStr <> semi
          , "export const _versionHash =" <+> jsString versionHashStr <> semi
          , "export const _backendVersion =" <+> pretty reachBackendVersion <> semi
          ]
  let go_api (p, mw) _ acc =
        case mw of
          Just w  -> M.insertWith (<>) p [w] acc
          Nothing -> acc
  let api_whichs = M.foldrWithKey go_api mempty epp_m
  api_wrappers <- mapM (uncurry jsApiWrapper) $ M.toAscList api_whichs
  partsp <- mapM (uncurry (jsPart epp_init)) $ M.toAscList epp_m
  cnpsp <- mapM (uncurry jsCnp) $ M.toAscList cr
  let connMap = M.fromList [(name, "_" <> pretty name) | name <- M.keys cr]
  ssmDoc <- mapM (\x -> jsAssertInfo (fst x) (snd x) Nothing) epp_stateSrcMap
  exportsp <- jsExports epp_exports
  viewsp <-
    local (\e -> e {ctxt_maps = dli_maps}) $
      jsViews epp_views
  mapsp <- jsMaps dli_maps
  let partMap = M.foldrWithKey (\ (p, _) _ acc -> M.insert p (pretty $ bunpack p) acc) mempty epp_m
  let go_api_map k v acc =
        let f = case k of
                Just k' -> M.insert (bunpack k') . jsObject
                Nothing -> M.union in
        let v' = M.map (pretty . bunpack . fst) v in
        f v' acc
  let apiMap = M.foldrWithKey go_api_map mempty epp_apis
  eventsp <- jsEvents epp_events
  return $ vsep $ [preamble, exportsp, eventsp, viewsp, mapsp] <> partsp <> api_wrappers <> cnpsp <> [jsObjectDef "_stateSourceMap" ssmDoc, jsObjectDef "_Connectors" connMap, jsObjectDef "_Participants" partMap, jsObjectDef "_APIs" apiMap]

backend_js :: Backend
backend_js outn crs p = do
  let jsf = outn "mjs"
  let ctxt_who = "Module"
  let ctxt_isAPI = False
  let ctxt_txn = 0
  let ctxt_mode = JM_Backend
  let ctxt_while = JWhile_None
  let ctxt_ctcs = Nothing
  let ctxt_maps = mempty
  let ctxt_ctr = getCounter p
  d <-
    flip runReaderT (JSCtxt {..}) $
      jsEPProg crs p
  LTIO.writeFile jsf $ render d
