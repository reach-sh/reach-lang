module Reach.Connector.ETH_Solidity (connect_eth) where

-- https://github.com/reach-sh/reach-lang/blob/8d912e0/hs/src/Reach/Connector/ETH_EVM.hs.dead

import Control.Monad
import Control.Monad.Reader
import Data.Aeson as Aeson
import Data.Aeson.Encode.Pretty
import Data.Bifunctor (Bifunctor (first))
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Data.List (intersperse)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as S
import Data.String (IsString)
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as LTIO
import Reach.AST.Base
import Reach.AST.DLBase
import Reach.AST.PL
import Reach.Connector
import Reach.Counter
import Reach.EmbeddedFiles
import Reach.Interference (colorProgram)
import Reach.Texty
import Reach.UnsafeUtil
import Reach.Util
import Reach.Version
import Reach.Warning
import System.Exit
import System.FilePath
import System.IO.Temp
import System.Process

--- Debugging tools

--- You can turn this to True and manually change the Solidity file
dontWriteSol :: Bool
dontWriteSol = False

maxDepth :: Int
maxDepth = 13

maxContractLen :: Int
maxContractLen = 24576

--- Solidity helpers

sb :: SrcLoc
sb = srcloc_builtin

solString :: String -> Doc
solString s = squotes $ pretty s

solNum :: Show n => n -> Doc
solNum i = pretty $ "uint256(" ++ show i ++ ")"

solBraces :: Doc -> Doc
solBraces body = braces (nest 2 $ hardline <> body)

data SolFunctionLike
  = SFL_Constructor
  | SFL_Function Bool (Doc)

solFunctionLike :: SolFunctionLike -> [Doc] -> Doc -> Doc -> Doc
solFunctionLike sfl args ret body =
  sflp <+> ret' <+> solBraces body
  where
    ret' = ext' <> ret
    (ext', sflp) =
      case sfl of
        SFL_Constructor ->
          (emptyDoc, solApply "constructor" args)
        SFL_Function ext name ->
          (ext'', "function" <+> solApply name args)
          where
            ext'' = if ext then "external " else " "

solFunction :: Doc -> [Doc] -> Doc -> Doc -> Doc
solFunction name =
  solFunctionLike (SFL_Function False name)

solContract :: String -> Doc -> Doc
solContract s body = "contract" <+> pretty s <+> solBraces body

solVersion :: Doc
solVersion = "pragma solidity ^0.8.2;"

solStdLib :: Doc
solStdLib = pretty $ B.unpack stdlib_sol

solCommas :: [Doc] -> Doc
solCommas args = hcat $ intersperse (comma <> space) args

solApply :: Doc -> [Doc] -> Doc
solApply f args = f <> parens (solCommas args)

solRequireMsg :: String -> App Doc
solRequireMsg umsg = do
  let smsg = unsafeRedactAbsStr umsg
  idx <- (liftIO . incCounter) =<< (ctxt_requireMsg <$> ask)
  -- XXX save the full string in a dictionary and emit it
  return $ solNum idx <+> "/*" <> solString smsg <> "*/"

solRequire :: String -> Doc -> App Doc
solRequire umsg a = do
  mmsg <- solRequireMsg umsg
  return $ solApply "reachRequire" $ [a, mmsg ]

solBinOp :: String -> Doc -> Doc -> Doc
solBinOp o l r = l <+> pretty o <+> r

solEq :: Doc -> Doc -> App Doc
solEq x y = solPrimApply PEQ [x, y]

solSet :: Doc -> Doc -> Doc
solSet x y = solBinOp "=" x y <> semi

solWhen :: Doc -> Doc -> Doc
solWhen c t = "if" <+> parens c <+> solBraces t

solIf :: Doc -> Doc -> Doc -> Doc
solIf c t f = solWhen c t <> hardline <> "else" <+> solBraces f

--- FIXME don't nest
solIfs :: [(Doc, Doc)] -> Doc
solIfs [] = emptyDoc
solIfs ((c, t) : more) = solIf c t $ solIfs more

solDecl :: Doc -> Doc -> Doc
solDecl n ty = ty <+> n

solStruct :: Doc -> [(Doc, Doc)] -> Maybe Doc
solStruct name = \case
  [] -> Nothing
  fields -> Just $ "struct" <+> name <+> solBraces (vsep $ map (<> semi) $ map (uncurry solDecl) fields)

solEnum :: Doc -> [Doc] -> Doc
solEnum name opts = "enum" <+> name <+> braces (hcat $ intersperse (comma <> space) opts)

--- Runtime helpers

solOutput_evt :: DLVar -> Doc
solOutput_evt dv = "oe_" <> solRawVar dv

solMsg_evt :: Pretty i => i -> Doc
solMsg_evt i = "e" <> pretty i

solMsg_fun :: Pretty i => i -> Doc
solMsg_fun i = "m" <> pretty i

solLoop_fun :: Pretty i => i -> Doc
solLoop_fun i = "l" <> pretty i

solMapVar :: DLMVar -> Doc
solMapVar mpv = pretty mpv

solMapRef :: DLMVar -> Doc
solMapRef mpv = pretty mpv <> "_ref"

solBlockNumber :: Doc
solBlockNumber = "uint256(block.number)"

solEncode :: [Doc] -> Doc
solEncode = solApply "abi.encode"

solHash :: [Doc] -> Doc
solHash a = solApply "uint256" [solApply "keccak256" [solEncode a]]

solArraySet :: Int -> Doc
solArraySet i = "array_set" <> pretty i

solArrayRef :: Doc -> Doc -> Doc
solArrayRef arr idx = arr <> brackets idx

solVariant :: Doc -> SLVar -> Doc
solVariant t vn = "_enum_" <> t <> "." <> pretty vn

solRawVar :: DLVar -> Doc
solRawVar (DLVar _ _ _ n) = pretty $ "v" ++ show n

solMemVar :: DLVar -> Doc
solMemVar dv = "_f." <> solRawVar dv

solArgSVSVar :: DLVar -> Doc
solArgSVSVar dv = "_a.svs." <> solRawVar dv

solArgMsgVar :: DLVar -> Doc
solArgMsgVar dv = "_a.msg." <> solRawVar dv

vsToType :: [DLVar] -> DLType
vsToType vs = T_Struct $ map go_ty vs
  where
    go_ty v = (show (solRawVar v), varType v)

objPrefix :: (Semigroup a, IsString a) => a -> a
objPrefix = ("_" <>)

--- Compiler

type VarMap = M.Map DLVar Doc

data SolCtxt = SolCtxt
  { ctxt_handler_num :: Int
  , ctxt_varm :: IORef VarMap
  , ctxt_mvars :: IORef (S.Set DLVar)
  , ctxt_depths :: IORef (M.Map DLVar Int)
  , ctxt_emit :: Doc
  , ctxt_typei :: IORef (M.Map DLType Int)
  , ctxt_typem :: IORef (M.Map DLType Doc)
  , ctxt_typed :: IORef (M.Map Int Doc)
  , ctxt_typef :: IORef (M.Map Int Doc)
  , ctxt_typeidx :: Counter
  , ctxt_plo :: PLOpts
  , ctxt_intidx :: Counter
  , ctxt_ints :: IORef (M.Map Int Doc)
  , ctxt_outputs :: IORef (M.Map DLVar Doc)
  , ctxt_hasViews :: Bool
  , ctxt_requireMsg :: Counter
  }

readCtxtIO :: (SolCtxt -> IORef a) -> App a
readCtxtIO f = (liftIO . readIORef) =<< (f <$> ask)

modifyCtxtIO :: (SolCtxt -> IORef a) -> (a -> a) -> App ()
modifyCtxtIO f m = (liftIO . (flip modifyIORef m)) =<< (f <$> ask)

type App = ReaderT SolCtxt IO

type AppT a = a -> App Doc

instance Semigroup a => Semigroup (App a) where
  mx <> my = (<>) <$> mx <*> my

instance Monoid a => Monoid (App a) where
  mempty = return mempty

addInterface :: Doc -> [Doc] -> Doc -> App Doc
addInterface f dom rng = do
  idx <- (liftIO . incCounter) =<< (ctxt_intidx <$> ask)
  let ip = "I" <> pretty idx
  let idef = "interface" <+> ip <+> solBraces ("function" <+> solApply f dom <+> "external returns" <+> parens rng <> semi)
  modifyCtxtIO ctxt_ints $ M.insert idx idef
  return $ ip <> "." <> f <> ".selector"

allocVarIdx :: App Int
allocVarIdx = do
  PLOpts {..} <- ctxt_plo <$> ask
  liftIO $ incCounter plo_counter

allocVar :: App Doc
allocVar = (pretty . (++) "v" . show) <$> allocVarIdx

extendVarMap :: VarMap -> App ()
extendVarMap vm1 = do
  varmr <- ctxt_varm <$> ask
  liftIO $ modifyIORef varmr $ (<>) vm1

freshVarMap :: App a -> App a
freshVarMap m = do
  let d f = liftIO . dupeIORef . f =<< ask
  varmr' <- d ctxt_varm
  mvars' <- d ctxt_mvars
  depths' <- d ctxt_depths
  local
    (\e ->
       e
         { ctxt_varm = varmr'
         , ctxt_mvars = mvars'
         , ctxt_depths = depths'
         })
    m

readMemVars :: App (S.Set DLVar)
readMemVars = readCtxtIO ctxt_mvars

addMemVar :: DLVar -> App ()
addMemVar v = do
  extendVarMap $ M.singleton v (solMemVar v)
  mvars <- ctxt_mvars <$> ask
  liftIO $ modifyIORef mvars $ S.insert v

addMemVars :: [DLVar] -> App ()
addMemVars = mapM_ addMemVar

recordDepth :: DLVar -> Int -> App ()
recordDepth v d = modifyCtxtIO ctxt_depths $ M.insert v d

readDepth :: DLVar -> App Int
readDepth v = do
  mvars <- readCtxtIO ctxt_depths
  return $ fromMaybe 0 $ M.lookup v mvars

addOutputEvent :: DLVar -> Doc -> App ()
addOutputEvent v d = modifyCtxtIO ctxt_outputs $ M.insert v d

class DepthOf a where
  depthOf :: a -> App Int

instance (Traversable t, DepthOf a) => DepthOf (t a) where
  depthOf o = maximum <$> mapM depthOf o

instance {-# OVERLAPS #-} (DepthOf a, DepthOf b) => DepthOf (a, b) where
  depthOf (x, y) = max <$> depthOf x <*> depthOf y

instance DepthOf DLVar where
  depthOf = readDepth

instance DepthOf DLArg where
  depthOf = \case
    DLA_Var v -> depthOf v
    DLA_Constant {} -> return 0
    DLA_Literal {} -> return 0
    DLA_Interact {} -> return 0

instance DepthOf DLLargeArg where
  depthOf = \case
    DLLA_Array _ as -> depthOf as
    DLLA_Tuple as -> depthOf as
    DLLA_Obj m -> depthOf m
    DLLA_Data _ _ x -> depthOf x
    DLLA_Struct kvs -> depthOf $ map snd kvs

instance DepthOf DLExpr where
  depthOf = \case
    DLE_Arg _ a -> depthOf a
    DLE_LArg _ a -> depthOf a
    DLE_Impossible {} -> return 0
    DLE_PrimOp _ _ as -> add1 $ depthOf as
    DLE_ArrayRef _ x y -> add1 $ depthOf [x, y]
    DLE_ArraySet _ x y z -> depthOf [x, y, z]
    DLE_ArrayConcat _ x y -> add1 $ depthOf [x, y]
    DLE_ArrayZip _ x y -> add1 $ depthOf [x, y]
    DLE_TupleRef _ x _ -> add1 $ depthOf x
    DLE_ObjectRef _ x _ -> add1 $ depthOf x
    DLE_Interact _ _ _ _ _ as -> depthOf as
    DLE_Digest _ as -> add1 $ depthOf as
    DLE_Claim _ _ _ a _ -> depthOf a
    DLE_Transfer _ x y z -> max <$> depthOf [x, y] <*> depthOf z
    DLE_TokenInit _ x -> depthOf x
    DLE_CheckPay _ _ y z -> max <$> depthOf y <*> depthOf z
    DLE_Wait _ x -> depthOf x
    DLE_PartSet _ _ x -> depthOf x
    DLE_MapRef _ _ x -> add1 $ depthOf x
    DLE_MapSet _ _ x y -> max <$> depthOf x <*> depthOf y
    DLE_Remote _ _ av _ (DLPayAmt net ks) as _ ->
      add1 $
        depthOf $
          av :
          net :
          pairList ks <> as
    where
      add1 m = (+) 1 <$> m
      pairList = concatMap (\(a, b) -> [a, b])

solVar :: AppT DLVar
solVar v = do
  varm <- readCtxtIO ctxt_varm
  case M.lookup v varm of
    Just x -> return $ x
    Nothing -> impossible $ "unbound var " ++ show v

mkSolType :: (Ord a) => (a -> App ()) -> (SolCtxt -> IORef (M.Map a Doc)) -> AppT a
mkSolType ensure f t = do
  ensure t
  (fromMaybe (impossible "solType") . M.lookup t) <$> readCtxtIO f

solType_ :: AppT DLType
solType_ = mkSolType ensureTypeDefined ctxt_typem

solType :: AppT DLType
solType t = do
  t' <- solType_ t
  case t' == "address" of
    True -> return $ t' <+> "payable"
    False -> return $ t'

solTypeI :: DLType -> App Int
solTypeI t = do
  ensureTypeDefined t
  (fromMaybe (impossible "solTypeI") . M.lookup t) <$> readCtxtIO ctxt_typei

mustBeMem :: DLType -> Bool
mustBeMem = \case
  T_Null -> False
  T_Bool -> False
  T_UInt -> False
  T_Bytes _ -> True
  T_Digest -> False
  T_Address -> False
  T_Token -> False
  T_Array {} -> True
  T_Tuple {} -> True
  T_Object {} -> True
  T_Data {} -> True
  T_Struct {} -> True

mayMemSol :: Doc -> Doc
mayMemSol x =
  case x' of
    "bool" -> no
    _ -> yes
  where
    x' = show x
    yes = x <> " memory"
    no = x

data ArgMode
  = AM_Call
  | AM_Memory
  | AM_Event

solArgLoc :: ArgMode -> Doc
solArgLoc = \case
  AM_Call -> " calldata"
  AM_Memory -> " memory"
  AM_Event -> ""

solLit :: DLLiteral -> Doc
solLit = \case
  DLL_Null -> "true"
  DLL_Bool True -> "true"
  DLL_Bool False -> "false"
  DLL_Int at i -> solNum $ checkIntLiteralC at connect_eth i
  DLL_Bytes s -> brackets $ solCommas $ map h $ B.unpack s
  where
    h x = solApply "uint8" [pretty $ show $ BI.c2w x]

solArg :: AppT DLArg
solArg = \case
  DLA_Var v -> solVar v
  DLA_Constant c -> return $ solLit $ conCons connect_eth c
  DLA_Literal c -> return $ solLit c
  DLA_Interact {} -> impossible "consensus interact"

solPrimApply :: PrimOp -> [Doc] -> App Doc
solPrimApply = \case
  SELF_ADDRESS -> impossible "self address"
  ADD -> safeOp "unsafeAdd" "+"
  SUB -> safeOp "unsafeSub" "-"
  MUL -> safeOp "unsafeMul" "*"
  DIV -> binOp "/"
  MOD -> binOp "%"
  PLT -> binOp "<"
  PLE -> binOp "<="
  PEQ -> binOp "=="
  PGE -> binOp ">="
  PGT -> binOp ">"
  LSH -> binOp "<<"
  RSH -> binOp ">>"
  BAND -> binOp "&"
  BIOR -> binOp "|"
  BXOR -> binOp "^"
  IF_THEN_ELSE -> \case
    -- XXX Copy the simplifications from ALGO.hs
    [c, t, f] -> return $ c <+> "?" <+> t <+> ":" <+> f
    _ -> impossible $ "emitSol: ITE wrong args"
  DIGEST_EQ -> binOp "=="
  ADDRESS_EQ -> binOp "=="
  TOKEN_EQ -> binOp "=="
  BYTES_CONCAT -> impossible "bytes concat"
  where
    safeOp fun op args = do
      PLOpts {..} <- ctxt_plo <$> ask
      case plo_verifyArithmetic of
        False -> binOp op args
        True -> return $ solApply fun args
    binOp op = \case
      [l, r] -> return $ solBinOp op l r
      _ -> impossible $ "emitSol: bin op args"

solLargeArg' :: Doc -> DLLargeArg -> App Doc
solLargeArg' dv la =
  case la of
    DLLA_Array _ as -> c <$> (zipWithM go ([0 ..] :: [Int]) as)
      where
        go i a = one ("[" <> pretty i <> "]") <$> solArg a
    DLLA_Tuple as -> c <$> (zipWithM go ([0 ..] :: [Int]) as)
      where
        go i a = one (".elem" <> pretty i) <$> solArg a
    DLLA_Obj m ->
      solLargeArg' dv $ DLLA_Struct $ map (first objPrefix) $ M.toAscList m
    DLLA_Data _ vn vv -> do
      t <- solType $ largeArgTypeOf la
      vv' <- solArg vv
      return $
        c
          [ one ".which" (solVariant t vn)
          , one ("._" <> pretty vn) vv'
          ]
    DLLA_Struct kvs -> c <$> (mapM go kvs)
      where
        go (k, a) = one ("." <> pretty k) <$> solArg a
  where
    one :: Doc -> Doc -> Doc
    one f v = dv <> f <+> "=" <+> v <> semi
    c = vsep

solLargeArg :: DLVar -> DLLargeArg -> App Doc
solLargeArg dv la = flip solLargeArg' la =<< solVar dv

solExpr :: Doc -> DLExpr -> App Doc
solExpr sp = \case
  DLE_Arg _ a -> spa $ solArg a
  DLE_LArg {} ->
    impossible "large arg"
  DLE_Impossible at msg ->
    expect_thrown at $ Err_Impossible msg
  DLE_PrimOp _ p args -> do
    args' <- mapM solArg args
    spa $ solPrimApply p args'
  DLE_ArrayRef _ ae ie ->
    spa $ (solArrayRef <$> solArg ae <*> solArg ie)
  DLE_ArraySet _ ae ie ve -> do
    args' <- mapM solArg [ae, ie, ve]
    ti <- solTypeI (argTypeOf ae)
    spa $ return $ solApply (solArraySet ti) args'
  DLE_ArrayConcat {} ->
    impossible "array concat"
  DLE_ArrayZip {} ->
    impossible "array zip"
  DLE_TupleRef _ ae i -> do
    ae' <- solArg ae
    return $ ae' <> ".elem" <> pretty i <> sp
  DLE_ObjectRef _ oe f -> do
    oe' <- solArg oe
    return $ oe' <> "." <> objPrefix (pretty f) <> sp
  DLE_Interact {} -> impossible "consensus interact"
  DLE_Digest _ args -> do
    args' <- mapM solArg args
    return $ (solHash $ args') <> sp
  DLE_Claim at fs ct a mmsg -> spa check
    where
      check = case ct of
        CT_Assert -> impossible "assert"
        CT_Assume _ -> require
        CT_Require -> require
        CT_Possible -> impossible "possible"
        CT_Unknowable {} -> impossible "unknowable"
      require = solRequire (show (at, fs, mmsg)) =<< solArg a
  DLE_Transfer _ who amt mtok ->
    spa $ solTransfer who amt mtok
  DLE_TokenInit {} -> return emptyDoc
  DLE_CheckPay at fs amt mtok -> do
    let require :: String -> Doc -> App Doc
        require msg e = spa $ solRequire (show (at, fs, msg)) e
    amt' <- solArg amt
    case mtok of
      Nothing -> do
        cmp <- solEq "msg.value" amt'
        require "verify network token pay amount" cmp
      Just tok -> do
        tok' <- solArg tok
        require "verify non-network token pay amount" $
          solApply "checkPayAmt" ["msg.sender", tok', amt']
  DLE_Wait {} -> return emptyDoc
  DLE_PartSet _ _ a -> spa $ solArg a
  DLE_MapRef _ mpv fa -> do
    fa' <- solArg fa
    return $ solApply (solMapRef mpv) [fa'] <> sp
  DLE_MapSet _ mpv fa (Just na) -> do
    fa' <- solArg fa
    solLargeArg' (solArrayRef (solMapVar mpv) fa') nla
    where
      nla = mdaToMaybeLA na_t (Just na)
      na_t = argTypeOf na
  DLE_MapSet _ mpv fa Nothing -> do
    fa' <- solArg fa
    return $ "delete" <+> solArrayRef (solMapVar mpv) fa' <> sp
  DLE_Remote {} -> impossible "remote"
  where
    spa m = (<> sp) <$> m

solTransfer :: DLArg -> DLArg -> Maybe DLArg -> App Doc
solTransfer who amt mtok = do
  who' <- solArg who
  amt' <- solArg amt
  case mtok of
    Nothing ->
      return $ who' <> "." <> solApply "transfer" [amt']
    Just tok -> do
      tok' <- solArg tok
      return $ solApply "safeTokenTransfer" [tok', who', amt']

solEvent :: Int -> [DLVar] -> [DLVar] -> App Doc
solEvent which svs msg = do
  arg_ty' <- solArgType svs msg
  return $ "event" <+> solApply (solMsg_evt which) [arg_ty' <+> "_a"] <> semi

solEventEmit :: Int -> Doc
solEventEmit which =
  "emit" <+> solApply (solMsg_evt which) ["_a"] <> semi

solAsnType :: [DLVar] -> App Doc
solAsnType = solType . vsToType

solAsnSet :: Doc -> [(DLVar, DLArg)] -> App [Doc]
solAsnSet asnv vs = do
  let go (v, a) = solSet (asnv <> "." <> solRawVar v) <$> solArg a
  vs' <- mapM go vs
  vs_ty' <- solAsnType $ map fst vs
  return $ [solDecl asnv (mayMemSol vs_ty') <> semi] <> vs'

solHashStateSet :: Int -> [(DLVar, DLArg)] -> App ([Doc], Doc)
solHashStateSet which svs = do
  let asnv = "nsvs"
  let sete = solHash [(solNum which), asnv]
  setl <- solAsnSet asnv svs
  return $ (setl, sete)

solHashStateCheck :: Int -> Doc
solHashStateCheck prev =
  solHash [(solNum prev), "_a.svs"]

arraySize :: DLArg -> Integer
arraySize a =
  case argTypeOf a of
    T_Bytes sz -> sz
    T_Array _ sz -> sz
    _ -> impossible "arraySize"

solSwitch :: AppT k -> SrcLoc -> DLVar -> SwitchCases k -> App Doc
solSwitch iter _at ov csm = do
  ovp <- solVar ov
  t <- solType $ argTypeOf (DLA_Var ov)
  let cm1 (vn, (mov', body)) = do
        c <- solEq (ovp <> ".which") (solVariant t vn)
        set' <-
          case mov' of
            Just ov' -> do
              addMemVar ov'
              return $ solSet (solMemVar ov') (ovp <> "._" <> pretty vn)
            Nothing -> return $ emptyDoc
        body' <- iter body
        let set_and_body' = vsep [set', body']
        return (c, set_and_body')
  solIfs <$> (mapM cm1 $ M.toAscList csm)

withArgLoc :: DLType -> Doc
withArgLoc t =
  solArgLoc $
    case mustBeMem t of
      True -> AM_Memory
      False -> AM_Event

solType_withArgLoc :: DLType -> App Doc
solType_withArgLoc t =
  (<> (withArgLoc t)) <$> solType t

doConcat :: DLVar -> DLArg -> DLArg -> App Doc
doConcat dv x y = do
  addMemVar dv
  dv' <- solVar dv
  let copy src (off :: Integer) = do
        let sz = arraySize src
        src' <- solArg src
        add <- solPrimApply ADD ["i", solNum off]
        let ref = solArrayRef src' "i"
        return $ "for" <+> parens ("uint256 i = 0" <> semi <+> "i <" <+> (pretty sz) <> semi <+> "i++") <> solBraces (solArrayRef dv' add <+> "=" <+> ref <> semi)
  x' <- copy x 0
  y' <- copy y (arraySize x)
  return $ vsep [x', y']

solCom :: AppT DLStmt
solCom = \case
  DL_Nop _ -> mempty
  DL_Let _ pv (DLE_Remote at fs av f (DLPayAmt net ks) as (DLWithBill nnTokRecvVar nonNetTokRecv nnTokRecvZero)) -> do
    av' <- solArg av
    as' <- mapM solArg as
    dom'mem <- mapM (solType_withArgLoc . argTypeOf) as
    let rng_ty = case pv of
          DLV_Eff -> T_Null
          DLV_Let _ dv ->
            case varType dv of
              T_Tuple [_, _, x] -> x
              T_Tuple [_, x] -> x
              _ -> impossible $ "remote not tuple"
    rng_ty' <- solType rng_ty
    let rng_ty'mem = rng_ty' <> withArgLoc rng_ty
    f' <- addInterface (pretty f) dom'mem rng_ty'mem
    let eargs = f' : as'
    v_succ <- allocVar
    v_return <- allocVar
    v_before <- allocVar
    -- Note: Not checking that the address is really a contract and not doing
    -- exactly what OpenZeppelin does
    netTokPaid <- solArg net
    ks' <- mapM (\(amt, ty) -> (,) <$> solArg amt <*> solArg ty) ks
    let getBalance tok = solApply "tokenBalanceOf" [tok, "address(this)"]
    nonNetTokApprovals <-
      mapM
        (\(amt, ty) -> do
           let approve = solApply "tokenApprove" [ty, av', amt]
           flip (<>) semi <$> solRequire "Approving remote ctc to transfer tokens" approve)
        ks'
    checkNonNetTokAllowances <-
      mapM
        (\(_, ty) -> do
           -- Ensure that the remote ctc transfered the approved funds
           let allowance = solApply "tokenAllowance" [ty, "address(this)", av']
           eq <- solEq allowance "0"
           req <- solRequire "Ensure remote ctc transferred approved funds" eq
           return $ vsep [req <> semi])
        ks'
    -- This is for when we don't know how much non-net tokens we will receive. i.e. `withBill`
    -- The amount of non-network tokens received will be stored in this tuple: nnTokRecvVar
    addMemVar nnTokRecvVar
    let nnTokRecv = solMemVar nnTokRecvVar
    (getDynamicNonNetTokBals, setDynamicNonNetTokBals) <-
      unzip
        <$> mapM
          (\(tok, i :: Int) -> do
             tv_before <- allocVar
             tokArg <- solArg tok
             -- Get balances of non-network tokens before call
             let s1 = solSet ("uint256" <+> tv_before) $ getBalance tokArg
             -- Get balances of non-network tokens after call
             tokRecv <- solPrimApply SUB [getBalance tokArg, tv_before]
             let s2 = solSet (nnTokRecv <> ".elem" <> pretty i) tokRecv
             return (s1, s2))
          (zip nonNetTokRecv [0 ..])
    let nonNetToksPayAmt = foldr' (\(a, t) acc -> M.insert t a acc) M.empty ks
    -- Ensure that the non-net tokens we are NOT expecting to receive
    -- do not have a change in balance
    (getUnexpectedNonNetTokBals, checkUnexpectedNonNetTokBals) <-
      unzip
        <$> mapM
          (\tok -> do
             tv_before <- allocVar
             tokArg <- solArg tok
             paid <- maybe (return "0") solArg $ M.lookup tok nonNetToksPayAmt
             sub <- solPrimApply SUB [getBalance tokArg, paid]
             let s1 = solSet ("uint256" <+> tv_before) sub
             tokRecv <- solPrimApply SUB [getBalance tokArg, tv_before]
             s2 <- solRequire "remote did not transfer unexpected non-network tokens" =<< solEq tokRecv "0"
             return (s1, s2 <> semi))
          nnTokRecvZero
    let call' = ".call{value:" <+> netTokPaid <> "}"
    let meBalance = "address(this).balance"
    let billOffset :: Int -> Doc
        billOffset i = viaShow $ if null nonNetTokRecv then i else i + 1
    pv' <- case pv of
      DLV_Eff -> return []
      DLV_Let _ dv -> do
        addMemVar dv
        sub' <- solPrimApply SUB [meBalance, v_before]
        let sub'l = [solSet (solMemVar dv <> ".elem0") sub']
        -- Non-network tokens received from remote call
        let nnsub'l = [solSet (solMemVar dv <> ".elem1") nnTokRecv | not (null nonNetTokRecv)]
        dv_ty' <- solType $ varType dv
        let go sv l = solApply (solOutput_evt dv) [l <+> sv dv] <> semi
        addOutputEvent dv $ "event" <+> go solRawVar dv_ty'
        let logl = ["emit" <+> go solMemVar ""]
        case rng_ty of
          T_Null ->
            return $ sub'l <> nnsub'l <> logl
          _ -> do
            let de' = solApply "abi.decode" [v_return, parens rng_ty']
            let de'l = [solSet (solMemVar dv <> ".elem" <> billOffset 1) de'] -- not always 2
            return $ sub'l <> nnsub'l <> de'l <> logl
    let e_data = solApply "abi.encodeWithSelector" eargs
    e_before <- solPrimApply SUB [meBalance, netTokPaid]
    err_msg <- solRequireMsg $ show (at, fs, ("remote " <> f <> " failed"))
    -- XXX we could assert that the balances of all our tokens is the same as
    -- it was before
    return $
      vsep $
        nonNetTokApprovals
          <> getDynamicNonNetTokBals
          <> getUnexpectedNonNetTokBals
          <> [ solSet ("uint256" <+> v_before) e_before
             , "(bool " <> v_succ <> ", bytes memory " <> v_return <> ")" <+> "=" <+> av' <> solApply call' [e_data] <> semi
             , solApply "checkFunReturn" [v_succ, v_return, err_msg] <> semi
             ]
          <> checkUnexpectedNonNetTokBals
          <> setDynamicNonNetTokBals
          <> checkNonNetTokAllowances
          <> pv'
  DL_Let _ (DLV_Let _ dv) (DLE_LArg _ la) -> do
    addMemVar dv
    solLargeArg dv la
  DL_Let _ (DLV_Let _ dv) (DLE_PrimOp _ BYTES_CONCAT [x, y]) -> do
    doConcat dv x y
  DL_Let _ (DLV_Let _ dv) (DLE_ArrayConcat _ x y) -> do
    doConcat dv x y
  DL_Let _ (DLV_Let _ dv@(DLVar _ _ t _)) (DLE_ArrayZip _ x y) -> do
    addMemVar dv
    let (xy_ty, xy_sz) = case t of
          T_Array a b -> (a, b)
          _ -> impossible "array_zip"
    tcon <- solType xy_ty
    dv' <- solVar dv
    let ith which = solArrayRef <$> solArg which <*> pure "i"
    x' <- ith x
    y' <- ith y
    return $ "for" <+> parens ("uint256 i = 0" <> semi <+> "i <" <+> (pretty $ xy_sz) <> semi <+> "i++") <> solBraces (solArrayRef dv' "i" <+> "=" <+> solApply tcon [x', y'] <> semi)
  DL_Let _ (DLV_Let pu dv) de ->
    case simple de of
      True -> no_def
      False ->
        case pu of
          DVC_Once -> no_def
          DVC_Many -> def
    where
      simple = \case
        DLE_Arg {} -> True
        DLE_ArrayRef {} -> True
        DLE_TupleRef {} -> True
        DLE_ObjectRef {} -> True
        _ -> False
      no_def = do
        dp <- depthOf de
        case dp > maxDepth of
          True -> def
          False -> do
            de' <- parens <$> solExpr emptyDoc de
            extendVarMap $ M.singleton dv de'
            recordDepth dv dp
            mempty
      def = do
        addMemVar dv
        de' <- solExpr emptyDoc de
        return $ solSet (solMemVar dv) de'
  DL_Let _ DLV_Eff de -> solExpr semi de
  DL_Var _ dv -> do
    addMemVar dv
    mempty
  DL_Set _ dv da -> solSet (solMemVar dv) <$> solArg da
  DL_LocalIf _ ca t f ->
    solIf <$> solArg ca <*> solPLTail t <*> solPLTail f
  DL_LocalSwitch at ov csm -> solSwitch solPLTail at ov csm
  DL_Only {} -> impossible $ "only in CT"
  DL_ArrayMap _ ans x a (DLBlock _ _ f r) -> do
    addMemVars $ [ans, a]
    let sz = arraySize x
    ans' <- solVar ans
    x' <- solArg x
    a' <- solVar a
    f' <- solPLTail f
    r' <- solArg r
    return $
      vsep
        [ "for" <+> parens ("uint256 i = 0" <> semi <+> "i <" <+> (pretty sz) <> semi <+> "i++")
            <> solBraces
              (vsep
                 [ a' <+> "=" <+> (solArrayRef x' "i") <> semi
                 , f'
                 , (solArrayRef ans' "i") <+> "=" <+> r' <> semi
                 ])
        ]
  DL_ArrayReduce _ ans x z b a (DLBlock _ _ f r) -> do
    addMemVars $ [ans, b, a]
    let sz = arraySize x
    ans' <- solVar ans
    x' <- solArg x
    z' <- solArg z
    a' <- solVar a
    b' <- solVar b
    f' <- solPLTail f
    r' <- solArg r
    return $
      vsep
        [ b' <+> "=" <+> z' <> semi
        , "for" <+> parens ("uint256 i = 0" <> semi <+> "i <" <+> (pretty sz) <> semi <+> "i++")
            <> solBraces
              (vsep
                 [ a' <+> "=" <+> (solArrayRef x' "i") <> semi
                 , f'
                 , b' <+> "=" <+> r' <> semi
                 ])
        , ans' <+> "=" <+> b' <> semi
        ]
  DL_MapReduce {} ->
    impossible $ "cannot inspect maps at runtime"
  DL_LocalDo _ t -> solPLTail t

solCom_ :: AppT a -> DLStmt -> AppT a
solCom_ iter m k = do
  m' <- solCom m
  k' <- iter k
  return $
    case m' == emptyDoc of
      True -> k'
      False -> m' <> hardline <> k'

solPLTail :: AppT DLTail
solPLTail = \case
  DT_Return _ -> mempty
  DT_Com m k -> solCom_ solPLTail m k

solCTail :: AppT CTail
solCTail = \case
  CT_Com m k -> solCom_ solCTail m k
  CT_If _ ca t f -> solIf <$> solArg ca <*> solCTail t <*> solCTail f
  CT_Switch at ov csm -> solSwitch solCTail at ov csm
  CT_Jump _ which svs (DLAssignment asnm) -> do
    let go_svs v = solSet ("la.svs." <> solRawVar v) <$> solVar v
    svs' <- mapM go_svs svs
    let go_asn (v, a) = solSet ("la.msg." <> solRawVar v) <$> solArg a
    asn' <- mapM go_asn (M.toAscList asnm)
    emit' <- ctxt_emit <$> ask
    argDefn <- solArgDefn_ "la" AM_Memory svs (map fst $ M.toAscList asnm)
    return $
      vsep $
        [ emit'
        , argDefn <> semi
        ]
          <> svs'
          <> asn'
          <> [solApply (solLoop_fun which) ["la"] <> semi]
  CT_From _ which (FI_Continue (ViewSave vi vvs) svs) -> do
    (setl, sete) <- solHashStateSet which svs
    emit' <- ctxt_emit <$> ask
    viewl <-
      (ctxt_hasViews <$> ask) >>= \case
        False -> return []
        True -> do
          let vi' = solNum vi
          let asnv = "vvs"
          setv <- solAsnSet asnv vvs
          return $
            setv
              <> [ solSet "current_view_i" vi'
                 , solSet "current_view_bs" $ solEncode [asnv]
                 ]
    return $ vsep $ [emit'] <> viewl <> setl <> [solSet ("current_state") sete]
  CT_From _ _ (FI_Halt _toks) -> do
    -- XXX we could "selfdestruct" our token holdings, based on _toks
    emit' <- ctxt_emit <$> ask
    hv <- ctxt_hasViews <$> ask
    let mviewl =
          case hv of
            False -> []
            True ->
              [ solSet "current_view_i" "0x0"
              , "delete current_view_bs;"
              ]
    return $
      vsep $
        [emit', solSet "current_state" "0x0"]
          <> mviewl
          <> [solApply "selfdestruct" ["payable(msg.sender)"] <> semi]

solFrame :: Int -> S.Set DLVar -> App (Doc, Doc)
solFrame i sim = do
  let mk_field dv@(DLVar _ _ t _) = (,) (solRawVar dv) <$> (solType t)
  fs <- mapM mk_field $ S.elems sim
  let framei = pretty $ "_F" ++ show i
  case solStruct framei fs of
    Nothing -> return $ (emptyDoc, emptyDoc)
    Just frame_defp -> do
      let frame_declp = (framei <+> "memory _f") <> semi
      return $ (frame_defp, frame_declp)

solCTail_top :: Int -> [DLVar] -> [DLVar] -> Maybe [DLVar] -> CTail -> App (Doc, Doc, Doc)
solCTail_top which svs msg mmsg ct = do
  let svsm = M.fromList $ map (\v -> (v, solArgSVSVar v)) svs
  let msgm = M.fromList $ map (\v -> (v, solArgMsgVar v)) msg
  let emitp = case mmsg of
        Just _ -> solEventEmit which
        Nothing -> emptyDoc
  extendVarMap $ svsm <> msgm
  ct' <- local
    (\e ->
       e
         { ctxt_emit = emitp
         , ctxt_handler_num = which
         })
    $ do
      solCTail ct
  mvars <- readMemVars
  (frameDefn, frameDecl) <- solFrame which mvars
  return (frameDefn, frameDecl, ct')

solArgType :: [DLVar] -> [DLVar] -> App Doc
solArgType svs msg = do
  let svs_ty = vsToType svs
  let msg_ty = vsToType msg
  let arg_ty = T_Struct $ [("svs", svs_ty), ("msg", msg_ty)]
  solType arg_ty

solArgDefn_ :: Doc -> ArgMode -> [DLVar] -> [DLVar] -> App Doc
solArgDefn_ name am svs msg = do
  arg_ty' <- solArgType svs msg
  return $ solDecl name (arg_ty' <> solArgLoc am)

solArgDefn :: ArgMode -> [DLVar] -> [DLVar] -> App Doc
solArgDefn = solArgDefn_ "_a"

solHandler :: Int -> CHandler -> App Doc
solHandler which h = freshVarMap $
  case h of
    C_Handler at interval last_timemv from prev svs msg timev ct -> do
      let checkMsg s = s <> " check at " <> show at
      let fromm = M.singleton from "payable(msg.sender)"
      let given_mm = M.fromList [(timev, solBlockNumber)]
      extendVarMap $ given_mm <> fromm
      timev' <- solVar timev
      (frameDefn, frameDecl, ctp) <- solCTail_top which svs msg (Just msg) ct
      evtDefn <- solEvent which svs msg
      let ret = "payable"
      plo <- ctxt_plo <$> ask
      (hashCheck, am, sfl) <-
        case (which, plo_deployMode plo) of
          (1, DM_firstMsg) ->
            return (emptyDoc, AM_Memory, SFL_Constructor)
          _ -> do
            eq' <- solEq ("current_state") (solHashStateCheck prev)
            req <- solRequire (checkMsg "state") eq'
            let hcp =
                  vsep
                    [ req <> semi
                    , -- This is a re-entrancy lock, because we know that
                      -- all methods start by checking the current state.
                      -- When we implement on-chain state, we need to do
                      -- this differently
                      solSet "current_state" "0x0"
                    ]
            return (hcp, AM_Call, SFL_Function True (solMsg_fun which))
      argDefn <- solArgDefn am svs msg
      timeoutCheck <-
        case last_timemv of
          Nothing -> return emptyDoc
          Just last_timev -> do
            last_timev' <- solVar last_timev
            let check sign mv =
                  case mv of
                    [] -> return Nothing
                    mvs -> do
                      mvs' <- mapM solArg mvs
                      let go_sum x y = solPrimApply ADD [x, y]
                      sum' <- foldlM go_sum last_timev' mvs'
                      Just <$> solPrimApply (if sign then PGE else PLT) [timev', sum']
            let CBetween ifrom ito = interval
            int_fromp <- check True ifrom
            int_top <- check False ito
            let req x = flip (<>) semi <$> solRequire (checkMsg "timeout") x
            case (int_fromp, int_top) of
              (Just x, Just y) -> req (solBinOp "&&" x y)
              (Just x, _) -> req x
              (_, Just y) -> req y
              _ -> return $ emptyDoc
      let body = vsep [hashCheck, frameDecl, timeoutCheck, ctp]
      let funDefn = solFunctionLike sfl [argDefn] ret body
      return $ vsep [evtDefn, frameDefn, funDefn]
    C_Loop _at svs lcmsg ct -> do
      let msg = lcmsg
      (frameDefn, frameDecl, ctp) <- solCTail_top which svs msg Nothing ct
      argDefn <- solArgDefn AM_Memory svs msg
      let ret = "internal"
      let body = vsep [frameDecl, ctp]
      let funDefn = solFunction (solLoop_fun which) [argDefn] ret body
      return $ vsep [frameDefn, funDefn]

solHandlers :: CHandlers -> App Doc
solHandlers (CHandlers hs) =
  vsep <$> (mapM (uncurry solHandler) $ M.toList hs)

solDefineType :: DLType -> App ()
solDefineType t = case t of
  T_Null -> base
  T_Bool -> base
  T_UInt -> base
  T_Bytes sz -> do
    -- NOTE: This wastes a lot of space, but we can't make fixed bytes larger
    -- than 32
    addMap $ "uint8[" <> pretty sz <> "]"
  T_Digest -> base
  T_Address -> base
  T_Token -> base
  T_Array _ 0 -> do
    addMap "bool"
  T_Array et sz -> do
    tn <- solType et
    let me = tn <> brackets (pretty sz)
    let inmem = solArgLoc AM_Memory
    let memem = me <> inmem
    let tnmem = tn <> (if mustBeMem et then inmem else "")
    let args =
          [ solDecl "arr" memem
          , solDecl "idx" "uint256"
          , solDecl "val" tnmem
          ]
    let ret = "pure internal" <+> "returns" <+> parens (solDecl "arrp" memem)
    let assign idx val = (solArrayRef "arrp" idx) <+> "=" <+> val <> semi
    let body =
          vsep
            [ ("for" <+> parens ("uint256 i = 0" <> semi <+> "i <" <+> (pretty sz) <> semi <+> "i++")
                 <> solBraces (assign "i" (solArrayRef "arr" "i")))
            , assign "idx" "val"
            ]
    addMap me
    i <- addId
    addFun i $ solFunction (solArraySet i) args ret body
  T_Tuple ats -> do
    let ats' = (flip zip) ats $ map (("elem" ++) . show) ([0 ..] :: [Int])
    addMap =<< doStruct ats'
  T_Object tm -> do
    addMap =<< (doStruct $ map (first objPrefix) $ M.toAscList tm)
  T_Data tm -> do
    tmn <- mapM solType tm
    --- XXX Try to use bytes and abi.decode; Why not right away? The
    --- length of the bytes would not be predictable, which means the
    --- gas cost would be arbitrary.
    (name, i) <- addName
    let enumn = "_enum_" <> name
    let enump = solEnum enumn $ map (pretty . fst) $ M.toAscList tmn
    let structp = fromMaybe (impossible "T_Data") $ solStruct name $ ("which", enumn) : map (\(k, kt) -> (pretty ("_" <> k), kt)) (M.toAscList tmn)
    addDef i $ vsep [enump, structp]
  T_Struct ats -> void $ doStruct ats
  where
    base = impossible "base"
    addMap d = modifyCtxtIO ctxt_typem $ M.insert t d
    addId = do
      i <- (liftIO . incCounter) =<< (ctxt_typeidx <$> ask)
      modifyCtxtIO ctxt_typei $ M.insert t i
      return $ i
    addName = do
      i <- addId
      let name = "T" <> pretty i
      addMap name
      return $ (name, i)
    addDef i d = modifyCtxtIO ctxt_typed $ M.insert i d
    addFun i f = modifyCtxtIO ctxt_typef $ M.insert i f
    doStruct ats = do
      atsn <- mapM (\(k, v) -> (,) (pretty k) <$> solType v) ats
      (name, i) <- addName
      let sp = solStruct name atsn
      case sp of
        -- XXX This really stinks
        Nothing -> do
          addMap "bool"
          return "bool"
        Just x -> do
          addDef i x
          return name

mkEnsureTypeDefined :: (Ord a) => (a -> App ()) -> (SolCtxt -> IORef (M.Map a b)) -> a -> App ()
mkEnsureTypeDefined define f t =
  (M.lookup t <$> readCtxtIO f) >>= \case
    Nothing -> define t
    Just _ -> return ()

ensureTypeDefined :: DLType -> App ()
ensureTypeDefined = mkEnsureTypeDefined solDefineType ctxt_typem

solEB :: [DLVar] -> AppT DLExportBlock
solEB args (DLinExportBlock _ mfargs (DLBlock _ _ t r)) = do
  let fargs = fromMaybe mempty mfargs
  let go a fa = do
        a' <- solVar a
        return $ (fa, a')
  extendVarMap =<< (M.fromList <$> zipWithM go args fargs)
  t' <- solPLTail t
  r' <- solArg r
  return $ vsep [t', "return" <+> r' <> semi]

solPLProg :: PLProg -> IO (ConnectorInfoMap, Doc)
solPLProg (PLProg _ plo@(PLOpts {..}) dli _ _ (CPProg at mvi hs)) = do
  let DLInit {..} = dli
  let ctxt_handler_num = 0
  let ctxt_emit = emptyDoc
  ctxt_varm <- newIORef mempty
  ctxt_mvars <- newIORef mempty
  ctxt_depths <- newIORef mempty
  ctxt_typei <- newIORef mempty
  let base_typem =
        M.fromList
          [ (T_Null, "bool")
          , (T_Bool, "bool")
          , (T_UInt, "uint256")
          , (T_Digest, "uint256")
          , (T_Address, "address")
          , (T_Token, "address")
          ]
  ctxt_typem <- newIORef base_typem
  ctxt_typef <- newIORef mempty
  ctxt_typed <- newIORef mempty
  ctxt_typeidx <- newCounter 0
  ctxt_intidx <- newCounter 0
  ctxt_requireMsg <- newCounter 5
  ctxt_ints <- newIORef mempty
  ctxt_outputs <- newIORef mempty
  let ctxt_plo = plo
  let ctxt_hasViews = isJust mvi
  flip runReaderT (SolCtxt {..}) $ do
    consp <-
      case plo_deployMode of
        DM_constructor -> do
          let (ctimem', csvs_, withC) =
                case dli_ctimem of
                  Nothing -> (mempty, mempty, id)
                  Just v ->
                    ( [solSet (solMemVar v) solBlockNumber]
                    , [v]
                    , (\m -> do
                         extendVarMap $ M.singleton v (solMemVar v)
                         m)
                    )
          let dli' = vsep $ ctimem'
          (cfDefn, cfDecl) <- withC $ solFrame 0 (S.fromList csvs_)
          let csvs_m = map (\x -> (x, DLA_Var x)) csvs_
          consbody <- withC $ solCTail (CT_From at 0 (FI_Continue (ViewSave 0 []) csvs_m))
          let evtBody = solApply (solMsg_evt (0 :: Int)) []
          let evtEmit = "emit" <+> evtBody <> semi
          let evtDefn = "event" <+> evtBody <> semi
          let consbody' = vsep [evtEmit, cfDecl, dli', consbody]
          let cDefn = solFunctionLike SFL_Constructor [] "payable" consbody'
          return $ vsep [evtDefn, cfDefn, cDefn]
        DM_firstMsg ->
          return $ emptyDoc
    let map_defn (mpv, mi) = do
          keyTy <- solType_ T_Address
          let mt = dlmi_tym mi
          valTy <- solType mt
          let args = [solDecl "addr" keyTy]
          let ret = "internal view returns (" <> valTy <> " memory res)"
          let ref = (solArrayRef (solMapVar mpv) "addr")
          do_none <- solLargeArg' "res" $ DLLA_Data (dataTypeMap mt) "None" $ DLA_Literal DLL_Null
          let do_some = solSet "res" ref
          eq <- solEq (ref <> ".which") (solVariant valTy "Some")
          let body = solIf eq do_some do_none
          let ref_defn = solFunction (solMapRef mpv) args ret body
          return $
            vsep $
              [ "mapping (" <> keyTy <> " => " <> valTy <> ") " <> solMapVar mpv <> semi
              , ref_defn
              ]
    map_defns <- mapM map_defn (M.toList dli_maps)
    (view_json, view_defns) <-
      case mvi of
        Nothing -> return (mempty, [])
        Just (vs, vi) -> do
          let vst = ["uint256 current_view_i;", "bytes current_view_bs;"]
          let tgo :: SLPart -> (SLVar, IType) -> App ((T.Text, Aeson.Value), Doc)
              tgo v (k, t) = do
                let vk_ = bunpack v <> "_" <> k
                let vk = pretty $ vk_
                let (dom, rng) = itype2arr t
                let mkarg domt = DLVar at Nothing domt <$> allocVarIdx
                args <- mapM mkarg dom
                let mkargvm arg = (arg, solRawVar arg)
                extendVarMap $ M.fromList $ map mkargvm args
                let solType_p am ty = do
                      ty' <- solType_ ty
                      let loc = solArgLoc $ if mustBeMem ty then am else AM_Event
                      return $ ty' <> loc
                let mkdom arg argt = do
                      argt' <- solType_p AM_Call argt
                      return $ solDecl (solRawVar arg) argt'
                dom' <- zipWithM mkdom args dom
                rng' <- solType_p AM_Memory rng
                let ret = "external view returns" <+> parens rng'
                illegal <- flip (<>) semi <$> solRequire "invalid view_i" "false"
                let igo (i, ViewInfo vvs vim) = freshVarMap $ do
                      c' <- solEq "current_view_i" $ solNum i
                      let asnv = "vvs"
                      vvs_ty' <- solAsnType vvs
                      let de' = solSet (parens $ solDecl asnv (mayMemSol vvs_ty')) $ solApply "abi.decode" ["current_view_bs", parens vvs_ty']
                      extendVarMap $ M.fromList $ map (\vv -> (vv, asnv <> "." <> solRawVar vv)) $ vvs
                      (defn, ret') <-
                        case M.lookup k (fromMaybe mempty $ M.lookup v vim) of
                          Just eb -> do
                            eb' <- solEB args eb
                            mvars <- readMemVars
                            which <- allocVarIdx
                            (frameDefn, frameDecl) <- solFrame which mvars
                            return (frameDefn, vsep [frameDecl, eb'])
                          Nothing ->
                            return $ (emptyDoc, illegal)
                      return $ (defn, solWhen c' $ vsep [de', ret'])
                (defns, body') <- unzip <$> (mapM igo $ M.toAscList vi)
                let body'' = vsep $ body' <> [illegal]
                return $
                  (,) (s2t k, Aeson.String $ s2t vk_) $
                    vsep $ defns <> [solFunction vk dom' ret body'']
          let vgo (v, tm) = do
                (o_ks, bs) <- unzip <$> (mapM (tgo v) $ M.toAscList tm)
                return $ (,) (b2t v, Aeson.object o_ks) $ vsep bs
          (o_vs, vfns) <- unzip <$> (mapM vgo $ M.toAscList vs)
          return $ (,) o_vs $ "" : vst <> vfns
    let state_defn =
          vsep $
            ["uint256 current_state;"] <> map_defns <> view_defns
    hs' <- solHandlers hs
    let getm ctxt_f = (vsep . map snd . M.toAscList) <$> (liftIO $ readIORef ctxt_f)
    typedsp <- getm ctxt_typed
    typefsp <- getm ctxt_typef
    intsp <- getm ctxt_ints
    outputsp <- getm ctxt_outputs
    -- XXX expose this to Reach? only include if bills?
    let defp = "receive () external payable {}"
    let ctcbody = vsep $ [state_defn, consp, typefsp, outputsp, hs', defp]
    let ctcp = solContract "ReachContract is Stdlib" $ ctcbody
    let cinfo =
          HM.fromList $
            [ ("deployMode", Aeson.String $ T.pack $ show plo_deployMode)
            , ("views", Aeson.object view_json)
            ]
    let preamble =
          vsep
            [ "// Automatically generated with Reach" <+> (pretty versionStr)
            , "pragma abicoder v2" <> semi
            ]
    return $ (cinfo, vsep $ [preamble, solVersion, solStdLib, typedsp, intsp, ctcp])

data CompiledSolRec = CompiledSolRec
  { csrAbi :: T.Text
  , csrCode :: T.Text
  }

instance FromJSON CompiledSolRec where
  parseJSON = withObject "CompiledSolRec" $ \o -> do
    ctcs <- o .: "contracts"
    case find (":ReachContract" `T.isSuffixOf`) (HM.keys ctcs) of
      Just ctcKey -> do
        ctc <- ctcs .: ctcKey
        (abio :: Value) <- ctc .: "abi"
        -- Why are we re-encoding? ethers takes the ABI as a string, not an
        -- object.
        let cfg = defConfig {confIndent = Spaces 2, confCompare = compare}
        let abit = T.pack $ LB.unpack $ encodePretty' cfg abio
        codebodyt <- ctc .: "bin"
        return
          CompiledSolRec
            { csrAbi = abit
            , csrCode = codebodyt
            }
      Nothing ->
        impossible "Expected contracts object to have a key with suffix ':ReachContract'"

try_compile_sol :: FilePath -> Maybe (Maybe Int) -> IO (Either String (String, CompiledSolRec))
try_compile_sol solf opt = do
  let o = (<>) ["--optimize"]
  let (me, oargs) =
        case opt of
          Nothing -> ("o0", [])
          Just mo ->
            case mo of
              Nothing -> ("oD", o [])
              Just r -> ("o" <> show r, o ["--optimize-runs=" <> show r])
  let args = oargs <> ["--combined-json", "abi,bin", solf]
  -- putStrLn $ "solc " <> (show args)
  (ec, stdout, stderr) <- liftIO $ readProcessWithExitCode "solc" args []
  let show_output =
        case stdout == "" of
          True -> stderr
          False -> "STDOUT:\n" ++ stdout ++ "\nSTDERR:\n" ++ stderr
  case ec of
    ExitFailure _ ->
      return $ Left show_output
    ExitSuccess ->
      case eitherDecode (LB.pack stdout) of
        Left m -> return $ Left $ "It produced invalid JSON output, which failed to decode with the message:\n" <> m
        Right x -> return $ Right (me, x)

compile_sol :: ConnectorInfoMap -> FilePath -> IO ConnectorInfo
compile_sol cinfo solf = do
  let shortEnough (_, CompiledSolRec {..}) =
        case len <= (2 * maxContractLen) of
          True -> Nothing
          False -> Just len
        where
          len = T.length csrCode
  let try = try_compile_sol solf
  let merr = \case
        Left e -> emitWarning $ W_SolidityOptimizeFailure e
        Right _ -> return ()
  let desperate = \case
        Right x -> \_ _ -> return $ x
        e@(Left bado) -> \case
          Right y -> \_ -> merr e >> return y
          Left _ -> \case
            Right z -> merr e >> return z
            Left _ ->
              impossible $ "The Solidity compiler failed with the message:\n" <> bado
  let tryN eA e1 =
        try Nothing >>= \case
          Right oN ->
            case shortEnough oN of
              Nothing -> merr eA >> return oN
              Just _lenN -> desperate eA e1 (Right oN)
          Left rN -> desperate eA e1 (Left rN)
  let try1 eA =
        try (Just $ Just 1) >>= \case
          Right o1 ->
            case shortEnough o1 of
              Nothing -> merr eA >> return o1
              Just _len1 -> tryN eA (Right o1)
          Left r1 -> tryN eA (Left r1)
  let tryA =
        try (Just Nothing) >>= \case
          Right oA ->
            case shortEnough oA of
              Nothing -> return $ oA
              Just _lenA -> try1 $ Right oA
          Left rA -> try1 $ Left rA
  (which, CompiledSolRec {..}) <- tryA
  return $ Aeson.Object $ HM.union cinfo $
    HM.fromList $ [ ("ABI", Aeson.String csrAbi)
                  , ("Bytecode", Aeson.String $ "0x" <> csrCode)
                  , ("Which", Aeson.String $ T.pack which)
                  , ("BytecodeLen", Aeson.Number $ (fromIntegral $ T.length csrCode) / 2)
                  ]

connect_eth :: Connector
connect_eth = Connector {..}
  where
    conName = "ETH"
    conCons DLC_UInt_max = DLL_Int sb $ 2 ^ (256 :: Integer) - 1
    conGen moutn pl = case moutn of
      Just outn -> go (outn "sol")
      Nothing -> withSystemTempDirectory "reachc-sol" $ \dir ->
        go (dir </> "compiled.sol")
      where
        go :: FilePath -> IO ConnectorInfo
        go solf = do
          ig <- colorProgram pl
          conShowP moutn "sol.colors" ig
          (cinfo, sol) <- solPLProg pl
          unless dontWriteSol $ do
            LTIO.writeFile solf $ render sol
          compile_sol cinfo solf
