module Reach.Connector.ETH_Solidity (connect_eth) where

-- https://github.com/reach-sh/reach-lang/blob/8d912e0/hs/src/Reach/Connector/ETH_EVM.hs.dead

import Control.Monad
import Control.Monad.Reader
import Data.Aeson as Aeson
import Data.Aeson.Encode.Pretty
import Data.Aeson.Text
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Data.List (intersperse)
import Data.Maybe (fromMaybe, fromJust)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LTIO
import Reach.AST.Base
import Reach.AST.DLBase
import Reach.AST.PL
import Reach.AddCounts
import Reach.Counter
import Reach.Connector
import Reach.EmbeddedFiles
import Reach.Texty
import Reach.UnsafeUtil
import Reach.Util
import Reach.Version
import System.Exit
import System.FilePath
import System.IO.Temp
import System.Process

--- Debugging tools

--- You can turn this to True and manually change the Solidity file
dontWriteSol :: Bool
dontWriteSol = False

includeRequireMsg :: Bool
includeRequireMsg = False

maxDepth :: Int
maxDepth = 15

--- Solidity helpers

sb :: SrcLoc
sb = srcloc_builtin

solString :: String -> Doc
solString s = squotes $ pretty s

solNum :: Show n => n -> Doc
solNum i = pretty $ "uint256(" ++ show i ++ ")"

solBraces :: Doc -> Doc
solBraces body = braces (nest 2 $ hardline <> body <> space)

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
solVersion = "pragma solidity ^0.8.0;"

solStdLib :: Doc
solStdLib = pretty $ B.unpack stdlib_sol

solApply :: Doc -> [Doc] -> Doc
solApply f args = f <> parens (hcat $ intersperse (comma <> space) args)

--- FIXME these add size to the contract without much payoff. A better
--- thing would be to encode a number and then emit a dictionary of
--- what the error codes mean that would be used by our connector
--- stdlib.
solRequire :: String -> Doc -> Doc
solRequire umsg a = solApply "require" $ [a] <> mmsg
  where
    smsg = unsafeRedactAbsStr umsg
    mmsg =
      case includeRequireMsg of
        True -> [solString smsg]
        False -> []

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

solHash :: [Doc] -> Doc
solHash a = solApply "uint256" [solApply "keccak256" [solApply "abi.encode" a]]

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

-- Note: This is really ugly, but not so so bad
data DLType_hc
  = T_OrderedObject [(SLVar, SolDLType)]
  deriving (Eq, Ord)
data SolDLType
  = SDLT_hc DLType_hc
  | SDLT DLType
  deriving (Eq, Ord)

vsToType :: [DLVar] -> DLType_hc
vsToType vs = T_OrderedObject $ map go_ty vs
  where go_ty v = (show (solRawVar v), SDLT $ varType v)

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
  , ctxt_typem_hc :: IORef (M.Map DLType_hc Doc)
  , ctxt_typed :: IORef (M.Map Int Doc)
  , ctxt_typeidx :: Counter
  , ctxt_plo :: PLOpts
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

extendVarMap :: VarMap -> App ()
extendVarMap vm1 = do
  varmr <- ctxt_varm <$> ask
  liftIO $ modifyIORef varmr $ (<>) vm1

dupeIORef :: IORef a -> IO (IORef a)
dupeIORef r = newIORef =<< readIORef r

freshVarMap :: App a -> App a
freshVarMap m = do
  let d f = liftIO . dupeIORef . f =<< ask
  varmr' <- d ctxt_varm
  mvars' <- d ctxt_mvars
  depths' <- d ctxt_depths
  local (\e -> e { ctxt_varm = varmr'
                 , ctxt_mvars = mvars'
                 , ctxt_depths = depths' }) m

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
recordDepth v d = do
  depths <- ctxt_depths <$> ask
  liftIO $ modifyIORef depths $ M.insert v d

readDepth :: DLVar -> App Int
readDepth v = do
  mvars <- readCtxtIO ctxt_depths
  return $ fromMaybe 0 $ M.lookup v mvars

class DepthOf a where
  depthOf :: a -> App Int

instance (Traversable t, DepthOf a) => DepthOf (t a) where
  depthOf o = maximum <$> mapM depthOf o

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
    DLE_Transfer _ x y -> depthOf [x, y]
    DLE_Wait _ x -> depthOf x
    DLE_PartSet _ _ x -> depthOf x
    DLE_MapRef _ _ x -> add1 $ depthOf x
    DLE_MapSet _ _ x y -> depthOf [x, y]
    DLE_MapDel _ _ x -> depthOf x
    where
      add1 m = (+) 1 <$> m

solVar :: AppT DLVar
solVar v = do
  varm <- readCtxtIO ctxt_varm
  case M.lookup v varm of
    Just x -> return $ x
    Nothing -> impossible $ "unbound var " ++ show v

mkSolType :: (Ord a) => (a -> App ()) -> (SolCtxt -> IORef (M.Map a Doc)) -> AppT a
mkSolType ensure f t = do
  ensure t
  (fromJust . M.lookup t) <$> readCtxtIO f

solType_hc :: AppT DLType_hc
solType_hc = mkSolType ensureTypeDefined_hc ctxt_typem_hc
solType :: AppT DLType
solType = mkSolType ensureTypeDefined ctxt_typem

solTypeI :: DLType -> App Int
solTypeI t = do
  ensureTypeDefined t
  (fromJust . M.lookup t) <$> readCtxtIO ctxt_typei

mustBeMem :: DLType -> Bool
mustBeMem = \case
  T_Null -> False
  T_Bool -> False
  T_UInt -> False
  T_Bytes _ -> True
  T_Digest -> False
  T_Address -> False
  T_Array {} -> True
  T_Tuple {} -> True
  T_Object {} -> True
  T_Data {} -> True

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
  DLL_Bytes s -> dquotes $ pretty $ B.unpack s

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
    DLLA_Obj m -> c <$> (mapM go $ M.toAscList m)
      where
        go (k, a) = one ("." <> pretty k) <$> solArg a
    DLLA_Data _ vn vv -> do
      t <- solType $ largeArgTypeOf la
      vv' <- solArg vv
      return $ c
        [ one ".which" (solVariant t vn)
        , one ("._" <> pretty vn) vv'
        ]
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
    return $ oe' <> "." <> pretty f <> sp
  DLE_Interact {} -> impossible "consensus interact"
  DLE_Digest _ args -> do
    args' <- mapM solArg args
    return $ (solHash $ args') <> sp
  DLE_Transfer _ who amt ->
    spa $ solTransfer who amt
  DLE_Claim at fs ct a mmsg -> spa check
    where
      check = case ct of
        CT_Assert -> impossible "assert"
        CT_Assume -> require
        CT_Require -> require
        CT_Possible -> impossible "possible"
        CT_Unknowable {} -> impossible "unknowable"
      require = solRequire (show (at, fs, mmsg)) <$> solArg a
  DLE_Wait {} -> return emptyDoc
  DLE_PartSet _ _ a -> spa $ solArg a
  DLE_MapRef _ mpv fa -> do
    fa' <- solArg fa
    return $ solApply (solMapRef mpv) [fa'] <> sp
  DLE_MapSet _ mpv fa na -> do
    fa' <- solArg fa
    solLargeArg' (solArrayRef (solMapVar mpv) fa') nla
    where
      nla = DLLA_Data (dataTypeMap $ maybeT na_t) "Some" na
      na_t = argTypeOf na
  DLE_MapDel _ mpv fa -> do
    fa' <- solArg fa
    return $ "delete" <+> solArrayRef (solMapVar mpv) fa' <> sp
  where
    spa m = (<> sp) <$> m

solTransfer :: DLArg -> DLArg -> App Doc
solTransfer who amt = do
  who' <- solArg who
  amt' <- solArg amt
  return $ who' <> "." <> solApply "transfer" [amt']

solEvent :: Int -> [DLVar] -> [DLVar] -> App Doc
solEvent which svs msg = do
  arg_ty' <- solArgType svs msg
  return $ "event" <+> solApply (solMsg_evt which) [arg_ty' <+> "_a"] <> semi

solEventEmit :: Int -> Doc
solEventEmit which =
  "emit" <+> solApply (solMsg_evt which) ["_a"] <> semi

solHashStateSet :: [(DLVar, DLArg)] -> App ([Doc], Doc)
solHashStateSet svs = do
  which <- ctxt_handler_num <$> ask
  let sete = solHash [(solNum which), "nsvs"]
  let go (v, a) = solSet ("nsvs." <> solRawVar v) <$> solArg a
  svs' <- mapM go svs
  let svs_ty = vsToType $ map fst svs
  svs_ty' <- solType_hc svs_ty
  let setl = [solDecl "nsvs" (svs_ty' <> " memory") <> semi] <> svs'
  return $ (setl, sete)

solHashStateCheck :: Int -> Doc
solHashStateCheck prev =
  solHash [(solNum prev), "_a.svs"]

arraySize :: DLArg -> Integer
arraySize a =
  case argTypeOf a of
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

solCom :: AppT PLCommon
solCom = \case
  DL_Nop _ -> mempty
  DL_Let _ (PV_Let _ dv) (DLE_LArg _ la) -> do
    addMemVar dv
    solLargeArg dv la
  DL_Let _ (PV_Let _ dv) (DLE_ArrayConcat _ x y) -> do
    addMemVar dv
    dv' <- solVar dv
    let copy src (off :: Integer) = do
          let sz = arraySize src
          src' <- solArg src
          add <- solPrimApply ADD ["i", solNum off]
          return $ "for" <+> parens ("uint256 i = 0" <> semi <+> "i <" <+> (pretty sz) <> semi <+> "i++") <> solBraces (solArrayRef dv' add <+> "=" <+> solArrayRef src' "i" <> semi)
    x' <- copy x 0
    y' <- copy y (arraySize x)
    return $ vsep [x', y']
  DL_Let _ (PV_Let _ dv@(DLVar _ _ t _)) (DLE_ArrayZip _ x y) -> do
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
  DL_Let _ (PV_Let pu dv) de ->
    case simple de of
      True -> no_def
      False ->
        case pu of
          PL_Once -> no_def
          PL_Many -> def
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
  DL_Let _ PV_Eff de -> solExpr semi de
  DL_Var _ dv -> do
    addMemVar dv
    mempty
  DL_Set _ dv da -> solSet (solMemVar dv) <$> solArg da
  DL_LocalIf _ ca t f ->
    solIf <$> solArg ca <*> solPLTail t <*> solPLTail f
  DL_LocalSwitch at ov csm -> solSwitch solPLTail at ov csm
  DL_ArrayMap _ ans x a (DLinBlock _ _ f r) -> do
    addMemVars $ [ans, a]
    let sz = arraySize x
    ans' <- solVar ans
    x' <- solArg x
    a' <- solVar a
    f' <- solPLTail f
    r' <- solArg r
    return $ vsep
          [ "for" <+> parens ("uint256 i = 0" <> semi <+> "i <" <+> (pretty sz) <> semi <+> "i++")
              <> solBraces
                (vsep
                   [ a' <+> "=" <+> (solArrayRef x' "i") <> semi
                   , f'
                   , (solArrayRef ans' "i") <+> "=" <+> r' <> semi
                   ])
          ]
  DL_ArrayReduce _ ans x z b a (DLinBlock _ _ f r) -> do
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

solCom_ :: AppT a -> PLCommon -> AppT a
solCom_ iter m k = do
  m' <- solCom m
  k' <- iter k
  return $
    case m' == emptyDoc of
      True -> k'
      False -> m' <> hardline <> k'

solPLTail :: AppT PLTail
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
    return $ vsep $
        [ emit'
        , argDefn <> semi
        ] <> svs' <> asn' <> [solApply (solLoop_fun which) ["la"] <> semi]
  CT_From _ _XXX_which (Just svs) -> do
    (setl, sete) <- solHashStateSet svs
    emit' <- ctxt_emit <$> ask
    return $ vsep $ [emit'] <> setl <> [solSet ("current_state") sete]
  CT_From _ _ Nothing -> do
    emit' <- ctxt_emit <$> ask
    return $
      vsep
        [ emit'
        , solSet ("current_state") ("0x0")
        , solApply "selfdestruct" ["payable(msg.sender)"] <> semi
        ]

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
  ct' <- local (\e -> e { ctxt_emit = emitp
                        , ctxt_handler_num = which }) $
            solCTail ct
  mvars <- readMemVars
  (frameDefn, frameDecl) <- solFrame which mvars
  return (frameDefn, frameDecl, ct')

solArgType :: [DLVar] -> [DLVar] -> App Doc
solArgType svs msg = do
  let svs_ty = SDLT_hc $ vsToType svs
  let msg_ty = SDLT_hc $ vsToType msg
  let arg_ty = T_OrderedObject $ [ ("svs", svs_ty), ("msg", msg_ty) ]
  solType_hc arg_ty

solArgDefn_ :: Doc -> ArgMode -> [DLVar] -> [DLVar] -> App Doc
solArgDefn_ name am svs msg = do
  arg_ty' <- solArgType svs msg
  return $ solDecl name (arg_ty' <> solArgLoc am)

solArgDefn :: ArgMode -> [DLVar] -> [DLVar] -> App Doc
solArgDefn = solArgDefn_ "_a"

solHandler :: Int -> CHandler -> App Doc
solHandler which h = freshVarMap $
  case h of
    C_Handler at interval last_timemv from prev svs msg amtv timev ct -> do
      let fromm = M.singleton from "payable(msg.sender)"
      let given_mm = M.fromList [(amtv, "msg.value"), (timev, solBlockNumber)]
      let checkMsg s = s <> " check at " <> show at
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
            let hcp = (solRequire (checkMsg "state") $ eq') <> semi
            return (hcp, AM_Call, SFL_Function True (solMsg_fun which))
      argDefn <- solArgDefn am svs msg
      timeoutCheck <-
          case last_timemv of
            Nothing -> return emptyDoc
            Just last_timev -> do
              last_timev' <- solVar last_timev
              let check sign mv =
                    case mv of
                      [] -> return "true"
                      mvs -> do
                        mvs' <- mapM solArg mvs
                        let go_sum x y = solPrimApply ADD [x, y]
                        sum' <- foldlM go_sum last_timev' mvs'
                        solPrimApply (if sign then PGE else PLT) [timev', sum']
              let CBetween ifrom ito = interval
              int_fromp <- check True ifrom
              int_top <- check False ito
              return $ solRequire (checkMsg "timeout") (solBinOp "&&" int_fromp int_top) <> semi
      let body = vsep [ hashCheck, frameDecl, timeoutCheck, ctp ]
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

solDefineType_hc :: DLType_hc -> App ()
solDefineType_hc = void . solDefineType_hc_

solDefineType_hc_ :: DLType_hc -> App Doc
solDefineType_hc_ t = case t of
  T_OrderedObject ats -> do
    atsn <- mapM (\(k,v) -> (,) (pretty k) <$> solType_ v) ats
    (name, i) <- addName
    let sp = solStruct name atsn
    case sp of
      -- XXX This really stinks
      Nothing -> addMap "bool"
      Just x -> do
        addDef i x
        return name
  where
    solType_ = \case
      SDLT_hc x -> solType_hc x
      SDLT x -> solType x
    addMap d = do
      modifyCtxtIO ctxt_typem_hc $ M.insert t d
      return d
    addId = (liftIO . incCounter) =<< (ctxt_typeidx <$> ask)
    addName = do
      i <- addId
      let name = "T" <> pretty i
      void $ addMap name
      return $ (name, i)
    addDef i d = modifyCtxtIO ctxt_typed $ M.insert i d

solDefineType :: DLType -> App ()
solDefineType t = case t of
  T_Null -> base
  T_Bool -> base
  T_UInt -> base
  T_Bytes sz -> do
    addMap $ "uint8[" <> pretty sz <> "]"
  T_Digest -> base
  T_Address -> base
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
    let ret = "internal" <+> "returns" <+> parens (solDecl "arrp" memem)
    let assign idx val = (solArrayRef "arrp" idx) <+> "=" <+> val <> semi
    let body =
          vsep
            [ ("for" <+> parens ("uint256 i = 0" <> semi <+> "i <" <+> (pretty sz) <> semi <+> "i++")
                 <> solBraces (assign "i" (solArrayRef "arr" "i")))
            , assign "idx" "val"
            ]
    addMap me
    i <- addId
    addDef i $ solFunction (solArraySet i) args ret body
  T_Tuple ats -> do
    let ats' = map SDLT ats
    let ats'' = (flip zip) ats' $ map (("elem" ++) . show) ([0 ..] :: [Int])
    name <- solDefineType_hc_ $ T_OrderedObject ats''
    addMap name
  T_Object tm -> do
    let tm' = map (\(k, v) -> (k, SDLT v)) $ M.toAscList tm
    name <- solDefineType_hc_ $ T_OrderedObject tm'
    addMap name
  T_Data tm -> do
    tmn <- mapM solType tm
    --- XXX Try to use bytes and abi.decode; Why not right away? The
    --- length of the bytes would not be predictable, which means the
    --- gas cost would be arbitrary.
    (name, i) <- addName
    let enumn = "_enum_" <> name
    let enump = solEnum enumn $ map (pretty . fst) $ M.toAscList tmn
    let structp = fromJust $ solStruct name $ ("which", enumn) : map (\(k, kt) -> (pretty ("_" <> k), kt)) (M.toAscList tmn)
    addDef i $ vsep [enump, structp]
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

mkEnsureTypeDefined :: (Ord a) => (a -> App ()) -> (SolCtxt -> IORef (M.Map a b)) -> a -> App ()
mkEnsureTypeDefined define f t =
  (M.lookup t <$> readCtxtIO f) >>= \case
    Nothing -> define t
    Just _ -> return ()

ensureTypeDefined :: DLType -> App ()
ensureTypeDefined = mkEnsureTypeDefined solDefineType ctxt_typem

ensureTypeDefined_hc :: DLType_hc -> App ()
ensureTypeDefined_hc = mkEnsureTypeDefined solDefineType_hc ctxt_typem_hc

solPLProg :: PLProg -> IO (ConnectorInfoMap, Doc)
solPLProg (PLProg _ plo@(PLOpts {..}) dli _ (CPProg at hs)) = do
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
          , (T_Address, "address payable")
          ]
  ctxt_typem <- newIORef base_typem
  ctxt_typem_hc <- newIORef mempty
  ctxt_typed <- newIORef mempty
  ctxt_typeidx <- newCounter 0
  let ctxt_plo = plo
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
                        , (\ m -> do
                             extendVarMap $ M.singleton v (solMemVar v)
                             m)
                        )
              let dli' = vsep $ ctimem'
              (cfDefn, cfDecl) <- withC $ solFrame 0 (S.fromList csvs_)
              let csvs_m = map (\x -> (x, DLA_Var x)) csvs_
              consbody <- withC $ solCTail (CT_From at 0 (Just csvs_m))
              let evtBody = solApply (solMsg_evt (0::Int)) []
              let evtEmit = "emit" <+> evtBody <> semi
              let evtDefn = "event" <+> evtBody <> semi
              let consbody' = vsep [ evtEmit, cfDecl, dli', consbody ]
              let cDefn = solFunctionLike SFL_Constructor [] "payable" consbody'
              return $ vsep [ evtDefn, cfDefn, cDefn ]
            DM_firstMsg ->
              return $ emptyDoc
    let map_defn (mpv, DLMapInfo {..}) = do
          let keyTy = "address"
          let mt = maybeT dlmi_ty
          valTy <- solType mt
          let args = [solDecl "addr" keyTy]
          let ret = "internal returns (" <> valTy <> " memory res)"
          let ref = (solArrayRef (solMapVar mpv) "addr")
          do_none <- solLargeArg' "res" $ DLLA_Data (dataTypeMap mt) "None" $ DLA_Literal DLL_Null
          let do_some = solSet "res" ref
          eq <- solEq (ref <> ".which") (solVariant valTy "Some")
          let body = solIf eq do_some do_none
          let ref_defn = solFunction (solMapRef mpv) args ret body
          return $ vsep $
            [ "mapping (" <> keyTy <> " => " <> valTy <> ") " <> solMapVar mpv <> semi
            , ref_defn
            ]
    map_defns <- mapM map_defn (M.toList dli_maps)
    let state_defn = vsep $ ["uint256 current_state;"] <> map_defns
    hs' <- solHandlers hs
    typesp <- (vsep . map snd . M.toAscList) <$> (liftIO $ readIORef ctxt_typed)
    let ctcbody = vsep $ [ state_defn, typesp, consp, hs' ]
    let ctcp = solContract "ReachContract is Stdlib" $ ctcbody
    let cinfo = HM.fromList [("deployMode", Aeson.String $ T.pack $ show plo_deployMode)]
    let preamble =
          vsep
            [ "// Automatically generated with Reach" <+> (pretty versionStr)
            , "pragma abicoder v2" <> semi
            ]
    return $ (cinfo, vsep $ [preamble, solVersion, solStdLib, ctcp])

data CompiledSolRec = CompiledSolRec
  { csrAbi :: T.Text
  , csrCode :: T.Text
  , csrOpcodes :: T.Text
  }

instance FromJSON CompiledSolRec where
  parseJSON = withObject "CompiledSolRec" $ \o -> do
    ctcs <- o .: "contracts"
    case find (":ReachContract" `T.isSuffixOf`) (HM.keys ctcs) of
      Just ctcKey -> do
        ctc <- ctcs .: ctcKey
        -- FIXME change ETH.ts so we don't serialize this, since we probably
        -- just un-serialize it on that end
        (abio :: Value) <- ctc .: "abi"
        let abit = LT.toStrict $ encodeToLazyText abio
        codebodyt <- ctc .: "bin"
        opcodest <- ctc .: "opcodes"
        return
          CompiledSolRec
            { csrAbi = abit
            , csrCode = codebodyt
            , csrOpcodes = opcodest
            }
      Nothing ->
        fail "Expected contracts object to have a key with suffix ':ReachContract'"

extract :: ConnectorInfoMap -> Value -> Either String ConnectorInfo
extract cinfo v = case fromJSON v of
  Error e -> Left e
  Success CompiledSolRec {..} ->
    case eitherDecode (LB.pack (T.unpack csrAbi)) of
      Left e -> Left e
      Right (csrAbi_parsed :: Value) ->
        Right $
          Aeson.Object $
            HM.union
              (HM.fromList
                 [ ("ABI", Aeson.String csrAbi_pretty)
                 , --- , ("Opcodes", T.unlines $ "" : (T.words $ csrOpcodes))
                   ("Bytecode", Aeson.String $ "0x" <> csrCode)
                 ])
              cinfo
        where
          csrAbi_pretty = T.pack $ LB.unpack $ encodePretty' cfg csrAbi_parsed
          cfg = defConfig {confIndent = Spaces 2, confCompare = compare}

compile_sol :: ConnectorInfoMap -> FilePath -> IO ConnectorInfo
compile_sol cinfo solf = do
  (ec, stdout, stderr) <-
    readProcessWithExitCode "solc" ["--optimize", "--combined-json", "abi,bin,opcodes", solf] []
  let show_output = "STDOUT:\n" ++ stdout ++ "\nSTDERR:\n" ++ stderr ++ "\n"
  case ec of
    ExitFailure _ -> die $ "solc failed:\n" ++ show_output
    ExitSuccess ->
      case (eitherDecode $ LB.pack stdout) of
        Right v ->
          case extract cinfo v of
            Right cr -> return cr
            Left err ->
              die $
                "failed to extract valid output from solc:\n" ++ show_output
                  ++ "Decode:\n"
                  ++ err
                  ++ "\n"
        Left err ->
          die $
            "solc failed to produce valid output:\n" ++ show_output
              ++ "Decode:\n"
              ++ err
              ++ "\n"

connect_eth :: Connector
connect_eth = Connector {..}
  where
    conName = "ETH"
    conCons DLC_UInt_max = DLL_Int sb $ 2 ^ (256 :: Integer) - 1
    conGen moutn pil = case moutn of
      Just outn -> go (outn "sol")
      Nothing -> withSystemTempDirectory "reachc-sol" $ \dir ->
        go (dir </> "compiled.sol")
      where
        go :: FilePath -> IO ConnectorInfo
        go solf = do
          pl <- add_counts pil
          conShowP moutn "sol.pl" pl
          (cinfo, sol) <- solPLProg pl
          unless dontWriteSol $ do
            LTIO.writeFile solf $ render sol
          compile_sol cinfo solf
