module Reach.Connector.ETH_Solidity (connect_eth) where

import Control.Monad
import Control.Monad.ST
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.HashMap.Strict as HM
import Data.List (find, foldl', intersperse)
import Data.List.Extra (mconcatMap)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.STRef
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import Reach.AST
import Reach.CollectTypes
import Reach.Connector
import Reach.EmbeddedFiles
import Reach.STCounter
import Reach.Type
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

--- Pretty helpers
vsep_with_blank :: [Doc a] -> Doc a
vsep_with_blank l = vsep $ intersperse emptyDoc l

--- Solidity helpers

solString :: String -> Doc a
solString s = squotes $ pretty s

solNum :: Show n => n -> Doc a
solNum i = pretty $ "uint256(" ++ show i ++ ")"

solBraces :: Doc a -> Doc a
solBraces body = braces (nest 2 $ hardline <> body <> space)

data SolFunctionLike a
  = SFL_Constructor
  | SFL_Function Bool (Doc a)

solFunctionLike :: SolFunctionLike a -> [Doc a] -> Doc a -> Doc a -> Doc a
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

solFunction :: Doc a -> [Doc a] -> Doc a -> Doc a -> Doc a
solFunction name =
  solFunctionLike (SFL_Function False name)

solContract :: String -> Doc a -> Doc a
solContract s body = "contract" <+> pretty s <+> solBraces body

solVersion :: Doc a
solVersion = "pragma solidity ^0.7.1;"

solStdLib :: Doc a
solStdLib = pretty $ B.unpack stdlib_sol

solApply :: Doc a -> [Doc a] -> Doc a
solApply f args = f <> parens (hcat $ intersperse (comma <> space) args)

--- XXX these add size to the contract without much payoff. A better
--- thing would be to encode a number and then emit a dictionary of
--- what the error codes mean that would be used by our connector
--- stdlib.
solRequire :: String -> Doc a -> Doc a
solRequire umsg a = solApply "require" $ [a] <> mmsg
  where
    smsg = unsafeRedactAbsStr umsg
    mmsg =
      case includeRequireMsg of
        True -> [solString smsg]
        False -> []

solBinOp :: String -> Doc a -> Doc a -> Doc a
solBinOp o l r = l <+> pretty o <+> r

solEq :: Doc a -> Doc a -> Doc a
solEq = solBinOp "=="

solAnd :: Doc a -> Doc a -> Doc a
solAnd = solBinOp "&&"

solBytesLength :: Doc a -> Doc a
solBytesLength x = "bytes(" <> x <> ").length"

solSet :: Doc a -> Doc a -> Doc a
solSet x y = solBinOp "=" x y <> semi

solIf :: Doc a -> Doc a -> Doc a -> Doc a
solIf c t f = "if" <+> parens c <+> solBraces t <> hardline <> "else" <+> solBraces f

--- FIXME don't nest
solIfs :: [(Doc a, Doc a)] -> Doc a
solIfs [] = emptyDoc
solIfs ((c, t) : more) = solIf c t $ solIfs more

solDecl :: Doc a -> Doc a -> Doc a
solDecl n ty = ty <+> n

solStruct :: Doc a -> [(Doc a, Doc a)] -> Doc a
solStruct name fields = "struct" <+> name <+> solBraces (vsep $ map (<> semi) $ map (uncurry solDecl) fields)

solEnum :: Doc a -> [Doc a] -> Doc a
solEnum name opts = "enum" <+> name <+> braces (hcat $ intersperse (comma <> space) opts)

--- Runtime helpers

solMsg_evt :: Pretty i => i -> Doc a
solMsg_evt i = "e" <> pretty i

solMsg_arg :: Pretty i => i -> Doc a
solMsg_arg i = "a" <> pretty i

solMsg_fun :: Pretty i => i -> Doc a
solMsg_fun i = "m" <> pretty i

solLoop_fun :: Pretty i => i -> Doc a
solLoop_fun i = "l" <> pretty i

solLastBlockDef :: Doc a
solLastBlockDef = "_last"

solLastBlock :: Doc a
solLastBlock = "_a." <> solLastBlockDef

solBlockNumber :: Doc a
solBlockNumber = "uint256(block.number)"

solHash :: [Doc a] -> Doc a
solHash a = solApply "uint256" [solApply "keccak256" [solApply "abi.encode" a]]

solArraySet :: Int -> Doc a
solArraySet i = "array_set" <> pretty i

solArrayRef :: Doc a -> Doc a -> Doc a
solArrayRef arr idx = arr <> brackets idx

solArrayLit :: [Doc a] -> Doc a
solArrayLit xs = brackets $ hcat $ intersperse (comma <> space) xs

solVariant :: Doc a -> SLVar -> Doc a
solVariant t vn = "_enum_" <> t <> "." <> pretty vn

--- Compiler

type VarMap a = M.Map DLVar (Doc a)

data SolCtxt a = SolCtxt
  { ctxt_handler_num :: Int
  , ctxt_varm :: VarMap a
  , ctxt_emit :: Doc a
  , ctxt_typei :: M.Map SLType Int
  , ctxt_typem :: M.Map SLType (Doc a)
  , ctxt_deployMode :: DeployMode
  }

instance Semigroup (SolCtxt a) where
  --- FIXME maybe merge the maps?
  _ <> x = x

solRawVar :: DLVar -> Doc a
solRawVar (DLVar _ _ _ n) = pretty $ "v" ++ show n

solMemVar :: DLVar -> Doc a
solMemVar dv = "_f." <> solRawVar dv

solArgVar :: DLVar -> Doc a
solArgVar dv = "_a." <> solRawVar dv

solVar :: SolCtxt a -> DLVar -> Doc a
solVar ctxt v =
  case M.lookup v (ctxt_varm ctxt) of
    Just x -> x
    Nothing -> impossible $ "unbound var " ++ show v

solType :: SolCtxt a -> SLType -> Doc a
solType ctxt t =
  case M.lookup t $ ctxt_typem ctxt of
    Nothing -> impossible "cannot map sol type"
    Just x -> x

solTypeI :: SolCtxt a -> SLType -> Int
solTypeI ctxt t =
  case M.lookup t $ ctxt_typei ctxt of
    Nothing -> impossible "cannot map sol type"
    Just x -> x

mustBeMem :: SLType -> Bool
mustBeMem = \case
  T_Null -> False
  T_Bool -> False
  T_UInt256 -> False
  T_Bytes -> True
  T_Address -> False
  T_Fun {} -> impossible "fun"
  T_Array {} -> True
  T_Tuple {} -> True
  T_Object {} -> True
  T_Data {} -> True
  T_Forall {} -> impossible "forall"
  T_Var {} -> impossible "var"
  T_Type {} -> impossible "type"

data ArgMode
  = AM_Call
  | AM_Memory
  | AM_Event

solArgLoc :: ArgMode -> Doc a
solArgLoc = \case
  AM_Call -> " calldata"
  AM_Memory -> " memory"
  AM_Event -> ""

solArgType :: SolCtxt a -> ArgMode -> SLType -> Doc a
solArgType ctxt am t = solType ctxt t <> loc_spec
  where
    loc_spec = if mustBeMem t then solArgLoc am else ""

solArgDecl :: SolCtxt a -> ArgMode -> DLVar -> Doc a
solArgDecl ctxt am dv@(DLVar _ _ t _) = solDecl (solRawVar dv) (solArgType ctxt am t)

solCon :: DLConstant -> Doc a
solCon = \case
  DLC_Null -> "true"
  DLC_Bool True -> "true"
  DLC_Bool False -> "false"
  DLC_Int i -> solNum i
  DLC_Bytes s -> dquotes $ pretty $ B.unpack s

solArg :: SolCtxt a -> DLArg -> Doc a
solArg ctxt da =
  case da of
    DLA_Var v -> solVar ctxt v
    DLA_Con c -> solCon c
    DLA_Array _ as -> brackets $ hsep $ punctuate comma $ map (solArg ctxt) as
    DLA_Tuple as -> con $ map (solArg ctxt) as
    DLA_Obj m -> con $ map ((solArg ctxt) . snd) $ M.toAscList m
    DLA_Data tm vn vv -> con $ (solVariant t vn) : (map (\(vn', ty) -> if vn == vn' then solArg ctxt vv else defaultVal ty) $ M.toAscList tm)
    DLA_Interact {} -> impossible "consensus interact"
  where
    t = solType ctxt (argTypeOf da)
    con = solApply t
    defaultVal = \case
      T_UInt256 -> "0"
      T_Bool -> "false"
      T_Null -> "false"
      T_Bytes -> "\"\"" -- XXX is this valid?
      T_Address -> "0x" <> pretty (replicate 64 '0')
      T_Fun {} -> impossible "defaultVal for Fun"
      T_Array ty n -> solArrayLit $ replicate (fromInteger n) $ defaultVal ty
      T_Tuple tys -> solArrayLit $ map defaultVal tys
      T_Object _tyMap {- (M.Map SLVar SLType) -} ->
        error $ "XXX defaultVal not yet implemented for Object"
      T_Data _variantMap {- (M.Map SLVar SLType) -} ->
        error $ "XXX defaultVal not yet implemented for Data"
      T_Forall {} -> impossible "defaultVal for Forall"
      T_Var {} -> impossible "defaultVal for Var"
      T_Type {} -> impossible "defaultVal for Type"

solPrimApply :: PrimOp -> [Doc a] -> Doc a
solPrimApply = \case
  ADD -> binOp "+"
  SUB -> binOp "-"
  MUL -> binOp "*"
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
    [c, t, f] -> c <+> "?" <+> t <+> ":" <+> f
    _ -> impossible $ "emitSol: ITE wrong args"
  BYTES_EQ -> \case
    [x, y] ->
      solAnd
        (solEq (solBytesLength x) (solBytesLength y))
        (solEq (solHash [x]) (solHash [y]))
    _ -> impossible $ "emitSol: BYTES_EQ wrong args"
  BALANCE -> \_ -> "address(this).balance"
  TXN_VALUE -> \_ -> "msg.value"
  where
    binOp op = \case
      [l, r] -> solBinOp op l r
      _ -> impossible $ "emitSol: bin op args"

solExpr :: SolCtxt a -> Doc a -> DLExpr -> Doc a
solExpr ctxt sp = \case
  DLE_Arg _ a -> solArg ctxt a <> sp
  DLE_Impossible at msg -> expect_throw at msg
  DLE_PrimOp _ p args ->
    (solPrimApply p $ map (solArg ctxt) args) <> sp
  DLE_ArrayRef _ _ ae _ ie ->
    solArrayRef (solArg ctxt ae) (solArg ctxt ie) <> sp
  DLE_ArraySet _ _ ae _ ie ve ->
    (solApply (solArraySet (solTypeI ctxt (argTypeOf ae))) $ map (solArg ctxt) [ae, ie, ve]) <> sp
  DLE_ArrayConcat {} ->
    impossible "array concat"
  DLE_ArrayZip {} ->
    impossible "array zip"
  DLE_TupleRef _ ae i ->
    (solArg ctxt ae) <> ".elem" <> pretty i <> sp
  DLE_ObjectRef _ oe f ->
    (solArg ctxt oe) <> "." <> pretty f <> sp
  DLE_Interact {} -> impossible "consensus interact"
  DLE_Digest _ args ->
    (solHash $ map (solArg ctxt) args) <> sp
  DLE_Transfer _ _ who amt ->
    solTransfer ctxt who amt <> sp
  DLE_Claim at _ ct a -> check <> sp
    where
      check = case ct of
        CT_Assert -> impossible "assert"
        CT_Assume -> require
        CT_Require -> require
        CT_Possible -> impossible "possible"
        CT_Unknowable {} -> impossible "unknowable"
      require = solRequire (show at) (solArg ctxt a)
  DLE_Wait {} -> emptyDoc
  DLE_PartSet _ _ a -> (solArg ctxt a) <> sp

solTransfer :: SolCtxt a -> DLArg -> DLArg -> Doc a
solTransfer ctxt who amt =
  (solArg ctxt who) <> "." <> solApply "transfer" [solArg ctxt amt]

solEvent :: SolCtxt a -> Int -> [DLVar] -> Doc a
solEvent ctxt which args =
  "event" <+> solApply (solMsg_evt which) (solDecl "_bal" (solType ctxt T_UInt256) : map (solArgDecl ctxt AM_Event) args) <> semi

solEventEmit :: SolCtxt a -> Int -> [DLVar] -> Doc a
solEventEmit ctxt which msg =
  "emit" <+> solApply (solMsg_evt which) (balancep : map (solVar ctxt) msg) <> semi
  where
    balancep = solPrimApply BALANCE []

data HashMode
  = HM_Set
  | HM_Check Int
  deriving (Eq, Show)

solHashState :: SolCtxt a -> HashMode -> [DLVar] -> Doc a
solHashState ctxt hm svs = solHash $ (solNum which_num) : which_last : (map (solVar ctxt) svs)
  where
    (which_last, which_num) =
      case hm of
        HM_Set -> (solBlockNumber, ctxt_handler_num ctxt)
        HM_Check prev -> (solLastBlock, prev)

solAsn :: SolCtxt a -> DLAssignment -> [Doc a]
solAsn ctxt (DLAssignment m) = map ((solArg ctxt) . snd) $ M.toAscList m

data SolTailRes a = SolTailRes (SolCtxt a) (Doc a)

instance Semigroup (SolTailRes a) where
  (SolTailRes ctxt_x xp) <> (SolTailRes ctxt_y yp) = SolTailRes (ctxt_x <> ctxt_y) (xp <> hardline <> yp)

arraySize :: DLArg -> Integer
arraySize a =
  case argTypeOf a of
    T_Array _ sz -> sz
    _ -> impossible "arraySize"

solSwitch :: (SolCtxt a -> k -> SolTailRes a) -> SolCtxt a -> SrcLoc -> DLVar -> SwitchCases k -> SolTailRes a
solSwitch iter ctxt _at ov csm = SolTailRes ctxt $ solIfs $ map cm1 $ M.toAscList csm
  where
    t = solType ctxt $ argTypeOf (DLA_Var ov)
    cm1 (vn, (mov', body)) = (c, set_and_body')
      where
        c = solEq ((solVar ctxt ov) <> ".which") (solVariant t vn)
        set_and_body' = vsep [set', body']
        set' = case mov' of
          Just ov' -> solSet (solMemVar ov') ((solVar ctxt ov) <> "._" <> pretty vn)
          Nothing -> emptyDoc
        SolTailRes _ body' = iter ctxt body

solCom :: (SolCtxt a -> k -> SolTailRes a) -> SolCtxt a -> PLCommon k -> SolTailRes a
solCom iter ctxt = \case
  PL_Return _ -> SolTailRes ctxt emptyDoc
  PL_Let _ _ dv (DLE_ArrayConcat _ x y) k -> SolTailRes ctxt concat_p <> iter ctxt k
    where
      concat_p = vsep [copy x 0, copy y (arraySize x)]
      copy src (off :: Integer) =
        "for" <+> parens ("uint256 i = 0" <> semi <+> "i <" <+> (pretty sz) <> semi <+> "i++")
          <> solBraces
            (solArrayRef (solVar ctxt dv) (solBinOp "+" "i" (solNum off)) <+> "="
               <+> solArrayRef (solArg ctxt src) "i" <> semi)
        where
          sz = arraySize src
  PL_Let _ _ dv@(DLVar _ _ t _) (DLE_ArrayZip _ x y) k -> SolTailRes ctxt zip_p <> iter ctxt k
    where
      zip_p =
        "for" <+> parens ("uint256 i = 0" <> semi <+> "i <" <+> (pretty $ xy_sz) <> semi <+> "i++")
          <> solBraces (solArrayRef (solVar ctxt dv) "i" <+> "=" <+> solApply tcon [ith x, ith y] <> semi)
      tcon = solType ctxt xy_ty
      ith which = solArrayRef (solArg ctxt which) "i"
      (xy_ty, xy_sz) = case t of
        T_Array a b -> (a, b)
        _ -> impossible "array_zip"
  PL_Let _ PL_Once dv de k -> iter ctxt' k
    where
      ctxt' = ctxt {ctxt_varm = M.insert dv de' $ ctxt_varm ctxt}
      de' = parens $ solExpr ctxt emptyDoc de
  PL_Let _ PL_Many dv de k -> SolTailRes ctxt dv_set <> iter ctxt k
    where
      dv_set = solSet (solMemVar dv) (solExpr ctxt emptyDoc de)
  PL_Eff _ de k -> SolTailRes ctxt dv_run <> iter ctxt k
    where
      dv_run = solExpr ctxt semi de
  PL_Var _ _ k -> iter ctxt k
  PL_Set _ dv da k -> SolTailRes ctxt dv_set <> iter ctxt k
    where
      dv_set = solSet (solMemVar dv) (solArg ctxt da)
  PL_LocalIf _ ca t f k -> SolTailRes ctxt (solIf ca' t' f') <> iter ctxt k
    where
      ca' = solArg ctxt ca
      SolTailRes _ t' = solPLTail ctxt t
      SolTailRes _ f' = solPLTail ctxt f
  PL_LocalSwitch at ov csm k -> solSwitch solPLTail ctxt at ov csm <> iter ctxt k
  PL_ArrayMap _ ans x a f r k ->
    SolTailRes ctxt map_p <> iter ctxt k
    where
      sz = arraySize x
      map_p =
        vsep
          [ "for" <+> parens ("uint256 i = 0" <> semi <+> "i <" <+> (pretty sz) <> semi <+> "i++")
              <> solBraces
                (vsep
                   [ solVar ctxt a <+> "=" <+> (solArrayRef (solArg ctxt x) "i") <> semi
                   , f'
                   , (solArrayRef (solVar ctxt ans) "i") <+> "=" <+> solArg fctxt r <> semi
                   ])
          ]
      SolTailRes fctxt f' = solPLTail ctxt f
  PL_ArrayReduce _ ans x z b a f r k ->
    SolTailRes ctxt reduce_p <> iter ctxt k
    where
      sz = arraySize x
      reduce_p =
        vsep
          [ (solVar ctxt b) <+> "=" <+> solArg ctxt z <> semi
          , "for" <+> parens ("uint256 i = 0" <> semi <+> "i <" <+> (pretty sz) <> semi <+> "i++")
              <> solBraces
                (vsep
                   [ solVar ctxt a <+> "=" <+> (solArrayRef (solArg ctxt x) "i") <> semi
                   , f'
                   , (solVar ctxt b) <+> "=" <+> solArg fctxt r <> semi
                   ])
          , (solVar ctxt ans) <+> "=" <+> (solVar ctxt b) <> semi
          ]
      SolTailRes fctxt f' = solPLTail ctxt f

solPLTail :: SolCtxt a -> PLTail -> SolTailRes a
solPLTail ctxt (PLTail m) = solCom solPLTail ctxt m

solCTail :: SolCtxt a -> CTail -> SolTailRes a
solCTail ctxt = \case
  CT_Com m -> solCom solCTail ctxt m
  CT_Seqn _ p k -> ptr <> solCTail ctxt' k
    where
      ptr@(SolTailRes ctxt' _) = solPLTail ctxt p
  CT_If _ ca t f -> SolTailRes ctxt' $ solIf (solArg ctxt ca) t' f'
    where
      SolTailRes ctxt'_t t' = solCTail ctxt t
      SolTailRes ctxt'_f f' = solCTail ctxt f
      ctxt' = ctxt'_t <> ctxt'_f
  CT_Switch at ov csm -> solSwitch solCTail ctxt at ov csm
  CT_Wait _ svs ->
    SolTailRes ctxt $
      vsep
        [ ctxt_emit ctxt
        , solSet ("current_state") (solHashState ctxt HM_Set svs)
        ]
  CT_Jump _ which svs asn ->
    SolTailRes ctxt $
      vsep
        [ ctxt_emit ctxt
        , solApply (solLoop_fun which) [solApply (solMsg_arg which) ((map (solVar ctxt) svs) ++ (solAsn ctxt asn))] <> semi
        ]
  CT_Halt _ ->
    SolTailRes ctxt $
      vsep
        [ ctxt_emit ctxt
        , solSet ("current_state") ("0x0")
        , solApply "selfdestruct" ["msg.sender"] <> semi
        ]

solFrame :: SolCtxt a -> Int -> S.Set DLVar -> (Doc a, Doc a)
solFrame ctxt i sim = if null fs then (emptyDoc, emptyDoc) else (frame_defp, frame_declp)
  where
    framei = pretty $ "_F" ++ show i
    frame_declp = (framei <+> "memory _f") <> semi
    frame_defp = solStruct framei fs
    fs = map mk_field $ S.elems sim
    mk_field dv@(DLVar _ _ t _) =
      ((solRawVar dv), (solType ctxt t))

manyVars_m :: (a -> S.Set DLVar) -> PLCommon a -> S.Set DLVar
manyVars_m iter = \case
  PL_Return {} -> mempty
  PL_Let _ lc dv de k -> mdv <> iter k
    where
      lc' = case de of
        DLE_ArrayConcat {} -> PL_Many
        DLE_ArrayZip {} -> PL_Many
        _ -> lc
      mdv = case lc' of
        PL_Once -> mempty
        PL_Many -> S.singleton dv
  PL_Eff _ _ k -> iter k
  PL_Var _ dv k -> S.insert dv $ iter k
  PL_Set _ _ _ k -> iter k
  PL_LocalIf _ _ t f k -> manyVars_p t <> manyVars_p f <> iter k
  PL_LocalSwitch _ _ csm k -> (mconcatMap cm1 $ M.elems csm) <> iter k
    where
      cm1 (mov', c) = S.union (S.fromList $ maybeToList mov') $ manyVars_p c
  PL_ArrayMap _ ans _ a f _ k ->
    s_inserts [ans, a] (manyVars_p f <> iter k)
  PL_ArrayReduce _ ans _ _ b a f _ k ->
    s_inserts [ans, b, a] (manyVars_p f <> iter k)
  where
    s_inserts l = S.union (S.fromList l)

manyVars_p :: PLTail -> S.Set DLVar
manyVars_p (PLTail m) = manyVars_m manyVars_p m

manyVars_c :: CTail -> S.Set DLVar
manyVars_c = \case
  CT_Com m -> manyVars_m manyVars_c m
  CT_Seqn _ p c -> manyVars_p p <> manyVars_c c
  CT_If _ _ t f -> manyVars_c t <> manyVars_c f
  CT_Switch _ _ csm -> mconcatMap cm1 $ M.elems csm
    where
      cm1 (mov', c) = S.union (S.fromList $ maybeToList mov') $ manyVars_c c
  CT_Wait {} -> mempty
  CT_Jump {} -> mempty
  CT_Halt {} -> mempty

solCTail_top :: SolCtxt a -> Int -> [DLVar] -> Maybe [DLVar] -> CTail -> (SolCtxt a, Doc a, Doc a, Doc a)
solCTail_top ctxt which vs mmsg ct = (ctxt'', frameDefn, frameDecl, ct')
  where
    argsm = M.fromList $ map (\v -> (v, solArgVar v)) vs
    mvars = manyVars_c ct
    mvarsm = M.fromList $ map (\v -> (v, solMemVar v)) $ S.toList mvars
    (frameDefn, frameDecl) = solFrame ctxt' which mvars
    SolTailRes ctxt'' ct' = solCTail ctxt' ct
    emitp = case mmsg of
      Just msg ->
        solEventEmit ctxt'_pre which msg
      Nothing ->
        emptyDoc
    ctxt' = ctxt'_pre {ctxt_emit = emitp}
    ctxt'_pre =
      ctxt
        { ctxt_handler_num = which
        , ctxt_varm = mvarsm <> argsm <> (ctxt_varm ctxt)
        }

solArgDefn :: SolCtxt a -> Int -> ArgMode -> [DLVar] -> (Doc a, [Doc a])
solArgDefn ctxt which am vs = (argDefn, argDefs)
  where
    argDefs = [solDecl "_a" ((solMsg_arg which) <> solArgLoc am)]
    argDefn = solStruct (solMsg_arg which) ntys
    ntys = mgiven ++ v_ntys
    mgiven = case am of
      AM_Call -> [(solLastBlockDef, (solType ctxt T_UInt256))]
      _ -> []
    v_ntys = map go vs
    go dv@(DLVar _ _ t _) = ((solRawVar dv), (solType ctxt t))

solHandler :: SolCtxt a -> Int -> CHandler -> Doc a
solHandler ctxt_top which (C_Handler at interval fs prev svs msg ct) =
  vsep [evtDefn, argDefn, frameDefn, funDefn]
  where
    checkMsg s = s <> " check at " <> show at
    vs = svs ++ msg
    ctxt_from = ctxt_top {ctxt_varm = fromm <> (ctxt_varm ctxt_top)}
    (ctxt, frameDefn, frameDecl, ctp) = solCTail_top ctxt_from which vs (Just msg) ct
    evtDefn = solEvent ctxt which msg
    (argDefn, argDefs) = solArgDefn ctxt which am vs
    ret = "payable"
    (hashCheck, am, sfl) =
      case (which, ctxt_deployMode ctxt_top) of
        (1, DM_firstMsg) ->
          (emptyDoc, AM_Memory, SFL_Constructor)
        _ ->
          (hcp, AM_Call, SFL_Function True (solMsg_fun which))
          where
            hcp = (solRequire (checkMsg "state") $ solEq ("current_state") (solHashState ctxt (HM_Check prev) svs)) <> semi
    funDefn = solFunctionLike sfl argDefs ret body
    body =
      vsep
        [ hashCheck
        , frameDecl
        , fromCheck
        , timeoutCheck
        , ctp
        ]
    (fromm, fromCheck) =
      case fs of
        FS_Join from -> ((M.singleton from "msg.sender"), emptyDoc)
        FS_Again from -> (mempty, (solRequire (checkMsg "sender") $ solEq ("msg.sender") (solVar ctxt from)) <> semi)
    timeoutCheck = solRequire (checkMsg "timeout") (solBinOp "&&" int_fromp int_top) <> semi
      where
        CBetween from to = interval
        int_fromp = check True from
        int_top = check False to
        check sign mv =
          case mv of
            [] -> "true"
            mvs -> solBinOp (if sign then ">=" else "<") solBlockNumber (foldl' (solBinOp "+") solLastBlock (map (solArg ctxt) mvs))
solHandler ctxt_top which (C_Loop _at svs msg ct) =
  vsep [argDefn, frameDefn, funDefn]
  where
    vs = svs ++ msg
    (ctxt_fin, frameDefn, frameDecl, ctp) = solCTail_top ctxt_top which vs Nothing ct
    (argDefn, argDefs) = solArgDefn ctxt_fin which AM_Memory vs
    ret = "internal"
    funDefn = solFunction (solLoop_fun which) argDefs ret body
    body = vsep [frameDecl, ctp]

solHandlers :: SolCtxt a -> CHandlers -> Doc a
solHandlers ctxt (CHandlers hs) = vsep_with_blank $ map (uncurry (solHandler ctxt)) $ M.toList hs

_solDefineType1 :: (SLType -> ST s (Doc a)) -> Int -> Doc a -> SLType -> ST s ((Doc a), (Doc a))
_solDefineType1 getTypeName i name = \case
  T_Null -> base
  T_Bool -> base
  T_UInt256 -> base
  T_Bytes -> base
  T_Address -> base
  T_Fun {} -> impossible "fun in ct"
  T_Forall {} -> impossible "forall in pl"
  T_Var {} -> impossible "var in pl"
  T_Array t sz -> do
    tn <- getTypeName t
    let me = tn <> brackets (pretty sz)
    let inmem = solArgLoc AM_Memory
    let memem = me <> inmem
    let tnmem = tn <> (if mustBeMem t then inmem else "")
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
    let set_defn = solFunction (solArraySet i) args ret body
    return $ (me, set_defn)
  T_Tuple ats -> do
    atsn <- mapM getTypeName ats
    return $ (name, solStruct name $ (flip zip) atsn $ map (pretty . ("elem" ++) . show) ([0 ..] :: [Int]))
  T_Object tm -> do
    tmn <- mapM getTypeName tm
    return $ (name, solStruct name $ map (\(k, v) -> (pretty k, v)) $ M.toAscList tmn)
  T_Data tm -> do
    tmn <- mapM getTypeName tm
    --- XXX Try to use bytes and abi.decode; Why not right away? The
    --- length of the bytes would not be predictable, which means the
    --- gas cost would be arbitrary.
    let enumn = "_enum_" <> name
    let enump = solEnum enumn $ map (pretty . fst) $ M.toAscList tmn
    let structp = solStruct name $ ("which", enumn) : map (\(k, t) -> (pretty ("_" <> k), t)) (M.toAscList tmn)
    return $ (name, vsep [enump, structp])
  T_Type {} -> impossible "type in pl"
  where
    base = impossible "base"

_solDefineType :: STCounter s -> STRef s (M.Map SLType Int) -> STRef s (M.Map SLType (Maybe (Doc a, Doc a))) -> SLType -> ST s (Doc a)
_solDefineType tcr timr tmr t = do
  tm <- readSTRef tmr
  case M.lookup t tm of
    Just (Just x) -> return $ fst x
    Just Nothing -> impossible $ "recursive type: " ++ show t
    Nothing -> do
      tn <- incSTCounter tcr
      modifySTRef timr $ M.insert t tn
      modifySTRef tmr $ M.insert t $ Nothing
      let n = pretty $ "T" ++ show tn
      (tr, def) <- _solDefineType1 (_solDefineType tcr timr tmr) tn n t
      modifySTRef tmr $ M.insert t $ Just (tr, def)
      return $ tr

solDefineTypes :: S.Set SLType -> (M.Map SLType Int, M.Map SLType (Doc a), Doc a)
solDefineTypes ts = (tim, M.map fst tm, vsep $ map snd $ M.elems tm)
  where
    base_typem =
      M.fromList
        [ (T_Null, "bool")
        , (T_Bool, "bool")
        , (T_UInt256, "uint256")
        , (T_Bytes, "bytes")
        , (T_Address, "address payable")
        ]
    base_tm = M.map (\t -> Just (t, emptyDoc)) base_typem
    tm = M.map (maybe (impossible "unfinished type") id) tmm
    (tim, tmm) = runST $ do
      tcr <- newSTCounter 0
      timr <- newSTRef mempty
      tmr <- newSTRef base_tm
      mapM_ (_solDefineType tcr timr tmr) $ S.toList ts
      liftM2 (,) (readSTRef timr) (readSTRef tmr)

solPLProg :: PLProg -> (ConnectorInfo, Doc a)
solPLProg (PLProg _ (PLOpts {..}) _ (CPProg at hs)) =
  (cinfo, vsep_with_blank $ [preamble, solVersion, solStdLib, ctcp])
  where
    ctcp =
      solContract "ReachContract is Stdlib" $
        ctcbody
    (typei, typem, typesp) = solDefineTypes $ cts hs
    ctxt =
      SolCtxt
        { ctxt_typem = typem
        , ctxt_typei = typei
        , ctxt_handler_num = 0
        , ctxt_emit = emptyDoc
        , ctxt_varm = mempty
        , ctxt_deployMode = plo_deployMode
        }
    ctcbody = vsep_with_blank $ [state_defn, consp, typesp, solHandlers ctxt hs]
    consp =
      case plo_deployMode of
        DM_constructor ->
          solFunctionLike SFL_Constructor [] "payable" consbody
          where
            SolTailRes _ consbody = solCTail ctxt (CT_Wait at [])
        DM_firstMsg ->
          emptyDoc
    cinfo = M.fromList [("deployMode", T.pack $ show plo_deployMode)]
    state_defn = "uint256 current_state;"
    preamble =
      vsep
        [ "// Automatically generated with Reach" <+> (pretty versionStr)
        , "pragma experimental ABIEncoderV2" <> semi
        ]

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
        abit <- ctc .: "abi"
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

extract :: ConnectorInfo -> Value -> Either String ConnectorResult
extract cinfo v = case fromJSON v of
  Error e -> Left e
  Success CompiledSolRec {..} ->
    case eitherDecode (LB.pack (T.unpack csrAbi)) of
      Left e -> Left e
      Right (csrAbi_parsed :: Value) ->
        Right $
          M.fromList
            [ ( "ETH"
              , M.union
                  (M.fromList
                     [ ("ABI", csrAbi_pretty)
                     , --- , ("Opcodes", T.unlines $ "" : (T.words $ csrOpcodes))
                       ("Bytecode", "0x" <> csrCode)
                     ])
                  cinfo
              )
            ]
        where
          csrAbi_pretty = T.pack $ LB.unpack $ encodePretty' cfg csrAbi_parsed
          cfg = defConfig {confIndent = Spaces 2, confCompare = compare}

compile_sol :: ConnectorInfo -> FilePath -> IO ConnectorResult
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
connect_eth outnMay pl = case outnMay of
  Just outn -> go (outn "sol")
  Nothing -> withSystemTempDirectory "reachc-sol" $ \dir ->
    go (dir </> "compiled.sol")
  where
    go :: FilePath -> IO ConnectorResult
    go solf = do
      let (cinfo, sol) = solPLProg pl
      unless dontWriteSol $ do
        writeFile solf (show sol)
      compile_sol cinfo solf
