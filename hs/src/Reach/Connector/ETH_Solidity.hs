module Reach.Connector.ETH_Solidity (connect_eth) where

-- https://github.com/reach-sh/reach-lang/blob/8d912e0/hs/src/Reach/Connector/ETH_EVM.hs.dead

import Control.Monad
import Control.Monad.ST
import Data.Aeson as Aeson
import Data.Aeson.Encode.Pretty
import Data.Aeson.Text
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
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LTIO
import Reach.AST.Base
import Reach.AST.DLBase
import Reach.AST.PL
import Reach.CollectTypes
import Reach.Connector
import Reach.EmbeddedFiles
import Reach.STCounter
import Reach.Texty
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
vsep_with_blank :: [Doc] -> Doc
vsep_with_blank l = vsep $ intersperse emptyDoc l

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

solEq :: SolCtxt -> Doc -> Doc -> Doc
solEq ctxt x y = solPrimApply ctxt PEQ [x, y]

-- solAnd :: Doc -> Doc -> Doc
-- solAnd x y = solBinOp "&&" x y

-- solBytesLength :: Doc -> Doc
-- solBytesLength x = "bytes(" <> x <> ").length"

solSet :: Doc -> Doc -> Doc
solSet x y = solBinOp "=" x y <> semi

solIf :: Doc -> Doc -> Doc -> Doc
solIf c t f = "if" <+> parens c <+> solBraces t <> hardline <> "else" <+> solBraces f

--- FIXME don't nest
solIfs :: [(Doc, Doc)] -> Doc
solIfs [] = emptyDoc
solIfs ((c, t) : more) = solIf c t $ solIfs more

solDecl :: Doc -> Doc -> Doc
solDecl n ty = ty <+> n

solStruct :: Doc -> [(Doc, Doc)] -> Doc
solStruct name fields = "struct" <+> name <+> solBraces (vsep $ map (<> semi) $ map (uncurry solDecl) fields)

solEnum :: Doc -> [Doc] -> Doc
solEnum name opts = "enum" <+> name <+> braces (hcat $ intersperse (comma <> space) opts)

--- Runtime helpers

solMsg_evt :: Pretty i => i -> Doc
solMsg_evt i = "e" <> pretty i

solMsg_arg :: Pretty i => i -> Doc
solMsg_arg i = "a" <> pretty i

solMsg_arg_svs :: Pretty i => i -> Doc
solMsg_arg_svs i = solMsg_arg i <> "svs"

solMsg_arg_postsvs :: Pretty i => i -> Doc
solMsg_arg_postsvs i = solMsg_arg i <> "postsvs"

solMsg_arg_msg :: Pretty i => i -> Doc
solMsg_arg_msg i = solMsg_arg i <> "msg"

solMsg_fun :: Pretty i => i -> Doc
solMsg_fun i = "m" <> pretty i

solLoop_fun :: Pretty i => i -> Doc
solLoop_fun i = "l" <> pretty i

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

--- Compiler

type VarMap = M.Map DLVar Doc

data SolCtxt = SolCtxt
  { ctxt_handler_num :: Int
  , ctxt_varm :: VarMap
  , ctxt_emit :: Doc
  , ctxt_typei :: M.Map DLType Int
  , ctxt_typem :: M.Map DLType Doc
  , ctxt_plo :: PLOpts
  }

instance Semigroup SolCtxt where
  --- FIXME maybe merge the maps?
  _ <> x = x

solRawVar :: DLVar -> Doc
solRawVar (DLVar _ _ _ n) = pretty $ "v" ++ show n

solMemVar :: DLVar -> Doc
solMemVar dv = "_f." <> solRawVar dv

solArgSVSVar :: DLVar -> Doc
solArgSVSVar dv = "_a.svs." <> solRawVar dv

solArgMsgVar :: DLVar -> Doc
solArgMsgVar dv = "_a.msg." <> solRawVar dv

solVar :: SolCtxt -> DLVar -> Doc
solVar ctxt v =
  case M.lookup v (ctxt_varm ctxt) of
    Just x -> x
    Nothing -> impossible $ "unbound var " ++ show v

solType :: SolCtxt -> DLType -> Doc
solType ctxt t =
  case M.lookup t $ ctxt_typem ctxt of
    Nothing -> impossible "cannot map sol type"
    Just x -> x

solTypeI :: SolCtxt -> DLType -> Int
solTypeI ctxt t =
  case M.lookup t $ ctxt_typei ctxt of
    Nothing -> impossible "cannot map sol type"
    Just x -> x

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

solVarDecl :: SolCtxt -> DLVar -> (Doc, Doc)
solVarDecl ctxt dv@(DLVar _ _ t _) =
  ((solRawVar dv), (solType ctxt t))

solLit :: DLLiteral -> Doc
solLit = \case
  DLL_Null -> "true"
  DLL_Bool True -> "true"
  DLL_Bool False -> "false"
  DLL_Int at i -> solNum $ checkIntLiteralC at connect_eth i
  DLL_Bytes s -> dquotes $ pretty $ B.unpack s

solArg :: SolCtxt -> DLArg -> Doc
solArg ctxt da =
  case da of
    DLA_Var v -> solVar ctxt v
    DLA_Constant c -> solLit $ conCons connect_eth c
    DLA_Literal c -> solLit c
    DLA_Interact {} -> impossible "consensus interact"

solPrimApply :: SolCtxt -> PrimOp -> [Doc] -> Doc
solPrimApply ctxt = \case
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
    [c, t, f] -> c <+> "?" <+> t <+> ":" <+> f
    _ -> impossible $ "emitSol: ITE wrong args"
  DIGEST_EQ -> binOp "=="
  ADDRESS_EQ -> binOp "=="
  where
    PLOpts {..} = ctxt_plo ctxt
    safeOp fun op =
      case plo_verifyOverflow of
        False -> binOp op
        True -> solApply fun
    binOp op = \case
      [l, r] -> solBinOp op l r
      _ -> impossible $ "emitSol: bin op args"

solLargeArg :: SolCtxt -> DLVar -> DLLargeArg -> Doc
solLargeArg ctxt dv la =
  case la of
    DLLA_Array _ as -> c $ zipWith go ([0 ..] :: [Int]) as
      where
        go i a = one ("[" <> pretty i <> "]") (solArg ctxt a)
    DLLA_Tuple as -> c $ zipWith go ([0 ..] :: [Int]) as
      where
        go i a = one (".elem" <> pretty i) (solArg ctxt a)
    DLLA_Obj m -> c $ map go $ M.toAscList m
      where
        go (k, a) = one ("." <> pretty k) (solArg ctxt a)
    DLLA_Data _ vn vv ->
      c
        [ one ".which" (solVariant t vn)
        , one ("._" <> pretty vn) (solArg ctxt vv)
        ]
  where
    t = solType ctxt $ largeArgTypeOf la
    one :: Doc -> Doc -> Doc
    one f v = solVar ctxt dv <> f <+> "=" <+> v <> semi
    c = vsep

solExpr :: SolCtxt -> Doc -> DLExpr -> Doc
solExpr ctxt sp = \case
  DLE_Arg _ a -> solArg ctxt a <> sp
  DLE_LArg {} ->
    impossible "large arg"
  DLE_Impossible at msg -> expect_thrown at $ Err_Impossible msg
  DLE_PrimOp _ p args ->
    (solPrimApply ctxt p $ map (solArg ctxt) args) <> sp
  DLE_ArrayRef _ ae ie ->
    solArrayRef (solArg ctxt ae) (solArg ctxt ie) <> sp
  DLE_ArraySet _ ae ie ve ->
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
  DLE_Transfer _ who amt ->
    solTransfer ctxt who amt <> sp
  DLE_Claim at fs ct a mmsg -> check <> sp
    where
      check = case ct of
        CT_Assert -> impossible "assert"
        CT_Assume -> require
        CT_Require -> require
        CT_Possible -> impossible "possible"
        CT_Unknowable {} -> impossible "unknowable"
      require = solRequire (show (at, fs, mmsg)) (solArg ctxt a)
  DLE_Wait {} -> emptyDoc
  DLE_PartSet _ _ a -> (solArg ctxt a) <> sp

solTransfer :: SolCtxt -> DLArg -> DLArg -> Doc
solTransfer ctxt who amt =
  (solArg ctxt who) <> "." <> solApply "transfer" [solArg ctxt amt]

solEvent :: SolCtxt -> Int -> Bool -> Doc
solEvent _ctxt which hasArgument =
  "event" <+> solApply (solMsg_evt which) args <> semi
  where
    args = case hasArgument of
      True -> [(solMsg_arg which) <+> "_a"]
      False -> []

solEventEmit :: SolCtxt -> Int -> Bool -> Doc
solEventEmit _ctxt which hasArgument =
  "emit" <+> solApply (solMsg_evt which) args <> semi
  where
    args = case hasArgument of
      True -> ["_a"]
      False -> []

solHashStateSet :: SolCtxt -> [(DLVar, DLArg)] -> ([Doc], Doc)
solHashStateSet ctxt svs = (setl, sete)
  where
    sete = solHash [(solNum which), "nsvs"]
    which = ctxt_handler_num ctxt
    setl = [solDecl "nsvs" ((solMsg_arg_postsvs which) <> " memory") <> semi] <> map go svs
    go (v, a) = solSet ("nsvs." <> solRawVar v) (solArg ctxt a)

solHashStateCheck :: SolCtxt -> Int -> Doc
solHashStateCheck _ctxt prev =
  solHash [(solNum prev), "_a.svs"]

data SolTailRes = SolTailRes SolCtxt Doc

instance Semigroup SolTailRes where
  (SolTailRes ctxt_x xp) <> (SolTailRes ctxt_y yp) = SolTailRes (ctxt_x <> ctxt_y) (xp <> hardline <> yp)

arraySize :: DLArg -> Integer
arraySize a =
  case argTypeOf a of
    T_Array _ sz -> sz
    _ -> impossible "arraySize"

solSwitch :: (SolCtxt -> k -> SolTailRes) -> SolCtxt -> SrcLoc -> DLVar -> SwitchCases k -> SolTailRes
solSwitch iter ctxt _at ov csm = SolTailRes ctxt $ solIfs $ map cm1 $ M.toAscList csm
  where
    t = solType ctxt $ argTypeOf (DLA_Var ov)
    cm1 (vn, (mov', body)) = (c, set_and_body')
      where
        c = solEq ctxt ((solVar ctxt ov) <> ".which") (solVariant t vn)
        set_and_body' = vsep [set', body']
        set' = case mov' of
          Just ov' -> solSet (solMemVar ov') ((solVar ctxt ov) <> "._" <> pretty vn)
          Nothing -> emptyDoc
        SolTailRes _ body' = iter ctxt body

solCom :: SolCtxt -> PLCommon -> SolTailRes
solCom ctxt = \case
  DL_Nop _ ->
    SolTailRes ctxt mempty
  DL_Let _ (PV_Let _ dv) (DLE_LArg _ la) ->
    SolTailRes ctxt la_p
    where
      la_p = solLargeArg ctxt dv la
  DL_Let _ (PV_Let _ dv) (DLE_ArrayConcat _ x y) ->
    SolTailRes ctxt concat_p
    where
      concat_p = vsep [copy x 0, copy y (arraySize x)]
      copy src (off :: Integer) =
        "for" <+> parens ("uint256 i = 0" <> semi <+> "i <" <+> (pretty sz) <> semi <+> "i++")
          <> solBraces
            (solArrayRef (solVar ctxt dv) (solPrimApply ctxt ADD ["i", (solNum off)]) <+> "="
               <+> solArrayRef (solArg ctxt src) "i" <> semi)
        where
          sz = arraySize src
  DL_Let _ (PV_Let _ dv@(DLVar _ _ t _)) (DLE_ArrayZip _ x y) ->
    SolTailRes ctxt zip_p
    where
      zip_p =
        "for" <+> parens ("uint256 i = 0" <> semi <+> "i <" <+> (pretty $ xy_sz) <> semi <+> "i++")
          <> solBraces (solArrayRef (solVar ctxt dv) "i" <+> "=" <+> solApply tcon [ith x, ith y] <> semi)
      tcon = solType ctxt xy_ty
      ith which = solArrayRef (solArg ctxt which) "i"
      (xy_ty, xy_sz) = case t of
        T_Array a b -> (a, b)
        _ -> impossible "array_zip"
  DL_Let _ (PV_Let PL_Once dv) de ->
    SolTailRes ctxt' mempty
    where
      ctxt' = ctxt {ctxt_varm = M.insert dv de' $ ctxt_varm ctxt}
      de' = parens $ solExpr ctxt emptyDoc de
  DL_Let _ (PV_Let PL_Many dv) de ->
    SolTailRes ctxt dv_set
    where
      dv_set = solSet (solMemVar dv) (solExpr ctxt emptyDoc de)
  DL_Let _ PV_Eff de -> SolTailRes ctxt dv_run
    where
      dv_run = solExpr ctxt semi de
  DL_Var {} -> SolTailRes ctxt mempty
  DL_Set _ dv da -> SolTailRes ctxt dv_set
    where
      dv_set = solSet (solMemVar dv) (solArg ctxt da)
  DL_LocalIf _ ca t f ->
    SolTailRes ctxt (solIf ca' t' f')
    where
      ca' = solArg ctxt ca
      SolTailRes _ t' = solPLTail ctxt t
      SolTailRes _ f' = solPLTail ctxt f
  DL_LocalSwitch at ov csm ->
    solSwitch solPLTail ctxt at ov csm
  DL_ArrayMap _ ans x a (DLinBlock _ _ f r) ->
    SolTailRes ctxt map_p
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
  DL_ArrayReduce _ ans x z b a (DLinBlock _ _ f r) ->
    SolTailRes ctxt reduce_p
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

solCom_ :: (SolCtxt -> a -> SolTailRes) -> SolCtxt -> PLCommon -> a -> SolTailRes
solCom_ iter ctxt m k = SolTailRes ctxt'' doc''
  where
    doc'' = m' <> hardline <> k'
    SolTailRes ctxt'' k' = iter ctxt' k
    SolTailRes ctxt' m' = solCom ctxt m

solPLTail :: SolCtxt -> PLTail -> SolTailRes
solPLTail ctxt = \case
  DT_Return _ -> SolTailRes ctxt emptyDoc
  DT_Com m k -> solCom_ solPLTail ctxt m k

solCTail :: SolCtxt -> CTail -> SolTailRes
solCTail ctxt = \case
  CT_Com m k -> solCom_ solCTail ctxt m k
  CT_If _ ca t f -> SolTailRes ctxt' $ solIf (solArg ctxt ca) t' f'
    where
      SolTailRes ctxt'_t t' = solCTail ctxt t
      SolTailRes ctxt'_f f' = solCTail ctxt f
      ctxt' = ctxt'_t <> ctxt'_f
  CT_Switch at ov csm -> solSwitch solCTail ctxt at ov csm
  CT_Jump _ which svs (DLAssignment asnm) ->
    SolTailRes ctxt $
      vsep $
        [ ctxt_emit ctxt
        , solDecl "la" ((solMsg_arg which) <> " memory") <> semi
        ]
          <> map go_svs svs
          <> map go_asn (M.toAscList asnm)
          <> [solApply (solLoop_fun which) ["la"] <> semi]
    where
      go_svs v = solSet ("la.svs." <> solRawVar v) (solVar ctxt v)
      go_asn (v, a) = solSet ("la.msg." <> solRawVar v) (solArg ctxt a)
  CT_From _ (Just svs) ->
    SolTailRes ctxt $
      vsep $
        [ctxt_emit ctxt]
          <> setl
          <> [solSet ("current_state") sete]
    where
      (setl, sete) = solHashStateSet ctxt svs
  CT_From _ Nothing ->
    SolTailRes ctxt $
      vsep
        [ ctxt_emit ctxt
        , solSet ("current_state") ("0x0")
        , solApply "selfdestruct" ["payable(msg.sender)"] <> semi
        ]

solFrame :: SolCtxt -> Int -> S.Set DLVar -> (Doc, Doc)
solFrame ctxt i sim = if null fs then (emptyDoc, emptyDoc) else (frame_defp, frame_declp)
  where
    framei = pretty $ "_F" ++ show i
    frame_declp = (framei <+> "memory _f") <> semi
    frame_defp = solStruct framei fs
    fs = map mk_field $ S.elems sim
    mk_field dv@(DLVar _ _ t _) =
      ((solRawVar dv), (solType ctxt t))

manyVars_m :: PLCommon -> S.Set DLVar
manyVars_m = \case
  DL_Nop _ -> mempty
  DL_Let _ (PV_Let lc dv) de -> mdv
    where
      lc' = case de of
        DLE_LArg {} -> PL_Many
        DLE_ArrayConcat {} -> PL_Many
        DLE_ArrayZip {} -> PL_Many
        _ -> lc
      mdv = case lc' of
        PL_Once -> mempty
        PL_Many -> S.singleton dv
  DL_Let _ PV_Eff _ -> mempty
  DL_Var _ dv -> S.singleton dv
  DL_Set {} -> mempty
  DL_LocalIf _ _ t f -> manyVars_p t <> manyVars_p f
  DL_LocalSwitch _ _ csm -> (mconcatMap cm1 $ M.elems csm)
    where
      cm1 (mov', c) = S.union (S.fromList $ maybeToList mov') $ manyVars_p c
  DL_ArrayMap _ ans _ a f ->
    s_inserts [ans, a] (manyVars_bl f)
  DL_ArrayReduce _ ans _ _ b a f ->
    s_inserts [ans, b, a] (manyVars_bl f)
  where
    s_inserts l = S.union (S.fromList l)

manyVars_p :: PLTail -> S.Set DLVar
manyVars_p = \case
  DT_Return _ -> mempty
  DT_Com m k -> manyVars_m m <> manyVars_p k

manyVars_bl :: PLBlock -> S.Set DLVar
manyVars_bl (DLinBlock _ _ t _) = manyVars_p t

manyVars_c :: CTail -> S.Set DLVar
manyVars_c = \case
  CT_Com m k -> manyVars_m m <> manyVars_c k
  CT_If _ _ t f -> manyVars_c t <> manyVars_c f
  CT_Switch _ _ csm -> mconcatMap cm1 $ M.elems csm
    where
      cm1 (mov', c) = S.union (S.fromList $ maybeToList mov') $ manyVars_c c
  CT_Jump {} -> mempty
  CT_From {} -> mempty

solCTail_top :: SolCtxt -> Int -> [DLVar] -> [DLVar] -> Maybe [DLVar] -> CTail -> (SolCtxt, Doc, Doc, Doc)
solCTail_top ctxt which svs msg mmsg ct = (ctxt'', frameDefn, frameDecl, ct')
  where
    svsm = M.fromList $ map (\v -> (v, solArgSVSVar v)) svs
    msgm = M.fromList $ map (\v -> (v, solArgMsgVar v)) msg
    mvars = manyVars_c ct
    mvarsm = M.fromList $ map (\v -> (v, solMemVar v)) $ S.toList mvars
    (frameDefn, frameDecl) = solFrame ctxt' which mvars
    SolTailRes ctxt'' ct' = solCTail ctxt' ct
    emitp = case mmsg of
      Just _ ->
        solEventEmit ctxt'_pre which True
      Nothing ->
        emptyDoc
    ctxt' = ctxt'_pre {ctxt_emit = emitp}
    ctxt'_pre =
      ctxt
        { ctxt_handler_num = which
        , ctxt_varm = mvarsm <> svsm <> msgm <> (ctxt_varm ctxt)
        }

solStructSVS :: SolCtxt -> Int -> [DLVar] -> Bool -> Doc
solStructSVS ctxt which svs add =
  solStruct (solMsg_arg_ which) svs_tys
  where
    solMsg_arg_ = if add then solMsg_arg_postsvs else solMsg_arg_svs
    svs_tys = map (solVarDecl ctxt) svs

data ArgDefnKind
  = ADK_Handler Int ArgMode
  | ADK_Loop [DLVar]

solArgDefn :: SolCtxt -> Int -> ArgDefnKind -> [DLVar] -> ([Doc], [Doc])
solArgDefn ctxt which adk msg = (argDefns, argDefs)
  where
    am =
      case adk of
        ADK_Handler _ x -> x
        ADK_Loop {} -> AM_Memory
    (which_svs_defn, which_svs_struct) =
      case adk of
        ADK_Handler prev _ -> ([], solMsg_arg_postsvs prev)
        ADK_Loop svs ->
          ([solStructSVS ctxt which svs False], solMsg_arg_svs which)
    argDefs = [solDecl "_a" ((solMsg_arg which) <> solArgLoc am)]
    argDefns =
      (case someArgs of
         True ->
           [solStruct (solMsg_arg_msg which) msg_tys]
         False ->
           [])
        <> which_svs_defn
        <> [solStruct (solMsg_arg which) arg_tys]
    msg_tys = map (solVarDecl ctxt) msg
    arg_tys =
      [("svs", which_svs_struct)]
        <> case someArgs of
          True -> [("msg", (solMsg_arg_msg which))]
          False -> []
    someArgs = not $ null msg_tys

solHandler :: SolCtxt -> Int -> CHandler -> Doc
solHandler ctxt_top which (C_Handler at interval last_timemv from prev svs msg amtv timev ct) =
  vsep $ argDefns <> [evtDefn, frameDefn, funDefn]
  where
    given_mm = M.fromList [(amtv, "msg.value"), (timev, solBlockNumber)]
    checkMsg s = s <> " check at " <> show at
    ctxt_from = ctxt_top {ctxt_varm = given_mm <> fromm <> (ctxt_varm ctxt_top)}
    (ctxt, frameDefn, frameDecl, ctp) =
      solCTail_top ctxt_from which svs msg (Just msg) ct
    evtDefn = solEvent ctxt which True
    (argDefns, argDefs) = solArgDefn ctxt which (ADK_Handler prev am) msg
    ret = "payable"
    (hashCheck, am, sfl) =
      case (which, plo_deployMode $ ctxt_plo ctxt_top) of
        (1, DM_firstMsg) ->
          (emptyDoc, AM_Memory, SFL_Constructor)
        _ ->
          (hcp, AM_Call, SFL_Function True (solMsg_fun which))
          where
            hcp = (solRequire (checkMsg "state") $ solEq ctxt ("current_state") (solHashStateCheck ctxt prev)) <> semi
    funDefn = solFunctionLike sfl argDefs ret body
    body =
      vsep
        [ hashCheck
        , frameDecl
        , fromCheck
        , timeoutCheck
        , ctp
        ]
    (fromm, fromCheck) = ((M.singleton from "payable(msg.sender)"), emptyDoc)
    timeoutCheck =
      case last_timemv of
        Nothing -> emptyDoc
        Just last_timev ->
          solRequire (checkMsg "timeout") (solBinOp "&&" int_fromp int_top) <> semi
          where
            CBetween ifrom ito = interval
            int_fromp = check True ifrom
            int_top = check False ito
            check sign mv =
              case mv of
                [] -> "true"
                mvs -> solPrimApply ctxt (if sign then PGE else PLT) [solVar ctxt timev, (foldl' (\x y -> solPrimApply ctxt ADD [x, y]) (solVar ctxt last_timev) (map (solArg ctxt) mvs))]
solHandler ctxt_top which (C_Loop _at svs lcmsg ct) =
  vsep $ argDefns <> [frameDefn, funDefn]
  where
    msg = map snd lcmsg
    (ctxt_fin, frameDefn, frameDecl, ctp) =
      solCTail_top ctxt_top which svs msg Nothing ct
    (argDefns, argDefs) = solArgDefn ctxt_fin which (ADK_Loop svs) msg
    ret = "internal"
    funDefn = solFunction (solLoop_fun which) argDefs ret body
    body = vsep [frameDecl, ctp]

solHandlers :: SolCtxt -> CHandlers -> Doc
solHandlers ctxt (CHandlers hs) = vsep_with_blank $ map (uncurry (solHandler ctxt)) $ M.toList hs

solHandlerStructSVS :: SolCtxt -> (S.Set Int, [Doc]) -> (Int, CHandler) -> (S.Set Int, [Doc])
solHandlerStructSVS _ acc (_which, C_Loop {}) = acc
solHandlerStructSVS ctxt (defd, res) (_which, C_Handler _ _ _ _ prev svs _ _ _ _) =
  case S.member prev defd of
    True -> (defd, res)
    False -> (defd', res')
  where
    res' = solStructSVS ctxt prev svs True : res
    defd' = S.insert prev defd

solHandlersStructSVS :: SolCtxt -> [DLVar] -> CHandlers -> Doc
solHandlersStructSVS ctxt csvs (CHandlers hs) =
  vsep_with_blank $
    snd $
      foldl' (solHandlerStructSVS ctxt) (defd, res0) $ M.toList hs
  where
    defd = S.singleton 0
    res0 = [solStructSVS ctxt 0 csvs True]

_solDefineType1 :: (DLType -> ST s (Doc)) -> Int -> Doc -> DLType -> ST s ((Doc), (Doc))
_solDefineType1 getTypeName i name = \case
  T_Null -> base
  T_Bool -> base
  T_UInt -> base
  T_Bytes _ -> base
  T_Digest -> base
  T_Address -> base
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
  where
    base = impossible "base"

_solDefineType :: STCounter s -> STRef s (M.Map DLType Int) -> STRef s (M.Map DLType (Maybe (Doc, Doc))) -> DLType -> ST s (Doc)
_solDefineType tcr timr tmr t = do
  tm <- readSTRef tmr
  case M.lookup t tm of
    Just (Just x) -> return $ fst x
    Just Nothing -> impossible $ "recursive type: " ++ show t
    Nothing ->
      case t of
        T_Bytes sz -> do
          let tr = "uint8[" <> pretty sz <> "]"
          modifySTRef tmr $ M.insert t $ Just (tr, emptyDoc)
          return $ tr
        _ -> do
          tn <- incSTCounter tcr
          modifySTRef timr $ M.insert t tn
          modifySTRef tmr $ M.insert t $ Nothing
          let n = pretty $ "T" ++ show tn
          (tr, def) <- _solDefineType1 (_solDefineType tcr timr tmr) tn n t
          modifySTRef tmr $ M.insert t $ Just (tr, def)
          return $ tr

solDefineTypes :: S.Set DLType -> (M.Map DLType Int, M.Map DLType (Doc), Doc)
solDefineTypes ts = (tim, M.map fst tm, vsep $ map snd $ M.elems tm)
  where
    base_typem =
      M.fromList
        [ (T_Null, "bool")
        , (T_Bool, "bool")
        , (T_UInt, "uint256")
        , (T_Digest, "uint256")
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

solPLProg :: PLProg -> (ConnectorInfoMap, Doc)
solPLProg (PLProg _ plo@(PLOpts {..}) dli _ (CPProg at hs)) =
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
        , ctxt_plo = plo
        }
    ctcbody =
      vsep_with_blank $
        [ state_defn
        , consp
        , typesp
        , solHandlersStructSVS ctxt csvs hs
        , solHandlers ctxt hs
        ]
    (csvs, consp) =
      case plo_deployMode of
        DM_constructor ->
          ( csvs_
          , vsep
              [ solEvent ctxt 0 False
              , cfDefn
              , solFunctionLike SFL_Constructor [] "payable" consbody'
              ]
          )
          where
            DLInit ctimem = dli
            dli' = vsep $ ctimem'
            (ctimem', csvs_, cctxt) =
              case ctimem of
                Nothing -> (mempty, mempty, ctxt)
                Just v ->
                  ( [solSet (solMemVar v) solBlockNumber]
                  , [v]
                  , ctxt {ctxt_varm = M.insert v (solMemVar v) (ctxt_varm ctxt)}
                  )
            (cfDefn, cfDecl) = solFrame cctxt 0 (S.fromList csvs_)
            consbody' =
              vsep
                [ solEventEmit ctxt 0 False
                , cfDecl
                , dli'
                , consbody
                ]
            csvs_m = map (\x -> (x, DLA_Var x)) csvs_
            SolTailRes _ consbody = solCTail cctxt (CT_From at (Just csvs_m))
        DM_firstMsg ->
          -- XXX This is a hack... there are no constructor SVSs when the
          -- deployment mode is firstMsg, but rather than allow the rest of the
          -- code to deal with this being missing (because Solidity doesn't
          -- allow empty structs), we force there to be one that will be
          -- ignored
          ([(DLVar at "fake" T_UInt 0)], emptyDoc)
    cinfo = HM.fromList [("deployMode", Aeson.String $ T.pack $ show plo_deployMode)]
    state_defn = "uint256 current_state;"
    preamble =
      vsep
        [ "// Automatically generated with Reach" <+> (pretty versionStr)
        , "pragma abicoder v2" <> semi
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
    conGen outnMay pl = case outnMay of
      Just outn -> go (outn "sol")
      Nothing -> withSystemTempDirectory "reachc-sol" $ \dir ->
        go (dir </> "compiled.sol")
      where
        go :: FilePath -> IO ConnectorInfo
        go solf = do
          let (cinfo, sol) = solPLProg pl
          unless dontWriteSol $ do
            LTIO.writeFile solf $ render sol
          compile_sol cinfo solf
