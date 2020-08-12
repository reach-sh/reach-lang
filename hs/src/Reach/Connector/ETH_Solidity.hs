module Reach.Connector.ETH_Solidity (connect_eth) where

import Control.Monad.ST
import Data.Aeson
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.HashMap.Strict as HM
import Data.List (find, foldl', intersperse)
import qualified Data.Map.Strict as M
import Data.STRef
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import Data.Version (showVersion)
import Paths_reach (version)
import Reach.AST
import Reach.CollectTypes
import Reach.Connector
import Reach.EmbeddedFiles
import Reach.STCounter
import Reach.Type
import Reach.Util
import System.Exit
import System.Process

--- Pretty helpers

vsep_with_blank :: [Doc a] -> Doc a
vsep_with_blank l = vsep $ intersperse emptyDoc l

--- Solidity helpers

solNum :: Show n => n -> Doc a
solNum i = pretty $ "uint256(" ++ show i ++ ")"

solBraces :: Doc a -> Doc a
solBraces body = braces (nest 2 $ hardline <> body <> space)

solFunction :: Doc a -> [Doc a] -> Doc a -> Doc a -> Doc a
solFunction name args ret body =
  "function" <+> solApply name args <+> ret <+> solBraces body

solContract :: String -> Doc a -> Doc a
solContract s body = "contract" <+> pretty s <+> solBraces body

solVersion :: Doc a
solVersion = "pragma solidity ^0.5.11;"

solStdLib :: Doc a
solStdLib = pretty $ B.unpack stdlib_sol

solApply :: Doc a -> [Doc a] -> Doc a
solApply f args = f <> parens (hcat $ intersperse (comma <> space) args)

solRequire :: Doc a -> Doc a
solRequire a = solApply "require" [a]

solBinOp :: String -> Doc a -> Doc a -> Doc a
solBinOp o l r = l <+> pretty o <+> r

solEq :: Doc a -> Doc a -> Doc a
solEq = solBinOp "=="

solSet :: Doc a -> Doc a -> Doc a
solSet x y = solBinOp "=" x y <> semi

solIf :: Doc a -> Doc a -> Doc a -> Doc a
solIf c t f = "if" <+> parens c <+> solBraces t <> hardline <> "else" <+> solBraces f

solDecl :: Doc a -> Doc a -> Doc a
solDecl n ty = ty <+> n

solStruct :: Doc a -> [(Doc a, Doc a)] -> Doc a
solStruct name fields = "struct" <+> name <+> solBraces (vsep $ map (<> semi) $ map (uncurry solDecl) fields)

--- Runtime helpers

solMsg_evt :: Pretty i => i -> Doc a
solMsg_evt i = "e" <> pretty i

solMsg_fun :: Pretty i => i -> Doc a
solMsg_fun i = "m" <> pretty i

solLoop_fun :: Pretty i => i -> Doc a
solLoop_fun i = "l" <> pretty i

solLastBlock :: Doc a
solLastBlock = "_last"

solBlockNumber :: Doc a
solBlockNumber = "uint256(block.number)"

solHash :: [Doc a] -> Doc a
solHash a = solApply "uint256" [solApply "keccak256" [solApply "abi.encodePacked" a]]

--- Compiler

type VarMap a = M.Map DLVar (Doc a)

data SolCtxt a = SolCtxt
  { ctxt_handler_num :: Int
  , ctxt_varm :: VarMap a
  , ctxt_emit :: Doc a
  , ctxt_typem :: M.Map SLType (Doc a)
  }

instance Semigroup (SolCtxt a) where
  --- FIXME maybe merge the maps?
  _ <> x = x

solRawVar :: DLVar -> Doc a
solRawVar (DLVar _ _ _ n) = pretty $ "v" ++ show n

solMemVar :: DLVar -> Doc a
solMemVar dv = "_f." <> solRawVar dv

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
  T_Obj {} -> True
  T_Forall {} -> impossible "forall"
  T_Var {} -> impossible "var"

solArgType :: SolCtxt a -> SLType -> Doc a
solArgType ctxt t = solType ctxt t <> (if mustBeMem t then " calldata" else "")

solArgDecl :: SolCtxt a -> DLVar -> Doc a
solArgDecl ctxt dv@(DLVar _ _ t _) = solDecl (solRawVar dv) (solArgType ctxt t)

solCon :: DLConstant -> Doc a
solCon = \case
  DLC_Null -> "void"
  DLC_Bool True -> "true"
  DLC_Bool False -> "false"
  DLC_Int i -> solNum i
  DLC_Bytes s -> dquotes $ pretty $ B.unpack s

solArg :: SolCtxt a -> DLArg -> Doc a
solArg ctxt = \case
  DLA_Var v -> solVar ctxt v
  DLA_Con c -> solCon c
  da@(DLA_Tuple as) -> solApply (solType ctxt (argTypeOf da)) $ map (solArg ctxt) as
  da@(DLA_Obj m) -> solApply (solType ctxt (argTypeOf da)) $ map ((solArg ctxt) . snd) $ M.toAscList m
  DLA_Interact {} -> impossible "consensus interact"

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
  BYTES_EQ -> binOp "=="
  BALANCE -> \_ -> "address(this).balance"
  TXN_VALUE -> \_ -> "msg.value"
  where
    binOp op = \case
      [l, r] -> solBinOp op l r
      _ -> impossible $ "emitSol: bin op args"

solExpr :: SolCtxt a -> DLExpr -> Doc a
solExpr ctxt = \case
  DLE_PrimOp _ p args -> solPrimApply p $ map (solArg ctxt) args
  DLE_ArrayRef _ _ ae _ ee ->
    (solArg ctxt ae) <> brackets (solArg ctxt ee)
  DLE_TupleRef _ ae i ->
    (solArg ctxt ae) <> ".elem" <> pretty i
  DLE_ObjectRef _ oe f -> (solArg ctxt oe) <> "." <> pretty f
  DLE_Interact {} -> impossible "consensus interact"
  DLE_Digest _ args -> solHash $ map (solArg ctxt) args

solTransfer :: SolCtxt a -> DLArg -> DLArg -> Doc a
solTransfer ctxt who amt =
  (solArg ctxt who) <> "." <> solApply "transfer" [solArg ctxt amt] <> semi

solEvent :: SolCtxt a -> Int -> [DLVar] -> Doc a
solEvent ctxt which args =
  "event" <+> solApply (solMsg_evt which) (solDecl "_bal" (solType ctxt T_UInt256) : map (solArgDecl ctxt) args) <> semi

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

solCom :: (SolCtxt a -> k -> SolTailRes a) -> SolCtxt a -> PLCommon k -> SolTailRes a
solCom iter ctxt = \case
  PL_Return _ -> SolTailRes ctxt emptyDoc
  PL_Let _ PL_Once dv de k -> iter ctxt' k
    where
      ctxt' = ctxt {ctxt_varm = M.insert dv de' $ ctxt_varm ctxt}
      de' = parens $ solExpr ctxt de
  PL_Let _ PL_Many dv de k -> SolTailRes ctxt dv_set <> iter ctxt k
    where
      dv_set = solSet (solMemVar dv) (solExpr ctxt de)
  PL_Eff _ de k -> SolTailRes ctxt dv_run <> iter ctxt k
    where
      dv_run = solExpr ctxt de <> semi
  PL_Var _ _ k -> iter ctxt k
  PL_Set _ dv da k -> SolTailRes ctxt dv_set <> iter ctxt k
    where
      dv_set = solSet (solMemVar dv) (solArg ctxt da)
  PL_Claim _ _ ct a k -> SolTailRes ctxt check <> iter ctxt k
    where
      check = case ct of
        CT_Assert -> emptyDoc
        CT_Assume -> require
        CT_Require -> require
        CT_Possible -> emptyDoc
      require = solRequire (solArg ctxt a) <> semi
  PL_LocalIf _ ca t f k -> SolTailRes ctxt (solIf ca' t' f') <> iter ctxt k
    where
      ca' = solArg ctxt ca
      SolTailRes _ t' = solPLTail ctxt t
      SolTailRes _ f' = solPLTail ctxt f

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
  CT_Transfer _ who amt k -> SolTailRes ctxt' $ vsep [solTransfer ctxt who amt, k']
    where
      SolTailRes ctxt' k' = solCTail ctxt k
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
        , solApply (solLoop_fun which) ((map (solVar ctxt) svs) ++ (solAsn ctxt asn)) <> semi
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
  PL_Let _ lc dv _ k -> mdv <> iter k
    where
      mdv = case lc of
        PL_Once -> mempty
        PL_Many -> S.singleton dv
  PL_Eff _ _ k -> iter k
  PL_Var _ dv k -> S.insert dv $ iter k
  PL_Set _ _ _ k -> iter k
  PL_Claim _ _ _ _ k -> iter k
  PL_LocalIf _ _ t f k -> manyVars_p t <> manyVars_p f <> iter k

manyVars_p :: PLTail -> S.Set DLVar
manyVars_p (PLTail m) = manyVars_m manyVars_p m

manyVars_c :: CTail -> S.Set DLVar
manyVars_c = \case
  CT_Com m -> manyVars_m manyVars_c m
  CT_Seqn _ p c -> manyVars_p p <> manyVars_c c
  CT_If _ _ t f -> manyVars_c t <> manyVars_c f
  CT_Transfer _ _ _ k -> manyVars_c k
  CT_Wait {} -> mempty
  CT_Jump {} -> mempty
  CT_Halt {} -> mempty

solCTail_top :: SolCtxt a -> Int -> [DLVar] -> Maybe [DLVar] -> CTail -> (SolCtxt a, Doc a, Doc a, Doc a)
solCTail_top ctxt which vs mmsg ct = (ctxt'', frameDefn, frameDecl, ct')
  where
    argsm = M.fromList $ map (\v -> (v, solRawVar v)) vs
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

solHandler :: SolCtxt a -> Int -> CHandler -> Doc a
solHandler ctxt_top which (C_Handler _at interval fs prev svs msg ct) = vsep [evtDefn, frameDefn, funDefn]
  where
    vs = svs ++ msg
    ctxt_from = ctxt_top {ctxt_varm = fromm <> (ctxt_varm ctxt_top)}
    (ctxt, frameDefn, frameDecl, ctp) = solCTail_top ctxt_from which vs (Just msg) ct
    evtDefn = solEvent ctxt which msg
    argDefs = (solDecl solLastBlock (solType ctxt T_UInt256)) : map (solArgDecl ctxt) vs
    ret = "external payable"
    funDefn = solFunction (solMsg_fun which) argDefs ret body
    body =
      vsep
        [ (solRequire $ solEq ("current_state") (solHashState ctxt (HM_Check prev) svs)) <> semi
        , frameDecl
        , fromCheck
        , timeoutCheck
        , ctp
        ]
    (fromm, fromCheck) =
      case fs of
        FS_Join from -> ((M.singleton from "msg.sender"), emptyDoc)
        FS_Again from -> (mempty, (solRequire $ solEq ("msg.sender") (solVar ctxt from)) <> semi)
    timeoutCheck = solRequire (solBinOp "&&" int_fromp int_top) <> semi
      where
        CBetween from to = interval
        int_fromp = check True from
        int_top = check False to
        check sign mv =
          case mv of
            [] -> "true"
            mvs -> solBinOp (if sign then ">=" else "<") solBlockNumber (foldl' (solBinOp "+") solLastBlock (map (solArg ctxt) mvs))
solHandler ctxt_top which (C_Loop _at svs msg ct) = vsep [frameDefn, funDefn]
  where
    vs = svs ++ msg
    (ctxt_fin, frameDefn, frameDecl, ctp) = solCTail_top ctxt_top which vs Nothing ct
    argDefs = map (solArgDecl ctxt_fin) vs
    ret = "internal"
    funDefn = solFunction (solLoop_fun which) argDefs ret body
    body = vsep [frameDecl, ctp]

solHandlers :: SolCtxt a -> CHandlers -> Doc a
solHandlers ctxt (CHandlers hs) = vsep_with_blank $ map (uncurry (solHandler ctxt)) $ M.toList hs

_solDefineType1 :: (SLType -> ST s (Doc a)) -> Doc a -> SLType -> ST s (Doc a)
_solDefineType1 getTypeName name = \case
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
    return $ tn <> brackets (pretty sz)
  T_Tuple ats -> do
    atsn <- mapM getTypeName ats
    return $ solStruct name $ (flip zip) atsn $ map (pretty . ("elem" ++) . show) ([0 ..] :: [Int])
  T_Obj tm -> do
    tmn <- mapM getTypeName tm
    return $ solStruct name $ map (\(k, v) -> (pretty k, v)) $ M.toAscList tmn
  where
    base = impossible "base"

_solDefineType :: STCounter s -> STRef s (M.Map SLType (Maybe (Doc a, Doc a))) -> SLType -> ST s (Doc a)
_solDefineType tcr tmr t = do
  tm <- readSTRef tmr
  case M.lookup t tm of
    Just (Just x) -> return $ fst x
    Just Nothing -> impossible $ "recursive type: " ++ show t
    Nothing -> do
      tn <- incSTCounter tcr
      modifySTRef tmr $ M.insert t $ Nothing
      let n = pretty $ "T" ++ show tn
      def <- _solDefineType1 (_solDefineType tcr tmr) n t
      modifySTRef tmr $ M.insert t $ Just (n, def)
      return $ n

solDefineTypes :: S.Set SLType -> (M.Map SLType (Doc a), Doc a)
solDefineTypes ts = (M.map fst tm, vsep $ map snd $ M.elems tm)
  where
    base_typem =
      M.fromList
        [ (T_Null, "void")
        , (T_Bool, "bool")
        , (T_UInt256, "uint256")
        , (T_Bytes, "bytes")
        , (T_Address, "address payable")
        ]
    base_tm = M.map (\t -> Just (t, emptyDoc)) base_typem
    tm = M.map (maybe (impossible "unfinished type") id) tmm
    tmm = runST $ do
      tcr <- newSTCounter 0
      tmr <- newSTRef base_tm
      mapM_ (_solDefineType tcr tmr) $ S.toList ts
      readSTRef tmr

solPLProg :: PLProg -> Doc a
solPLProg (PLProg _ _ (CPProg at hs)) =
  vsep_with_blank $ [preamble, solVersion, solStdLib, ctcp]
  where
    ctcp =
      solContract "ReachContract is Stdlib" $
        ctcbody
    (typem, typesp) = solDefineTypes $ cts hs
    ctxt =
      SolCtxt
        { ctxt_typem = typem
        , ctxt_handler_num = 0
        , ctxt_emit = emptyDoc
        , ctxt_varm = mempty
        }
    ctcbody = vsep_with_blank $ [state_defn, consp, typesp, solHandlers ctxt hs]
    consp = solApply "constructor" [] <+> "public payable" <+> solBraces consbody
    SolTailRes _ consbody = solCTail ctxt (CT_Wait at [])
    state_defn = "uint256 current_state;"
    preamble = pretty $ "// Automatically generated with Reach " ++ showVersion version

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
        abit <- ctc .: "abi"
        codebodyt <- ctc .: "bin"
        return CompiledSolRec {csrAbi = abit, csrCode = codebodyt}
      Nothing ->
        fail "Expected contracts object to have a key with suffix ':ReachContract'"

extract :: Value -> Either String ConnectorResult
extract v = case fromJSON v of
  Error e -> Left e
  Success CompiledSolRec {csrAbi, csrCode} ->
    Right $ M.fromList
            [ ( "ETH"
              , M.fromList
                [ ("ABI", csrAbi)
                , ("Bytecode", "0x" <> csrCode)
                ]
              )
            ]

compile_sol :: FilePath -> IO ConnectorResult
compile_sol solf = do
  (ec, stdout, stderr) <-
    readProcessWithExitCode "solc" ["--optimize", "--combined-json", "abi,bin", solf] []
  let show_output = "STDOUT:\n" ++ stdout ++ "\nSTDERR:\n" ++ stderr ++ "\n"
  case ec of
    ExitFailure _ -> die $ "solc failed:\n" ++ show_output
    ExitSuccess ->
      case (eitherDecode $ LB.pack stdout) of
        Right v ->
          case extract v of
            Right cr -> return cr
            Left err ->
              die $ "failed to extract valid output from solc:\n" ++ show_output
              ++ "Decode:\n" ++ err ++ "\n"
        Left err ->
          die $ "solc failed to produce valid output:\n" ++ show_output
          ++ "Decode:\n" ++ err ++ "\n"

connect_eth :: Connector
connect_eth outn pl = do
  let solf = outn "sol"
  writeFile solf (show (solPLProg pl))
  compile_sol solf
