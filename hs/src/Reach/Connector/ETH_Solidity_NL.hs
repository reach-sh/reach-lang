{-# OPTIONS_GHC -Wwarn=unused-matches #-} --- XXX
module Reach.Connector.ETH_Solidity_NL (connect_eth) where

import Data.Aeson
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import System.Exit
import System.Process
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (find, foldl', intersperse)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
---import Debug.Trace
import Data.Version (showVersion)
import Paths_reach (version)
import Reach.EmbeddedFiles
import Reach.NL_AST
import Reach.Util
import Reach.Connector
import Data.Text.Prettyprint.Doc

--- Pretty helpers

vsep_with_blank :: [Doc a] -> Doc a
vsep_with_blank l = vsep $ intersperse emptyDoc l

--- Solidity helpers

solNum :: Show n => n -> Doc a
solNum i = pretty $ "uint256(" ++ show i ++ ")"

solBraces :: Doc a -> Doc a
solBraces body = braces (nest 2 $ hardline <> body <> space)

solFunction :: String -> [Doc a] -> Doc a -> Doc a -> Doc a
solFunction name args ret body =
  "function" <+> solApply name args <+> ret <+> solBraces body

solContract :: String -> Doc a -> Doc a
solContract s body = "contract" <+> pretty s <+> solBraces body

solVersion :: Doc a
solVersion = "pragma solidity ^0.5.11;"

solStdLib :: Doc a
solStdLib = pretty $ B.unpack stdlib_sol

solApply :: String -> [Doc a] -> Doc a
solApply f args = pretty f <> parens (hcat $ intersperse (comma <> space) args)

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

solDecl :: String -> Doc a -> Doc a
solDecl ty n = pretty ty <+> n

--- Runtime helpers

solMsg_evt :: Show i => i -> String
solMsg_evt i = "e" ++ show i

solMsg_fun :: Show i => i -> String
solMsg_fun i = "m" ++ show i

solLoop_fun :: Show i => i -> String
solLoop_fun i = "l" ++ show i

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
  , ctxt_varm :: VarMap a }

solRawVar :: DLVar -> Doc a
solRawVar (DLVar _ _ _ n) = pretty $ "v" ++ show n

solMemVar :: DLVar -> Doc a
solMemVar dv = "_f." <> solRawVar dv

solVar :: SolCtxt a -> DLVar -> Doc a
solVar ctxt v =
  case M.lookup v (ctxt_varm ctxt) of
    Just x -> x
    Nothing -> impossible $ "unbound var " ++ show v

solType :: SLType -> String
solType = \case
  T_Null -> "void"
  T_Bool -> "bool"
  T_UInt256 -> "uint256"
  T_Bytes -> "bytes"
  T_Address -> "address payable"
  T_Fun {} -> impossible "fun"
  T_Array {} -> error "XXX"
  T_Obj {} -> error "XXX"
  T_Forall {} -> impossible "forall"
  T_Var {} -> impossible "var"

mustBeMem :: SLType -> Bool
mustBeMem = \case
  T_Null -> False
  T_Bool -> False
  T_UInt256 -> False
  T_Bytes -> True
  T_Address -> True
  T_Fun {} -> impossible "fun"
  T_Array {} -> True
  T_Obj {} -> True
  T_Forall {} -> impossible "forall"
  T_Var {} -> impossible "var"

solArgType :: SLType -> String
solArgType t = solType t <> (if mustBeMem t then " calldata" else "")

solFieldDecl :: DLVar -> Doc a
solFieldDecl dv@(DLVar _ _ t _) = solDecl (solType t) (solRawVar dv)

solArgDecl :: DLVar -> Doc a
solArgDecl dv@(DLVar _ _ t _) = solDecl (solArgType t) (solRawVar dv)

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
  DLA_Array {} -> error "XXX"
  DLA_Obj {} -> error "XXX"
  DLA_Interact {} -> impossible "consensus interact"

solCPrimApply :: ConsensusPrimOp -> [Doc a] -> Doc a
solCPrimApply = \case
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

solPrimApply :: PrimOp -> [Doc a] -> Doc a
solPrimApply = \case
  CP cp -> solCPrimApply cp
  _ -> impossible "consensus local prim"

solExpr :: SolCtxt a -> DLExpr -> Doc a
solExpr ctxt = \case
  DLE_PrimOp _ p args -> solPrimApply p $ map (solArg ctxt) args
  DLE_ArrayRef _ ae ee -> (solArg ctxt ae) <> brackets (solArg ctxt ee)
  DLE_ObjectRef _ oe f -> (solArg ctxt oe) <> "." <> pretty f
  DLE_Interact {} -> impossible "consensus interact"
  DLE_Digest _ args -> solHash $ map (solArg ctxt) args

solCom :: (SolCtxt a -> k -> Doc a) -> SolCtxt a -> PLCommon k -> Doc a
solCom iter ctxt = \case
  PL_Return _ -> emptyDoc
  PL_Let _ PL_Once dv de k -> iter ctxt' k
    where ctxt' = ctxt { ctxt_varm = M.insert dv de' $ ctxt_varm ctxt }
          de' = parens $ solExpr ctxt de
  PL_Let _ PL_Many dv de k -> vsep [ dv_set, iter ctxt k ]
    where dv_set = solSet (solMemVar dv) (solExpr ctxt de) <> semi
  PL_Eff _ de k -> vsep [ dv_run, iter ctxt k ]
    where dv_run = solExpr ctxt de <> semi
  PL_Var _ _ k -> iter ctxt k
  PL_Set _ dv da k -> vsep [ dv_set, iter ctxt k ]
    where dv_set = solSet (solMemVar dv) (solArg ctxt da) <> semi
  PL_Claim _ _ ct a k -> vsep [ check, iter ctxt k ]
    where check = case ct of CT_Assert -> emptyDoc
                             CT_Assume -> require
                             CT_Require -> require
                             CT_Possible -> emptyDoc
          require = solRequire (solArg ctxt a) <> semi
  PL_LocalIf _ ca t f k -> vsep [ solIf ca' t' f', iter ctxt k ]
    where ca' = solArg ctxt ca
          t' = solPLTail ctxt t
          f' = solPLTail ctxt f

solPLTail :: SolCtxt a -> PLTail -> Doc a
solPLTail ctxt (PLTail m) = solCom solPLTail ctxt m

solTransfer :: SolCtxt a -> DLArg -> DLArg -> Doc a
solTransfer ctxt who amt =
  (solArg ctxt who) <> "." <> solApply "transfer" [solArg ctxt amt] <> semi

solEvent :: Int -> [DLVar] -> Doc a
solEvent which args =
  "event" <+> solApply (solMsg_evt which) (solDecl (solType T_UInt256) "_bal" : map solArgDecl args) <> semi

solEventEmit :: SolCtxt a -> Int -> [DLVar] -> Doc a
solEventEmit ctxt which msg =
  "emit" <+> solApply (solMsg_evt which) (balancep : map (solVar ctxt) msg) <> semi
  where balancep = solCPrimApply BALANCE []

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

solCTail :: SolCtxt a -> CTail -> Doc a
solCTail ctxt = \case
  CT_Com m -> solCom solCTail ctxt m
  CT_Seqn _ p k ->
    vsep [ solPLTail ctxt p
         , solCTail ctxt k ]
  CT_If _ ca t f ->
    solIf (solArg ctxt ca) (solCTail ctxt t) (solCTail ctxt f)
  CT_Transfer _ who amt k ->
    vsep [ solTransfer ctxt who amt
         , solCTail ctxt k ]
  CT_Wait _ svs ->
    solSet ("current_state") (solHashState ctxt HM_Set svs)
  CT_Jump _ which svs asn ->
    solApply (solLoop_fun which) ((map (solVar ctxt) svs) ++ (solAsn ctxt asn)) <> semi
  CT_Halt _ ->
    vsep [ solSet ("current_state") ("0x0")
         , solApply "selfdestruct" ["msg.sender"] <> semi
         ]

solFrame :: Int -> S.Set DLVar -> (Doc a, Doc a)
solFrame i sim = if null var_decls then (emptyDoc, emptyDoc) else (frame_defp, frame_declp)
  where
    framei = "_F" ++ show i
    frame_declp = (pretty $ framei ++ " memory _f") <> semi
    frame_defp = pretty ("struct " ++ framei) <+> solBraces (vsep var_decls)
    var_decls = map mk_var_decl $ S.elems sim
    mk_var_decl v = solFieldDecl v <> semi

manyVars_m :: (a -> S.Set DLVar) -> PLCommon a -> S.Set DLVar
manyVars_m iter = \case
  PL_Return {} -> mempty
  PL_Let _ lc dv _ k -> mdv <> iter k
    where mdv = case lc of PL_Once -> mempty
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

solCTail_top :: VarMap a -> Int -> [DLVar] -> CTail -> (SolCtxt a, Doc a, Doc a, Doc a)
solCTail_top basem which vs ct = (ctxt, frameDefn, frameDecl, solCTail ctxt ct)
  where argsm = M.fromList $ map (\v -> (v, solRawVar v)) vs
        mvars = manyVars_c ct
        mvarsm = M.fromList $ map (\v -> (v, solMemVar v)) $ S.toList mvars
        (frameDefn, frameDecl) = solFrame which mvars
        ctxt = SolCtxt { ctxt_handler_num = which
                       , ctxt_varm = mvarsm <> argsm <> basem }

solHandler :: forall a . Int -> CHandler -> Doc a
solHandler which (C_Handler _at interval fs prev svs msg ct) = vsep [ evtDefn, frameDefn, funDefn ]
  where vs = svs ++ msg
        (ctxt, frameDefn, frameDecl, ctp ) = solCTail_top fromm which vs ct
        evtDefn = solEvent which msg
        argDefs = (solDecl (solType T_UInt256) solLastBlock) : map solArgDecl vs
        ret = "external payable"
        funDefn = solFunction (solMsg_fun which) argDefs ret body
        body = vsep [ (solRequire $ solEq ("current_state") (solHashState ctxt (HM_Check prev) svs)) <> semi
                    , frameDecl
                    , fromCheck
                    , timeoutCheck
                    , ctp
                    , solEventEmit ctxt which msg
                    ]
        (fromm, fromCheck) =
          case fs of
            FS_Join from -> ((M.singleton from "msg.sender"), emptyDoc)
            FS_Again from -> (mempty, (solRequire $ solEq ("msg.sender") (solVar ctxt from)) <> semi)
        timeoutCheck = solRequire (solBinOp "&&" int_fromp int_top) <> semi
          where CBetween from to = interval
                int_fromp = check True from
                int_top = check False to
                check sign mv =
                  case mv of
                    [] -> "true"
                    mvs -> solBinOp (if sign then ">=" else "<") solBlockNumber (foldl' (solBinOp "+") solLastBlock (map (solArg ctxt) mvs))

solHandler which (C_Loop _at svs msg ct) = vsep [ frameDefn, funDefn ]
  where vs = svs ++ msg
        (_, frameDefn, frameDecl, ctp) = solCTail_top mempty which vs ct
        argDefs = map solArgDecl vs
        ret = "internal"
        funDefn = solFunction (solLoop_fun which) argDefs ret body
        body = vsep [frameDecl, ctp]

solHandlers :: CHandlers -> Doc a
solHandlers (CHandlers hs) = vsep_with_blank $ map (uncurry solHandler) $ M.toList hs

solPLProg :: PLProg -> Doc a
solPLProg (PLProg _ _ (CPProg at hs)) =
  vsep_with_blank $ [preamble, solVersion, solStdLib, ctcp]
  where
    ctcp =
      solContract "ReachContract is Stdlib" $
        ctcbody
    ctcbody = vsep_with_blank $ [state_defn, consp, solHandlers hs]
    consp = solApply "constructor" [] <+> "public payable" <+> solBraces consbody
    ctxt0 = SolCtxt { ctxt_handler_num = 0
                    , ctxt_varm = mempty }
    consbody = solCTail ctxt0 (CT_Wait at [])
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

extract :: Value -> ConnectorResult
extract v = case fromJSON v of
  Error e -> error e -- XXX
  Success CompiledSolRec {csrAbi, csrCode} ->
    M.fromList
      [ ( "ETH"
        , M.fromList
            [ ("ABI", csrAbi)
            , ("Bytecode", "0x" <> csrCode) ] ) ]

compile_sol :: FilePath -> IO ConnectorResult
compile_sol solf = do
  (ec, stdout, stderr) <-
    readProcessWithExitCode "solc" ["--optimize", "--combined-json", "abi,bin", solf] []
  let show_output = "STDOUT:\n" ++ stdout ++ "\nSTDERR:\n" ++ stderr ++ "\n"
  case ec of
    ExitFailure _ -> die $ "solc failed:\n" ++ show_output
    ExitSuccess ->
      case (eitherDecode $ LB.pack stdout) of
        Right v -> return $ extract v
        Left err ->
          die $ "solc failed to produce valid output:\n" ++ show_output
              ++ "Decode:\n" ++ err ++ "\n"
  
connect_eth :: Connector
connect_eth outn pl = do
  let solf = outn "sol"
  writeFile solf (show (solPLProg pl))
  compile_sol solf
