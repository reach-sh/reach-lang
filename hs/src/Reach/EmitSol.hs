{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Reach.EmitSol where

import Data.List (intersperse)
import Data.Text.Prettyprint.Doc
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Char8 as BS
import System.Process
import System.Exit
import Data.Aeson
import Data.FileEmbed
import Paths_reach (version)
import Data.Version (showVersion)

import Reach.AST

{- AST add-ons
 -}

solMsg_evt :: Show i => i -> String
solMsg_evt i = "e" ++ show i

solMsg_fun :: Show i => i -> String
solMsg_fun i = "m" ++ show i

solLoop_fun :: Show i => i -> String
solLoop_fun i = "l" ++ show i

solType :: BaseType -> String
solType BT_UInt256 = "uint256"
solType BT_Bool = "bool"
solType BT_Bytes = "bytes"
solType BT_Address = "address payable"

{- De-ANF information

   We could incorporate this into the structures directly.
 -}

type CCounts = M.Map BLVar Int

cmerge :: CCounts -> CCounts -> CCounts
cmerge m1 m2 = M.unionWith (+) m1 m2

cmerges :: [CCounts] -> CCounts
cmerges [] = M.empty
cmerges (m1:ms) = cmerge m1 $ cmerges ms

usesBLArg :: BLArg a -> CCounts
usesBLArg (BL_Con _ _) = M.empty
usesBLArg (BL_Var _ bv) = M.singleton bv 1

usesCExpr :: CExpr a -> CCounts
usesCExpr (C_PrimApp _ _ al) = cmerges $ map usesBLArg al

usesCStmt :: CStmt a  -> CCounts
usesCStmt (C_Claim _ _ a) = usesBLArg a
usesCStmt (C_Transfer _ _ a) = usesBLArg a

usesBLVars :: [BLVar] -> CCounts
usesBLVars vs = M.fromList $ map (\v->(v,1)) vs

usesCTail :: CTail a -> CCounts
usesCTail (C_Halt _) = M.empty
usesCTail (C_Wait _ _ vs) = usesBLVars vs
usesCTail (C_If _ ca tt ft) = cmerges [ cs1, cs2, cs3 ]
  where cs1 = usesBLArg ca
        cs2 = usesCTail tt
        cs3 = usesCTail ft
usesCTail (C_Let _ _ ce kt) = cmerge cs1 cs2
  where cs1 = usesCExpr ce
        cs2 = usesCTail kt
usesCTail (C_Do _ cs kt) = cmerge cs1 cs2
  where cs1 = usesCStmt cs
        cs2 = usesCTail kt
usesCTail (C_Jump _ _ vs _ a) = cmerge cs1 cs2
  where cs1 = usesBLVars vs
        cs2 = usesBLArg a

{- Compilation to Solidity

   The handler program becomes a contract factory where the first
   interaction is tied into the contract creation. The contract has a
   different function for each consensus block. The arguments are the
   input variables. At the end, the output variables are emitted via
   an event.

   In the future, when we compile to EVM directly, it won't work that
   way though. Instead, we'll do dispatch ourselves.
 -}

type SolRenaming a = M.Map BLVar (Doc a)
type SolInMemory = S.Set BLVar

solArgType :: BaseType -> String
solArgType BT_Bytes = "bytes calldata"
solArgType t = solType t

solVarType :: BaseType -> String
solVarType BT_Bytes = "bytes memory"
solVarType t = solType t

solBraces :: Doc a -> Doc a
solBraces body = braces (nest 2 $ hardline <> body <> space)

solFunction :: String -> [Doc a] -> Doc a -> Doc a -> Doc a
solFunction name args ret body =
  "function" <+> solApply name args <+> ret <+> solBraces body

solEvent :: String -> [Doc a] -> Doc a
solEvent name args =
  "event" <+> solApply name args <> semi

solDecl :: String -> Doc a -> Doc a
solDecl ty n = pretty ty <+> n

solRawVar :: BLVar -> Doc a
solRawVar (n, _) = pretty $ "v" ++ show n

solVar :: SolInMemory -> SolRenaming a -> BLVar -> Doc a
solVar sim ρ bv = p
  where p = case M.lookup bv ρ of
              Nothing -> bvp
              Just v -> v
        bvp = if S.member bv sim then
                solMemVar bv
              else
                solRawVar bv

solNum :: Show n => n -> Doc a
solNum i = pretty $ "uint256(" ++ show i ++ ")"

solCon :: Constant -> Doc a
solCon (Con_I i) = solNum i
solCon (Con_B True) = "true"
solCon (Con_B False) = "false"
solCon (Con_BS s) = pretty $ "\"" ++ show s ++ "\""

solArg :: SolInMemory -> SolRenaming a -> BLArg b -> Doc a
solArg sim ρ (BL_Var _ v) = solVar sim ρ v
solArg _ _ (BL_Con _ c) = solCon c

solFieldDecl :: BLVar -> Doc a
solFieldDecl bv@(_, (_, bt)) = solDecl (solType bt) (solRawVar bv)

solArgDecl :: BLVar -> Doc a
solArgDecl bv@(_, (_, bt)) = solDecl (solArgType bt) (solRawVar bv)

solMemVar :: BLVar -> Doc a
solMemVar bv = "_f." <> solRawVar bv

solVarDecl :: BLVar -> Doc a
solVarDecl bv = solMemVar bv

solContract :: String -> Doc a -> Doc a
solContract s body = "contract" <+> pretty s <+> solBraces body

solVersion :: Doc a
solVersion = "pragma solidity ^0.5.11;"

solStdLib :: Doc a
solStdLib = pretty $ BS.unpack $(embedFile "../sol/stdlib.sol")

solApply :: String -> [Doc a] -> Doc a
solApply f args = pretty f <> parens (hcat $ intersperse (comma <> space) args)

solRequire :: Doc a -> Doc a
solRequire a = solApply "require" [ a ]

solBinOp :: String -> Doc a -> Doc a -> Doc a
solBinOp o l r = l <+> pretty o <+> r

solEq :: Doc a -> Doc a -> Doc a
solEq = solBinOp "=="

solSet :: Doc a -> Doc a -> Doc a
solSet = solBinOp "="

solLastBlock :: Doc a
solLastBlock = "_last"

solBlockNumber :: Doc a
solBlockNumber = "uint256(block.number)"

solHash :: [Doc a] -> Doc a
solHash a = solApply "uint256" [ solApply "keccak256" [ solApply "abi.encodePacked" a ] ]

solHashState :: SolInMemory -> SolRenaming a -> Int -> Bool -> [BLVar] -> Doc a
solHashState sim ρ i check svs = solHash $ (solNum i) : which_last : (map (solVar sim ρ) svs)
  where which_last = if check then solLastBlock else solBlockNumber

solPrimApply :: C_Prim -> [Doc a] -> Doc a
solPrimApply pr args =
  case pr of
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
    IF_THEN_ELSE -> case args of
                      [ c, t, f ] -> c <+> "?" <+> t <+> ":" <+> f
                      _ -> spa_error ()
    UINT256_TO_BYTES -> solApply "abi.encodePacked" args
    DIGEST -> case args of
                [ a ] -> solHash [a]
                _ -> spa_error ()
    BYTES_EQ -> binOp "=="
    BYTES_LEN -> solApply "BYTES_LEN" args
    BCAT -> solApply "BCAT" args
    BCAT_LEFT -> solApply "BCAT_LEFT" args
    BCAT_RIGHT -> solApply "BCAT_RIGHT" args
    BALANCE -> "address(this).balance"
    TXN_VALUE -> "msg.value"
  where binOp op = case args of
          [ l, r ] -> solBinOp op l r
          _ -> spa_error ()
        spa_error () = error "solPrimApply"

solCExpr :: SolInMemory -> SolRenaming a -> CExpr b -> Doc a
solCExpr sim ρ (C_PrimApp _ pr al) = solPrimApply pr $ map (solArg sim ρ) al

solCStmt :: SolInMemory -> SolRenaming a -> CStmt b -> Doc a
solCStmt _ _ (C_Claim _ CT_Possible _) = emptyDoc
solCStmt _ _ (C_Claim _ CT_Assert _) = emptyDoc
solCStmt sim ρ (C_Claim _ _ a) = (solRequire $ solArg sim ρ a) <> semi <> hardline
solCStmt sim ρ (C_Transfer _ p a) = solVar sim ρ p <> "." <> solApply "transfer" [ solArg sim ρ a ] <> semi <> hardline

solCTail :: Doc a -> SolInMemory -> SolRenaming a -> CCounts -> CTail b -> Doc a
solCTail emitp sim ρ ccs ct =
  case ct of
    C_Halt _ ->
      emitp <> vsep [ solSet ("current_state") ("0x0") <> semi,
                      solApply "selfdestruct" [ "msg.sender" ] <> semi ]
    C_Wait _ last_i svs ->
      emitp <> (solSet ("current_state") (solHashState sim ρ last_i False svs)) <> semi
    C_If _ ca tt ft ->
      "if" <+> parens (solArg sim ρ ca) <+> bp tt <> hardline <> "else" <+> bp ft
      where bp at = solBraces $ solCTail emitp sim ρ ccs at
    C_Let _ bv ce kt ->
      case M.lookup bv ccs of
        Just 0 -> solCTail emitp sim ρ ccs kt
        Just 1 -> solCTail emitp sim ρ' ccs kt
          where ρ' = M.insert bv (parens (solCExpr sim ρ ce)) ρ
        _ -> vsep [ solVarDecl bv <+> "=" <+> solCExpr sim ρ ce <> semi,
                    solCTail emitp sim ρ ccs kt ]
    C_Do _ cs kt -> solCStmt sim ρ cs <> (solCTail emitp sim ρ ccs kt)
    C_Jump _ which vs _ a ->
      emitp <> solApply (solLoop_fun which) ((map (solVar sim ρ) vs) ++ [ solArg sim ρ a ]) <> semi

solFrame :: Int -> SolInMemory -> (Doc a, Doc a)
solFrame i sim = if null var_decls then (emptyDoc, emptyDoc) else (frame_defp, frame_declp)
  where framei = "_F" ++ show i
        frame_declp = (pretty $ framei ++ " memory _f") <> semi
        frame_defp = pretty ("struct " ++ framei) <+> solBraces (vsep var_decls)
        var_decls = map mk_var_decl $ S.elems sim
        mk_var_decl v = solFieldDecl v <> semi

makeSIM :: CCounts -> [BLVar] -> SolInMemory
makeSIM ccs vs = S.unions $ map f $ M.toList ccs
  where f (v, c) =
          if c <= 1 || elem v vs then
            S.empty
          else
            S.singleton v

solHandler :: CHandler b -> Doc a
solHandler (C_Handler _ (is_join, from) is_timeout (last_i, svs) msg delay body i) = vsep [ evtp, frame_defp, funp ]
  where msg_rs = map solRawVar msg
        msg_ds = map solArgDecl msg
        msg_eds = map solFieldDecl msg
        arg_ds = (solDecl (solType BT_UInt256) solLastBlock) : map solArgDecl svs ++ msg_ds
        evts = solMsg_evt i
        evtp = solEvent evts msg_eds
        sim0 = makeSIM ccs (svs ++ msg)
        sim = if is_join then S.insert from sim0 else sim0
        (frame_defp, frame_declp) = solFrame i sim
        funp = solFunction (solMsg_fun i) arg_ds retp bodyp
        retp = "external payable"
        emitp = "emit" <+> solApply evts msg_rs <> semi <> hardline
        ccs = usesCTail body
        ρ = M.empty
        fromp = if is_join then
                  solVarDecl from <+> "=" <+> "msg.sender"
                else
                  solRequire $ solEq ("msg.sender") (solVar sim ρ from)
        bodyp = vsep [ (solRequire $ solEq ("current_state") (solHashState sim ρ last_i True svs)) <> semi,
                       frame_declp,
                       fromp <> semi,
                       (solRequire $ solBinOp (if is_timeout then ">=" else "<") solBlockNumber (solBinOp "+" solLastBlock (solArg sim ρ delay))) <> semi,
                       solCTail emitp sim ρ ccs body ]
solHandler (C_Loop _ svs arg _inv body i) = vsep [ frame_defp, funp ]
  where funp = solFunction (solLoop_fun i) arg_ds retp (vsep [ frame_declp, bodyp ])
        sim = makeSIM ccs (arg : svs)
        (frame_defp, frame_declp) = solFrame i sim
        arg_ds = map solArgDecl svs ++ [ solArgDecl arg ]
        retp = "internal"
        ccs = usesCTail body
        bodyp = solCTail "" sim M.empty ccs body

solHandlers :: [CHandler b] -> Doc a
solHandlers hs = vsep $ intersperse emptyDoc $ map solHandler hs

vsep_with_blank :: [Doc a] -> Doc a
vsep_with_blank l = vsep $ intersperse emptyDoc l

emit_sol :: BLProgram b -> Doc a
emit_sol (BL_Prog _ _ (C_Prog ca hs)) =
  vsep_with_blank $ [ preamble, solVersion, solStdLib, ctcp ]
  where ctcp = solContract "ReachContract is Stdlib"
               $ ctcbody
        ctcbody = vsep $ [state_defn, emptyDoc, consp, emptyDoc, solHandlers hs]
        consp = solApply "constructor" [] <+> "public payable" <+> solBraces consbody
        consbody = solCTail emptyDoc S.empty M.empty M.empty (C_Wait ca 0 [])
        state_defn = "uint256 current_state;"
        preamble = pretty $ "// Automatically generated with Reach " ++ showVersion version

type CompiledSol = (String, String)

extract :: Value -> CompiledSol
extract v = (abi, code)
  where Object hm = v
        Just (Object ctcs) = HM.lookup "contracts" hm
        [ _, thectc ] = HM.keys ctcs
        Just (Object ctc) = HM.lookup thectc ctcs
        Just (String abit) = HM.lookup "abi" ctc
        abi = T.unpack abit
        Just (String codebodyt) = HM.lookup "bin" ctc
        code = "\"0x" ++ T.unpack codebodyt ++ "\""

compile_sol :: String -> BLProgram a -> IO CompiledSol
compile_sol solf blp = do
  writeFile solf (show (emit_sol blp))
  ( ec, stdout, stderr ) <- readProcessWithExitCode "solc" ["--optimize", "--combined-json", "abi,bin", solf] []
  case ec of
    ExitFailure _ ->
      die $ "solc errored:\n"
      ++ "STDOUT:\n" ++ stdout ++ "\n"
      ++ "STDERR:\n" ++ stderr ++ "\n"
    ExitSuccess ->
      case (eitherDecode $ B.pack stdout) of
        Right v -> return $ extract v
        Left err ->
          die $ "solc failed to produce valid output:\n"
          ++ "STDOUT:\n" ++ stdout ++ "\n"
          ++ "STDERR:\n" ++ stderr ++ "\n"
          ++ "Decode Error:\n" ++ err ++ "\n"
