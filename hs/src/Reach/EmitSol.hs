{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Reach.EmitSol where

import Data.List (intersperse)
import Data.Text.Prettyprint.Doc
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Char8 as BS
import System.Process
import System.Exit
import Data.Aeson
import Data.FileEmbed

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
solType AT_UInt256 = "uint256"
solType AT_Bool = "bool"
solType AT_Bytes = "bytes"

reachAddress :: String
reachAddress = "0x9527B6b278aBf20Ff7D69b5FE0696eb937e633e6"

{- De-ANF information

   We could incorporate this into the structures directly.
 -}

type CCounts = M.Map BLVar Int

cmerge :: CCounts -> CCounts -> CCounts
cmerge m1 m2 = M.unionWith (+) m1 m2

cmerges :: [CCounts] -> CCounts
cmerges [] = M.empty
cmerges (m1:ms) = cmerge m1 $ cmerges ms

usesBLArg :: BLArg -> CCounts
usesBLArg (BL_Con _) = M.empty
usesBLArg (BL_Var bv) = M.singleton bv 1

usesCExpr :: CExpr -> CCounts
usesCExpr (C_PrimApp _ al) = cmerges $ map usesBLArg al

usesCStmt :: CStmt -> CCounts
usesCStmt (C_Claim _ a) = usesBLArg a
usesCStmt (C_Transfer _ a) = usesBLArg a

usesCTail :: CTail -> CCounts
usesCTail (C_Halt) = M.empty
usesCTail (C_Wait _ vs) = M.fromList $ map (\v->(v,1)) vs
usesCTail (C_If ca tt ft) = cmerges [ cs1, cs2, cs3 ]
  where cs1 = usesBLArg ca
        cs2 = usesCTail tt
        cs3 = usesCTail ft
usesCTail (C_Let _ ce kt) = cmerge cs1 cs2
  where cs1 = usesCExpr ce
        cs2 = usesCTail kt
usesCTail (C_Do cs kt) = cmerge cs1 cs2
  where cs1 = usesCStmt cs
        cs2 = usesCTail kt
usesCTail (C_Jump which vs a) = cmerge cs1 cs2
  where cs1 = usesCTail (C_Wait which vs)
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

solArgType :: BaseType -> String
solArgType AT_Bytes = "bytes calldata"
solArgType t = solType t

solVarType :: BaseType -> String
solVarType AT_Bytes = "bytes memory"
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
solRawVar (n, _, _) = pretty $ "v" ++ show n

solVar :: SolRenaming a -> BLVar -> Doc a
solVar ρ bv = p
  where p = case M.lookup bv ρ of
              Nothing -> solRawVar bv
              Just v -> v

solNum :: Show n => n -> Doc a
solNum i = pretty $ "uint256(" ++ show i ++ ")"

solCon :: Constant -> Doc a
solCon (Con_I i) = solNum i
solCon (Con_B True) = "true"
solCon (Con_B False) = "false"
solCon (Con_BS s) = pretty $ "\"" ++ show s ++ "\""

solArg :: SolRenaming a -> BLArg -> Doc a
solArg ρ (BL_Var v) = solVar ρ v
solArg _ (BL_Con c) = solCon c

solPartVar :: Participant -> Doc a
solPartVar p = pretty $ "p" ++ p

solFieldDecl :: BLVar -> Doc a
solFieldDecl bv@(_, _, bt) = solDecl (solType bt) (solRawVar bv)

solArgDecl :: BLVar -> Doc a
solArgDecl bv@(_, _, bt) = solDecl (solArgType bt) (solRawVar bv)

solVarDecl :: BLVar -> Doc a
solVarDecl bv@(_, _, bt) = solDecl (solVarType bt) (solRawVar bv)

solPartDecl :: Participant -> Doc a
solPartDecl p = solDecl "address payable" (solPartVar p)

solContract :: String -> Doc a -> Doc a
solContract s body = "contract" <+> pretty s <+> solBraces body

solVersion :: Doc a
solVersion = "pragma solidity ^0.5.2;"

withoutSolVersionPragmas :: BS.ByteString -> String
withoutSolVersionPragmas b = (BS.unpack . BS.unlines) s
  where s = filter (not . ("pragma solidity " `BS.isInfixOf`)) $ BS.lines b

solStdLib :: Doc a
solStdLib = pretty $ withoutSolVersionPragmas $(embedFile "../sol/stdlib.sol")

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

solHash :: [Doc a] -> Doc a
solHash a = solApply "uint256" [ solApply "keccak256" [ solApply "abi.encodePacked" a ] ]

solHashState :: SolRenaming a -> Int -> [Participant] -> [BLVar] -> Doc a
solHashState ρ i ps svs = solHash $ (solNum i) : (map solPartVar ps) ++ (map (solVar ρ) svs)

solRequireSender :: Participant -> Doc a
solRequireSender from = solRequire $ solEq ("msg.sender") (solPartVar from)

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

solCExpr :: SolRenaming a -> CExpr -> Doc a
solCExpr ρ (C_PrimApp pr al) = solPrimApply pr $ map (solArg ρ) al

solCStmt :: SolRenaming a -> CStmt -> Doc a
solCStmt _ (C_Claim CT_Possible _) = emptyDoc
solCStmt _ (C_Claim CT_Assert _) = emptyDoc
solCStmt ρ (C_Claim _ a) = (solRequire $ solArg ρ a) <> semi <> hardline
solCStmt ρ (C_Transfer p a) = solPartVar p <> "." <> solApply "transfer" [ solArg ρ a ] <> semi <> hardline

solCTail :: [Participant] -> Doc a -> SolRenaming a -> CCounts -> CTail -> Doc a
solCTail ps emitp ρ ccs ct =
  case ct of
    C_Halt ->
      emitp <> vsep [ solSet ("current_state") ("0x0") <> semi,
                      solApply "selfdestruct" [ solApply "address" [ pretty reachAddress ] ] <> semi ]
    C_Wait i svs ->
      emitp <> (solSet ("current_state") (solHashState ρ i ps svs)) <> semi
    C_If ca tt ft ->
      "if" <+> parens (solArg ρ ca) <+> bp tt <> hardline <> "else" <+> bp ft
      where bp at = solBraces $ solCTail ps emitp ρ ccs at
    C_Let bv ce kt ->
      case M.lookup bv ccs of
        Just 0 -> solCTail ps emitp ρ ccs kt
        Just 1 -> solCTail ps emitp ρ' ccs kt
          where ρ' = M.insert bv (parens (solCExpr ρ ce)) ρ
        _ -> vsep [ solVarDecl bv <+> "=" <+> solCExpr ρ ce <> semi,
                    solCTail ps emitp ρ ccs kt ]
    C_Do cs kt -> solCStmt ρ cs <> (solCTail ps emitp ρ ccs kt)
    C_Jump which vs a ->
      emitp <> solApply (solLoop_fun which) ((map solPartVar ps) ++ (map solRawVar vs) ++ [ solArg ρ a ]) <> semi

solHandler :: [Participant] -> Int -> CHandler -> Doc a
solHandler ps i (C_Handler from svs msg body) = vsep [ evtp, funp ]
  where msg_rs = map solRawVar msg
        msg_ds = map solArgDecl msg
        msg_eds = map solFieldDecl msg
        arg_ds = map solPartDecl ps ++ map solArgDecl svs ++ msg_ds
        evts = solMsg_evt i
        evtp = solEvent evts msg_eds
        funp = solFunction (solMsg_fun i) arg_ds retp bodyp
        retp = "external payable"
        emitp = "emit" <+> solApply evts msg_rs <> semi <> hardline
        ccs = usesCTail body
        ρ = M.empty
        bodyp = vsep [ (solRequire $ solEq ("current_state") (solHashState ρ i ps svs)) <> semi,
                       solRequireSender from <> semi,
                       solCTail ps emitp ρ ccs body ]
solHandler ps i (C_Loop svs arg _inv body) = funp
  where funp = solFunction (solLoop_fun i) arg_ds retp bodyp
        arg_ds = map solPartDecl ps ++  map solArgDecl svs ++ [ solArgDecl arg ]
        retp = "internal"
        ccs = usesCTail body
        bodyp = solCTail ps "" M.empty ccs body

solHandlers :: [Participant] -> [CHandler] -> Doc a
solHandlers ps hs = vsep $ intersperse emptyDoc $ zipWith (solHandler ps) [0..] hs

vsep_with_blank :: [Doc a] -> Doc a
vsep_with_blank l = vsep $ intersperse emptyDoc l

emit_sol :: BLProgram -> Doc a
emit_sol (BL_Prog _ (C_Prog ps hs)) =
  vsep_with_blank $ [ solVersion, solStdLib, ctcp ]
  where ctcp = solContract "ReachContract is Stdlib"
               $ ctcbody
        ctcbody = vsep $ [state_defn, emptyDoc, consp, emptyDoc, solHandlers ps hs]
        consp = solApply "constructor" p_ds <+> "public payable" <+> solBraces consbody
        consbody = solCTail ps emptyDoc M.empty M.empty (C_Wait 0 [])
        state_defn = "uint256 current_state;"
        p_ds = map solPartDecl ps

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

compile_sol :: String -> BLProgram -> IO CompiledSol
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
