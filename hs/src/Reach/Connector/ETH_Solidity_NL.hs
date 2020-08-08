module Reach.Connector.ETH_Solidity_NL (connect_eth) where

import Data.Aeson
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import System.Exit
import System.Process
import qualified Data.Map.Strict as M
import Data.List (find, intersperse)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
---import Debug.Trace
import Data.Version (showVersion)
import Paths_reach (version)
import Reach.EmbeddedFiles
import Reach.NL_AST
import Reach.Connector
import Data.Text.Prettyprint.Doc

--- Pretty helpers

vsep_with_blank :: [Doc a] -> Doc a
vsep_with_blank l = vsep $ intersperse emptyDoc l

--- Solidity helpers

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
solSet = solBinOp "="

--- Runtime helpers

solLastBlock :: Doc a
solLastBlock = "_last"

solBlockNumber :: Doc a
solBlockNumber = "uint256(block.number)"

solHash :: [Doc a] -> Doc a
solHash a = solApply "uint256" [solApply "keccak256" [solApply "abi.encodePacked" a]]

--- Compiler

data SolCtxt = SolCtxt { }

solCTail :: SolCtxt -> CTail -> Doc a
solCTail _ctxt _ct = error "XXX"

solHandler :: Int -> CHandler -> Doc a
solHandler _h = error "XXX"

solHandlers :: CHandlers -> Doc a
solHandlers (CHandlers hs) = vsep_with_blank $ map (uncurry solHandler) $ M.toList hs

solPL :: PLProg -> Doc a
solPL (PLProg _ _ (CPProg at hs)) =
  vsep_with_blank $ [preamble, solVersion, solStdLib, ctcp]
  where
    ctcp =
      solContract "ReachContract is Stdlib" $
        ctcbody
    ctcbody = vsep_with_blank $ [state_defn, consp, solHandlers hs]
    consp = solApply "constructor" [] <+> "public payable" <+> solBraces consbody
    ctxt0 = SolCtxt {}
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
      
connect_eth :: Connector
connect_eth outn pl = do
  let solf = outn "sol"
  writeFile solf (show (solPL pl))
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
