module Reach.Connector.ETH_solc
  ( compile_sol_
  , compile_sol_extract
  , CompiledSolRec(..)
  ) where

import Control.Monad.Reader
import Data.Aeson as Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.String
import qualified Data.Text as T
import Reach.Util
import Reach.Warning
import System.Exit
import System.Process.ByteString

maxContractLen :: Int
maxContractLen = 24576

newtype CompiledSolRecs = CompiledSolRecs CompiledSolRecsM

type CompiledSolRecsM = M.Map T.Text CompiledSolRec

instance FromJSON CompiledSolRecs where
  parseJSON = withObject "CompiledSolRecs" $ \o -> do
    let ctcs' = kmToM o
    CompiledSolRecs <$> mapM parseJSON ctcs'

data CompiledSolRec = CompiledSolRec
  { csrAbi :: T.Text
  , csrCode :: T.Text
  }

instance FromJSON CompiledSolRec where
  parseJSON = withObject "CompiledSolRec" $ \ctc -> do
    (abio :: Value) <- ctc .: "abi"
    -- Why are we re-encoding? ethers takes the ABI as a string, not an object.
    let cfg = defConfig {confIndent = Spaces 0, confCompare = compare}
    let csrAbi = T.pack $ LB.unpack $ encodePretty' cfg abio
    a <- ctc .: "evm"
    b <- a .: "bytecode"
    csrCode <- b .: "object"
    return $ CompiledSolRec {..}

data SolOutputFull = SolOutputFull
  { sofContracts :: M.Map T.Text CompiledSolRecs
  }

instance FromJSON SolOutputFull where
  parseJSON = withObject "SolOutputFull" $ \o -> do
    sofContracts <- o .: "contracts"
    return $ SolOutputFull {..}

theKey :: T.Text
theKey = "theReachKey"

type E x = Either String x

compile_sol_parse :: BS.ByteString -> E CompiledSolRecsM
compile_sol_parse stdout =
  case eitherDecodeStrict stdout of
    Left m -> Left $ "It produced invalid JSON output, which failed to decode with the message:\n" <> m
    Right (SolOutputFull cs) ->
      case M.lookup theKey cs of
        Nothing -> Left $ "The compilation key was missing"
        Just (CompiledSolRecs xs) -> return xs

compile_sol_extract :: String -> String -> BS.ByteString -> E CompiledSolRec
compile_sol_extract solf cn stdout = do
  xs <- compile_sol_parse stdout
  let k = s2t $ solf <> ":" <> cn
  let ks = M.keys xs
  let xs' = M.filterWithKey (\k' _ -> T.isSuffixOf k' k) xs
  case M.toAscList xs' of
    [ (_, x) ] -> Right x
    _ -> Left $ "Expected contracts object to have unique key " <> show k <> " but had " <> show (M.keys xs') <> " from " <> show ks

array :: ToJSON a => [a] -> Value
array = toJSONList

tj :: ToJSON a => a -> Value
tj = toJSON

data OptimizationPolicy = OP
  { opEnabled :: Bool
  , opRuns :: Int
  , opInliner :: Bool
  , opIR :: Bool
  , opSpecialSeq :: Bool
  }

policies :: [OptimizationPolicy]
policies =
  [ OP { opRuns = 1, .. }
  --, OP { opInliner = False, opRuns = 1, opSpecialSeq = True, .. }
  --, OP { .. }
  --, OP { opInliner = False, .. }
  --, OP { opInliner = False, opRuns = 1, .. }
  , OP { opIR = False, .. }
  , OP { opEnabled = False, .. }
  ]
  where
    opIR = True
    opEnabled = True
    opRuns = 1
    opInliner = True
    opSpecialSeq = False

try_compile_sol :: FilePath -> String -> OptimizationPolicy -> IO (E CompiledSolRec)
try_compile_sol solf cn (OP {..}) = do
  let theKey' = fromString $ T.unpack theKey
  let msteps =
        case opSpecialSeq of
          False -> []
          True -> [("optimizerSteps", tj $ concat
            -- Copied from https://github.com/ethereum/solidity/blob/ea78c8fd31b99451e663f06bbb9925da7bc22b03/libsolidity/interface/OptimiserSettings.h#L44
            -- The names come from https://github.com/ethereum/solidity/blob/ea78c8fd31b99451e663f06bbb9925da7bc22b03/libyul/optimiser/Suite.cpp#L248
            [ "dhfoDgvulfnTUtnIf" -- None of these can make stack problems worse
            , "["
            , "xa[r]EscLM" -- Turn into SSA and simplify
            , "cCTUtTOntnfDIul" -- Perform structural simplification
            , "Lcul" -- Simplify again
            , "Vcul [j]" -- Reverse SSA
            -- should have good "compilability" property here.
            , "Tpeul" -- Run functional expression inliner
            , "xa[rul]" -- Prune a bit more in SSA
            , "xa[r]cL" -- Turn into SSA again and simplify
            --, "gvif" -- Run full inliner
            , "CTUca[r]LSsTFOtfDnca[r]Iulc" -- SSA plus simplify
            , "]"
            , "jmul[jul] VcTOcul jmul" -- Make source short and pretty
            ])]
  let spec = object $
        [ ("language", "Solidity")
        , ("sources", object $
          [ (theKey', object $
            [ ("urls", array [ solf ])
            ])
          ])
        , ("settings", object $
          [ ("optimizer", object $
            [ ("enabled", tj opEnabled)
            , ("runs", tj opRuns)
            , ("details", object $
              [ ("peephole", tj True)
              , ("inliner", tj opInliner)
              , ("jumpdestRemover", tj True)
              , ("orderLiterals", tj True)
              , ("deduplicate", tj True)
              , ("cse", tj True)
              , ("constantOptimizer", tj True)
              , ("yul", tj True)
              , ("yulDetails", object $
                [ ("stackAllocation", tj True)
                ] <> msteps)
              ])
            ])
            , ("viaIR", tj opIR)
            , ("debug", object $
              [ ("revertStrings", "strip")
              , ("debugInfo", array ([]::[String]))
              ])
            , ("metadata", object $
              [ ("bytecodeHash", "none")
              ])
            , ("outputSelection", object $
              [ ("*", object $
                [ ("*", array $
                  ([ "abi"
                  , "evm.bytecode.object"
                  ] :: [String]))
                ])
          ])
          ])
        ]
  (ec, stdout, stderr) <-
    liftIO $ readProcessWithExitCode "solc" ["--standard-json"] $
      LB.toStrict $ encode spec
  BS.writeFile (solf <> ".json") stdout
  let show_output =
        case stdout == "" of
          True -> stderr
          False -> "STDOUT:\n" <> stdout <> "\nSTDERR:\n" <> stderr
  case ec of
    ExitFailure _ -> return $ Left $ bunpack show_output
    ExitSuccess -> return $ compile_sol_extract solf cn stdout

checkLen :: E CompiledSolRec -> E CompiledSolRec
checkLen = \case
  Left x -> Left x
  Right x@(CompiledSolRec {..}) ->
    case len <= maxContractLen of
      True -> Right x
      False -> Left $ "The bytecode exceeds the maximum limit; it is " <> show len <> ", but the limit is " <> show maxContractLen
    where
      len :: Int = floor $ (((fromIntegral $ T.length csrCode) / 2) :: Double)

compile_sol_ :: FilePath -> String -> IO (E CompiledSolRec)
compile_sol_ solf cn = try Nothing policies
  where
    try merr = \case
      [] -> return $ Left $ "The Solidity compiler failed with the message:\n" <> (fromMaybe (impossible "compile_sol_") merr)
      opt : more -> do
        case merr of
          Nothing -> return ()
          Just e -> emitWarning Nothing $ W_SolidityOptimizeFailure e
        let f = case more of
                  [] -> id
                  _ -> checkLen
        (f <$> try_compile_sol solf cn opt) >>= \case
          Right x -> return $ Right x
          Left bado -> try (Just bado) more
