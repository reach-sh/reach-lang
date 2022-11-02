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
    ctcs <- o .: "contracts"
    let ctcs' = kmToM ctcs
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
    csrCode <- ctc .: "bin"
    return $ CompiledSolRec {..}

compile_sol_parse :: BS.ByteString -> Either String CompiledSolRecsM
compile_sol_parse stdout =
  case eitherDecodeStrict stdout of
    Left m -> Left $ "It produced invalid JSON output, which failed to decode with the message:\n" <> m
    Right (CompiledSolRecs xs) ->
      return xs

compile_sol_extract :: String -> String -> BS.ByteString -> Either String CompiledSolRec
compile_sol_extract solf cn stdout = do
  xs <- compile_sol_parse stdout
  let k = s2t $ solf <> ":" <> cn
  let ks = M.keys xs
  let xs' = M.filterWithKey (\k' _ -> T.isSuffixOf k' k) xs
  case M.toAscList xs' of
    [ (_, x) ] -> Right x
    _ -> Left $ "Expected contracts object to have unique key " <> show k <> " but had " <> show (M.keys xs') <> " from " <> show ks


try_compile_sol :: FilePath -> String -> Maybe (Maybe Int) -> IO (Either String (String, CompiledSolRec))
try_compile_sol solf cn opt = do
  let o = (<>) ["--optimize"]
  let (me, oargs) =
        case opt of
          Nothing -> ("o0", [])
          Just mo ->
            case mo of
              Nothing -> ("oD", o [])
              Just r -> ("o" <> show r, o ["--optimize-runs=" <> show r])
  let fmts = "abi,bin"
  let args = oargs <> ["--combined-json", fmts, solf]
  -- putStrLn $ "solc " <> (show args)
  (ec, stdout, stderr) <- liftIO $ readProcessWithExitCode "solc" args mempty
  let show_output =
        case stdout == "" of
          True -> stderr
          False -> "STDOUT:\n" <> stdout <> "\nSTDERR:\n" <> stderr
  case ec of
    ExitFailure _ -> return $ Left $ bunpack show_output
    ExitSuccess -> return $ fmap ((,) me) $ compile_sol_extract solf cn stdout

compile_sol_ :: FilePath -> String -> IO (Either String (String, CompiledSolRec))
compile_sol_ solf cn = do
  let shortEnough (_, CompiledSolRec {..}) =
        case len <= (2 * maxContractLen) of
          True -> Nothing
          False -> Just len
        where
          len = T.length csrCode
  let try = try_compile_sol solf cn
  let merr = \case
        Left e -> emitWarning Nothing $ W_SolidityOptimizeFailure e
        Right _ -> return ()
  let desperate = \case
        Right x -> \_ _ -> return $ Right x
        e@(Left bado) -> \case
          Right y -> \_ -> merr e >> return (Right y)
          Left _ -> \case
            Right z -> merr e >> return (Right z)
            Left _ -> return $ Left $ "The Solidity compiler failed with the message:\n" <> bado
  let tryN eA e1 =
        try Nothing >>= \case
          Right oN -> do
            let roN = Right oN
            case shortEnough oN of
              Nothing -> merr eA >> return roN
              Just _lenN -> desperate eA e1 roN
          Left rN -> desperate eA e1 (Left rN)
  let try1 eA =
        try (Just $ Just 1) >>= \case
          Right o1 -> do
            let ro1 = Right o1
            case shortEnough o1 of
              Nothing -> merr eA >> return ro1
              Just _len1 -> tryN eA ro1
          Left r1 -> tryN eA (Left r1)
  let tryA =
        try (Just Nothing) >>= \case
          Right oA -> do
            let roA = Right oA
            case shortEnough oA of
              Nothing -> return roA
              Just _lenA -> try1 roA
          Left rA -> try1 $ Left rA
  tryA
