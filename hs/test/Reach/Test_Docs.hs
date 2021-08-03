module Reach.Test_Docs ( spec_errorCodes ) where

import           Control.Monad                (when)
import           Data.List                    ((\\))
import           Generic.Data                 (gconNum)
import           Reach.AST.Base               (ImpossibleError (..), errPrefix)
import           Reach.Connector              (ConnectorError (..))
import           Reach.Connector.ETH_Solidity (connect_eth)
import           Reach.EPP                    (EPPError (..))
import           Reach.Eval.Error             (EvalError (..))
import qualified Reach.Linearize              as Linearize
import           Reach.PackageImport          (PkgError (..))
import           Reach.Parser                 (ParserError (..))
import           Reach.Util                   (makeErrCode)
import           System.Directory             (getDirectoryContents,
                                               removePathForcibly)
import           System.Exit                  (ExitCode (ExitFailure, ExitSuccess))
import           System.FilePath              (combine)
import           System.Process               (readProcessWithExitCode)
import           Test.Hspec                   (Spec, after_, before_, describe,
                                               it, shouldBe)

docName :: String
docName = "ref-error-codes"

outputDir :: FilePath
outputDir = "."

buildDir :: FilePath
buildDir = combine outputDir docName

-- For each type of Error:
--  Get the error code prefix (use random constructor)
--  Get the number of data constructors in type
allErrorCodes :: [(String, Int)]
allErrorCodes =
  [ (errPrefix Err_Block_Variable, gconNum @EvalError)
  , (errPrefix (Err_IntLiteralRange connect_eth 0 0 0), gconNum @ConnectorError)
  , (errPrefix Err_ContinueDomination, gconNum @EPPError)
  , (errPrefix (Linearize.Err_Unreachable ""), gconNum @Linearize.Error)
  , (errPrefix Err_Unauthorized, gconNum @PkgError)
  , (errPrefix Err_Parse_JSIdentNone, gconNum @ParserError)
  , (errPrefix Err_Impossible_InspectForall, gconNum @ImpossibleError)
  ]

buildDocs :: IO ()
buildDocs = do
  (errCode, _, stderr') <- readProcessWithExitCode "raco"
        ["scribble", "--dest", outputDir, "--htmls", "../docs-src/" <> docName <> ".scrbl"] ""
  case errCode of
    ExitSuccess   -> return ()
    ExitFailure n -> error $ show n <> ": " <> stderr'

destroyDocs :: IO ()
destroyDocs = removePathForcibly buildDir

getFilename :: String -> Int -> [String]
getFilename errTy numOfConstrs =
  map ((<> ".html") . makeErrCode errTy) [0 .. numOfConstrs - 1]

isMissingDocs :: IO Bool
isMissingDocs = do
  actual <- getDirectoryContents buildDir
  let expected = concatMap (uncurry getFilename) allErrorCodes
  let missing = expected \\ actual
  let isMissingCases = not $ null missing
  when isMissingCases $ do
    putStrLn $ "These error codes do not have docs: " <> show missing
  return isMissingCases

spec_errorCodes :: Spec
spec_errorCodes =
  before_ buildDocs $
    after_ destroyDocs $
      describe "error codes" $
        it "have docs" $
          isMissingDocs >>= shouldBe False
