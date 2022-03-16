module Reach.Test_Docs (spec_errorCodes, spec_baseAndStdlibIdentifiers) where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Aeson as A
import qualified Data.Map as M
import Generic.Data
import Reach.APICut (APICutError (..))
import Reach.AST.Base (ImpossibleError (..), errPrefix)
import Reach.Connector (ConnectorError (..))
import Reach.Connector.ALGO (AlgoError (..))
import Reach.EPP (EPPError (..))
import Reach.Eval.Error (EvalError (..))
import qualified Reach.Linearize as Linearize
import Reach.PackageImport (PkgError (..))
import Reach.Parser (ParserError (..))
import Reach.Util
import Reach.Warning (Warning (W_ExternalObject))
import System.Directory
import System.Process
import Test.Hspec

rshDocDir :: String
rshDocDir = "../docs/src/rsh/"

rshDoc :: String -> String
rshDoc path = rshDocDir <> path <> "/index.md"

errDoc :: String
errDoc = rshDoc "errors"

allRshDocs :: IO [String]
allRshDocs = do
  files <- listDirectory rshDocDir
  dirs <- filterM (\fn -> doesDirectoryExist $ rshDocDir <> fn) files
  return $ map rshDoc dirs

-- For each type of Error:
--  Get the error code prefix (use random constructor)
--  Get the number of data constructors in type
allErrorCodes :: [(String, Int)]
allErrorCodes =
  [ (errPrefix Err_Block_Variable, gconNum @EvalError)
  , (errPrefix (Err_IntLiteralRange "" 0 0 0), gconNum @ConnectorError)
  , (errPrefix Err_ContinueDomination, gconNum @EPPError)
  , (errPrefix (Linearize.Err_Unreachable ""), gconNum @Linearize.Error)
  , (errPrefix Err_Unauthorized, gconNum @PkgError)
  , (errPrefix Err_Parse_JSIdentNone, gconNum @ParserError)
  , (errPrefix (Err_Impossible_Inspect ""), gconNum @ImpossibleError)
  , (errPrefix Err_TransferNewToken, gconNum @AlgoError)
  , (errPrefix (API_NoIn ""), gconNum @APICutError)
  , (errPrefix W_ExternalObject, gconNum @Warning)
  ]

getCodes :: String -> Int -> [T.Text]
getCodes pre num =
  map (go . makeErrCode pre) [0 .. (num -1)]
  where
    go x = "## {#" <> T.pack x <> "}"

missingDocs :: IO [String] -> IO [T.Text] -> IO [T.Text]
missingDocs docNames expected = do
  docNames' <- docNames
  actuals <- mapM TIO.readFile docNames'
  let actual = T.unlines actuals
  let present = flip T.isInfixOf actual
  expected' <- expected
  return $ filter (not . present) expected'

allErrorStrings :: IO [T.Text]
allErrorStrings = return $ concatMap (uncurry getCodes) allErrorCodes

spec_errorCodes :: Spec
spec_errorCodes =
  describe "error codes" $
    it "have docs" $
      missingDocs (return [errDoc]) allErrorStrings >>= flip shouldBe []

spec_baseAndStdlibIdentifiers :: Spec
spec_baseAndStdlibIdentifiers =
  describe "base and stdlib identifiers" $
    it "have docs" $
      missingDocs allRshDocs baseAndStdlibIdentifiers
      >>= flip shouldBe []

baseAndStdlibIdentifiers :: IO [T.Text]
baseAndStdlibIdentifiers = do
  json <- readProcess "reachc" ["--print-keyword-info"] ""
  let obj = A.decode (lbpack json) :: Maybe (M.Map T.Text A.Value)
  case obj of
    Nothing -> return ["Error getting base and stdlib identifiers for test" :: T.Text]
    Just o -> return $ map (\x -> "ref(\"rsh\", \"" <> x <> "\")")
      $ filter (not . T.isInfixOf "_") $ M.keys o
