module Reach.Test_Docs ( spec_errorCodes ) where

import           Generic.Data
import           Reach.AST.Base               (ImpossibleError (..), errPrefix)
import           Reach.APICut                 (APICutError (..))
import           Reach.Connector              (ConnectorError (..))
import           Reach.EPP                    (EPPError (..))
import           Reach.Eval.Error             (EvalError (..))
import qualified Reach.Linearize              as Linearize
import           Reach.PackageImport          (PkgError (..))
import           Reach.Parser                 (ParserError (..))
import           Reach.Util
import           Test.Hspec
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Reach.Connector.ALGO         (AlgoError(..))
import Reach.Warning (Warning (W_ExternalObject))

docName :: String
docName = "../docs/dev/src/rsh/errors/index.md"

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
  , (errPrefix Err_Impossible_InspectForall, gconNum @ImpossibleError)
  , (errPrefix Err_TransferNewToken, gconNum @AlgoError)
  , (errPrefix (API_NoIn ""), gconNum @APICutError)
  , (errPrefix W_ExternalObject, gconNum @Warning)
  ]

getCodes :: String -> Int -> [T.Text]
getCodes pre num =
  map (go . makeErrCode pre) [0 .. (num-1)]
  where
    go x = "## {#" <> T.pack x <> "}"

missingDocs :: IO [T.Text]
missingDocs = do
  actual <- TIO.readFile docName
  let expected = concatMap (uncurry getCodes) allErrorCodes
  let present = flip T.isInfixOf actual
  return $ filter (not . present) expected

spec_errorCodes :: Spec
spec_errorCodes =
  describe "error codes" $
    it "have docs" $
      missingDocs >>= flip shouldBe []
