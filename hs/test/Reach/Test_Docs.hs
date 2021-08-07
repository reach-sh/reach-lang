module Reach.Test_Docs ( spec_errorCodes ) where

import           Generic.Data
import           Reach.AST.Base               (ImpossibleError (..), errPrefix)
import           Reach.Connector              (ConnectorError (..))
import           Reach.Connector.ETH_Solidity (connect_eth)
import           Reach.EPP                    (EPPError (..))
import           Reach.Eval.Error             (EvalError (..))
import qualified Reach.Linearize              as Linearize
import           Reach.PackageImport          (PkgError (..))
import           Reach.Parser                 (ParserError (..))
import           Reach.Util
import           Test.Hspec
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

docName :: String
docName = "../docs-src/ref-error-codes.scrbl"

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

getCodes :: String -> Int -> [T.Text]
getCodes pre num =
  map (go . makeErrCode pre) [0 .. num - 1]
  where
    go x = "@error{" <> T.pack x <> "}"

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
