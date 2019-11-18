import Test.Hspec
import Test.Hspec.SmallCheck

import System.Directory
import Control.Exception
import System.Process
import System.Exit

import Reach.ParserInternal
import Generics.Deriving

mustExist :: FilePath -> Expectation
mustExist fp = do
  e <- doesPathExist fp
  if e then
    True `shouldBe` True
  else
    expectationFailure $ "file does not exist: " ++ fp

parse_error_example :: ParseError -> Expectation
parse_error_example pe = do
  let which = conNameOf pe
  let expth ext = "test.rsh/" ++ which ++ "." ++ ext
  let expected_p = expth "txt"
  let actual_p = expth "rsh"
  mustExist expected_p
  mustExist actual_p
  expected <- readFile expected_p
  actual_r <- try $ readReachFile actual_p
  case actual_r of
    Right _ ->
      expectationFailure $ "expected a failure for " ++ which
    Left (ErrorCall actual_x) ->
      (actual_x ++ "\n") `shouldBe` expected

main :: IO ()
main = hspec $ do
  describe "RPS" $ do
    it "RPS end-to-end" $ do
      --- XXX remove echo
      (system "echo 'cd ../examples/rps && make clean build'") `shouldReturn` ExitSuccess
  describe "Parser" $ do
    it "stdlib_defs is valid" $ do
      stdlib_defs >>= (`shouldSatisfy` (not . null))
    it "all parse errors have examples" $ property $
      parse_error_example
