{-# OPTIONS_GHC -fno-warn-orphans #-}
import Test.Hspec
import Test.Hspec.SmallCheck

import System.Directory
import Control.Exception
import Control.DeepSeq
import System.Process
import System.Exit
import Generics.Deriving

import Reach.ParserInternal
import Reach.Compiler

import Language.JavaScript.Parser.SrcLocation
instance NFData TP where
  rnf (TP _) = ()
instance NFData TokenPosn where
  rnf (TokenPn _ _ _) = ()

mustExist :: FilePath -> Expectation
mustExist fp = do
  e <- doesPathExist fp
  if e then
    True `shouldBe` True
  else
    expectationFailure $ "file does not exist: " ++ fp

try_hard :: NFData a => Exception e => IO a -> IO (Either e a)
try_hard m = do
  one <- try m
  case one of
    Left _ -> return one
    Right p -> try $ evaluate $ force p

err_example :: Show a => NFData a => String -> (FilePath -> IO a) -> Expectation
err_example which f = do
  let expth ext = "test.rsh/" ++ which ++ "." ++ ext
  let expected_p = expth "txt"
  let actual_p = expth "rsh"
  mustExist actual_p
  mustExist expected_p
  expected <- readFile expected_p
  actual_r <- try_hard $ f actual_p
  case actual_r of
    Right r ->
      expectationFailure $ "expected a failure for " ++ which ++ " but, got: " ++ show r
    Left (ErrorCall actual_x) ->
      (actual_x ++ "\n") `shouldBe` expected

parse_err_example :: ParseErr -> Expectation
parse_err_example pe =
  err_example (conNameOf pe) readReachFile

compile_err_example :: CompileErr -> Expectation
compile_err_example ce =
  err_example (conNameOf ce) f
  where f n = compile $ CompilerOpts "test.out" n

main :: IO ()
main = hspec $ do
  describe "RPS" $ do
    it "RPS end-to-end" $ do
      --- XXX remove echo
      (system "echo 'cd ../examples/rps && make clean build'") `shouldReturn` ExitSuccess
  describe "Parser" $ do
    it "stdlib_defs is valid" $ do
      stdlib_defs >>= (`shouldSatisfy` (not . null))
    it "all parse errs have examples" $ property $
      parse_err_example
  describe "Compiler" $ do
    it "while no continue" $ do
      compile_err_example CE_WhileNoContinue
    it "all compile errs have examples" $ property $
      compile_err_example
