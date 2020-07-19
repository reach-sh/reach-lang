{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Control.DeepSeq
import Control.Exception
import Data.Functor.Identity
import Data.List (isPrefixOf, (\\))
import Data.Proxy
import Data.Typeable
import Generics.Deriving
import Language.JavaScript.Parser.SrcLocation
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.Process
import Test.Hspec
import Test.SmallCheck.Series
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.Hspec
import Test.Tasty.Runners.AntXML

import Reach.ParserInternal
import Reach.Compiler (CompileErr, compile)
import Reach.CompilerTool
import Reach.Util

import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO


instance NFData TP where
  rnf (TP _) = ()
instance NFData TokenPosn where
  rnf (TokenPn _ _ _) = ()

class (Generic a, Typeable a, ConNames (Rep a), Serial Identity a) => ReachErr a
instance ReachErr ParseErr
instance ReachErr CompileErr

try_hard :: NFData a => Exception e => IO a -> IO (Either e a)
try_hard m = do
  one <- try m
  case one of
    Left _ -> return one
    Right p -> try $ evaluate $ force p

dropDefines :: [T.Text] -> [T.Text]
dropDefines = filter (\line -> not $ "(define" `T.isInfixOf` line)

dropHypotheticalIntearcts :: [T.Text] -> [T.Text]
dropHypotheticalIntearcts = filter (\line -> not $ "... interact returns " `T.isPrefixOf` line)

test_compile :: FilePath -> IO ()
test_compile n = do
  opts <- makeCompilerOpts $ CompilerToolOpts
    { cto_outputDir = "test.out"
    , cto_source = n
    , cto_expCon = False
    , cto_expComp = False
    }
  compile opts

errExampleBs :: (Show a, NFData a) => (FilePath -> IO a) -> FilePath -> IO LB.ByteString
errExampleBs k fp = withCurrentDirectory dir (try_hard (k fpRel)) >>= \case
  Right r ->
    fail $ "expected a failure, but got: " ++ show r
  Left (ErrorCall e) ->
    return $ LB.fromStrict $ bpack e
  where
    dir = takeDirectory fp
    fpRel = takeFileName fp

errExample :: (Show a, NFData a) => String -> (FilePath -> IO a) -> FilePath -> TestTree
errExample ext k fp = do
  let goldenFile = replaceExtension fp ext
      fpBase = takeBaseName fp
  goldenVsString fpBase goldenFile (errExampleBs k fp)

test_compile_vererr :: FilePath -> IO (LB.ByteString, LB.ByteString)
test_compile_vererr pf = do
  let examples_dir = "../examples"
  let dest = T.unpack . T.replace "__" "/" . T.pack $ takeFileName pf
  let orig = dropExtension $ dropExtension dest
  let patchCmd = "patch --quiet -d " ++ examples_dir ++ " -i " ++ pf ++ " -o " ++ dest ++ " " ++ orig
  -- putStrLn patchCmd
  ExitSuccess <- system patchCmd
  -- putStrLn "...patch applied"
  let rdest = examples_dir </> dest
  -- putStrLn "compiling..."
  (_, Just hout, Just herr, hP) <-
    createProcess (proc "stack" ["exec", "--", "reachc", "-o", "test.out", rdest]){ std_out = CreatePipe,
                                                                                    std_err = CreatePipe }
  outT <- TIO.hGetContents hout
  err <- LB.hGetContents herr
  -- strip out the defines
  let outT' = T.unlines $ dropHypotheticalIntearcts $ dropDefines $ T.lines outT
  let out = LB.fromStrict $ TE.encodeUtf8 outT'
  -- putStrLn $ "...finished: " ++ show (LB.length out)
  _ <- waitForProcess hP
  removeFile rdest
  return (out, err)


stderroutExampleBs :: (FilePath -> IO (LB.ByteString, LB.ByteString)) -> FilePath -> IO LB.ByteString
stderroutExampleBs k fp = do
  (out, err) <- k fp
  let tag t x = "<" <> t <> ">\n" <> x <> "\n</" <> t <> ">\n"
  let res = (tag "out" out) <> (tag "err" err)
  return res


stderroutExample :: String -> (FilePath -> IO (LB.ByteString, LB.ByteString)) -> FilePath -> TestTree
stderroutExample ext k fp = do
  let goldenFile = replaceExtension fp ext
      fpBase = takeBaseName fp
  goldenVsString fpBase goldenFile (stderroutExampleBs k fp)

parseErrExample :: FilePath -> TestTree
parseErrExample = errExample ".txt" readReachFile

compileErrExample :: FilePath -> TestTree
compileErrExample = errExample ".txt" test_compile

verifyErrExample :: FilePath -> TestTree
verifyErrExample = stderroutExample ".out" test_compile_vererr


-- It is dumb that both testSpec and it require descriptive strings
testNotEmpty :: String -> [a] -> IO TestTree
testNotEmpty label xs = testSpec label $ it "is not empty" $ case xs of
  [] -> expectationFailure "... it is empty =["
  (_:_) -> pure ()

testExamplesCover ::
  forall err proxy. (ReachErr err) =>
  proxy err -> [FilePath] -> IO TestTree
testExamplesCover p sources = testSpec label t where
    label = "Examples covering " <> ty
    ty = show $ typeRep p
    constrs = listSeries 1 :: [err]
    cNames = map conNameOf constrs
    t = it "list of constructors with missing examples is empty" $ do
      let missing = cNames \\ map takeBaseName sources
      missing `shouldBe` []

testsFor :: ReachErr err =>
  proxy err -> (FilePath -> TestTree) -> String -> FilePath -> IO TestTree
testsFor p mkTest ext subdir = do
  (sources, gTests) <- goldenTests' mkTest ext subdir
  testCov <- testExamplesCover p sources
  let groupLabel = subdir
  let tests = testCov : gTests
  return $ testGroup groupLabel tests

goldenTests' :: (FilePath -> TestTree) -> String -> FilePath -> IO ([FilePath], [TestTree])
goldenTests' mkTest ext subdir = do
  curDir <- getCurrentDirectory
  let dir = curDir </> "test-examples" </> subdir
  sources <- findByExtension [ext] dir
  testNe <- testNotEmpty "this subdir" sources
  let testSources = testGroup ("testing each " <> ext <> "file") $ map mkTest sources
  let tests = [testNe, testSources]
  return (sources, tests)

goldenTests :: (FilePath -> TestTree) -> String -> FilePath -> IO TestTree
goldenTests mkTest ext subdir = do
  (_, gTests) <- goldenTests' mkTest ext subdir
  let groupLabel = subdir
  return $ testGroup groupLabel gTests

main :: IO ()
main = do
  parseTests <- testsFor (Proxy @ParseErr) parseErrExample ".rsh" "parse-errors"
  compileTests <- testsFor (Proxy @CompileErr) compileErrExample ".rsh" "compile-errors"
  verifyTests <- goldenTests verifyErrExample ".patch" "verification-errors"
  args <- getArgs
  -- antXMLRunner isn't very polite when you leave off --xml
  let theMain = case any ("--xml" `isPrefixOf`) args of
        True -> defaultMainWithIngredients (antXMLRunner:defaultIngredients)
        False -> defaultMain
  theMain $ testGroup "tests"
    [ parseTests
    , compileTests
    , verifyTests
    ]
