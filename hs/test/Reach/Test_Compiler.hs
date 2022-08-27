module Reach.Test_Compiler
  ( test_examples
  , test_yes
  , test_no
  , test_printKeywordInfo
  )
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.List (intersperse, isInfixOf)
import qualified Data.Set as S
import Reach.Util
import System.Directory
import System.Exit
import System.FilePath
import System.Process
import Test.Tasty
import Test.Tasty.Golden

type Output = BS.ByteString

type CompileOutput = Either Output Output

type Expect = CompileOutput -> IO Output

-- | Replace all instances of findThis in inThis with replaceWithThis
replaceBs :: Output -> Output -> Output -> Output
replaceBs findThis replaceWithThis = go
  where
    go inThis = case BS.breakSubstring findThis inThis of
      (bs0, bs1) -> case BS.stripPrefix findThis bs1 of
        Just bs1' -> bs0 <> replaceWithThis <> go bs1'
        Nothing -> inThis

stripCallStack :: Output -> Output
stripCallStack x =
  fst $ BS.breakSubstring "CallStack (from HasCallStack):" x

filterOutLines :: Output -> Output -> Output
filterOutLines prefix bs0 = bs'
  where
    bs1s = BS.split 10 bs0
    p = not . BS.isPrefixOf prefix
    bs2s = intersperse (BS.singleton 10) $ filter p bs1s
    bs' = BS.concat $ bs2s

testCompileOut :: FilePath -> FilePath -> IO CompileOutput
testCompileOut cwd afp = do
  cfp <- canonicalizePath $ cwd </> afp
  let dir = takeDirectory cfp
  (ec, outs, errs) <-
    withCurrentDirectory dir $ do
      rfp <- makeRelativeToCurrentDirectory cfp
      readProcessWithExitCode "reachc" ["--disable-reporting", rfp] ""
  let out = bpack outs
  let err = bpack errs
  let fmt0 = out <> err
  let fmt1 = stripCallStack fmt0
  let fmt2 = replaceBs (bpack dir) "." fmt1
  let fmt3 = filterOutLines "*Premium*" fmt2
  let fmt = fmt3
  case ec of
    ExitSuccess -> return $ Right fmt
    ExitFailure _ -> return $ Left fmt

goldenTests :: FilePath -> (FilePath -> Expect) -> IO TestTree
goldenTests subdir expect =
  withCurrentDirectory subdir $ do
    fs <- findByExtension [".rsh"] "."
    let fs' = filter (not . (".reach" `isInfixOf`)) fs
    cwd <- getCurrentDirectory
    let cmd r n = ["diff", "-u", r, n]
    let mkTest fp = do
          let gfp = replaceExtension fp ".txt"
          let f = BSL.fromStrict <$> (expect fp =<< testCompileOut cwd fp)
          return $ goldenVsStringDiff (subdir </> fp) cmd (cwd </> gfp) f
    testGroup subdir <$> mapM mkTest fs'

o2s :: Output -> String
o2s = bunpack

testFail :: Expect
testFail = \case
  Left s -> return s
  Right s -> fail $ o2s $ "Expected failure, but did not fail.\n" <> s

testSucc :: Expect
testSucc = \case
  Left s -> fail $ o2s $ "Expected success, but failed.\n" <> s
  Right s -> return s

test_yes :: IO TestTree
test_yes = goldenTests "t/y" (const testSucc)

test_no :: IO TestTree
test_no = goldenTests "t/n" (const testFail)

test_examples :: IO TestTree
test_examples = goldenTests "../examples" f
  where
    fails =
      S.fromList $
        [ "./pkg/index.rsh"
        , "./pkg/local.rsh"
        , "./pkg/index-master.rsh"
        , "./rps-4-attack/index-bad.rsh"
        , "./rps-4-attack/index-fails.rsh"
        , "./overview/index-error.rsh"
        , "./rsvp-5-cede/index-fail.rsh"
        ]
    f fp =
      case S.member fp fails of
        True -> testFail
        False -> testSucc

test_printKeywordInfo :: IO TestTree
test_printKeywordInfo = do
  cwd <- getCurrentDirectory
  return $ goldenVsStringDiff "print-keyword-info"
    (\ref new -> ["diff", "-u", ref, new])
    (cwd </> "../vsce/data/print-keyword-info.json")
    $ (readProcess "reachc" ["--print-keyword-info"] "") >>= (return . lbpack)
