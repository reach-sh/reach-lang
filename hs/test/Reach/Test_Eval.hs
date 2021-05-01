module Reach.Test_Eval
  ( spec_examples_cover_EvalError
  , spec_examples_cover_ParserError
  , spec_ImportSource
  , test_compileBundle_errs
  , main
  )
where

import Data.Either (isLeft)
import Data.Proxy
import Reach.Eval.Error
import Reach.Eval.ImportSource
import Reach.Parser
import Reach.Test.Util
import Test.Hspec
import Test.Tasty

test_compileBundle_errs :: IO TestTree
test_compileBundle_errs = goldenTests compileTestFail ".rsh" "nl-eval-errors"

spec_examples_cover_EvalError :: Spec
spec_examples_cover_EvalError =
  mkSpecExamplesCoverCtors p exceptions ".rsh" "nl-eval-errors"
  where
    p = Proxy @EvalError
    exceptions =
      [ "Err_App_InvalidArgs"
      , "Err_CannotReturn" -- most attempts were not valid js
      , "Err_Decl_IllegalJS"
      , "Err_Each_NotParticipant"
      , "Err_Each_NotTuple"
      , "Err_ExpectedPublic" -- may not be possible with new enforced _ ident conventions
      , "Err_Eval_IllegalLift"
      , "Err_Eval_IndirectRefNotArray"
      , "Err_Eval_NoReturn" -- not syntactically possible?
      , "Err_Eval_NotApplicableVals" -- previous test example subsumed by Err_ToConsensus_TimeoutArgs
      , "Err_Only_NotOneClosure"
      , "Err_Import_IllegalJS"
      , "Err_Obj_IllegalFieldValues" -- not possible with Grammar7?
      , "Err_ToConsensus_Double" -- prevented by earlier parsing?
      , "Err_TopFun_NoName" -- hiding behind Err_Type_None
      , "Err_While_IllegalInvariant"
      , "Err_Type_Mismatch"
      , "Err_Type_None"
      , "Err_Type_NotDT"
      , "Err_Type_NotApplicable"
      , "Err_TypeMeets_dMismatch"
      , "Err_View_CannotExpose"
      ]

spec_examples_cover_ParserError :: Spec
spec_examples_cover_ParserError =
  mkSpecExamplesCoverCtors p exceptions ".rsh" "nl-eval-errors"
  where
    p = Proxy @ParserError
    exceptions =
      [ "Err_Parser_Arrow_NoFormals" -- (=> e) didn't work
      , "Err_Parse_IllegalLiteral" -- undefined didn't work
      , "Err_Parse_NotModule"
      , "Err_Parse_JSIdentNone"
      ]

spec_ImportSource :: Spec
spec_ImportSource = describe "Module `Reach.Eval.ImportSource`" $ do

  describe "exports an `importSource` function which" $ do
    it "can distinguish local imports" $ do
      let f = "./examples/nim/index-abstract.rsh"
          n = "@bork"
      importSource f `shouldBe` (Right $ ImportLocal f)
      isLeft (importSource n) `shouldBe` True

    -- TODO
    -- describe "can distinguish remote GitHub imports" $ do
    --   it "in long-form" $ do
    --     importSource "@github.com:reach-sh/reach-lang#6c3dd0f/examples/exports/index.rsh"
    --       `shouldBe` (Right $ ImportRemoteGit $ GitHub
    --         (HostGitAcct "reach-sh")
    --         (HostGitRepo "reach-lang")
    --         (HostGitRef  "6c3dd0f")
    --         (HostGitDir  [ "examples", "exports" ])
    --         (HostGitFile "index.rsh"))

    --   it "(by default) when no host is specified" $ do
    --     importSource "@reach-sh/reach-lang#6c3dd0f/module.rsh"
    --       `shouldBe` (Right $ ImportRemoteGit $ GitHub
    --         (HostGitAcct "reach-sh")
    --         (HostGitRepo "reach-lang")
    --         (HostGitRef  "6c3dd0f")
    --         (HostGitDir  [])
    --         (HostGitFile "module.rsh"))

  describe "exports a `gitUriOf` function which" $ do
    let uriShouldBe i o = case importSource i of
          Right (ImportRemoteGit h) -> gitUriOf h `shouldBe` o
          _ -> fail $ "Invalid translation " <> i <> " -> " <> o

    describe "translates GitHub imports of" $ do

      it "long-form into their corresponding git URIs" $
        "@github.com:reach-sh/reach-lang#6c3dd0f/examples/exports/index.rsh"
          `uriShouldBe`
        "https://github.com/reach-sh/reach-lang.git"

    describe "translates BitBucket imports of" $ do

      it "long-form into their corresponding git URIs" $
        "@bitbucket.org:reach-sh/fancy-lib#v0.1.2/src/lib.rsh"
          `uriShouldBe`
        "https://bitbucket.org/reach-sh/fancy-lib.git"


main :: IO ()
main = hspec spec_ImportSource
