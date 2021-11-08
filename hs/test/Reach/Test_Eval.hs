module Reach.Test_Eval
  ( spec_examples_cover_EvalError
  , spec_examples_cover_ParserError
  , spec_examples_cover_APICutError
  )
where

import Data.List ((\\))
import Data.Proxy
import Generics.Deriving
import Reach.Eval.Error
import Reach.Parser
import Reach.APICut
import System.Directory
import System.FilePath
import Test.Hspec
import Test.Tasty.Golden

mkSpecExamplesCoverStrs :: [String] -> String -> FilePath -> Spec
mkSpecExamplesCoverStrs strs ext subdir = describe subdir $
  it "covers all specified examples" $ do
    curDir <- getCurrentDirectory
    let dir = curDir </> "t" </> subdir
    doesDirectoryExist dir `shouldReturn` True
    sources <- findByExtension [ext] dir
    let missing = strs \\ map takeBaseName sources
    missing `shouldBe` []

mkSpecExamplesCoverCtors
  :: forall err proxy.
  (Generic err, ConNames (Rep err))
  => proxy err
  -> [String]
  -> String
  -> FilePath
  -> Spec
mkSpecExamplesCoverCtors _ exceptions = mkSpecExamplesCoverStrs strs
  where
    strs = conNames (error "unused" :: err) \\ exceptions

spec_examples_cover_EvalError :: Spec
spec_examples_cover_EvalError =
  mkSpecExamplesCoverCtors p exceptions ".rsh" "n"
  where
    p = Proxy @EvalError
    exceptions =
      [ "Err_App_InvalidArgs"
      , "Err_CannotReturn" -- most attempts were not valid js
      , "Err_App_Interact_NotFirstOrder"
      , "Err_Decl_IllegalJS"
      , "Err_Each_NotParticipant"
      , "Err_Each_NotTuple"
      , "Err_ExpectedPublic" -- may not be possible with new enforced _ ident conventions
      , "Err_Eval_IllegalLift"
      , "Err_Eval_NoReturn" -- not syntactically possible?
      , "Err_Only_NotOneClosure"
      , "Err_Import_IllegalJS"
      , "Err_Obj_IllegalFieldValues" -- not possible with Grammar7?
      , "Err_TopFun_NoName" -- hiding behind Err_Type_None
      , "Err_While_IllegalInvariant"
      , "Err_Type_Mismatch"
      , "Err_Type_None"
      , "Err_Type_NotApplicable"
      , "Err_TypeMeets_dMismatch"
      , "Err_View_CannotExpose"
      , "Err_Module_Return" -- shadowed by Err_Eval_NoReturn
      ]

spec_examples_cover_ParserError :: Spec
spec_examples_cover_ParserError =
  mkSpecExamplesCoverCtors p exceptions ".rsh" "n"
  where
    p = Proxy @ParserError
    exceptions =
      [ "Err_Parser_Arrow_NoFormals" -- (=> e) didn't work
      , "Err_Parse_IllegalLiteral" -- undefined didn't work
      , "Err_Parse_NotModule"
      , "Err_Parse_JSIdentNone"
      ]

spec_examples_cover_APICutError :: Spec
spec_examples_cover_APICutError =
  mkSpecExamplesCoverCtors p exceptions ".rsh" "n"
  where
    p = Proxy @APICutError
    exceptions = []
