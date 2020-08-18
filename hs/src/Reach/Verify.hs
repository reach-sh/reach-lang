module Reach.Verify (verify) where

import qualified Data.Text as T
import Reach.AST
import Reach.Verify.SMT
import System.Exit

data VerifierName = Boolector | CVC4 | Yices | Z3
  deriving (Read, Show, Eq)

verify :: Maybe (T.Text -> String) -> LLProg -> IO ExitCode
verify outnMay lp =
  --- The verifier should not be choosable by the user, but we may
  --- automatically select different provers based on the attributes
  --- of the program.
  case Z3 of
    Z3 ->
      smt "z3" ["-smt2", "-in"]
    Yices ->
      -- known not to work.
      -- - doesn't support declare-datatypes
      smt "yices-smt2" []
    CVC4 ->
      smt "cvc4" ["--lang=smt2", "--incremental"]
    Boolector ->
      -- known not to work.
      -- - doesn't support unsat-cores
      -- - doesn't support declare-datatypes
      smt "boolector" ["--smt2"]
  where
    smt = verify_smt (($ "smt") <$> outnMay) lp
