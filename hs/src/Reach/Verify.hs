module Reach.Verify where

import qualified Data.Text as T
import Reach.NL_AST
import Reach.Verify.SMT

data VerifierName = Boolector | CVC4 | Yices | Z3
  deriving (Read, Show, Eq)

verify :: (T.Text -> String) -> VerifierName -> LLProg -> IO ()
verify outn which lp =
  case which of
    Z3 ->
      smt "z3" ["-smt2", "-in"]
    Yices ->
      -- XXX: known not to work.
      -- - doesn't support declare-datatypes
      smt "yices-smt2" []
    CVC4 ->
      smt "cvc4" ["--lang=smt2", "--incremental"]
    Boolector ->
      -- XXX: known not to work.
      -- - doesn't support unsat-cores
      -- - doesn't support declare-datatypes
      smt "boolector" ["--smt2"] 
  where smt = verify_smt (outn "smt") lp
