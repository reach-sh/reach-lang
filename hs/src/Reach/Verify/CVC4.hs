module Reach.Verify.CVC4 where

import Reach.Verify.Verifier
import Reach.Verify.SMT
import SimpleSMT

verify_cvc4 :: FilePath -> Verifier
verify_cvc4 = verify_smt mkSolver
  where
    mkSolver = newSolver "cvc4" ["--lang=smt2", "--incremental"]
