module Reach.Verify.Z3 where

import Reach.Verify.SMT
import Reach.Verify.Verifier
import SimpleSMT

verify_z3 :: FilePath -> Verifier
verify_z3 = verify_smt mkSolver
  where
    mkSolver = newSolver "z3" ["-smt2", "-in"]
