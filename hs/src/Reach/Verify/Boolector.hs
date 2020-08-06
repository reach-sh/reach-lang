module Reach.Verify.Boolector where

import Reach.Verify.SMT
import Reach.Verify.Verifier
import SimpleSMT

-- XXX: known not to work.
-- - doesn't support unsat-cores
-- - doesn't support declare-datatypes
verify_boolector :: FilePath -> Verifier
verify_boolector = verify_smt mkSolver
  where
    mkSolver = newSolver "boolector" ["--smt2"]
