module Reach.Verify.Yices where

import Reach.Verify.SMT
import Reach.Verify.Verifier
import SimpleSMT

-- XXX: known not to work.
-- - doesn't support declare-datatypes
verify_yices :: FilePath -> Verifier
verify_yices = verify_smt mkSolver
  where
    mkSolver = newSolver "yices-smt2" []
