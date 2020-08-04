module Reach.Verify.Boolector where

import Reach.AST
import Reach.Verify.SMT
import SimpleSMT

-- XXX: known not to work.
-- - doesn't support unsat-cores
-- - doesn't support declare-datatypes
verify_boolector :: Show a => FilePath -> ILProgram a -> IO ()
verify_boolector = verify_smt mkSolver
  where
    mkSolver = newSolver "boolector" ["--smt2"]
