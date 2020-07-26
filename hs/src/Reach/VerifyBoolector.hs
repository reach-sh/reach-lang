module Reach.VerifyBoolector where

import Reach.AST
import Reach.VerifySMT
import SimpleSMT

-- Note: known not to work.
-- - doesn't support unsat-cores
-- - doesn't support declare-datatypes
verify_boolector :: Show a => FilePath -> ILProgram a -> IO ()
verify_boolector = verify_smt mkSolver
  where
    mkSolver = newSolver "boolector" ["--smt2"]
