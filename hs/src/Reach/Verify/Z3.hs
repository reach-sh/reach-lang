module Reach.Verify.Z3 where

import Reach.AST
import Reach.Verify.SMT
import SimpleSMT

verify_z3 :: Show a => FilePath -> ILProgram a -> IO ()
verify_z3 = verify_smt mkSolver
  where
    mkSolver = newSolver "z3" ["-smt2", "-in"]
