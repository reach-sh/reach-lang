module Reach.Verify.CVC4 where

import Reach.AST
import Reach.Verify.SMT
import SimpleSMT

verify_cvc4 :: Show a => FilePath -> ILProgram a -> IO ()
verify_cvc4 = verify_smt mkSolver
  where
    mkSolver = newSolver "cvc4" ["--lang=smt2", "--incremental"]
