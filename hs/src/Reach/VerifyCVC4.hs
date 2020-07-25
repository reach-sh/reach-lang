module Reach.VerifyCVC4 where

import Reach.AST
import Reach.VerifySMT
import SimpleSMT

verify_cvc4 :: Show a => FilePath -> ILProgram a -> IO ()
verify_cvc4 = verify_smt mkSolver
  where
    mkSolver = newSolver "cvc4" ["--lang=smt2", "--incremental"]
