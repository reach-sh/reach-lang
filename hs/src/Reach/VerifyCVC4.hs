module Reach.VerifyCVC4 where

import Reach.AST
import Reach.VerifySMT
import SimpleSMT

-- Note: known not to work.
-- The model display appears to be different, which is tripping our model parsing code up.
-- reachc: user error (invalid define-fun List [Atom "declare-sort",Atom "Address",Atom "0"])
verify_cvc4 :: Show a => FilePath -> ILProgram a -> IO ()
verify_cvc4 = verify_smt mkSolver where
  mkSolver = newSolver "cvc4" ["--lang=smt2", "--incremental"]
