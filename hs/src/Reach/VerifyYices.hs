module Reach.VerifyYices where

import Reach.AST
import Reach.VerifySMT
import SimpleSMT

verify_yices :: Show a => FilePath -> ILProgram a -> IO ()
verify_yices = verify_smt mkSolver
  where
    mkSolver = newSolver "yices-smt2" []
