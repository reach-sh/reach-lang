module Reach.VerifyYices where

import Reach.AST
import Reach.VerifySMT
import SimpleSMT

-- XXX: known not to work.
-- - doesn't support declare-datatypes
verify_yices :: Show a => FilePath -> ILProgram a -> IO ()
verify_yices = verify_smt mkSolver
  where
    mkSolver = newSolver "yices-smt2" []
