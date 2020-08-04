module Reach.Verify.Verifier where

import Reach.NL_AST

type Verifier = LLProg -> IO ()
