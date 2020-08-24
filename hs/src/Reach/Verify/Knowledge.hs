module Reach.Verify.Knowledge (verify_knowledge) where

import Reach.AST
import Reach.Verify.Shared

verify_knowledge :: Maybe FilePath -> VerifySt -> LLProg -> IO ()
verify_knowledge _mout _vst _lp = do
  putStrLn $ "XXX verify_knowledge"
  mempty
