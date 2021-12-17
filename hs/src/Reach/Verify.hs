module Reach.Verify
  ( verify
  , VerifyOpts(..)
  ) where

import Control.Monad
import GHC.Conc (numCapabilities)
import Reach.AST.LL
import Reach.Counter
import Reach.Verify.Knowledge
import Reach.Verify.SMT
import Reach.Verify.Shared
import System.Exit

data VerifierName = Boolector | CVC4 | Yices | Z3
  deriving (Read, Show, Eq)

verify :: VerifyOpts -> LLProg -> IO ExitCode
verify vst_vo lp@(LLProg _ llo _ _ _ _ _ _ _) = do
  vst_res_succ <- newCounter 0
  vst_res_fail <- newCounter 0
  vst_res_time <- newCounter 0
  vst_res_reps <- newCounter 0
  let vst = VerifySt {..}
  verify_knowledge vst lp
  --- The verifier should not be choosable by the user, but we may
  --- automatically select different provers based on the attributes
  --- of the program.
  let smt :: String -> [String] -> IO ()
      smt s a = void $ verify_smt vst lp s a
  let cpus = show numCapabilities
  case Z3 of
    Z3 ->
      smt "z3" [("smt.threads=" <> cpus), ("sat.threads=" <> cpus), "-smt2", "-in"]
  -- XXX "pattern match is redundant"
  -- Yices ->
  --   -- known not to work.
  --   -- - doesn't support declare-datatypes
  --   smt "yices-smt2" []
  -- CVC4 ->
  --   smt "cvc4" ["--lang=smt2", "--incremental"]
  -- Boolector ->
  --   -- known not to work.
  --   -- - doesn't support unsat-cores
  --   -- - doesn't support declare-datatypes
  --   smt "boolector" ["--smt2"]
  ss0 <- readCounter vst_res_succ
  let ss = ss0 + (llo_droppedAsserts llo)
  fs <- readCounter vst_res_fail
  ts <- readCounter vst_res_time
  rs <- readCounter vst_res_reps
  putStr $ "Checked " ++ (show $ ss + fs) ++ " theorems"
  case fs == 0 && ts == 0 of
    True -> do
      putStrLn $ "; No failures!"
      return ExitSuccess
    False -> do
      when (fs > 0) $ do
        putStr $ "; " ++ show fs ++ " failures"
      when (ts > 0) $ do
        putStr $ "; " ++ show ts ++ " timeouts"
      when (rs > 0) $ do
        putStr $ " (and " ++ show rs ++ " omitted repeats)"
      putStrLn $ " :'("
      return $ ExitFailure 1
