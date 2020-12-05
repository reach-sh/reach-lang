module Reach.Verify (verify) where

import Control.Monad
import Data.IORef
import qualified Data.Text as T
import Reach.AST.LL
import Reach.Connector
import Reach.Verify.Knowledge
import Reach.Verify.SMT
import Reach.Verify.Shared
import System.Exit

data VerifierName = Boolector | CVC4 | Yices | Z3
  deriving (Read, Show, Eq)

verify :: Maybe (T.Text -> String) -> Maybe [Connector] -> LLProg -> IO ExitCode
verify outnMay mvcs lp = do
  succ_ref <- newIORef 0
  fail_ref <- newIORef 0
  let vst =
        VerifySt
          { vst_res_succ = succ_ref
          , vst_res_fail = fail_ref
          }
  verify_knowledge (($ "know") <$> outnMay) vst lp
  --- The verifier should not be choosable by the user, but we may
  --- automatically select different provers based on the attributes
  --- of the program.
  let smt :: String -> [String] -> IO ()
      smt s a = void $ verify_smt (($ "smt") <$> outnMay) mvcs vst lp s a
  case Z3 of
    Z3 ->
      smt "z3" ["-smt2", "-in"]
    Yices ->
      -- known not to work.
      -- - doesn't support declare-datatypes
      smt "yices-smt2" []
    CVC4 ->
      smt "cvc4" ["--lang=smt2", "--incremental"]
    Boolector ->
      -- known not to work.
      -- - doesn't support unsat-cores
      -- - doesn't support declare-datatypes
      smt "boolector" ["--smt2"]
  ss <- readIORef succ_ref
  fs <- readIORef fail_ref
  putStr $ "Checked " ++ (show $ ss + fs) ++ " theorems;"
  case fs == 0 of
    True -> do
      putStrLn $ " No failures!"
      return ExitSuccess
    False -> do
      putStrLn $ " " ++ show fs ++ " failures. :'("
      return $ ExitFailure 1
