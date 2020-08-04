module Reach.Verify where

import qualified Data.Text as T
import Reach.AST
import Reach.Verify.Boolector
import Reach.Verify.CVC4
import Reach.Verify.Yices
import Reach.Verify.Z3

data Verifier = Boolector | CVC4 | Yices | Z3
  deriving (Read, Show, Eq)

verify :: Show a => (T.Text -> String) -> Verifier -> ILProgram a -> IO ()
verify outn which ilp =
  case which of
    Z3 -> verify_z3 (outn "z3") ilp
    Yices -> verify_yices (outn "yi") ilp
    CVC4 -> verify_cvc4 (outn "cvc4") ilp
    Boolector -> verify_boolector (outn "br") ilp
