module Reach.EmbeddedFiles (runtime_smt2, runtime_bt_smt2, stdlib_sol, stdlib_rsh, mkEmbed) where

import Data.ByteString (ByteString)
import Data.FileEmbed
import Language.Haskell.TH.Syntax

runtime_smt2 :: ByteString
runtime_smt2 = $(makeRelativeToProject "./smt2/runtime.smt2" >>= embedFile)

runtime_bt_smt2 :: ByteString
runtime_bt_smt2 = $(makeRelativeToProject "./smt2/runtime-bt.smt2" >>= embedFile)

stdlib_sol :: ByteString
stdlib_sol = $(makeRelativeToProject "./sol/stdlib.sol" >>= embedFile)

stdlib_rsh :: ByteString
stdlib_rsh = $(makeRelativeToProject "./rsh/stdlib.rsh" >>= embedFile)

-- Exported as a shorthand convenience for app/reach/Main.hs to work around
-- GHC's "stage restriction" rules
mkEmbed :: FilePath -> Q Exp
mkEmbed f = makeRelativeToProject ("./app/reach/embed/" <> f) >>= embedStringFile
