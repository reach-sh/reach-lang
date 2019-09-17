import System.Process
import System.Exit

import Reach.Util

main :: IO ()
main = do
  maybeDie $ system "cd ../examples/rps && make clean build"
  exitSuccess
