module Reach.CompilerTool where

import qualified Filesystem.Path.CurrentOS as FP
import Reach.Compiler (compile)
import Reach.CompilerNL
import System.Directory

data CompilerToolOpts = CompilerToolOpts
  { cto_outputDir :: FilePath
  , cto_source :: FilePath
  , cto_tops :: [ String ]
  , cto_expComp :: Bool
  }

makeCompilerOpts :: CompilerToolOpts -> IO CompilerOpts
makeCompilerOpts CompilerToolOpts {..} = do
  let srcp = cto_source
  let outd = cto_outputDir
  let outdp = FP.decodeString outd
  let outn ext = FP.encodeString $ FP.append outdp $ (FP.filename $ FP.decodeString srcp) `FP.replaceExtension` ext
  createDirectoryIfMissing True outd
  return $
    CompilerOpts
      { output = outn
      , source = srcp
      , tops = cto_tops
      }

compilerToolMain :: CompilerToolOpts -> IO ()
compilerToolMain ctool_opts@CompilerToolOpts {..} = do
  copts <- makeCompilerOpts ctool_opts
  case cto_expComp of
    True -> compileNL copts
    False -> compile copts
