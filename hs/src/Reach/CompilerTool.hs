module Reach.CompilerTool (CompilerToolOpts (..), makeCompilerOpts, compilerToolMain) where

import Control.Monad
import qualified Data.Set as S
import qualified Filesystem.Path.CurrentOS as FP
import Reach.Compiler
import System.Directory

data CompilerToolOpts = CompilerToolOpts
  { cto_outputDir :: FilePath
  , cto_dirDotReach :: FilePath
  , cto_installPkgs :: Bool
  , cto_source :: FilePath
  , cto_tops :: [String]
  , cto_intermediateFiles :: Bool
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
      , tops = if null cto_tops then Nothing else Just (S.fromList cto_tops)
      , intermediateFiles = cto_intermediateFiles
      , dirDotReach = cto_dirDotReach
      , installPkgs = cto_installPkgs
      }

compilerToolMain :: CompilerToolOpts -> IO ()
compilerToolMain = makeCompilerOpts >=> compile
