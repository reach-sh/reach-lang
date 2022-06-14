{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Use if" -}

module ReachPC.DirScan (Hash, FileHash(..), FileData(..), FileHashTree, FileDataTree, readUpdatedFiles, hashDirectory) where

import System.Directory (listDirectory, doesFileExist, pathIsSymbolicLink)
import System.FilePath (addTrailingPathSeparator, (</>))
import System.FilePath.Glob as G
import qualified Data.Map as M
import Data.List (partition)
import qualified Data.ByteString as BS
import Data.Char (isSpace)
import Crypto.Hash (hash, SHA1, Digest) 

type Hash = Digest SHA1

data FileHash 
  = FH_File Hash
  | FH_Directory
  | FH_Deleted
  deriving (Show, Eq)

data FileData 
  = FD_File BS.ByteString
  | FD_Directory
  | FD_Deleted
  deriving (Show, Eq)

type FileHashTree = M.Map FilePath FileHash

type FileDataTree = M.Map FilePath FileData

readUpdatedFiles :: FilePath -> FileHashTree -> IO FileDataTree
readUpdatedFiles root remoteHashes = do
  localHashes <- hashDirectory root 
  let updateHashTree = compareHashTrees localHashes remoteHashes
  readFilesInHashTree root updateHashTree

-- Converts a FileHashTree into a FileDataTree by reading file entries
readFilesInHashTree :: FilePath -> FileHashTree -> IO FileDataTree   
readFilesInHashTree root tree = M.fromList <$> mapM readHash (M.toList tree)
 where
  readHash (p, fh) = case fh of
    FH_File _ -> sequence (p, FD_File <$> BS.readFile (root </> p))
    FH_Directory -> return (p, FD_Directory)
    FH_Deleted -> return (p, FD_Deleted)

-- Produces a hash tree of file updates that need to be sent to the remote.
-- Removes redundant hashes and records deleted files
compareHashTrees :: FileHashTree -> FileHashTree -> FileHashTree
compareHashTrees localHashes remoteHashes = M.union updatedFiles deletedFiles
 where
  deletedFiles = M.map (const FH_Deleted) $ M.difference localHashes remoteHashes
  updatedFiles = M.differenceWith hasChanged remoteHashes localHashes
  hasChanged l r = if l == r then Nothing else Just l

hashFile :: FilePath -> IO Hash
hashFile = fmap hash . BS.readFile

-- Takes a directory and walks it recursively. Hashes each file encountered,
-- records directories encountered, and ignores symlinks. Special files ???
-- Reads ".reachignore" if it exists and ignores files that match patterns in it
hashDirectory :: FilePath -> IO FileHashTree
hashDirectory root = M.mapKeys dropRootPrefix <$> doScan [] [root] M.empty
 where
  dropRootPrefix = drop $ length $ addTrailingPathSeparator root
  isIgnored ignores path = any (`G.match` path) ignores
  readIgnores dir = do
    let path = dir </> ".reachignore"
    haveIgnores <- doesFileExist path
    case haveIgnores of
      False -> return []
      True -> do
        ignoresFile <- readFile path
        let patternChar c = not $ isSpace c || c == '#'
        let ignoresPatterns = filter (not . null) $ map (takeWhile patternChar) $ lines ignoresFile
        let ignores = map (G.compile . (dir </>)) ignoresPatterns
        return ignores
  doScan _ [] hashTree = return hashTree
  doScan ignores (dir:dirStack) hashTree = do
    -- Read .reachignore in this directory
    newIgnores <- readIgnores dir
    let ignores' = newIgnores <> ignores
    
    -- Walk current dir
    paths <- listDirectory dir
    let paths' = map (dir </>) paths
    pathsAreFile <- mapM doesFileExist paths'
    pathsAreSymlink <- mapM pathIsSymbolicLink paths'
    let pathTypes = zip3 paths' pathsAreFile pathsAreSymlink
    let pathTypes' = [(p, isFile) | (p, isFile, isSymlink) <- pathTypes, 
                                    not isSymlink, 
                                    not $ isIgnored ignores' p]
    let (files, dirs) = partition snd pathTypes'
    let files' = map fst files
    let dirs' = map fst dirs
    
    -- Hash encountered files
    fileHashes <- mapM hashFile files'
    let fileHashes' = zip files' (map FH_File fileHashes)
    let newFilesMap = M.fromList fileHashes'

    -- Add remaining dirs to stack and hashes map
    let dirStack' = dirs' <> dirStack
    let newDirsMap = M.fromList (map (, FH_Directory) dirs')
    
    let hashTree' = M.unions [hashTree, newFilesMap, newDirsMap]
    doScan ignores' dirStack' hashTree'
