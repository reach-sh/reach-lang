module Reach.OutputUtil
  ( Outputer
  , wrapOutput
  , mayOutput
  , mustOutput
  , atomicWriteFile
  )
where

import Control.Monad
import qualified Data.Text as T
import System.Directory

type Outputer = Bool -> T.Text -> (Bool, FilePath)

wrapOutput :: T.Text -> Outputer -> Outputer
wrapOutput pre inner opt post = inner opt (pre <> post)

mayOutput :: Monad m => (Bool, FilePath) -> (FilePath -> m ()) -> m ()
mayOutput (shouldWrite, p) j = when shouldWrite $ j p

mustOutput :: Monad m => Outputer -> T.Text -> (FilePath -> m ()) -> m FilePath
mustOutput out lab j = do
  let (_, f) = out True lab
  j f
  return f

-- Write to a temporary sibling and rename into place, so consumers never
-- observe a partially-written artifact.
atomicWriteFile :: (FilePath -> a -> IO ()) -> FilePath -> a -> IO ()
atomicWriteFile wr fp x = do
  let tmp = fp <> ".tmp"
  wr tmp x
  renameFile tmp fp
