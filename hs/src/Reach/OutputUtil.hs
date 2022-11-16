module Reach.OutputUtil
  ( Outputer
  , wrapOutput
  , mayOutput
  , mustOutput
  ) where

import Control.Monad
import qualified Data.Text as T

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
