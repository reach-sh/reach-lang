module Reach.Parser.Common
  ( ParserOpts(..)
  ) where

data ParserOpts = ParserOpts
  { dirDotReach :: FilePath
  , canGit      :: Bool
  }
