module Reach.Dotty
  ( DotGraph
  , DotEdge
  , dotty
  ) where

import Data.List (intercalate)
import qualified Data.Map.Strict as M

type DotEdge = (String, String, M.Map String String)
type DotGraph = [DotEdge]

dotty :: DotGraph -> String
dotty es = unlines $ [preamble, "digraph {"] <> map go es <> ["}"]
  where
    preamble = "// This file is in the DOT file format. Upload or copy it into a Graphviz engine, such as https://dreampuf.github.io/GraphvizOnline"
    go (f, t, am) = " " <> show f <> "->" <> show t <> "[" <> a' <> "]"
      where
        a' = intercalate "," $ map (\(k, v) -> k <> "=" <> v) $ M.toAscList am
