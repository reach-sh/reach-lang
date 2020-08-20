module Reach.Report.TH (reachJSONOptions) where

import Data.Aeson.TH

reachJSONOptions :: Options
reachJSONOptions =
  defaultOptions
    { fieldLabelModifier = \s -> case break (== '_') s of
        (_, '_' : s') -> s'
        _ -> s
    }
