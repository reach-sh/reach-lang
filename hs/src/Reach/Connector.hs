module Reach.Connector
  ( ConnectorInfoMap
  , ConnectorInfo (..)
  , ConnectorResult
  , Connector (..)
  , dlo_lims_meet
  , Connectors
  )
where

import Algebra.Lattice
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Reach.AST

type ConnectorInfoMap =
  M.Map String ConnectorInfo

data ConnectorInfo
  = CI_Null
  | CI_Bool Bool
  | CI_Int Integer
  | CI_Text T.Text
  | CI_Array [ConnectorInfo]
  | CI_Obj ConnectorInfoMap

data Connector = Connector
  { conName :: String
  , conLims :: SLLimits
  , conGen :: Maybe (T.Text -> String) -> PLProg -> IO ConnectorInfo
  }

dlo_lims_meet :: Connectors -> DLOpts -> DLOpts
dlo_lims_meet cns dlo = dlo { dlo_lims = meets ls }
  where
    ls = map conLims cs
    cs = map (cns M.!) $ dlo_connectors dlo

type Connectors =
  M.Map String Connector

type ConnectorResult =
  M.Map String ConnectorInfo
