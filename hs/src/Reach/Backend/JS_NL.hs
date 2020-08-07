module Reach.Backend.JS_NL where

import Debug.Trace
--- import qualified Data.Text as T
--- import Reach.NL_AST
--- import Reach.Connector
import Reach.Backend

backend_js :: Backend
backend_js _outn _crs _pl = do
  traceM "XXX backend_js"
  return ()
