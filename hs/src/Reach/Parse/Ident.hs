module Reach.Parse.Ident
  ( ParseIdentErr (..)
  , parseIdent
  )
where

import Control.Monad.Except
import Language.JavaScript.Parser.AST
import Reach.AST
import Reach.JSUtil

data ParseIdentErr
  = Err_ParseIdent_JSIdentNone -- I dunno when this ever happens

parseIdent :: JSIdent -> Except ParseIdentErr (SrcLoc, String)
parseIdent = \case
  JSIdentName at ident -> pure (srcloc_jsa_only at, ident)
  JSIdentNone -> throwError Err_ParseIdent_JSIdentNone
