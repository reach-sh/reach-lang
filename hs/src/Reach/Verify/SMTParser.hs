module Reach.Verify.SMTParser where

import qualified Data.Map as M
import Data.Maybe
import SimpleSMT hiding (not)

data SMTDefine
  = -- | name, hasArgs, type, val
    SMTDefine String Bool SExpr SExpr

parse_define :: SExpr -> (Maybe SMTDefine)
parse_define (List (Atom "define-fun" : Atom name : List args : tySExpr : valSExpr : [])) = Just $ SMTDefine name hasArgs tySExpr valSExpr
  where
    hasArgs = not $ null args
parse_define (List (Atom "declare-sort" : _)) = Nothing
parse_define (List (Atom "declare-datatypes" : _)) = Nothing
parse_define e = error $ "invalid define-fun " <> show e

parse_modelse :: SExpr -> [SMTDefine]
parse_modelse e =
  case e of
    (List (Atom "model" : sexprs)) ->
      catMaybes $ map parse_define sexprs
    _ ->
      error $ "invalid model " <> show e

parseModel :: SExpr -> M.Map String (SExpr, SExpr)
parseModel e = M.fromList m''
  where
    m = parse_modelse e
    m' = filter (\(SMTDefine _ hasArgs _ _) -> not hasArgs) m
    m'' = map (\(SMTDefine name _ ty val) -> (name, (ty, val))) m'
