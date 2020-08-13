module Reach.JSUtil (jscl_flatten, jsctl_flatten, jsa_flatten, dropEmptyJSStmts, jsStmtToBlock, tp, srcloc_jsa) where

import Language.JavaScript.Parser
import Language.JavaScript.Parser.AST
import Reach.AST

jscl_flatten :: JSCommaList a -> [a]
jscl_flatten (JSLCons a _ b) = (jscl_flatten a) ++ [b]
jscl_flatten (JSLOne a) = [a]
jscl_flatten (JSLNil) = []

jsctl_flatten :: JSCommaTrailingList a -> [a]
jsctl_flatten (JSCTLComma a _) = jscl_flatten a
jsctl_flatten (JSCTLNone a) = jscl_flatten a

jsa_flatten :: [JSArrayElement] -> [JSExpression]
jsa_flatten a = concatMap f a
  where
    f (JSArrayComma _) = []
    f (JSArrayElement e) = [e]

jsStmtToBlock :: JSStatement -> JSBlock
jsStmtToBlock = \case
  JSStatementBlock ba bodyss aa _ -> JSBlock ba bodyss aa
  bodys -> JSBlock JSNoAnnot [bodys] JSNoAnnot

dropEmptyJSStmts :: [JSStatement] -> [JSStatement]
dropEmptyJSStmts [] = []
dropEmptyJSStmts (s : ks) =
  case s of
    (JSStatementBlock a ss b sp) ->
      case dropEmptyJSStmts ss of
        [] -> ks'
        ss' -> (JSStatementBlock a ss' b sp) : ks'
    (JSEmptyStatement _) -> ks'
    _ -> s : ks'
  where
    ks' = dropEmptyJSStmts ks

tp :: JSAnnot -> Maybe TokenPosn
tp (JSAnnot x _) = Just x
tp JSAnnotSpace = Nothing
tp JSNoAnnot = Nothing

srcloc_jsa :: String -> JSAnnot -> SrcLoc -> SrcLoc
srcloc_jsa lab a at = srcloc_at lab (tp a) at
