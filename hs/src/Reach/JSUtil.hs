module Reach.JSUtil (jso_flatten, jscl_flatten, jsctl_flatten, toJSCL, jsa_flatten, dropEmptyJSStmts, jsArrowStmtToBlock, jsStmtToBlock, tp, srcloc_jsa) where

import Language.JavaScript.Parser
import Language.JavaScript.Parser.AST
import Reach.AST

jso_flatten :: JSObjectPropertyList -> [JSObjectProperty]
jso_flatten = \case
  JSCTLComma jscl _ -> jscl_flatten jscl
  JSCTLNone jscl -> jscl_flatten jscl

jscl_flatten :: JSCommaList a -> [a]
jscl_flatten (JSLCons a _ b) = (jscl_flatten a) ++ [b]
jscl_flatten (JSLOne a) = [a]
jscl_flatten (JSLNil) = []

toJSCL :: [a] -> JSCommaList a
toJSCL l = helper (reverse l) JSLNil
  where
    helper [] acc = acc
    helper (x : ys) acc =
      helper ys (JSLCons acc JSNoAnnot x)

jsctl_flatten :: JSCommaTrailingList a -> [a]
jsctl_flatten (JSCTLComma a _) = jscl_flatten a
jsctl_flatten (JSCTLNone a) = jscl_flatten a

jsa_flatten :: [JSArrayElement] -> [JSExpression]
jsa_flatten a = concatMap f a
  where
    f (JSArrayComma _) = []
    f (JSArrayElement e) = [e]

jsArrowStmtToBlock :: JSStatement -> JSBlock
jsArrowStmtToBlock = \case
  JSExpressionStatement e sp -> JSBlock JSNoAnnot [JSReturn JSNoAnnot (Just e) sp] JSNoAnnot
  JSMethodCall e la args ra sp -> JSBlock la [JSReturn la (Just (JSCallExpression e la args ra)) sp] ra
  s -> jsStmtToBlock s

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
