module Reach.JSUtil
  ( jso_flatten
  , jscl_flatten
  , jsctl_flatten
  , toJSCL
  , jsa_flatten
  , dropEmptyJSStmts
  , jsArrowStmtToBlock
  , jsConciseBodyToBlock
  , jsConciseBodyToStmt
  , jsConciseBodyToStmts
  , jsStmtToBlock
  , tp
  , srcloc_jsa
  , srcloc_after_semi
  , srcloc_lab_only
  , srcloc_jsa_only
  , srcloc_src_only
  , mkCommaList
  , mkArrowParameterList
  )
where

import Language.JavaScript.Parser
import Language.JavaScript.Parser.AST
import Reach.AST.Base

jso_flatten :: JSObjectPropertyList -> [JSObjectProperty]
jso_flatten = \case
  JSCTLComma jscl _ -> jscl_flatten jscl
  JSCTLNone jscl -> jscl_flatten jscl

jscl_flatten :: JSCommaList a -> [a]
jscl_flatten (JSLCons a _ b) = (jscl_flatten a) ++ [b]
jscl_flatten (JSLOne a) = [a]
jscl_flatten (JSLNil) = []

toJSCL :: [a] -> JSCommaList a
toJSCL l = helper l' JSLNil
  where
    -- XXX This makes no sense to me
    sanity = False
    l' =
      case sanity of
        True -> reverse l
        False -> l
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

jsConciseBodyToBlock :: JSConciseBody -> JSBlock
jsConciseBodyToBlock = \case
  JSConciseFunctionBody b -> b
  JSConciseExpressionBody e -> JSBlock JSNoAnnot [JSReturn JSNoAnnot (Just e) JSSemiAuto] JSNoAnnot

jsConciseBodyToStmts :: JSConciseBody -> [JSStatement]
jsConciseBodyToStmts = \case
  JSConciseFunctionBody (JSBlock _ stmts _) -> stmts
  JSConciseExpressionBody e -> [JSReturn JSNoAnnot (Just e) JSSemiAuto]

jsConciseBodyToStmt :: JSConciseBody -> JSStatement
jsConciseBodyToStmt = \case
  JSConciseFunctionBody (JSBlock l stmts r) -> JSStatementBlock l stmts r JSSemiAuto
  JSConciseExpressionBody e -> JSStatementBlock JSNoAnnot [JSReturn JSNoAnnot (Just e) JSSemiAuto] JSNoAnnot JSSemiAuto

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

srcloc_after_semi :: String -> JSAnnot -> JSSemi -> SrcLoc -> SrcLoc
srcloc_after_semi lab a sp at =
  case sp of
    JSSemi x -> srcloc_jsa (alab ++ " semicolon") x at
    JSSemiAuto -> srcloc_jsa alab a at
  where
    alab = "after " ++ lab

srcloc_lab_only :: String -> SrcLoc
srcloc_lab_only s = SrcLoc (Just s) Nothing Nothing

srcloc_jsa_only :: JSAnnot -> SrcLoc
srcloc_jsa_only a = SrcLoc Nothing (tp a) Nothing

srcloc_src_only :: ReachSource -> SrcLoc
srcloc_src_only src = SrcLoc Nothing Nothing (Just src)

mkCommaList :: [JSExpression] -> JSCommaList JSExpression
mkCommaList = aux . reverse
  where
    aux (h : t) = JSLCons (mkCommaList t) JSNoAnnot h
    aux [] = JSLNil

mkArrowParameterList :: [JSExpression] -> JSArrowParameterList
mkArrowParameterList args = JSParenthesizedArrowParameterList JSNoAnnot (mkCommaList args) JSNoAnnot
