module Reach.JSUtil
  ( jso_flatten
  , jscl_flatten
  , jsctl_flatten
  , toJSCL
  , jsa_flatten
  , dropEmptyJSStmts
  , jsArrowStmtToBlock
  , jsStmtToBlock
  , tp
  , srcloc_jsa
  , srcloc_after_semi
  , srcloc_lab_only
  , srcloc_jsa_only
  , srcloc_src_only
  , mkCommaList
  , mkCommaTrailingList
  , jsa
  , a2sp
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

mkCommaTrailingList :: [a] -> JSCommaTrailingList a
mkCommaTrailingList xs = JSCTLComma (toJSCL xs) JSNoAnnot

a2sp :: JSAnnot -> JSSemi
a2sp = JSSemi

--

class HasJSAnnot a where
  jsa :: a -> JSAnnot

instance HasJSAnnot JSAssignOp where
  jsa = \case
    JSAssign a -> a
    JSTimesAssign a -> a
    JSDivideAssign a -> a
    JSModAssign a -> a
    JSPlusAssign a -> a
    JSMinusAssign a -> a
    JSLshAssign a -> a
    JSRshAssign a -> a
    JSUrshAssign a -> a
    JSBwAndAssign a -> a
    JSBwXorAssign a -> a
    JSBwOrAssign a -> a

instance HasJSAnnot JSUnaryOp where
  jsa = \case
    JSUnaryOpDecr a -> a
    JSUnaryOpDelete a -> a
    JSUnaryOpIncr a -> a
    JSUnaryOpMinus a -> a
    JSUnaryOpNot a -> a
    JSUnaryOpPlus a -> a
    JSUnaryOpTilde a -> a
    JSUnaryOpTypeof a -> a
    JSUnaryOpVoid a -> a

instance HasJSAnnot JSBinOp where
  jsa = \case
    JSBinOpAnd a -> a
    JSBinOpBitAnd a -> a
    JSBinOpBitOr a -> a
    JSBinOpBitXor a -> a
    JSBinOpDivide a -> a
    JSBinOpEq a -> a
    JSBinOpGe a -> a
    JSBinOpGt a -> a
    JSBinOpIn a -> a
    JSBinOpInstanceOf a -> a
    JSBinOpLe a -> a
    JSBinOpLsh a -> a
    JSBinOpLt a -> a
    JSBinOpMinus a -> a
    JSBinOpMod a -> a
    JSBinOpNeq a -> a
    JSBinOpOf a -> a
    JSBinOpOr a -> a
    JSBinOpPlus a -> a
    JSBinOpRsh a -> a
    JSBinOpStrictEq a -> a
    JSBinOpStrictNeq a -> a
    JSBinOpTimes a -> a
    JSBinOpUrsh a -> a

instance HasJSAnnot JSExpression where
  jsa = \case
    JSIdentifier a _ -> a
    JSDecimal a _ -> a
    JSLiteral a _ -> a
    JSHexInteger a _ -> a
    JSOctal a _ -> a
    JSStringLiteral a _ -> a
    JSRegEx a _ -> a
    JSArrayLiteral a _ _ -> a
    JSAssignExpression a _ _ -> jsa a
    JSAwaitExpression a _ -> a
    JSCallExpression a _ _ _ -> jsa a
    JSCallExpressionDot a _ _ -> jsa a
    JSCallExpressionSquare a _ _ _ -> jsa a
    JSClassExpression a _ _ _ _ _ -> a
    JSCommaExpression a _ _ -> jsa a
    JSExpressionBinary a _ _ -> jsa a
    JSExpressionParen a _ _ -> a
    JSExpressionPostfix a _ -> jsa a
    JSExpressionTernary a _ _ _ _ -> jsa a
    JSArrowExpression _ a _ -> a
    JSFunctionExpression a _ _ _ _ _ -> a
    JSGeneratorExpression a _ _ _ _ _ _ -> a
    JSMemberDot a _ _ -> jsa a
    JSMemberExpression a _ _ _ -> jsa a
    JSMemberNew a _ _ _ _ -> a
    JSMemberSquare a _ _ _ -> jsa a
    JSNewExpression a _ -> a
    JSObjectLiteral a _ _ -> a
    JSSpreadExpression a _ -> a
    JSTemplateLiteral _ a _ _ -> a
    JSUnaryExpression a _ -> jsa a
    JSVarInitExpression a _ -> jsa a
    JSYieldExpression a _ -> a
    JSYieldFromExpression a _ _ -> a

