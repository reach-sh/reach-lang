module Reach.JSUtil
  ( jso_flatten
  , jscl_flatten
  , jsctl_flatten
  , toJSCL
  , jsa_flatten
  , dropEmptyJSStmts
  , jsArrowBodyToRetBlock
  , jsArrowBodyToStmt
  , jsArrowBodyToBlock
  , jsStmtToConciseBody
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
  , toJSArray
  , jsBlockToStmts
  , jsFlattenLHS
  , jsString
  , jsaList
  )
where

import Data.Foldable (foldl')
import Language.JavaScript.Parser
import Language.JavaScript.Parser.AST
import Reach.AST.Base

jso_flatten :: JSObjectPropertyList -> [JSObjectProperty]
jso_flatten = \case
  JSCTLComma jscl _ -> jscl_flatten jscl
  JSCTLNone jscl -> jscl_flatten jscl

jscl_flatten :: JSCommaList a -> [a]
jscl_flatten = \case
  JSLNil -> []
  JSLOne a -> [a]
  JSLCons a _ b -> (jscl_flatten a) ++ [b]

toJSCL :: HasJSAnnot a => [a] -> JSCommaList a
toJSCL = \case
  [] -> JSLNil
  [a] -> JSLOne a
  x : ys -> foldl' (flip JSLCons (jsa x)) (JSLOne x) ys

jsctl_flatten :: JSCommaTrailingList a -> [a]
jsctl_flatten = \case
  JSCTLComma a _ -> jscl_flatten a
  JSCTLNone a -> jscl_flatten a

jsa_flatten :: [JSArrayElement] -> [JSExpression]
jsa_flatten a = concatMap f a
  where
    f (JSArrayComma _) = []
    f (JSArrayElement e) = [e]

jsFlattenLHS :: JSExpression -> [JSExpression]
jsFlattenLHS e =
  case e of
    JSIdentifier _ "_" -> []
    JSIdentifier {} -> [e]
    JSArrayLiteral _ as _ -> concatMap jsFlattenLHS $ jsa_flatten as
    JSAssignExpression lhs _ _ -> [lhs]
    JSObjectLiteral _ ps _ ->
      concatMap (\case
      JSPropertyNameandValue jpn _ _ ->
        case jpn of
          JSPropertyIdent ja s -> [JSIdentifier ja s]
          _ -> []
      JSPropertyIdentRef ja s -> [JSIdentifier ja s]
      JSObjectMethod _ -> []
      JSObjectSpread _ je -> jsFlattenLHS je
      ) $ jso_flatten ps
    _ -> []

toJSArray :: [JSExpression] -> [JSArrayElement]
toJSArray a = concatMap f a
  where
    f e = [JSArrayElement e, JSArrayComma (jsa e)]

jsArrowBodyToRetBlock :: JSConciseBody -> JSBlock
jsArrowBodyToRetBlock = \case
  JSConciseExprBody e -> JSBlock a [JSReturn a (Just e) (JSSemi a)] a
    where a = jsa e
  JSConciseFunBody b -> b

jsArrowBodyToBlock :: JSConciseBody -> JSBlock
jsArrowBodyToBlock = \case
  JSConciseExprBody e -> JSBlock a [JSExpressionStatement e (JSSemi a)] a
    where a = jsa e
  JSConciseFunBody b -> b

jsArrowBodyToStmt :: JSConciseBody -> JSStatement
jsArrowBodyToStmt = \case
  JSConciseExprBody e -> JSExpressionStatement e JSSemiAuto
  JSConciseFunBody (JSBlock a ss a2) -> JSStatementBlock a ss a2 JSSemiAuto

jsStmtToBlock :: JSStatement -> JSBlock
jsStmtToBlock = \case
  JSStatementBlock ba bodyss aa _ -> JSBlock ba bodyss aa
  bodys -> JSBlock a [bodys] a
    where a = jsa bodys

jsStmtToConciseBody :: JSStatement -> JSConciseBody
jsStmtToConciseBody = \case
  JSExpressionStatement e _ -> JSConciseExprBody e
  JSMethodCall e la args ra _ -> JSConciseExprBody (JSCallExpression e la args ra)
  ow -> JSConciseFunBody $ jsStmtToBlock ow

jsBlockToStmts :: JSBlock -> [JSStatement]
jsBlockToStmts (JSBlock _ s _) = s

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
tp = \case
  JSAnnot x _ -> Just x
  JSAnnotSpace -> Nothing
  JSNoAnnot -> Nothing

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
    aux (h : t) = JSLCons (mkCommaList t) (jsa h) h
    aux [] = JSLNil

mkCommaTrailingList :: HasJSAnnot a => [a] -> JSCommaTrailingList a
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

instance HasJSAnnot JSObjectProperty where
  jsa = \case
    JSPropertyNameandValue _ ja _ -> ja
    JSPropertyIdentRef ja _ -> ja
    JSObjectMethod jmd -> jsa jmd
    JSObjectSpread ja _ -> ja

instance HasJSAnnot JSMethodDefinition where
  jsa = \case
    JSMethodDefinition _ ja _ _ _ -> ja
    JSGeneratorMethodDefinition ja _ _ _ _ _ -> ja
    JSPropertyAccessor _ _ ja _ _ _ -> ja

instance HasJSAnnot JSStatement where
  jsa = \case
    JSStatementBlock ja _ _ _ -> ja
    JSBreak ja _ _ -> ja
    JSLet ja _ _ -> ja
    JSClass ja _ _ _ _ _ _ -> ja
    JSConstant ja _ _ -> ja
    JSContinue ja _ _ -> ja
    JSDoWhile ja _ _ _ _ _ _ -> ja
    JSFor ja _ _ _ _ _ _ _ _ -> ja
    JSForIn ja _ _ _ _ _ _ -> ja
    JSForVar ja _ _ _ _ _ _ _ _ _ -> ja
    JSForVarIn ja _ _ _ _ _ _ _ -> ja
    JSForLet ja _ _ _ _ _ _ _ _ _ -> ja
    JSForLetIn ja _ _ _ _ _ _ _ -> ja
    JSForLetOf ja _ _ _ _ _ _ _ -> ja
    JSForConst ja _ _ _ _ _ _ _ _ _ -> ja
    JSForConstIn ja _ _ _ _ _ _ _ -> ja
    JSForConstOf ja _ _ _ _ _ _ _ -> ja
    JSForOf ja _ _ _ _ _ _ -> ja
    JSForVarOf ja _ _ _ _ _ _ _ -> ja
    JSAsyncFunction ja _ _ _ _ _ _ _ -> ja
    JSFunction ja _ _ _ _ _ _ -> ja
    JSGenerator ja _ _ _ _ _ _ _ -> ja
    JSIf ja _ _ _ _ -> ja
    JSIfElse ja _ _ _ _ _ _ -> ja
    JSLabelled _ ja _ -> ja
    JSEmptyStatement ja -> ja
    JSExpressionStatement e _ -> jsa e
    JSAssignStatement e _ _ _ -> jsa e
    JSMethodCall _ ja _ _ _ -> ja
    JSReturn ja _ _ -> ja
    JSSwitch ja _ _ _ _ _ _ _ -> ja
    JSThrow ja _ _ -> ja
    JSTry ja _ _ _ -> ja
    JSVariable ja _ _ -> ja
    JSWhile ja _ _ _ _ -> ja
    JSWith ja _ _ _ _ _ -> ja

jsaList :: HasJSAnnot a => JSAnnot -> [a] -> JSAnnot
jsaList def = \case
  [] -> def
  h:_ -> jsa h

jsString :: JSAnnot -> String -> JSExpression
jsString a s = JSStringLiteral a $ "'" <> s <> "'"
