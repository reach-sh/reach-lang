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
  , rjsa
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
      concatMap
        (\case
           JSPropertyNameandValue jpn _ _ ->
             case jpn of
               JSPropertyIdent ja s -> [JSIdentifier ja s]
               _ -> []
           JSPropertyIdentRef ja s -> [JSIdentifier ja s]
           JSObjectMethod _ -> []
           JSObjectSpread _ je -> jsFlattenLHS je)
        $ jso_flatten ps
    _ -> []

toJSArray :: [JSExpression] -> [JSArrayElement]
toJSArray a = concatMap f a
  where
    f e = [JSArrayElement e, JSArrayComma (jsa e)]

jsArrowBodyToRetBlock :: JSConciseBody -> JSBlock
jsArrowBodyToRetBlock = \case
  JSConciseExprBody e -> JSBlock a [JSReturn a (Just e) (JSSemi a)] a
    where
      a = jsa e
  JSConciseFunBody b -> b

jsArrowBodyToBlock :: JSConciseBody -> JSBlock
jsArrowBodyToBlock = \case
  JSConciseExprBody e -> JSBlock a [JSExpressionStatement e (JSSemi a)] a
    where
      a = jsa e
  JSConciseFunBody b -> b

jsArrowBodyToStmt :: JSConciseBody -> JSStatement
jsArrowBodyToStmt = \case
  JSConciseExprBody e -> JSExpressionStatement e JSSemiAuto
  JSConciseFunBody (JSBlock a ss a2) -> JSStatementBlock a ss a2 JSSemiAuto

jsStmtToBlock :: JSStatement -> JSBlock
jsStmtToBlock = \case
  JSStatementBlock ba bodyss aa _ -> JSBlock ba bodyss aa
  bodys -> JSBlock a [bodys] a
    where
      a = jsa bodys

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
  rjsa :: JSAnnot -> a -> a

rjsaJSL :: HasJSAnnot a => JSAnnot -> JSCommaList a -> JSCommaList a
rjsaJSL a' y = (rjsaJSL a' y)

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
  rjsa a' = \case
    JSAssign _ -> JSAssign a'
    JSTimesAssign _ -> JSTimesAssign a'
    JSDivideAssign _ -> JSDivideAssign a'
    JSModAssign _ -> JSModAssign a'
    JSPlusAssign _ -> JSPlusAssign a'
    JSMinusAssign _ -> JSMinusAssign a'
    JSLshAssign _ -> JSLshAssign a'
    JSRshAssign _ -> JSRshAssign a'
    JSUrshAssign _ -> JSUrshAssign a'
    JSBwAndAssign _ -> JSBwAndAssign a'
    JSBwXorAssign _ -> JSBwXorAssign a'
    JSBwOrAssign _ -> JSBwOrAssign a'

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
  rjsa a' = \case
    JSUnaryOpDecr _ -> JSUnaryOpDecr a'
    JSUnaryOpDelete _ -> JSUnaryOpDelete a'
    JSUnaryOpIncr _ -> JSUnaryOpIncr a'
    JSUnaryOpMinus _ -> JSUnaryOpMinus a'
    JSUnaryOpNot _ -> JSUnaryOpNot a'
    JSUnaryOpPlus _ -> JSUnaryOpPlus a'
    JSUnaryOpTilde _ -> JSUnaryOpTilde a'
    JSUnaryOpTypeof _ -> JSUnaryOpTypeof a'
    JSUnaryOpVoid _ -> JSUnaryOpVoid a'

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
  rjsa a' = \case
    JSBinOpAnd _ -> JSBinOpAnd a'
    JSBinOpBitAnd _ -> JSBinOpBitAnd a'
    JSBinOpBitOr _ -> JSBinOpBitOr a'
    JSBinOpBitXor _ -> JSBinOpBitXor a'
    JSBinOpDivide _ -> JSBinOpDivide a'
    JSBinOpEq _ -> JSBinOpEq a'
    JSBinOpGe _ -> JSBinOpGe a'
    JSBinOpGt _ -> JSBinOpGt a'
    JSBinOpIn _ -> JSBinOpIn a'
    JSBinOpInstanceOf _ -> JSBinOpInstanceOf a'
    JSBinOpLe _ -> JSBinOpLe a'
    JSBinOpLsh _ -> JSBinOpLsh a'
    JSBinOpLt _ -> JSBinOpLt a'
    JSBinOpMinus _ -> JSBinOpMinus a'
    JSBinOpMod _ -> JSBinOpMod a'
    JSBinOpNeq _ -> JSBinOpNeq a'
    JSBinOpOf _ -> JSBinOpOf a'
    JSBinOpOr _ -> JSBinOpOr a'
    JSBinOpPlus _ -> JSBinOpPlus a'
    JSBinOpRsh _ -> JSBinOpRsh a'
    JSBinOpStrictEq _ -> JSBinOpStrictEq a'
    JSBinOpStrictNeq _ -> JSBinOpStrictNeq a'
    JSBinOpTimes _ -> JSBinOpTimes a'
    JSBinOpUrsh _ -> JSBinOpUrsh a'

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
  rjsa a' = \case
    JSIdentifier _ x -> JSIdentifier a' x
    JSDecimal _ x -> JSDecimal a' x
    JSLiteral _ x -> JSLiteral a' x
    JSHexInteger _ x -> JSHexInteger a' x
    JSOctal _ x -> JSOctal a' x
    JSStringLiteral _ x -> JSStringLiteral a' x
    JSRegEx _ x -> JSRegEx a' x
    JSArrayLiteral _ x _ -> JSArrayLiteral a' x a'
    JSAssignExpression a x y -> JSAssignExpression (rjsa a' a) (rjsa a' x) (rjsa a' y)
    JSAwaitExpression _ x -> JSAwaitExpression a' (rjsa a' x)
    JSCallExpression a _ y _ -> JSCallExpression (rjsa a' a) a' (rjsaJSL a' y) a'
    JSCallExpressionDot a _ y -> JSCallExpressionDot (rjsa a' a) a' (rjsa a' y)
    JSCallExpressionSquare a _ y _ -> JSCallExpressionSquare (rjsa a' a) a' (rjsa a' y) a'
    JSClassExpression _ x y z r _ -> JSClassExpression a' x y z r a'
    JSCommaExpression a _ y -> JSCommaExpression (rjsa a' a) a' (rjsa a' y)
    JSExpressionBinary a x y -> JSExpressionBinary (rjsa a' a) (rjsa a' x) (rjsa a' y)
    JSExpressionParen _ x _ -> JSExpressionParen a' (rjsa a' x) a'
    JSExpressionPostfix a x -> JSExpressionPostfix (rjsa a' a) (rjsa a' x)
    JSExpressionTernary a _ y _ r -> JSExpressionTernary (rjsa a' a) a' (rjsa a' y) a' (rjsa a' r)
    JSArrowExpression x _ y -> JSArrowExpression x a' y
    JSFunctionExpression _ x _ z _ s -> JSFunctionExpression a' x a' (rjsaJSL a' z) a' s
    JSGeneratorExpression _ _ y _ r _ t -> JSGeneratorExpression a' a' y a' (rjsaJSL a' r) a' t
    JSMemberDot a _ y -> JSMemberDot (rjsa a' a) a' (rjsa a' y)
    JSMemberExpression a _ y _ -> JSMemberExpression (rjsa a' a) a' (rjsaJSL a' y) a'
    JSMemberNew _ x _ z _ -> JSMemberNew a' (rjsa a' x) a' (rjsaJSL a' z) a'
    JSMemberSquare a _ y _ -> JSMemberSquare (rjsa a' a) a'  (rjsa a' y) a'
    JSNewExpression _ x -> JSNewExpression a' (rjsa a' x)
    JSObjectLiteral _ x _ -> JSObjectLiteral a' x a'
    JSSpreadExpression _ x -> JSSpreadExpression a' (rjsa a' x)
    JSTemplateLiteral x _ y z -> JSTemplateLiteral (fmap (rjsa a') x) a' y z
    JSUnaryExpression a x -> JSUnaryExpression (rjsa a' a) (rjsa a' x)
    JSVarInitExpression a x -> JSVarInitExpression (rjsa a' a) x
    JSYieldExpression _ x -> JSYieldExpression a' (fmap (rjsa a') x)
    JSYieldFromExpression _ _ y -> JSYieldFromExpression a' a' (rjsa a' y)

instance HasJSAnnot JSObjectProperty where
  jsa = \case
    JSPropertyNameandValue _ ja _ -> ja
    JSPropertyIdentRef ja _ -> ja
    JSObjectMethod jmd -> jsa jmd
    JSObjectSpread ja _ -> ja
  rjsa a' = \case
    JSPropertyNameandValue x _ y -> JSPropertyNameandValue x a' (fmap (rjsa a') y)
    JSPropertyIdentRef _ x -> JSPropertyIdentRef a' x
    JSObjectMethod jmd -> JSObjectMethod $ rjsa a' jmd
    JSObjectSpread _ x -> JSObjectSpread a' (rjsa a' x)

instance HasJSAnnot JSMethodDefinition where
  jsa = \case
    JSMethodDefinition _ ja _ _ _ -> ja
    JSGeneratorMethodDefinition ja _ _ _ _ _ -> ja
    JSPropertyAccessor _ _ ja _ _ _ -> ja
  rjsa a' = \case
    JSMethodDefinition x _ y _ r -> JSMethodDefinition x a' (rjsaJSL a' y) a' r
    JSGeneratorMethodDefinition _ x _ z _ s -> JSGeneratorMethodDefinition a' x a' (rjsaJSL a' z) a' s
    JSPropertyAccessor x y _ z _ s -> JSPropertyAccessor x y a' (rjsaJSL a' z) a' s

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
  rjsa a' = \case
    JSStatementBlock _ x _ z -> JSStatementBlock a' (fmap (rjsa a') x) a' z
    JSBreak _ x y -> JSBreak a' x y
    JSLet _ x y -> JSLet a' (toJSCL (map (rjsa a') $ jscl_flatten x)) y
    JSClass _ x y _z r _s t -> JSClass a' x y a' r a' t
    JSConstant _ x y -> JSConstant a' (toJSCL (map (rjsa a') $ jscl_flatten x)) y
    JSContinue _ x y -> JSContinue a' x y
    JSDoWhile _ x _ _ r _ t -> JSDoWhile a' (rjsa a' x) a' a' (rjsa a' r) a' t
    JSFor _ _ y _ r _ t u v -> JSFor a' a' (rjsaJSL a' y) a' (rjsaJSL a' r) a' (rjsaJSL a' t) u (rjsa a' v)
    JSForIn _ _ y z r s t -> JSForIn a' a' (rjsa a' y) z (rjsa a' r) s (rjsa a' t)
    JSForVar _ _ _ z _ s _ u _ w -> JSForVar a' a' a' (rjsaJSL a' z) a' (rjsaJSL a' s) a' (rjsaJSL a' u) a' (rjsa a' w)
    JSForVarIn _ _ _ z r s _ u -> JSForVarIn a' a' a' (rjsa a' z) r (rjsa a' s) a' (rjsa a' u)
    JSForLet _ _ _ z _ s _ u _ w -> JSForLet a' a' a' (rjsaJSL a' z) a' (rjsaJSL a' s) a' (rjsaJSL a' u) a' (rjsa a' w)
    JSForLetIn _ _x _ z r s _ u -> JSForLetIn a' a' a' (rjsa a' z) r (rjsa a' s) a' (rjsa a' u)
    JSForLetOf _ _x _ z r s _ u -> JSForLetOf a' a' a' (rjsa a' z) r (rjsa a' s) a' (rjsa a' u)
    JSForConst _ _x _y z _r s _t u _v w -> JSForConst a' a' a' (rjsaJSL a' z) a' (rjsaJSL a' s) a' (rjsaJSL a' u) a' (rjsa a' w)
    JSForConstIn _ _ _ z r s _ u -> JSForConstIn a' a' a' (rjsa a' z) r (rjsa a' s) a' (rjsa a' u)
    JSForConstOf _ _ _ z r s _ u -> JSForConstOf a' a' a' (rjsa a' z) r (rjsa a' s) a' (rjsa a' u)
    JSForOf _ _ y z r _ t -> JSForOf a' a' (rjsa a' y) z (rjsa a' r) a' (rjsa a' t)
    JSForVarOf _ _ _ z r s _ u -> JSForVarOf a' a' a' (rjsa a' z) r (rjsa a' s) a' (rjsa a' u)
    JSAsyncFunction _ _x y _ r _ t u -> JSAsyncFunction a' a' y a' (rjsaJSL a' r) a' t u
    JSFunction _ x _ z _ s t -> JSFunction a' x a' (rjsaJSL a' z) a' s t
    JSGenerator _ _ y _ r _ t u -> JSGenerator a' a' y a' (rjsaJSL a' r) a' t u
    JSIf _ _x y _ r -> JSIf a' a' (rjsa a' y) a' (rjsa a' r)
    JSIfElse _ _ y _ r _ t -> JSIfElse a' a' (rjsa a' y) a' (rjsa a' r) a' (rjsa a' t)
    JSLabelled x _ y -> JSLabelled x a' (rjsa a' y)
    JSEmptyStatement _ -> JSEmptyStatement a'
    JSExpressionStatement e x -> JSExpressionStatement (rjsa a' e) x
    JSAssignStatement e x y z -> JSAssignStatement (rjsa a' e) x (rjsa a' y) z
    JSMethodCall r _ x _ z -> JSMethodCall (rjsa a' r) a' (rjsaJSL a' x) a' z
    JSReturn _ x y -> JSReturn a' (fmap (rjsa a') x) y
    JSSwitch _ _ y _ _ s _ u -> JSSwitch a' a' (rjsa a' y) a' a' s a' u
    JSThrow _ x y -> JSThrow a' (rjsa a' x) y
    JSTry _ x y z -> JSTry a' x y z
    JSVariable _ x y -> JSVariable a' (rjsaJSL a' x) y
    JSWhile _ _ y _ r -> JSWhile a' a' (rjsa a' y) a' (rjsa a' r)
    JSWith _ _ y _ r s -> JSWith a' a' (rjsa a' y) a' (rjsa a' r) s

jsaList :: HasJSAnnot a => JSAnnot -> [a] -> JSAnnot
jsaList def = \case
  [] -> def
  h : _ -> jsa h

jsString :: JSAnnot -> String -> JSExpression
jsString a s = JSStringLiteral a $ "'" <> s <> "'"
