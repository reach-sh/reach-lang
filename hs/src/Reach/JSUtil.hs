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
  , getFormals
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
  JSLCons a _ b -> (jscl_flatten a) <> [b]

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
    JSSemi x -> srcloc_jsa (alab <> " semicolon") x at
    JSSemiAuto -> srcloc_jsa alab a at
  where
    alab = "after " <> lab

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

getFormals :: JSExpression -> [JSExpression]
getFormals = \case
  JSArrowExpression x _ _ ->
    case x of
      JSUnparenthesizedArrowParameter (JSIdentName at s) -> [JSIdentifier at s]
      JSParenthesizedArrowParameterList _ jcl _ -> jscl_flatten jcl
      _ -> []
  JSExpressionParen _ e _ -> getFormals e
  _ -> []

--

class HasJSAnnot a where
  jsa :: a -> JSAnnot

class RepJSAnnot a where
  rjsa :: JSAnnot -> a -> a

instance (RepJSAnnot a) => RepJSAnnot (JSCommaList a) where
  rjsa a' = \case
    JSLNil -> JSLNil
    JSLOne a -> JSLOne $ rjsa a' a
    JSLCons cl _ x -> JSLCons (rjsa a' cl) a' $ rjsa a' x

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

instance RepJSAnnot JSAssignOp where
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

instance RepJSAnnot JSUnaryOp where
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

instance RepJSAnnot JSBinOp where
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

instance HasJSAnnot JSArrayElement where
  jsa = \case
    JSArrayElement e -> jsa e
    JSArrayComma a -> a

instance RepJSAnnot JSArrayElement where
  rjsa a' = \case
    JSArrayElement e -> JSArrayElement $ rjsa a' e
    JSArrayComma _ -> JSArrayComma a'

instance RepJSAnnot JSObjectPropertyList where
  rjsa a' = \case
    JSCTLComma l _ -> JSCTLComma (rjsa a' l) a'
    JSCTLNone l  -> JSCTLNone $ rjsa a' l

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

instance RepJSAnnot JSExpression where
  rjsa a' = \case
    JSIdentifier _ x -> JSIdentifier a' x
    JSDecimal _ x -> JSDecimal a' x
    JSLiteral _ x -> JSLiteral a' x
    JSHexInteger _ x -> JSHexInteger a' x
    JSOctal _ x -> JSOctal a' x
    JSStringLiteral _ x -> JSStringLiteral a' x
    JSRegEx _ x -> JSRegEx a' x
    JSArrayLiteral _ x _ -> JSArrayLiteral a' (f x) a'
    JSAssignExpression a x y -> JSAssignExpression (f a) (f x) (f y)
    JSAwaitExpression _ x -> JSAwaitExpression a' (f x)
    JSCallExpression a _ y _ -> JSCallExpression (f a) a' (f y) a'
    JSCallExpressionDot a _ y -> JSCallExpressionDot (f a) a' (f y)
    JSCallExpressionSquare a _ y _ -> JSCallExpressionSquare (f a) a' (f y) a'
    JSClassExpression _ x y _ r _ -> JSClassExpression a' (f x) y a' r a'
    JSCommaExpression a _ y -> JSCommaExpression (f a) a' (f y)
    JSExpressionBinary a x y -> JSExpressionBinary (f a) (f x) (f y)
    JSExpressionParen _ x _ -> JSExpressionParen a' (f x) a'
    JSExpressionPostfix a x -> JSExpressionPostfix (f a) (f x)
    JSExpressionTernary a _ y _ r -> JSExpressionTernary (f a) a' (f y) a' (f r)
    JSArrowExpression x _ y -> JSArrowExpression (f x) a' $ f y
    JSFunctionExpression _ x _ z _ s -> JSFunctionExpression a' (f x) a' (f z) a' (f s)
    JSGeneratorExpression _ _ y _ r _ t -> JSGeneratorExpression a' a' y a' (f r) a' (f t)
    JSMemberDot a _ y -> JSMemberDot (f a) a' (f y)
    JSMemberExpression a _ y _ -> JSMemberExpression (f a) a' (f y) a'
    JSMemberNew _ x _ z _ -> JSMemberNew a' (f x) a' (f z) a'
    JSMemberSquare a _ y _ -> JSMemberSquare (f a) a'  (f y) a'
    JSNewExpression _ x -> JSNewExpression a' (f x)
    JSObjectLiteral _ x _ -> JSObjectLiteral a' (f x) a'
    JSSpreadExpression _ x -> JSSpreadExpression a' (f x)
    JSTemplateLiteral x _ y z -> JSTemplateLiteral (f x) a' y (f z)
    JSUnaryExpression a x -> JSUnaryExpression (f a) (f x)
    JSVarInitExpression a x -> JSVarInitExpression (f a) $ f x
    JSYieldExpression _ x -> JSYieldExpression a' (f x)
    JSYieldFromExpression _ _ y -> JSYieldFromExpression a' a' (f y)
    where
      f :: (RepJSAnnot a) => a -> a
      f = rjsa a'

instance HasJSAnnot JSObjectProperty where
  jsa = \case
    JSPropertyNameandValue _ ja _ -> ja
    JSPropertyIdentRef ja _ -> ja
    JSObjectMethod jmd -> jsa jmd
    JSObjectSpread ja _ -> ja

instance RepJSAnnot JSObjectProperty where
  rjsa a' = \case
    JSPropertyNameandValue x _ y -> JSPropertyNameandValue (f x) a' (f y)
    JSPropertyIdentRef _ x -> JSPropertyIdentRef a' x
    JSObjectMethod jmd -> JSObjectMethod $ f jmd
    JSObjectSpread _ x -> JSObjectSpread a' (f x)
    where
      f :: (RepJSAnnot a) => a -> a
      f = rjsa a'

instance HasJSAnnot JSMethodDefinition where
  jsa = \case
    JSMethodDefinition _ ja _ _ _ -> ja
    JSGeneratorMethodDefinition ja _ _ _ _ _ -> ja
    JSPropertyAccessor _ _ ja _ _ _ -> ja

instance RepJSAnnot JSMethodDefinition where
  rjsa a' = \case
    JSMethodDefinition x _ y _ r -> JSMethodDefinition x a' (f y) a' (f r)
    JSGeneratorMethodDefinition _ x _ z _ s -> JSGeneratorMethodDefinition a' x a' (f z) a' (f s)
    JSPropertyAccessor x y _ z _ s -> JSPropertyAccessor x y a' (f z) a' (f s)
    where
      f :: (RepJSAnnot a) => a -> a
      f = rjsa a'

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

instance RepJSAnnot JSStatement where
  rjsa a' = \case
    JSStatementBlock _ x _ z -> JSStatementBlock a' (f x) a' (f z)
    JSBreak _ x y -> JSBreak a' (f x) (f y)
    JSLet _ x y -> JSLet a' (f x) (f y)
    JSClass _ x y _z r _s t -> JSClass a' (f x) y a' r a' (f t)
    JSConstant _ x y -> JSConstant a' (f x) y
    JSContinue _ x y -> JSContinue a' (f x) (f y)
    JSDoWhile _ x _ _ r _ t -> JSDoWhile a' (f x) a' a' (f r) a' (f t)
    JSFor _ _ y _ r _ t _ v -> JSFor a' a' (f y) a' (f r) a' (f t) a' (f v)
    JSForIn _ _ y z r _s t -> JSForIn a' a' (f y) (f z) (f r) a' (f t)
    JSForVar _ _ _ z _ s _ u _ w -> JSForVar a' a' a' (f z) a' (f s) a' (f u) a' (f w)
    JSForVarIn _ _ _ z r s _ u -> JSForVarIn a' a' a' (f z) r (f s) a' (f u)
    JSForLet _ _ _ z _ s _ u _ w -> JSForLet a' a' a' (f z) a' (f s) a' (f u) a' (f w)
    JSForLetIn _ _x _ z r s _ u -> JSForLetIn a' a' a' (f z) (f r) (f s) a' (f u)
    JSForLetOf _ _x _ z r s _ u -> JSForLetOf a' a' a' (f z) (f r) (f s) a' (f u)
    JSForConst _ _x _y z _r s _t u _v w -> JSForConst a' a' a' (f z) a' (f s) a' (f u) a' (f w)
    JSForConstIn _ _ _ z r s _ u -> JSForConstIn a' a' a' (f z) r (f s) a' (f u)
    JSForConstOf _ _ _ z r s _ u -> JSForConstOf a' a' a' (f z) r (f s) a' (f u)
    JSForOf _ _ y z r _ t -> JSForOf a' a' (f y) (f z) (f r) a' (f t)
    JSForVarOf _ _ _ z r s _ u -> JSForVarOf a' a' a' (f z) (f r) (f s) a' (f u)
    JSAsyncFunction _ _x y _ r _ t u -> JSAsyncFunction a' a' y a' (f r) a' (f t) (f u)
    JSFunction _ x _ z _ s t -> JSFunction a' (f x) a' (f z) a' (f s) (f t)
    JSGenerator _ _ y _ r _ t u -> JSGenerator a' a' (f y) a' (f r) a' (f t) (f u)
    JSIf _ _x y _ r -> JSIf a' a' (f y) a' (f r)
    JSIfElse _ _ y _ r _ t -> JSIfElse a' a' (f y) a' (f r) a' (f t)
    JSLabelled x _ y -> JSLabelled (f x) a' (f y)
    JSEmptyStatement _ -> JSEmptyStatement a'
    JSExpressionStatement e x -> JSExpressionStatement (f e) (f x)
    JSAssignStatement e x y z -> JSAssignStatement (f e) (f x) (f y) (f z)
    JSMethodCall r _ x _ z -> JSMethodCall (f r) a' (f x) a' (f z)
    JSReturn _ x y -> JSReturn a' (f x) (f y)
    JSSwitch _ _ y _ _ s _ u -> JSSwitch a' a' (f y) a' a' s a' (f u)
    JSThrow _ x y -> JSThrow a' (f x) (f y)
    JSTry _ x y z -> JSTry a' (f x) (f y) (f z)
    JSVariable _ x y -> JSVariable a' (f x) (f y)
    JSWhile _ _ y _ r -> JSWhile a' a' (f y) a' (f r)
    JSWith _ _ y _ r s -> JSWith a' a' (f y) a' (f r) (f s)
    where
      f :: (RepJSAnnot a) => a -> a
      f = rjsa a'

instance RepJSAnnot JSArrowParameterList where
  rjsa a' = \case
    JSUnparenthesizedArrowParameter y -> JSUnparenthesizedArrowParameter (f y)
    JSParenthesizedArrowParameterList _ l _ -> JSParenthesizedArrowParameterList a' (f l) a'
    where
      f :: (RepJSAnnot a) => a -> a
      f = rjsa a'

instance RepJSAnnot JSIdent where
  rjsa a' = \case
    JSIdentName _ s -> JSIdentName a' s
    JSIdentNone -> JSIdentNone

instance RepJSAnnot JSBlock where
  rjsa a' = \case
    JSBlock _ l _ -> JSBlock a' (rjsa a' l) a'

instance RepJSAnnot JSTemplatePart where
  rjsa a' = \case
    JSTemplatePart x _ s -> JSTemplatePart (rjsa a' x) a' s

instance RepJSAnnot JSVarInitializer where
  rjsa a' = \case
    JSVarInit _ x -> JSVarInit a' x
    JSVarInitNone -> JSVarInitNone

instance RepJSAnnot JSPropertyName where
  rjsa a' = \case
    JSPropertyIdent _ s  -> JSPropertyIdent a' s
    JSPropertyString _ s -> JSPropertyString a' s
    JSPropertyNumber _ s -> JSPropertyNumber a' s
    JSPropertyComputed _ x _ -> JSPropertyComputed a' (rjsa a' x) a'

instance RepJSAnnot JSSemi where
  rjsa a' = \case
    JSSemi _ -> JSSemi a'
    JSSemiAuto -> JSSemiAuto

instance RepJSAnnot JSTryCatch where
  rjsa a' = \case
    JSCatch _ _ x _ y -> JSCatch a' a' (f x) a' y
    JSCatchIf _ _ x _ y _ z -> JSCatchIf a' a' (f x) a' (f y) a' (f z)
    where
      f :: (RepJSAnnot a) => a -> a
      f = rjsa a'

instance RepJSAnnot JSTryFinally where
  rjsa a' = \case
    JSFinally _ x -> JSFinally a' $ rjsa a' x
    JSNoFinally -> JSNoFinally

instance RepJSAnnot JSConciseBody where
  rjsa a' = \case
    JSConciseExprBody e -> JSConciseExprBody $ rjsa a' e
    JSConciseFunBody b -> JSConciseFunBody $ rjsa a' b

instance (RepJSAnnot a) => RepJSAnnot [a] where
  rjsa a' l = map (rjsa a') l

instance RepJSAnnot a => RepJSAnnot (Maybe a) where
  rjsa a' l = fmap (rjsa a') l

jsaList :: HasJSAnnot a => JSAnnot -> [a] -> JSAnnot
jsaList def = \case
  [] -> def
  h : _ -> jsa h

jsString :: JSAnnot -> String -> JSExpression
jsString a s = JSStringLiteral a $ "'" <> s <> "'"
