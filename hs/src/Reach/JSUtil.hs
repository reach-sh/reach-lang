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
  rjsa :: a -> JSAnnot -> a

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
  rjsa b a' = case b of
    JSAssign _a -> JSAssign a'
    JSTimesAssign _a -> JSTimesAssign a'
    JSDivideAssign _a -> JSDivideAssign a'
    JSModAssign _a -> JSModAssign a'
    JSPlusAssign _a -> JSPlusAssign a'
    JSMinusAssign _a -> JSMinusAssign a'
    JSLshAssign _a -> JSLshAssign a'
    JSRshAssign _a -> JSRshAssign a'
    JSUrshAssign _a -> JSUrshAssign a'
    JSBwAndAssign _a -> JSBwAndAssign a'
    JSBwXorAssign _a -> JSBwXorAssign a'
    JSBwOrAssign _a -> JSBwOrAssign a'

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
  rjsa b a' = case b of
    JSUnaryOpDecr _a -> JSUnaryOpDecr a'
    JSUnaryOpDelete _a -> JSUnaryOpDelete a'
    JSUnaryOpIncr _a -> JSUnaryOpIncr a'
    JSUnaryOpMinus _a -> JSUnaryOpMinus a'
    JSUnaryOpNot _a -> JSUnaryOpNot a'
    JSUnaryOpPlus _a -> JSUnaryOpPlus a'
    JSUnaryOpTilde _a -> JSUnaryOpTilde a'
    JSUnaryOpTypeof _a -> JSUnaryOpTypeof a'
    JSUnaryOpVoid _a -> JSUnaryOpVoid a'

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
  rjsa b a' = case b of
    JSBinOpAnd _a -> JSBinOpAnd a'
    JSBinOpBitAnd _a -> JSBinOpBitAnd a'
    JSBinOpBitOr _a -> JSBinOpBitOr a'
    JSBinOpBitXor _a -> JSBinOpBitXor a'
    JSBinOpDivide _a -> JSBinOpDivide a'
    JSBinOpEq _a -> JSBinOpEq a'
    JSBinOpGe _a -> JSBinOpGe a'
    JSBinOpGt _a -> JSBinOpGt a'
    JSBinOpIn _a -> JSBinOpIn a'
    JSBinOpInstanceOf _a -> JSBinOpInstanceOf a'
    JSBinOpLe _a -> JSBinOpLe a'
    JSBinOpLsh _a -> JSBinOpLsh a'
    JSBinOpLt _a -> JSBinOpLt a'
    JSBinOpMinus _a -> JSBinOpMinus a'
    JSBinOpMod _a -> JSBinOpMod a'
    JSBinOpNeq _a -> JSBinOpNeq a'
    JSBinOpOf _a -> JSBinOpOf a'
    JSBinOpOr _a -> JSBinOpOr a'
    JSBinOpPlus _a -> JSBinOpPlus a'
    JSBinOpRsh _a -> JSBinOpRsh a'
    JSBinOpStrictEq _a -> JSBinOpStrictEq a'
    JSBinOpStrictNeq _a -> JSBinOpStrictNeq a'
    JSBinOpTimes _a -> JSBinOpTimes a'
    JSBinOpUrsh _a -> JSBinOpUrsh a'

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
  rjsa b a' = case b of
    JSIdentifier _a x -> JSIdentifier a' x
    JSDecimal _a x -> JSDecimal a' x
    JSLiteral _a x -> JSLiteral a' x
    JSHexInteger _a x -> JSHexInteger a' x
    JSOctal _a x -> JSOctal a' x
    JSStringLiteral _a x -> JSStringLiteral a' x
    JSRegEx _a x -> JSRegEx a' x
    JSArrayLiteral _a x y -> JSArrayLiteral a' x y
    JSAssignExpression a x y -> JSAssignExpression (rjsa a a') x y
    JSAwaitExpression _a x -> JSAwaitExpression a' x
    JSCallExpression a x y z -> JSCallExpression (rjsa a a') x y z
    JSCallExpressionDot a x y -> JSCallExpressionDot (rjsa a a') x y
    JSCallExpressionSquare a x y z -> JSCallExpressionSquare (rjsa a a') x y z
    JSClassExpression _a x y z r s -> JSClassExpression a' x y z r s
    JSCommaExpression a x y -> JSCommaExpression (rjsa a a') x y
    JSExpressionBinary a x y -> JSExpressionBinary (rjsa a a') x y
    JSExpressionParen _a x y -> JSExpressionParen a' x y
    JSExpressionPostfix a x -> JSExpressionPostfix (rjsa a a') x
    JSExpressionTernary a x y z r -> JSExpressionTernary (rjsa a a') x y z r
    JSArrowExpression x _a y -> JSArrowExpression x a' y
    JSFunctionExpression _a x y z r s -> JSFunctionExpression a' x y z r s
    JSGeneratorExpression _a x y z r s t -> JSGeneratorExpression a' x y z r s t
    JSMemberDot a x y -> JSMemberDot (rjsa a a') x y
    JSMemberExpression a x y z -> JSMemberExpression (rjsa a a') x y z
    JSMemberNew _a x y z r -> JSMemberNew a' x y z r
    JSMemberSquare a x y z -> JSMemberSquare (rjsa a a') x y z
    JSNewExpression _a x -> JSNewExpression a' x
    JSObjectLiteral _a x y -> JSObjectLiteral a' x y
    JSSpreadExpression _a x -> JSSpreadExpression a' x
    JSTemplateLiteral x _a y z -> JSTemplateLiteral x a' y z
    JSUnaryExpression a x -> JSUnaryExpression (rjsa a a') x
    JSVarInitExpression a x -> JSVarInitExpression (rjsa a a') x
    JSYieldExpression _a x -> JSYieldExpression a' x
    JSYieldFromExpression _a x y -> JSYieldFromExpression a' x y

instance HasJSAnnot JSObjectProperty where
  jsa = \case
    JSPropertyNameandValue _ ja _ -> ja
    JSPropertyIdentRef ja _ -> ja
    JSObjectMethod jmd -> jsa jmd
    JSObjectSpread ja _ -> ja
  rjsa b a' = case b of
    JSPropertyNameandValue x _ja y -> JSPropertyNameandValue x a' y
    JSPropertyIdentRef _ja x -> JSPropertyIdentRef a' x
    JSObjectMethod jmd -> JSObjectMethod $ rjsa jmd a'
    JSObjectSpread _ja x -> JSObjectSpread a' x

instance HasJSAnnot JSMethodDefinition where
  jsa = \case
    JSMethodDefinition _ ja _ _ _ -> ja
    JSGeneratorMethodDefinition ja _ _ _ _ _ -> ja
    JSPropertyAccessor _ _ ja _ _ _ -> ja
  rjsa b a' = case b of
    JSMethodDefinition x _ja y z r -> JSMethodDefinition x a' y z r
    JSGeneratorMethodDefinition _ja x y z r s -> JSGeneratorMethodDefinition a' x y z r s
    JSPropertyAccessor x y _ja z r s -> JSPropertyAccessor x y a' z r s

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
  rjsa b a' = case b of
    JSStatementBlock _ja x y z -> JSStatementBlock a' x y z
    JSBreak _ja x y -> JSBreak a' x y
    JSLet _ja x y -> JSLet a' x y
    JSClass _ja x y z r s t -> JSClass a' x y z r s t
    JSConstant _ja x y -> JSConstant a' x y
    JSContinue _ja x y -> JSContinue a' x y
    JSDoWhile _ja x y z r s t -> JSDoWhile a' x y z r s t
    JSFor _ja x y z r s t u v -> JSFor a' x y z r s t u v
    JSForIn _ja x y z r s t -> JSForIn a' x y z r s t
    JSForVar _ja x y z r s t u v w -> JSForVar a' x y z r s t u v w
    JSForVarIn _ja x y z r s t u -> JSForVarIn a' x y z r s t u
    JSForLet _ja x y z r s t u v w -> JSForLet a' x y z r s t u v w
    JSForLetIn _ja x y z r s t u -> JSForLetIn a' x y z r s t u
    JSForLetOf _ja x y z r s t u -> JSForLetOf a' x y z r s t u
    JSForConst _ja x y z r s t u v w -> JSForConst a' x y z r s t u v w
    JSForConstIn _ja x y z r s t u -> JSForConstIn a' x y z r s t u
    JSForConstOf _ja x y z r s t u -> JSForConstOf a' x y z r s t u
    JSForOf _ja x y z r s t -> JSForOf a' x y z r s t
    JSForVarOf _ja x y z r s t u -> JSForVarOf a' x y z r s t u
    JSAsyncFunction _ja x y z r s t u -> JSAsyncFunction a' x y z r s t u
    JSFunction _ja x y z r s t -> JSFunction a' x y z r s t
    JSGenerator _ja x y z r s t u -> JSGenerator a' x y z r s t u
    JSIf _ja x y z r -> JSIf a' x y z r
    JSIfElse _ja x y z r s t -> JSIfElse a' x y z r s t
    JSLabelled x _ja y -> JSLabelled x a' y
    JSEmptyStatement _ja -> JSEmptyStatement a'
    JSExpressionStatement e x -> JSExpressionStatement (rjsa e a') x
    JSAssignStatement e x y z -> JSAssignStatement (rjsa e a') x y z
    JSMethodCall r _ja x y z -> JSMethodCall r a' x y z
    JSReturn _ja x y -> JSReturn a' x y
    JSSwitch _ja x y z r s t u -> JSSwitch a' x y z r s t u
    JSThrow _ja x y -> JSThrow a' x y
    JSTry _ja x y z -> JSTry a' x y z
    JSVariable _ja x y -> JSVariable a' x y
    JSWhile _ja x y z r -> JSWhile a' x y z r
    JSWith _ja x y z r s -> JSWith a' x y z r s

jsaList :: HasJSAnnot a => JSAnnot -> [a] -> JSAnnot
jsaList def = \case
  [] -> def
  h : _ -> jsa h

jsString :: JSAnnot -> String -> JSExpression
jsString a s = JSStringLiteral a $ "'" <> s <> "'"
