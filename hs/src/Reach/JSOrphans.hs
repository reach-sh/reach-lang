{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Reach.JSOrphans () where

import qualified Data.Text.Lazy as T
import Language.JavaScript.Parser
import Language.JavaScript.Parser.Lexer
import Reach.Texty
import Language.JavaScript.Parser.AST

instance Ord TokenPosn where
  compare (TokenPn x_a x_l x_c) (TokenPn y_a y_l y_c) =
    compare [x_a, x_l, x_c] [y_a, y_l, y_c]

-- | This is fine because the Show instance is derived
deriving instance Read Token

viaJS :: (JSAnnot -> JSAST) -> Doc
viaJS mk = DText $ T.pack $ unwords . words $ T.unpack $ renderToText $ mk JSNoAnnot

instance Pretty JSArrayElement where
  pretty = \case
    JSArrayElement je -> pretty je
    JSArrayComma _ -> ","

instance Pretty JSAssignOp where
  pretty = \case
    JSAssign _ -> "="
    JSTimesAssign _ -> "*="
    JSDivideAssign _ -> "/="
    JSModAssign _ -> "%="
    JSPlusAssign _ -> "+="
    JSMinusAssign _ -> "-="
    JSLshAssign _ -> "<<="
    JSRshAssign _ -> ">>="
    JSUrshAssign _ -> ">>>="
    JSBwAndAssign _ -> "&="
    JSBwXorAssign _ -> "^="
    JSBwOrAssign _ -> "|="

instance Pretty JSBinOp where
  pretty = \case
    JSBinOpAnd _ -> "&&"
    JSBinOpBitAnd _ -> "&"
    JSBinOpBitOr _ -> "|"
    JSBinOpBitXor _ -> "^"
    JSBinOpDivide _ -> "/"
    JSBinOpEq _ -> "=="
    JSBinOpGe _ -> ">="
    JSBinOpGt _ -> ">"
    JSBinOpIn _ -> "in "
    JSBinOpInstanceOf _ -> "instanceof"
    JSBinOpLe _ -> "<="
    JSBinOpLsh _ -> "<<"
    JSBinOpLt _ -> "<"
    JSBinOpMinus _ -> "-"
    JSBinOpMod _ -> "%"
    JSBinOpNeq _ -> "!="
    JSBinOpOf _ -> "of "
    JSBinOpOr _ -> "||"
    JSBinOpPlus _ -> "+"
    JSBinOpRsh _ -> ">>"
    JSBinOpStrictEq _ -> "==="
    JSBinOpStrictNeq _ -> "!=="
    JSBinOpTimes _ -> "*"
    JSBinOpUrsh _ -> ">>>"

instance Pretty JSUnaryOp where
  pretty = \case
    JSUnaryOpDecr _ -> "--"
    JSUnaryOpDelete _ -> "delete"
    JSUnaryOpIncr _ -> "++"
    JSUnaryOpMinus _ -> "-"
    JSUnaryOpNot _ -> "!"
    JSUnaryOpPlus _ -> "+"
    JSUnaryOpTilde _ -> "~"
    JSUnaryOpTypeof _ -> "typeof"
    JSUnaryOpVoid _ -> "void"

instance Pretty a => Pretty (JSCommaList a) where
  pretty = \case
    JSLCons jcl _ a -> pretty jcl <> "," <+> pretty a
    JSLOne a -> pretty a
    JSLNil -> ""

instance Pretty JSIdent where
  pretty = \case
    JSIdentName _ s -> viaShow s
    JSIdentNone -> ""

instance Pretty JSArrowParameterList where
  pretty = \case
    JSUnparenthesizedArrowParameter ji -> pretty ji
    JSParenthesizedArrowParameterList _ jcl _ -> parens (pretty jcl)

instance Pretty JSBlock where
  pretty = \case JSBlock _ jss _ -> braces (hardline <> pretty jss <> hardline)

instance Pretty JSConciseBody where
  pretty = \case
    JSConciseExprBody je -> pretty je
    JSConciseFunBody jb -> pretty jb

instance Pretty JSPropertyName where
  pretty = \case
    JSPropertyIdent _ s -> pretty s
    JSPropertyString _ s -> pretty s
    JSPropertyNumber _ s -> pretty s
    JSPropertyComputed _ je _' -> pretty je

instance Pretty JSMethodDefinition where
  pretty = \case
    JSMethodDefinition jpn _ jcl _ jb -> pretty jpn <+> parens (pretty jcl) <+> pretty jb
    JSGeneratorMethodDefinition {} -> "<generator method>"
    JSPropertyAccessor {} -> "<property accessor>"

instance Pretty JSObjectProperty where
  pretty = \case
    JSPropertyNameandValue jpn _ (je:[]) -> pretty jpn <+> ":" <+> pretty je
    JSPropertyNameandValue jpn _ jes -> pretty jpn <+> ":" <+> pretty jes
    JSPropertyIdentRef _ s -> pretty s
    JSObjectMethod jmd -> pretty jmd
    JSObjectSpread _ je -> "..." <> pretty je

instance Pretty JSObjectPropertyList where
  pretty = \case
    JSCTLComma jcl _ -> pretty jcl
    JSCTLNone jcl -> pretty jcl

instance Pretty JSVarInitializer where
  pretty = \case
    JSVarInit _ je -> "=" <+> pretty je
    JSVarInitNone -> ""

instance Pretty JSExpression where
  pretty = \case
    JSIdentifier _ ('.':s') -> pretty $ "internal_" <> s'
    JSIdentifier _ s -> pretty s
    JSDecimal _ s -> pretty s
    JSLiteral _ ".null" -> "secret(null)"
    JSLiteral _ s -> pretty s
    JSHexInteger _ s -> pretty s
    JSOctal _ s -> pretty s
    JSStringLiteral _ s -> pretty s
    JSRegEx _ s -> pretty s
    JSArrayLiteral _ jaes _' -> brackets (hcat $ map pretty jaes)
    JSAssignExpression je jao je' -> pretty je <+> pretty jao <+> pretty je'
    JSAwaitExpression _ je -> "await" <+> pretty je
    JSCallExpression (je@JSArrowExpression {}) _ jcl _' -> parens (pretty je) <> parens (pretty jcl)
    JSCallExpression je _ jcl _' -> pretty je <> parens (pretty jcl)
    JSExpressionBinary je jbo je' -> pretty je <+> pretty jbo <+> pretty je'
    JSExpressionParen _ je _' -> parens (pretty je)
    JSExpressionTernary je _ je' _' je2 -> pretty je <+> "?" <+> pretty je' <+> ":" <+> pretty je2
    JSArrowExpression japl _ jcb -> pretty japl <+> "=>" <+> pretty jcb
    JSFunctionExpression _ ji _' jcl _ jb -> "function" <+> pretty ji <+> parens (pretty jcl) <+> pretty jb
    JSMemberDot je _ je' -> pretty je <> "." <> pretty je'
    JSMemberExpression je _ jcl _' -> pretty je <> parens (pretty jcl)
    JSSpreadExpression _ je -> "..." <> pretty je
    JSMemberSquare je _ je' _' -> pretty je <> brackets (pretty je')
    JSNewExpression _ je -> "new" <+> pretty je
    JSObjectLiteral _ jctl _' -> braces (pretty jctl)
    JSUnaryExpression juo je -> pretty juo <+> pretty je
    JSVarInitExpression je jvi -> pretty je <+> pretty jvi
    ow -> viaJS $ JSAstExpression ow

instance Pretty [JSStatement] where
  pretty ss = do
    let ss' = map pretty ss
    concatWith (\ l r -> l <> hardline <> r) ss'

instance Pretty JSSwitchParts where
  pretty = \case
    JSCase _ je _' jss -> "case" <+> pretty je <> ":" <+> braces (pretty jss)
    JSDefault _ _' jss -> "default:" <+> pretty jss

instance Pretty [JSSwitchParts] where
  pretty ss = do
    let ss' = map pretty ss
    concatWith (\ l r -> l <> hardline <> r) ss'

instance Pretty JSTryCatch where
  pretty = \case
    JSCatch _ _' je _ jb -> "catch" <+> parens (pretty je) <+> pretty jb
    JSCatchIf {} -> "<catch if>"

instance Pretty JSStatement where
  pretty = (<> ";") . \case
    JSStatementBlock _ jss _' _ -> pretty jss
    JSBreak _ ji _ -> "break" <+> pretty ji
    JSLet _ jcl _ -> "let" <+> pretty jcl
    JSConstant _ jcl _ -> "const" <+> pretty jcl
    JSContinue _ ji _ -> "continue" <+> pretty ji
    JSFunction _ ji _' jcl _ jb _ -> "function" <+> pretty ji <+> parens (pretty jcl) <+> pretty jb
    JSIf _ _' je _ t -> "if" <+> pretty je <+> braces (pretty t)
    JSIfElse _ _' je _ a _ b -> "if" <+> pretty je <+> braces (pretty a) <+> "else" <+> braces (pretty b)
    JSExpressionStatement je _ -> pretty je
    JSAssignStatement je jao je' _ -> pretty je <+> pretty jao <+> pretty je'
    JSMethodCall je _ jcl _' _ -> pretty je <+> parens (pretty jcl)
    JSReturn _ (Just je) _ -> "return" <+> pretty je
    JSReturn _ _ _ -> "return"
    JSSwitch _ _' je _ _ jsps _ _ -> "switch" <+> parens (pretty je) <+> braces (pretty jsps)
    JSThrow _ je _ -> "throw" <+> pretty je
    JSTry _ jb jtcs _ -> "try" <+> pretty jb <+> pretty jtcs
    JSVariable _ jcl _ -> pretty jcl
    JSWhile _ _' je _ s -> "while" <+> parens (pretty je) <+> pretty s
    ow -> viaJS $ JSAstStatement ow
