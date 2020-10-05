{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Reach.JSOrphans () where

import Control.DeepSeq
import GHC.Generics
import Language.JavaScript.Parser
import Language.JavaScript.Parser.AST
import Language.JavaScript.Parser.Lexer

instance Ord TokenPosn where
  compare (TokenPn x_a x_l x_c) (TokenPn y_a y_l y_c) =
    compare [x_a, x_l, x_c] [y_a, y_l, y_c]

deriving instance Generic TokenPosn

instance NFData TokenPosn

deriving instance Generic JSBlock

instance NFData JSBlock

deriving instance Generic JSExpression

instance NFData JSExpression

deriving instance Generic JSAnnot

instance NFData JSAnnot

deriving instance Generic JSStatement

instance NFData JSStatement

deriving instance Generic CommentAnnotation

instance NFData CommentAnnotation

deriving instance Generic (JSCommaList a)

instance (NFData a) => NFData (JSCommaTrailingList a)

deriving instance Generic (JSCommaTrailingList a)

instance (NFData a) => NFData (JSCommaList a)

deriving instance Generic JSArrayElement

instance NFData JSArrayElement

deriving instance Generic JSAssignOp

instance NFData JSAssignOp

deriving instance Generic JSIdent

instance NFData JSIdent

deriving instance Generic JSClassHeritage

instance NFData JSClassHeritage

deriving instance Generic JSClassElement

instance NFData JSClassElement

deriving instance Generic JSBinOp

instance NFData JSBinOp

deriving instance Generic JSUnaryOp

instance NFData JSUnaryOp

deriving instance Generic JSArrowParameterList

instance NFData JSArrowParameterList

deriving instance Generic JSObjectProperty

instance NFData JSObjectProperty

deriving instance Generic JSTemplatePart

instance NFData JSTemplatePart

deriving instance Generic JSVarInitializer

instance NFData JSVarInitializer

deriving instance Generic JSSemi

instance NFData JSSemi

deriving instance Generic JSSwitchParts

instance NFData JSSwitchParts

deriving instance Generic JSTryCatch

instance NFData JSTryCatch

deriving instance Generic JSTryFinally

instance NFData JSTryFinally

deriving instance Generic JSMethodDefinition

instance NFData JSMethodDefinition

deriving instance Generic JSPropertyName

instance NFData JSPropertyName

deriving instance Generic JSAccessor

instance NFData JSAccessor

deriving instance Generic JSExportDeclaration

instance NFData JSExportDeclaration

deriving instance Generic JSExportClause

instance NFData JSExportClause

deriving instance Generic JSFromClause

instance NFData JSFromClause

deriving instance Generic JSExportSpecifier

instance NFData JSExportSpecifier

deriving instance Generic JSImportDeclaration

instance NFData JSImportDeclaration

deriving instance Generic JSImportClause

instance NFData JSImportClause

deriving instance Generic JSImportNameSpace

instance NFData JSImportNameSpace

deriving instance Generic JSImportsNamed

instance NFData JSImportsNamed

deriving instance Generic JSImportSpecifier

instance NFData JSImportSpecifier

-- deriving instance Generic JSMethodDefinition
-- instance NFData JSMethodDefinition

-- | This is fine because the Show instance is derived
deriving instance Read Token

deriving instance Generic Token
