{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Reach.JSOrphans () where

import Language.JavaScript.Parser
import Language.JavaScript.Parser.Lexer
import Reach.Texty

instance Ord TokenPosn where
  compare (TokenPn x_a x_l x_c) (TokenPn y_a y_l y_c) =
    compare [x_a, x_l, x_c] [y_a, y_l, y_c]

-- | This is fine because the Show instance is derived
deriving instance Read Token

viaJS :: (JSAnnot -> JSAST) -> Doc
viaJS mk = DText $ renderToText $ mk JSNoAnnot

instance Pretty JSExpression where
  pretty = viaJS . JSAstExpression

instance Pretty JSStatement where
  pretty = viaJS . JSAstStatement
