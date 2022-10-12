{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Reach.AST.PL where

import Reach.AST.Base
import Reach.AST.DLBase
import Reach.Texty

data PLProg a b = PLProg
  { plp_at :: SrcLoc
  , plp_epp :: a
  , plp_cpp :: b
  }
  deriving (Eq)

instance (Pretty a, Pretty b) => Pretty (PLProg a b) where
  pretty (PLProg _ e c) =
    "#lang pl" <> hardline
      <> pretty e
      <> hardline
      <> pretty c
      <> hardline

instance HasCounter a => HasCounter (PLProg a b) where
  getCounter = getCounter . plp_epp
