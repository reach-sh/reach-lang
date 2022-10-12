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

plp_epp_mod :: (a -> IO a') -> PLProg a b -> IO (PLProg a' b)
plp_epp_mod f (PLProg {..}) = flip (PLProg plp_at) plp_cpp <$> f plp_epp

plp_cpp_mod :: (b -> IO b') -> PLProg a b -> IO (PLProg a b')
plp_cpp_mod f (PLProg {..}) = PLProg plp_at plp_epp <$> f plp_cpp
