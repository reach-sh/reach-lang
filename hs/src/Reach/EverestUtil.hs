{-# LANGUAGE CPP #-}

module Reach.EverestUtil
  ( hasReachEverest
  , onlyWithReachEverest
  , everestSelect
  ) where

hasReachEverest :: Bool
hasReachEverest =
#ifdef REACH_EVEREST
  True
#else
  False
#endif

onlyWithReachEverest :: String
onlyWithReachEverest = "only available with Reach Everest"

everestSelect :: a -> a -> a
everestSelect no yes = if hasReachEverest then yes else no
