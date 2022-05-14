{-# LANGUAGE CPP #-}

module Reach.EverestUtil
  ( hasReachEverest
  , onlyWithReachEverest
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
