{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Reach.Eval.Types where

import qualified Data.Map.Strict as M
import Generics.Deriving
import Reach.AST.Base
import Reach.AST.DL (DLSBlock)
import Reach.AST.DLBase
import Reach.AST.SL

recursionDepthLimit :: Int
recursionDepthLimit = 2 ^ (16 :: Int)

type SLValTy = (SLVal, Maybe DLType)

data SLMode
  = --- The top-level of a module, before the App starts
    SLM_Module
  | --- The initial mode of an app where participants/views are defined
    SLM_AppInit
  | --- Once "deploy"ed, the app moves to a "step"
    SLM_Step
  | --- An "only" moves from "step" to "local step" and then to "step" again, where x = live
    SLM_LocalStep
  | SLM_LocalPure
  | --- A "toconsensus" moves from "step" to "consensus step" then to "step" again
    SLM_ConsensusStep
  | SLM_ConsensusPure
  deriving (Bounded, Enum, Eq, Generic)

instance Show SLMode where
  show = \case
    SLM_Module -> "module"
    SLM_AppInit -> "app init"
    SLM_Step -> "step"
    SLM_LocalStep -> "local step"
    SLM_LocalPure -> "local pure"
    SLM_ConsensusStep -> "consensus step"
    SLM_ConsensusPure -> "consensus pure"

isConsensusStep :: SLMode -> Bool
isConsensusStep = \case
  SLM_ConsensusStep -> True
  SLM_ConsensusPure -> True
  _ -> False

isLocalStep :: SLMode -> Bool
isLocalStep = \case
  SLM_LocalStep -> True
  SLM_LocalPure -> True
  _ -> False

--- A state represents the state of the protocol, so it is returned
--- out of a function call.
data SLState = SLState
  { --- A function call may modify the mode
    st_mode :: SLMode
  , st_live :: Bool
  , st_after_ctor :: Bool
  , st_after_first :: Bool
  , --- A function call may cause a participant to join
    st_pdvs :: SLPartDVars
  , st_toks :: [DLVar]
  }
  deriving (Eq, Show)

all_slm_modes :: [SLMode]
all_slm_modes = enumFrom minBound

pure_mode :: SLMode -> SLMode
pure_mode (SLM_LocalStep) = SLM_LocalPure
pure_mode (SLM_ConsensusStep) = SLM_ConsensusPure
pure_mode ow = ow

type SLPartDVars = M.Map SLPart DLVar

data DLValue
  = DLV_Arg SrcLoc DLArg
  | DLV_Fun SrcLoc [DLVar] DLSBlock
  | DLV_Array SrcLoc DLType [DLValue]
  | DLV_Tuple SrcLoc [DLValue]
  | DLV_Obj SrcLoc (M.Map SLVar DLValue)
  | DLV_Data SrcLoc (M.Map SLVar DLType) String DLValue
  | DLV_Struct SrcLoc [(SLVar, DLValue)]

instance SrcLocOf DLValue where
  srclocOf = \case
    DLV_Arg at _ -> at
    DLV_Fun at _ _ -> at
    DLV_Array at _ _ -> at
    DLV_Tuple at _ -> at
    DLV_Obj at _ -> at
    DLV_Data at _ _ _ -> at
    DLV_Struct at _ -> at

data TransferType
  = TT_Pay
  | TT_Bill
  deriving (Eq, Generic)

instance Show TransferType where
  show k = drop 3 $ conNameOf k

data SolReservedNames
  = SOL_address
  | SOL_after
  | SOL_alias
  | SOL_anonymous
  | SOL_apply
  | SOL_auto
  | SOL_case
  | SOL_constant
  | SOL_copyof
  | SOL_default
  | SOL_define
  | SOL_external
  | SOL_final
  | SOL_immutable
  | SOL_implements
  | SOL_in
  | SOL_indexed
  | SOL_inline
  | SOL_internal
  | SOL_let
  | SOL_macro
  | SOL_match
  | SOL_mutable
  | SOL_null
  | SOL_of
  | SOL_override
  | SOL_partial
  | SOL_payable
  | SOL_private
  | SOL_promise
  | SOL_public
  | SOL_pure
  | SOL_reference
  | SOL_relocatable
  | SOL_sealed
  | SOL_sizeof
  | SOL_static
  | SOL_super
  | SOL_supports
  | SOL_switch
  | SOL_this
  | SOL_typedef
  | SOL_typeof
  | SOL_unchecked
  | SOL_view
  | SOL_virtual
  deriving (Bounded, Enum, Eq, Generic)

instance Show SolReservedNames where
  show k = drop 4 $ conNameOf k

solReservedNames :: [SolReservedNames]
solReservedNames = enumFrom minBound
