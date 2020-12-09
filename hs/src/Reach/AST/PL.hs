{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Reach.AST.PL where

import qualified Data.Map.Strict as M
import GHC.Generics
import Reach.AST.Base
import Reach.AST.DLBase

data PLLetCat
  = PL_Once
  | PL_Many
  deriving (Eq, Show)

instance Semigroup PLLetCat where
  _ <> _ = PL_Many

data PLCommon a
  = PL_Return SrcLoc
  | PL_Let SrcLoc PLLetCat DLVar DLExpr a
  | PL_ArrayMap SrcLoc DLVar DLArg DLVar PLBlock a
  | PL_ArrayReduce SrcLoc DLVar DLArg DLArg DLVar DLVar PLBlock a
  | PL_Eff SrcLoc DLExpr a
  | PL_Var SrcLoc DLVar a
  | PL_Set SrcLoc DLVar DLArg a
  | PL_LocalIf SrcLoc DLArg PLTail PLTail a
  | PL_LocalSwitch SrcLoc DLVar (SwitchCases PLTail) a
  deriving (Eq, Show)

data PLTail
  = PLTail (PLCommon PLTail)
  deriving (Eq, Show)

data PLBlock
  = PLBlock SrcLoc PLTail DLArg
  deriving (Eq, Show)

data ETail
  = ET_Com (PLCommon ETail)
  | ET_Stop SrcLoc
  | ET_If SrcLoc DLArg ETail ETail
  | ET_Switch SrcLoc DLVar (SwitchCases ETail)
  | ET_FromConsensus SrcLoc Int (Maybe [DLVar]) ETail
  | ET_ToConsensus
      { et_tc_at :: SrcLoc
      , et_tc_from :: DLVar
      , et_tc_prev :: Int
      , et_tc_which :: Int
      , et_tc_from_me
        :: ( ---     args     amt   when   saved_vs just-me
             Maybe ([DLArg], DLArg, DLArg, [DLVar], Bool)
             )
      , et_tc_from_msg :: [DLVar]
      , et_tc_from_amtv :: DLVar
      , et_tc_from_mtime :: (Maybe ([DLArg], ETail))
      , et_tc_cons :: ETail
      }
  | ET_While
      { et_w_at :: SrcLoc
      , et_w_asn :: DLAssignment
      , et_w_cond :: PLBlock
      , et_w_body :: ETail
      , et_w_k :: ETail
      }
  | ET_Continue SrcLoc DLAssignment
  | ET_ConsensusOnly SrcLoc PLTail ETail
  deriving (Eq, Show)

data EPProg
  = EPProg SrcLoc InteractEnv ETail
  deriving (Eq, Show)

data CTail
  = CT_Com (PLCommon CTail)
  | CT_If SrcLoc DLArg CTail CTail
  | CT_Switch SrcLoc DLVar (SwitchCases CTail)
  | CT_From SrcLoc (Maybe [DLVar])
  | CT_Jump SrcLoc Int [DLVar] DLAssignment
  deriving (Eq, Show)

data CInterval
  = CBetween [DLArg] [DLArg]
  deriving (Show, Eq)

default_interval :: CInterval
default_interval = CBetween [] []

interval_from :: CInterval -> [DLArg]
interval_from (CBetween froml _) = froml

interval_add_from :: CInterval -> DLArg -> CInterval
interval_add_from (CBetween froml tol) x =
  CBetween (x : froml) (x : tol)

interval_add_to :: CInterval -> DLArg -> CInterval
interval_add_to (CBetween froml tol) x =
  CBetween froml (x : tol)

interval_no_to :: CInterval -> CInterval
interval_no_to (CBetween froml _) =
  CBetween froml []

data CHandler
  = C_Handler
      { ch_at :: SrcLoc
      , ch_int :: CInterval
      , ch_from :: DLVar
      , ch_last :: Int
      , ch_svs :: [DLVar]
      , ch_msg :: [DLVar]
      , ch_amtv :: DLVar
      , ch_body :: CTail
      }
  | C_Loop
      { cl_at :: SrcLoc
      , cl_svs :: [DLVar]
      , cl_vars :: [(PLLetCat, DLVar)]
      , cl_body :: CTail
      }
  deriving (Eq, Show)

newtype CHandlers = CHandlers (M.Map Int CHandler)
  deriving (Eq, Show)
  deriving newtype (Monoid, Semigroup)

data CPProg
  = CPProg SrcLoc CHandlers
  deriving (Eq, Show)

newtype EPPs = EPPs (M.Map SLPart EPProg)
  deriving (Eq, Show)
  deriving newtype (Monoid, Semigroup)

data PLOpts = PLOpts
  { plo_deployMode :: DeployMode
  , plo_verifyOverflow :: Bool
  }
  deriving (Generic, Eq, Show)

data PLProg
  = PLProg SrcLoc PLOpts EPPs CPProg
  deriving (Eq, Show)
