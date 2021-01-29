{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Reach.AST.PL where

import qualified Data.Map.Strict as M
import GHC.Generics
import Reach.AST.Base
import Reach.AST.DLBase
import Reach.Counter

data PLLetCat
  = PL_Once
  | PL_Many
  deriving (Eq, Show)

instance Semigroup PLLetCat where
  _ <> _ = PL_Many

data PLVar
  = PV_Eff
  | PV_Let PLLetCat DLVar
  deriving (Eq, Show)

type PLCommon = DLinStmt PLVar

type PLTail = DLinTail PLVar

type PLBlock = DLinBlock PLVar

-- NOTE switch to Maybe DLAssignment and make sure we have a consistent order,
-- like with M.toAscList
type FromInfo = Maybe [(DLVar, DLArg)]

data ETail
  = ET_Com PLCommon ETail
  | ET_Stop SrcLoc
  | ET_If SrcLoc DLArg ETail ETail
  | ET_Switch SrcLoc DLVar (SwitchCases ETail)
  | ET_FromConsensus SrcLoc Int FromInfo ETail
  | ET_ToConsensus
      { et_tc_at :: SrcLoc
      , et_tc_from :: DLVar
      , et_tc_prev :: Int
      , et_tc_last_timev :: Maybe DLVar
      , et_tc_which :: Int
      , et_tc_from_me
        :: ( ---     args     amt   when   saved_vs just-me
             Maybe ([DLArg], DLArg, DLArg, [DLVar], Bool)
             )
      , et_tc_from_msg :: [DLVar]
      , et_tc_from_amtv :: DLVar
      , et_tc_from_timev :: DLVar
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
  = CT_Com PLCommon CTail
  | CT_If SrcLoc DLArg CTail CTail
  | CT_Switch SrcLoc DLVar (SwitchCases CTail)
  | CT_From SrcLoc FromInfo
  | CT_Jump SrcLoc Int [DLVar] DLAssignment
  deriving (Eq, Show)

data CInterval a
  = CBetween [a] [a]
  deriving (Show, Eq)

data CHandler
  = C_Handler
      { ch_at :: SrcLoc
      , ch_int :: CInterval DLArg
      , ch_last_timev :: Maybe DLVar
      , ch_from :: DLVar
      , ch_last :: Int
      , ch_svs :: [DLVar]
      , ch_msg :: [DLVar]
      , ch_amtv :: DLVar
      , ch_timev :: DLVar
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
  , plo_counter :: Counter
  }
  deriving (Generic, Eq)

data PLProg
  = PLProg SrcLoc PLOpts DLInit EPPs CPProg
  deriving (Eq)
