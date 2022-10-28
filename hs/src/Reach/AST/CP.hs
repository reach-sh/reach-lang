{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Reach.AST.CP where

import qualified Data.Map.Strict as M
import GHC.Generics
import Reach.AST.Base
import Reach.AST.DLBase
import Reach.Counter
import Reach.Pretty
import Reach.Texty

data CTail
  = CT_Com DLStmt CTail
  | CT_If SrcLoc DLArg CTail CTail
  | CT_Switch SrcLoc DLVar (SwitchCases CTail)
  | CT_From SrcLoc Int FromInfo
  | CT_Jump SrcLoc Int [DLVar] DLAssignment
  deriving (Eq)

instance SrcLocOf CTail where
  srclocOf = \case
    CT_Com s _ -> srclocOf s
    CT_If a _ _ _ -> a
    CT_Switch a _ _ -> a
    CT_From a _ _ -> a
    CT_Jump a _ _ _ -> a

instance Pretty CTail where
  pretty = \case
    CT_Com e k -> pretty e <> hardline <> pretty k
    CT_If _ ca tt ft -> prettyIfp ca tt ft
    CT_Switch _ ov csm -> prettySwitch ov csm
    CT_From _ which fi -> pform "from" $ pretty which <> "," <+> pretty fi
    CT_Jump _ which vars assignment -> pform "jump!" args
      where
        args = pretty which <+> pretty vars <+> pretty assignment

data CHandler
  = C_Handler
      { ch_at :: SrcLoc
      , ch_int :: CInterval DLTimeArg
      , ch_from :: DLVar
      , ch_last :: Int
      , ch_svs :: [DLVarLet]
      , ch_msg :: [DLVarLet]
      , ch_timev :: DLVar
      , ch_secsv :: DLVar
      , ch_body :: CTail
      }
  | C_Loop
      { cl_at :: SrcLoc
      , cl_svs :: [DLVarLet]
      , cl_vars :: [DLVarLet]
      , cl_body :: CTail
      }
  deriving (Eq)

instance Pretty CHandler where
  pretty (C_Handler {..}) =
    pbrackets
      [ pretty ch_from
      , pretty ch_int
      , "last = " <> pretty ch_last
      , pretty ch_svs
      , pretty (map varLetType ch_svs)
      , pretty ch_msg
      , pretty (map varLetType ch_msg)
      , "timev = " <> pretty ch_timev
      , "secsv = " <> pretty ch_secsv
      , render_nest $ pretty ch_body
      ]
  pretty (C_Loop {..}) =
    pbrackets
      [ "loop!"
      , pretty cl_svs
      , pretty cl_vars
      , render_nest $ pretty cl_body
      ]

instance SrcLocOf CHandler where
  srclocOf = \case
    C_Handler {..} -> ch_at
    C_Loop {..} -> cl_at

newtype CHandlers = CHandlers (M.Map Int CHandler)
  deriving (Eq)
  deriving newtype (Monoid, Semigroup)

instance Pretty CHandlers where
  pretty (CHandlers m) =
    render_obj m

data CPOpts = CPOpts
  { cpo_untrustworthyMaps :: Bool
  , cpo_counter :: Counter
  }
  deriving (Generic, Eq)

instance HasCounter CPOpts where
  getCounter (CPOpts {..}) = cpo_counter

instance HasUntrustworthyMaps CPOpts where
  getUntrustworthyMaps (CPOpts {..}) = cpo_untrustworthyMaps

data CPProg = CPProg
  { cpp_at :: SrcLoc
  , cpp_opts :: CPOpts
  , cpp_init :: DLInit
  , cpp_views :: DLViewsX
  , cpp_apis :: ApiInfos
  , cpp_events :: DLEvents
  , cpp_handlers :: CHandlers
  }
  deriving (Eq)

instance Pretty CPProg where
  pretty (CPProg {..}) =
    "CP" <+> render_obj (M.fromList $
      [ (("init"::String), pretty cpp_init)
      , ("views", pretty cpp_views)
      , ("apis", pretty cpp_apis)
      , ("events", pretty cpp_events)
      , ("handlers", pretty cpp_handlers)
      ])

instance HasCounter CPProg where
  getCounter = getCounter . cpp_opts
