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

data CInterval a
  = CBetween (Maybe a) (Maybe a)
  deriving (Show, Eq)

instance Pretty a => Pretty (CInterval a) where
  pretty (CBetween f t) = pform "between" $ go f <+> go t
    where
      go = brackets . pretty

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
  pretty (C_Handler _ int fs last_i svs msg timev secsv body) =
    pbrackets
      [ pretty fs
      , pretty int
      , "last = " <> pretty last_i
      , pretty svs
      , pretty (map varLetType svs)
      , pretty msg
      , pretty (map varLetType msg)
      , "timev = " <> pretty timev
      , "secsv = " <> pretty secsv
      , render_nest $ pretty body
      ]
  pretty (C_Loop _ svs vars body) =
    pbrackets
      [ "loop!"
      , pretty svs
      , pretty vars
      , render_nest $ pretty body
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

type ViewsInfo = InterfaceLikeMap DLExportBlock

data ViewInfo = ViewInfo [DLVar] ViewsInfo
  deriving (Eq)

instance Pretty ViewInfo where
  pretty (ViewInfo vs vi) =
    pform "view" (pretty vs <+> pretty vi)

type ViewInfos = M.Map Int ViewInfo

type CPViews = DLViews

type ApiInfos = M.Map SLPart (M.Map Int ApiInfo)

data CPOpts = CPOpts
  { cpo_untrustworthyMaps :: Bool
  , cpo_counter :: Counter
  }
  deriving (Generic, Eq)

instance HasCounter CPOpts where
  getCounter (CPOpts {..}) = cpo_counter

data CPProg = CPProg
  { cpp_at :: SrcLoc
  , cpp_opts :: CPOpts
  , cpp_views :: (CPViews, ViewInfos)
  , cpp_apis :: ApiInfos
  , cpp_events :: DLEvents
  , cpp_handlers :: CHandlers
  }
  deriving (Eq)

instance Pretty CPProg where
  pretty (CPProg {..}) =
    "views:" <+> pretty cpp_views <> hardline
      <> "apiInfo:" <+> pretty cpp_apis
      <> hardline
      <> "events:" <+> pretty cpp_events
      <> hardline
      <> pretty cpp_handlers
