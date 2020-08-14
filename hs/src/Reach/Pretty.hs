{-# OPTIONS_GHC -fno-warn-orphans #-}

module Reach.Pretty () where

import qualified Data.Map.Strict as M
import Data.Text.Prettyprint.Doc
import Reach.AST

pform :: Doc ann -> Doc ann -> Doc ann
pform f xs = group $ parens $ f <+> xs

pform_ :: Doc ann -> Doc ann
pform_ f = pform f mempty

pbrackets :: [Doc ann] -> Doc ann
pbrackets xs = group $ render_nest $ vsep $ punctuate comma xs

instance Pretty SLPart where
  pretty = viaShow

instance Pretty SLType where
  pretty = viaShow

instance Pretty SecurityLevel where
  pretty = \case
    Public -> "public"
    Secret -> "secret"

instance Pretty SLVal where
  pretty = \case
    SLV_Null {} -> "null"
    SLV_Bool _ b -> pretty b
    SLV_Int _ i -> pretty i
    SLV_Bytes _ b -> pretty b
    SLV_Tuple _ as ->
      brackets $ hsep $ punctuate comma $ map pretty as
    SLV_Object _ m -> render_obj m
    SLV_Clo {} -> "<closure>"
    SLV_DLVar v -> pretty v
    SLV_Type t -> "<type: " <> pretty t <> ">"
    SLV_Participant _ who _ _ _ ->
      "<participant: " <> pretty who <> ">"
    SLV_Prim {} -> "<primitive>"
    SLV_Form {} -> "<form>"

instance Pretty DLVar where
  --- pretty (DLVar _ s t i) = viaShow s <> ":" <> viaShow t <> ":" <> viaShow i
  pretty (DLVar _ _ _ i) = "v" <> viaShow i

render_obj :: Pretty k => Pretty v => M.Map k v -> Doc a
render_obj env =
  braces $ nest 2 $ hardline <> (concatWith (surround (comma <> hardline)) $ map render_p $ M.toList env)
  where
    render_p (k, oa) = pretty k <+> "=" <+> pretty oa

instance Pretty DLArg where
  pretty a =
    case a of
      DLA_Var v -> pretty v
      DLA_Con c -> viaShow c
      DLA_Tuple as -> brackets $ render_das as
      DLA_Obj env -> render_obj env
      DLA_Interact who m t ->
        "interact(" <> render_sp who <> ")." <> viaShow m <> parens (pretty t)

render_das :: [DLArg] -> Doc a
render_das as = hcat $ punctuate comma $ map pretty as

instance Pretty DLExpr where
  pretty e =
    case e of
      DLE_PrimOp _ o as -> viaShow o <> parens (render_das as)
      DLE_ArrayRef _ _ a _ o -> pretty a <> brackets (pretty o)
      DLE_TupleRef _ a i -> pretty a <> brackets (pretty i)
      DLE_ObjectRef _ a f -> pretty a <> "." <> pretty f
      DLE_Interact _ who m _ as -> "interact(" <> render_sp who <> ")." <> viaShow m <> parens (render_das as)
      DLE_Digest _ as -> "digest" <> parens (render_das as)

render_sp :: SLPart -> Doc a
render_sp p = viaShow p

render_nest :: Doc a -> Doc a
render_nest inner = nest 2 $ braces (hardline <> inner <> " ")

prettyIf :: Pretty c => c -> Doc a -> Doc a -> Doc a
prettyIf ca t f =
  "if" <+> pretty ca <+> "then"
    <+> render_nest t <> hardline <> "else"
    <+> render_nest f <> semi

prettyIfp :: Pretty c => Pretty e => c -> e -> e -> Doc a
prettyIfp ca t f = prettyIf ca (pretty t) (pretty f)

prettyWhile :: (Pretty a, Pretty b, Pretty c) => a -> b -> c -> Doc ann -> Doc ann
prettyWhile asn inv cond bodyp =
  "loopvar" <+> pretty asn <> semi <> hardline
    <> "invariant"
    <> render_nest (pretty inv)
    <> hardline
    <> "while"
    <> render_nest (pretty cond)
    <> hardline
    <> (render_nest bodyp)

prettyContinue :: Pretty b => b -> Doc a
prettyContinue cont_da =
  pretty cont_da <> hardline <> "continue" <> semi

prettyClaim :: Show a => Pretty b => a -> b -> Doc c
prettyClaim ct a = "claim" <> parens (viaShow ct) <> parens (pretty a) <> semi

prettyTransfer :: DLArg -> DLArg -> Doc a
prettyTransfer who da =
  "transfer." <> parens (pretty da) <> ".to" <> parens (pretty who) <> semi

prettyStop :: Doc a
prettyStop = "exit" <> parens (emptyDoc) <> semi

instance Pretty DLAssignment where
  pretty (DLAssignment m) = render_obj m

instance Pretty FromSpec where
  pretty (FS_Join dv) = "join(" <> pretty dv <> ")"
  pretty (FS_Again dv) = "again(" <> pretty dv <> ")"

instance Pretty DLStmt where
  pretty d =
    case d of
      DLS_Let _ v e ->
        "const" <+> pretty v <+> "=" <+> pretty e <> semi
      DLS_Claim _ _ ct a -> prettyClaim ct a
      DLS_If _ ca ts fs ->
        prettyIf ca (render_dls ts) (render_dls fs)
      DLS_Transfer _ _ who da ->
        prettyTransfer who da
      DLS_Return _ ret sv ->
        "throw" <> parens (pretty sv) <> ".to" <> parens (viaShow ret) <> semi
      DLS_Prompt _ ret bodys ->
        "prompt" <> parens pret <+> ns bodys <> semi
        where
          pret =
            case ret of
              Left _ -> emptyDoc
              Right dv -> "let" <+> pretty dv
      DLS_Stop _ _ ->
        prettyStop
      DLS_Only _ who onlys ->
        "only" <> parens (render_sp who) <+> ns onlys <> semi
      DLS_ToConsensus _ who fs as vs amt mtime cons ->
        "publish" <> (cm $ [render_sp who, pretty fs]) <> parens (render_das as) <> (cm $ map pretty vs) <> amtp <> timep <> ns cons
        where
          amtp = ".pay" <> parens (pretty amt)
          timep =
            case mtime of
              Nothing -> ""
              Just (td, tp) -> nest 2 (hardline <> ".timeout" <> (cm [pretty td, (render_nest $ render_dls tp)]))
      DLS_FromConsensus _ more ->
        "commit()" <> semi <> hardline <> render_dls more
      DLS_While _ asn inv cond body ->
        prettyWhile asn inv cond $ render_dls body
      DLS_Continue _ cont_da ->
        prettyContinue cont_da
    where
      ns x = render_nest $ render_dls x
      cm l = parens (hsep $ punctuate comma $ l)

render_dls :: DLStmts -> Doc a
render_dls ss = concatWith (surround hardline) $ fmap pretty ss

instance Pretty DLBlock where
  pretty (DLBlock _at _ ss da) =
    render_dls ss <> hardline <> "return" <+> pretty da <> semi

instance Pretty InteractEnv where
  pretty (InteractEnv m) = "interact" <+> render_obj m

instance Pretty SLParts where
  pretty (SLParts m) = "parts" <+> render_obj m <> semi

instance Pretty DLProg where
  pretty (DLProg _at sps ds) =
    "#lang dl" <> hardline
      <> pretty sps
      <> hardline
      <> hardline
      <> render_dls ds

--- Linear language
instance Pretty a => Pretty (LLCommon a) where
  pretty l =
    case l of
      LL_Return _at -> mempty
      LL_Let _at dv de k ->
        "const" <+> pretty dv <+> "=" <+> pretty de <> semi
          <> hardline
          <> pretty k
      LL_Var _at dv k ->
        "let" <+> pretty dv <> semi <> hardline <> pretty k
      LL_Set _at dv da k ->
        pretty dv <+> "=" <+> pretty da <> semi <> hardline <> pretty k
      LL_Claim _at _f ct a k ->
        prettyClaim ct a <> hardline <> pretty k
      LL_LocalIf _at ca t f k ->
        prettyIfp ca t f <> hardline <> pretty k

instance Pretty LLLocal where
  pretty (LLL_Com x) = pretty x

instance Pretty LLConsensus where
  pretty s =
    case s of
      LLC_Com x -> pretty x
      LLC_If _at ca t f -> prettyIfp ca t f
      LLC_Transfer _at _ who da k ->
        prettyTransfer who da <> hardline <> pretty k
      LLC_FromConsensus _at _ret_at k ->
        "commit()" <> semi <> hardline <> pretty k
      LLC_While _at asn inv cond body k ->
        prettyWhile asn inv cond (pretty body) <> hardline <> pretty k
      LLC_Continue _at asn ->
        prettyContinue asn

instance Pretty a => Pretty (LLBlock a) where
  pretty (LLBlock _ _ ts ta) =
    (pretty ts) <> hardline <> "return" <+> pretty ta <> semi

instance Pretty LLStep where
  pretty s =
    case s of
      LLS_Com x -> pretty x
      LLS_Stop _at _ -> prettyStop
      LLS_Only _at who onlys k ->
        "only" <> parens (render_sp who) <+> ns (pretty onlys) <> semi <> hardline <> pretty k
      LLS_ToConsensus _at who fs as vs amt mtime cons ->
        "publish" <> (cm $ [render_sp who, pretty fs]) <> parens (render_das as) <> (cm $ map pretty vs) <> amtp <> timep <> ns (pretty cons)
        where
          amtp = ".pay" <> parens (pretty amt)
          timep =
            case mtime of
              Nothing -> mempty
              Just (td, tl) -> nest 2 (hardline <> ".timeout" <> parens (cm [pretty td, (render_nest $ pretty tl)]))
    where
      cm l = parens (hsep $ punctuate comma $ l)
      ns = render_nest

instance Pretty LLProg where
  pretty (LLProg _at sps db) =
    "#lang ll" <> hardline
      <> pretty sps
      <> hardline
      <> hardline
      <> pretty db

--- Projected Language

instance Pretty PLLetCat where
  pretty PL_Many = "*"
  pretty PL_Once = "!"

instance Pretty a => Pretty (PLCommon a) where
  pretty l =
    case l of
      PL_Return _at -> mempty
      PL_Let _at lc dv de k ->
        "const" <+> pretty lc <> pretty dv <+> "=" <+> pretty de <> semi
          <> hardline
          <> pretty k
      PL_Eff _ de k ->
        "eff" <+> pretty de <> semi <> hardline <> pretty k
      PL_Var _at dv k ->
        "let" <+> pretty dv <> semi <> hardline <> pretty k
      PL_Set _at dv da k ->
        pretty dv <+> "=" <+> pretty da <> semi <> hardline <> pretty k
      PL_Claim _at _f ct a k ->
        prettyClaim ct a <> hardline <> pretty k
      PL_LocalIf _at ca t f k ->
        prettyIfp ca t f <> hardline <> pretty k

instance Pretty PLTail where
  pretty (PLTail x) = pretty x

instance Pretty ETail where
  pretty e =
    case e of
      ET_Com c -> pretty c
      ET_Seqn _ x y -> pretty x <> hardline <> pretty y
      ET_Stop _ -> emptyDoc
      ET_If _ ca t f -> prettyIfp ca t f
      ET_ToConsensus _ fs which msend msg mtime k ->
        "sendrecv" <+> fsp <+> whichp <+> parens msendp <> (cm $ map pretty msg) <> timep <> ns (pretty k)
        where
          fsp = pretty fs
          whichp = viaShow which
          msendp =
            case msend of
              Nothing -> mempty
              Just (as, amt, saved) -> ".publish" <> cm [parens (render_das as), pretty amt, cm (map pretty saved)]
          timep =
            case mtime of
              Nothing -> mempty
              Just (td, tl) -> nest 2 (hardline <> ".timeout" <> (cm [pretty td, (render_nest $ pretty tl)]))
      ET_While _ asn cond body k ->
        prettyWhile asn () cond (pretty body) <> hardline <> pretty k
      ET_Continue _ asn -> prettyContinue asn
    where
      ns = render_nest
      cm l = parens (hsep $ punctuate comma $ l)

instance Pretty PLBlock where
  pretty (PLBlock _ pltail dlarg) = pform "begin" body
    where
      body = pretty pltail <+> pretty dlarg

instance Pretty EPProg where
  pretty (EPProg _ ie et) =
    pretty ie <> semi <> hardline <> pretty et

instance Pretty EPPs where
  pretty (EPPs m) = render_obj m

instance Pretty PLProg where
  pretty (PLProg _ ps cp) =
    "#lang pl" <> hardline
      <> pretty ps
      <> hardline
      <> hardline
      <> pretty cp

instance Pretty CPProg where
  pretty (CPProg _ chs) = pretty chs

instance Pretty CHandlers where
  pretty (CHandlers m) =
    render_obj m

instance Pretty CHandler where
  pretty (C_Handler _ int fs last_i svs msg body) =
    pbrackets
      [ pretty fs
      , pretty int
      , "last = " <> pretty last_i
      , pretty svs
      , pretty msg
      , render_nest $ pretty body
      ]
  pretty (C_Loop _ svs vars body) =
    pbrackets
      [ "loop!"
      , pretty svs
      , pretty vars
      , render_nest $ pretty body
      ]

instance Pretty CInterval where
  pretty (CBetween from to) = pform "between" $ pretty from <+> pretty to

instance Pretty CTail where
  pretty (CT_Com e) = pretty e
  pretty (CT_Seqn _ x y) = pretty x <> hardline <> pretty y
  pretty (CT_If _ ca tt ft) = prettyIfp ca tt ft
  pretty (CT_Transfer _ to amt ctail) =
    prettyTransfer to amt <> hardline <> pretty ctail
  pretty (CT_Wait _ vars) = pform "wait!" $ pretty vars
  pretty (CT_Jump _ which vars assignment) = pform "jump!" args
    where
      args = pretty which <+> pretty vars <+> pretty assignment
  pretty (CT_Halt _) = pform_ "halt!"
