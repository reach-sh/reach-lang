{-# OPTIONS_GHC -fno-warn-orphans #-}

module Reach.Pretty () where

import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import Generics.Deriving (conNameOf)
import Reach.AST.Base
import Reach.AST.DL
import Reach.AST.DLBase
import Reach.AST.LL
import Reach.AST.PL
import Reach.AST.SL
import Reach.Texty

pform :: Doc -> Doc -> Doc
pform f xs = group $ parens $ f <+> xs

pform_ :: Doc -> Doc
pform_ f = pform f mempty

pbrackets :: [Doc] -> Doc
pbrackets xs = group $ render_nest $ vsep $ punctuate comma xs

instance (Pretty a, Pretty b) => Pretty (Either a b) where
  pretty = \case
    Left x -> pretty x
    Right x -> pretty x

instance Pretty SrcLoc where
  pretty = viaShow

instance Pretty SLPart where
  pretty = viaShow

instance Pretty DLType where
  pretty = viaShow

instance Pretty IType where
  pretty = viaShow

instance Pretty SLType where
  pretty = viaShow

instance Pretty SecurityLevel where
  pretty = \case
    Public -> "public"
    Secret -> "secret"

instance Pretty SLSSVal where
  pretty (SLSSVal _ level val) = pretty (level, val) -- <> "@" <> pretty at
  -- TODO: incorporate srcloc in pretty?

instance Pretty SLVal where
  pretty = \case
    SLV_Null {} -> "null"
    SLV_Bool _ b -> pretty b
    SLV_Int _ i -> pretty i
    SLV_Bytes _ b -> pretty b
    SLV_Array at t as ->
      "array" <> parens (pretty t <> comma <+> pretty (SLV_Tuple at as))
    SLV_Tuple _ as ->
      brackets $ hsep $ punctuate comma $ map pretty as
    SLV_Object _ (Just lab) _ -> pretty lab
    SLV_Object _ _ m -> render_obj m
    SLV_Clo {} -> "<closure>"
    SLV_Data _ _ vn vv -> "<" <> pretty vn <> " " <> pretty vv <> ">"
    SLV_DLC c -> "<constant: " <> viaShow c <> ">"
    SLV_DLVar v -> pretty v
    SLV_Type t -> "<type: " <> pretty t <> ">"
    SLV_Connector cn -> "<connector: " <> pretty cn <> ">"
    SLV_Participant _ who _ _ ->
      "<participant: " <> pretty who <> ">"
    SLV_RaceParticipant _ whos ->
      "<race: " <> pretty whos <> ">"
    SLV_Prim p -> "<primitive: " <> pretty (conNameOf p) <> ">"
    SLV_Form f -> "<form: " <> pretty (conNameOf f) <> ">"
    SLV_Kwd k -> pretty k

instance Pretty SLKwd where
  pretty = viaShow

instance Pretty FluidVar where
  pretty = \case
    FV_balance -> "balance"
    FV_thisConsensusTime -> "thisConsensusTime"
    FV_lastConsensusTime -> "lastConsensusTime"

instance Pretty DLVar where
  --- pretty (DLVar _ s t i) = viaShow s <> ":" <> viaShow t <> ":" <> viaShow i
  pretty (DLVar _ _ _ i) = "v" <> viaShow i

render_obj :: Pretty k => Pretty v => M.Map k v -> Doc
render_obj env =
  braces $ nest 2 $ hardline <> (concatWith (surround (comma <> hardline)) $ map render_p $ M.toList env)
  where
    render_p (k, oa) = pretty k <+> "=" <+> pretty oa

instance Pretty DLConstant where
  pretty = \case
    DLC_UInt_max -> "UInt.max"

instance Pretty DLLiteral where
  pretty = \case
    DLL_Null -> "null"
    DLL_Bool b -> if b then "#t" else "#f"
    DLL_Int _ i -> viaShow i
    DLL_Bytes bs -> dquotes (viaShow bs)

instance Pretty DLArg where
  pretty = \case
    DLA_Var v -> pretty v
    DLA_Constant c -> pretty c
    DLA_Literal c -> pretty c
    DLA_Interact who m t ->
      "interact(" <> render_sp who <> ")." <> viaShow m <> parens (pretty t)

render_das :: Pretty a => [a] -> Doc
render_das as = hsep $ punctuate comma $ map pretty as

instance Pretty DLLargeArg where
  pretty = \case
    DLLA_Array t as -> "array" <> parens (pretty t <> comma <+> pretty (DLLA_Tuple as))
    DLLA_Tuple as -> brackets $ render_das as
    DLLA_Obj env -> render_obj env
    DLLA_Data _ vn vv -> "<" <> pretty vn <> " " <> pretty vv <> ">"

instance Pretty PrimOp where
  pretty = \case
    ADD -> "+"
    SUB -> "-"
    MUL -> "*"
    DIV -> "/"
    MOD -> "%"
    PLT -> "<"
    PLE -> "<="
    PEQ -> "=="
    PGE -> ">="
    PGT -> ">"
    IF_THEN_ELSE -> "ite"
    DIGEST_EQ -> "=="
    ADDRESS_EQ -> "=="
    SELF_ADDRESS -> "selfAddress"
    LSH -> "<<"
    RSH -> ">>"
    BAND -> "&"
    BIOR -> "|"
    BXOR -> "^"

instance Pretty DLExpr where
  pretty e =
    case e of
      DLE_Arg _ a -> pretty a
      DLE_LArg _ a -> pretty a
      DLE_Impossible _ msg -> "impossible" <> parens (pretty msg)
      DLE_PrimOp _ IF_THEN_ELSE [c, t, el] -> pretty c <> " ? " <> pretty t <> " : " <> pretty el
      DLE_PrimOp _ o [a] -> pretty o <> pretty a
      DLE_PrimOp _ o [a, b] -> hsep [pretty a, pretty o, pretty b]
      DLE_PrimOp _ o as -> pretty o <> parens (render_das as)
      DLE_ArrayRef _ a o -> pretty a <> brackets (pretty o)
      DLE_ArraySet _ a i v -> "array_set" <> (parens $ render_das [a, i, v])
      DLE_ArrayConcat _ x y -> "array_concat" <> (parens $ render_das [x, y])
      DLE_ArrayZip _ x y -> "array_zip" <> (parens $ render_das [x, y])
      DLE_TupleRef _ a i -> pretty a <> brackets (pretty i)
      DLE_ObjectRef _ a f -> pretty a <> "." <> pretty f
      DLE_Interact _ _ who m _ as -> "interact(" <> render_sp who <> ")." <> viaShow m <> parens (render_das as)
      DLE_Digest _ as -> "digest" <> parens (render_das as)
      DLE_Claim _ _ ct a m -> prettyClaim ct a m
      DLE_Transfer _ who da ->
        prettyTransfer who da
      DLE_Wait _ a -> "wait" <> parens (pretty a)
      DLE_PartSet _ who a -> render_sp who <> ".set" <> parens (pretty a)

instance Pretty ClaimType where
  pretty = \case
    CT_Assert -> "assert"
    CT_Assume -> "assume"
    CT_Require -> "require"
    CT_Possible -> "possible"
    CT_Unknowable p as -> "unknowable" <> parens (pretty p <> render_das as)

render_sp :: SLPart -> Doc
render_sp p = viaShow p

render_nest :: Doc -> Doc
render_nest inner = nest 2 $ braces (hardline <> inner <> " ")

prettyIf :: Pretty c => c -> Doc -> Doc -> Doc
prettyIf ca t f =
  "if" <+> pretty ca <+> "then"
    <+> render_nest t <> hardline <> "else"
    <+> render_nest f <> semi

prettyIfp :: Pretty c => Pretty e => c -> e -> e -> Doc
prettyIfp ca t f = prettyIf ca (pretty t) (pretty f)

prettySwitch :: (Pretty a, Pretty b, Pretty c, Pretty d) => a -> M.Map b (c, d) -> Doc
prettySwitch ov csm =
  "switch" <+> parens (pretty ov) <+> render_nest (concatWith (surround hardline) $ map render_p $ M.toList csm)
  where
    render_p (k, (mnv, ss)) = "case" <+> pretty k <+> "as" <+> pretty mnv <> ":" <+> render_nest (pretty ss)

prettyWhile :: (Pretty a, Pretty b, Pretty c) => a -> b -> c -> Doc -> Doc
prettyWhile asn inv cond bodyp =
  "loopvar" <+> pretty asn <> semi <> hardline
    <> "invariant"
    <> render_nest (pretty inv)
    <> hardline
    <> "while"
    <> render_nest (pretty cond)
    <> hardline
    <> (render_nest bodyp)

prettyContinue :: Pretty b => b -> Doc
prettyContinue cont_da =
  pretty cont_da <> hardline <> "continue" <> semi

prettyClaim :: Show a => Pretty b => Show c => a -> b -> c -> Doc
prettyClaim ct a m = "claim" <> parens (viaShow ct) <> parens (pretty a <> comma <+> viaShow m)

prettyTransfer :: DLArg -> DLArg -> Doc
prettyTransfer who da =
  "transfer." <> parens (pretty da) <> ".to" <> parens (pretty who)

prettyStop :: Doc
prettyStop = "exit" <> parens (emptyDoc) <> semi

prettyMap :: Pretty a => DLVar -> DLArg -> DLVar -> a -> Doc
prettyMap ans x a f =
  "map" <+> pretty ans <+> "=" <+> "for" <+> parens (pretty a <+> "in" <+> pretty x)
    <+> braces (nest 2 $ hardline <> pretty f)

prettyReduce :: Pretty a => DLVar -> DLArg -> DLArg -> DLVar -> DLVar -> a -> Doc
prettyReduce ans x z b a f =
  "reduce" <+> pretty ans <+> "=" <+> "for" <+> parens (pretty b <+> "=" <+> pretty z <> semi <+> pretty a <+> "in" <+> pretty x)
    <+> braces (nest 2 $ hardline <> pretty f)

prettyToConsensus :: Pretty c => (a -> Doc) -> (b -> Doc) -> M.Map SLPart (Bool, [DLArg], DLArg, DLArg) -> (Maybe DLVar, DLVar, [DLVar], DLVar, DLVar, a) -> (Maybe (c, b)) -> Doc
prettyToConsensus fa fb send (ltv, win, msg, amtv, tv, body) mtime =
  "publish" <> parens emptyDoc
    <> nest
      2
      (hardline <> mtime'
         <> concatWith (surround hardline) (map go $ M.toList send)
         <> hardline
         <> ".recv"
         <> parens (hsep $ punctuate comma $ [pretty ltv, pretty win, pretty msg, pretty amtv, pretty tv, render_nest (fa body)])
         <> semi)
  where
    go (p, (isClass, args, amta, whena)) =
      ".case" <> parens (hsep $ punctuate comma $ [pretty p, pretty isClass, pretty args, pretty amta, pretty whena])
    mtime' =
      case mtime of
        Nothing -> emptyDoc
        Just (delaya, tbody) ->
          ".timeout" <> parens (hsep $ punctuate comma $ [pretty delaya, render_nest (fb tbody)]) <> hardline

prettyCommit :: Doc
prettyCommit = "commit();"

instance Pretty DLAssignment where
  pretty (DLAssignment m) = render_obj m

instance Pretty DLStmt where
  pretty d =
    case d of
      DLS_Let _ mv e ->
        case mv of
          Just v ->
            "const" <+> pretty v <+> "=" <+> pretty e <> semi
          Nothing ->
            pretty e <> semi
      DLS_ArrayMap _ ans x a f -> prettyMap ans x a f
      DLS_ArrayReduce _ ans x z b a f -> prettyReduce ans x z b a f
      DLS_If _ ca _ ts fs ->
        prettyIf ca (render_dls ts) (render_dls fs)
      DLS_Switch _ ov _ csm ->
        prettySwitch ov csm
      DLS_Return _ ret sv ->
        "throw" <> parens (pretty sv) <> ".to" <> parens (viaShow ret) <> semi
      DLS_Prompt _ ret bodys ->
        "prompt" <> parens pret <+> ns bodys <> semi
        where
          pret =
            case ret of
              Left dv -> "const" <+> pretty dv
              Right (dv, retm) ->
                "let" <+> pretty dv <+> "with" <+> render_obj retm
      DLS_Stop _ ->
        prettyStop
      DLS_Only _ who onlys ->
        "only" <> parens (render_sp who) <+> ns onlys <> semi
      DLS_ToConsensus {..} ->
        prettyToConsensus render_dls render_dls dls_tc_send dls_tc_recv dls_tc_mtime
      DLS_FromConsensus _ more ->
        prettyCommit <> hardline <> render_dls more
      DLS_While _ asn inv cond body ->
        prettyWhile asn inv cond $ render_dls body
      DLS_Continue _ cont_da ->
        prettyContinue cont_da
      DLS_FluidSet _ fv da ->
        "fluid" <+> pretty fv <+> ":=" <+> pretty da
      DLS_FluidRef _ dv fv ->
        pretty dv <+> "<-" <+> "fluid" <+> pretty fv
    where
      ns x = render_nest $ render_dls x

render_dls :: DLStmts -> Doc
render_dls ss = concatWith (surround hardline) $ fmap pretty ss

instance Pretty (Seq.Seq DLStmt) where
  pretty = render_dls

instance Pretty DLBlock where
  pretty (DLBlock _at _ ss da) =
    render_dls ss <> hardline <> "return" <+> pretty da <> semi

instance Pretty InteractEnv where
  pretty (InteractEnv m) = "interact" <+> render_obj m

instance Pretty SLParts where
  pretty (SLParts m) = "parts" <+> render_obj m <> semi

instance Pretty DLInit where
  pretty (DLInit ctimem) =
    "// initialization" <> hardline
      <> ctimem'
    where
      ctimem' = case ctimem of
        Nothing -> "// no ctime" <> hardline
        Just x -> "const" <+> pretty x <+> "=" <+> "creationTime();" <> hardline

instance Pretty DLProg where
  pretty (DLProg _at _ sps dli ds) =
    "#lang dl" <> hardline
      <> pretty sps
      <> hardline
      <> hardline
      <> pretty dli
      <> render_dls ds

--- DLin language
instance Pretty a => Pretty (DLinStmt a) where
  pretty = \case
    DL_Nop _ -> mempty
    DL_Let _at x de -> "const" <+> pretty x <+> "=" <+> pretty de <> semi
    DL_ArrayMap _ ans x a f -> prettyMap ans x a f
    DL_ArrayReduce _ ans x z b a f -> prettyReduce ans x z b a f
    DL_Var _at dv -> "let" <+> pretty dv <> semi
    DL_Set _at dv da -> pretty dv <+> "=" <+> pretty da <> semi
    DL_LocalIf _at ca t f -> prettyIfp ca t f
    DL_LocalSwitch _at ov csm -> prettySwitch ov csm

instance Pretty a => Pretty (DLinTail a) where
  pretty = \case
    DT_Return _at -> mempty
    DT_Com x k -> pretty x <> hardline <> pretty k

instance Pretty a => Pretty (DLinBlock a) where
  pretty (DLinBlock _ _ ts ta) =
    (pretty ts) <> hardline <> "return" <+> pretty ta <> semi

--- Linear language
instance Pretty LLConsensus where
  pretty = \case
    LLC_Com x k -> pretty x <> hardline <> pretty k
    LLC_If _at ca t f -> prettyIfp ca t f
    LLC_Switch _at ov csm -> prettySwitch ov csm
    LLC_FromConsensus _at _ret_at k ->
      prettyCommit <> hardline <> pretty k
    LLC_While _at asn inv cond body k ->
      prettyWhile asn inv cond (pretty body) <> hardline <> pretty k
    LLC_Continue _at asn ->
      prettyContinue asn
    LLC_Only _at who onlys k ->
      "only" <> parens (render_sp who) <+> render_nest (pretty onlys) <> semi <> hardline <> pretty k

instance Pretty LLStep where
  pretty = \case
    LLS_Com x k -> pretty x <> hardline <> pretty k
    LLS_Stop _at -> prettyStop
    LLS_Only _at who onlys k ->
      "only" <> parens (render_sp who) <+> render_nest (pretty onlys) <> semi <> hardline <> pretty k
    LLS_ToConsensus {..} ->
      prettyToConsensus pretty pretty lls_tc_send lls_tc_recv lls_tc_mtime

instance Pretty LLProg where
  pretty (LLProg _at _ sps dli db) =
    "#lang ll" <> hardline
      <> pretty sps
      <> hardline
      <> hardline
      <> pretty dli
      <> pretty db

--- Projected Language

instance Pretty PLLetCat where
  pretty PL_Many = "*"
  pretty PL_Once = "!"

instance Pretty PLVar where
  pretty = \case
    PV_Eff -> "eff"
    PV_Let lc x -> pretty x <> pretty lc

instance Pretty ETail where
  pretty e =
    case e of
      ET_Com c k -> pretty c <> hardline <> pretty k
      ET_Stop _ -> emptyDoc
      ET_If _ ca t f -> prettyIfp ca t f
      ET_Switch _ ov csm -> prettySwitch ov csm
      ET_FromConsensus _ which msvs k ->
        "fromConsensus" <+> whichp <+> msvs' <+> semi
          <> hardline
          <> pretty k
        where
          msvs' = case msvs of
            Nothing -> emptyDoc
            Just svs -> cm $ map pretty svs
          whichp = viaShow which
      ET_ToConsensus _ fs prev last_timev which msend msg amtv timev mtime k ->
        "sendrecv" <+> fsp <+> prevp <+> pretty last_timev <+> whichp <+> parens msendp <> (cm $ map pretty msg) <+> pretty amtv <+> pretty timev <> timep <> ns (pretty k)
        where
          fsp = pretty fs
          prevp = viaShow prev
          whichp = viaShow which
          msendp =
            case msend of
              Nothing -> mempty
              Just (as, amt, whena, saved, soloSend) -> ".publish" <> cm [parens (render_das as), pretty amt, pretty whena, cm (map pretty saved), pretty soloSend]
          timep =
            case mtime of
              Nothing -> mempty
              Just (td, tl) -> nest 2 (hardline <> ".timeout" <> (cm [pretty td, (render_nest $ pretty tl)]))
      ET_While _ asn cond body k ->
        prettyWhile asn () cond (pretty body) <> hardline <> pretty k
      ET_Continue _ asn -> prettyContinue asn
      ET_ConsensusOnly _ lt k ->
        pretty lt <> hardline <> pretty k
    where
      ns = render_nest
      cm l = parens (hsep $ punctuate comma $ l)

instance Pretty EPProg where
  pretty (EPProg _ ie et) =
    pretty ie <> semi <> hardline <> pretty et

instance Pretty EPPs where
  pretty (EPPs m) = render_obj m

instance Pretty PLProg where
  pretty (PLProg _ _ dli ps cp) =
    "#lang pl" <> hardline
      <> pretty dli
      <> hardline
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
  pretty (C_Handler _ int last_timev fs last_i svs msg amtv timev body) =
    pbrackets
      [ pretty fs
      , pretty int
      , "last_timev = " <> pretty last_timev
      , "last = " <> pretty last_i
      , pretty svs
      , pretty (map varType svs)
      , pretty msg
      , pretty (map varType msg)
      , pretty amtv
      , "timev = " <> pretty timev
      , render_nest $ pretty body
      ]
  pretty (C_Loop _ svs vars body) =
    pbrackets
      [ "loop!"
      , pretty svs
      , pretty vars
      , render_nest $ pretty body
      ]

instance Pretty a => Pretty (CInterval a) where
  pretty (CBetween from to) = pform "between" $ go from <+> go to
    where
      go = brackets . render_das

instance Pretty CTail where
  pretty (CT_Com e k) = pretty e <> hardline <> pretty k
  pretty (CT_If _ ca tt ft) = prettyIfp ca tt ft
  pretty (CT_Switch _ ov csm) = prettySwitch ov csm
  pretty (CT_From _ mvars) =
    case mvars of
      Nothing -> pform_ "halt!"
      Just vars -> pform "wait!" $ pretty vars
  pretty (CT_Jump _ which vars assignment) = pform "jump!" args
    where
      args = pretty which <+> pretty vars <+> pretty assignment
