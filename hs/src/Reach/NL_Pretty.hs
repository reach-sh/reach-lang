{-# OPTIONS_GHC -fno-warn-orphans #-}

module Reach.NL_Pretty where

import qualified Data.Map.Strict as M
import Data.Text.Prettyprint.Doc
import Reach.NL_AST

instance Pretty SLPart where
  pretty = viaShow

instance Pretty SLType where
  pretty = viaShow --- XXX

instance Pretty DLVar where
  pretty (DLVar _ s t i) = viaShow s <> ":" <> viaShow t <> ":" <> viaShow i

render_obj :: Pretty k => Pretty v => M.Map k v -> Doc a
render_obj env =
  braces $ nest 2 $ hardline <> (concatWith (surround (comma <> hardline)) $ map render_p $ M.toList env)
  where
    render_p (k, oa) = pretty k <+> ":" <+> pretty oa

instance Pretty DLArg where
  pretty a =
    case a of
      DLA_Var v -> pretty v
      DLA_Con c -> viaShow c
      DLA_Array as -> brackets $ render_das as
      DLA_Obj env -> render_obj env

render_das :: [DLArg] -> Doc a
render_das as = hcat $ punctuate comma $ map pretty as

instance Pretty DLExpr where
  pretty e =
    case e of
      DLE_PrimOp _ o as -> viaShow o <> parens (render_das as)
      DLE_ArrayRef _ a o -> pretty a <> brackets (pretty o)
      DLE_Interact _ m as -> "interact." <> viaShow m <> parens (render_das as)
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

instance Pretty DLStmt where
  pretty d =
    case d of
      DLS_Let _ v e ->
        "const" <+> pretty v <+> "=" <+> pretty e <> semi
      DLS_Claim _ _ ct a ->
        "claim" <> parens (viaShow ct) <> parens (pretty a) <> semi
      DLS_If _ ca ts fs ->
        prettyIf ca (render_dls ts) (render_dls fs)
      DLS_Transfer _ who da ->
        "transfer." <> parens (pretty da) <> ".to" <> parens (render_sp who) <> semi
      DLS_Return _ ret sv ->
        "throw" <> parens (viaShow sv) <> ".to" <> parens (viaShow ret) <> semi
      DLS_Prompt _ ret bodys ->
        "prompt" <> parens (viaShow ret) <+> ns bodys <> semi
      DLS_Only _ who onlys ->
        "only" <> parens (render_sp who) <+> ns onlys <> semi
      DLS_ToConsensus _ who as vs mamt mtime cons ->
        "publish" <> parens (render_sp who) <> (render_das as) <> (cm $ map pretty vs) <> amtp <> timep <> ns cons
        where
          amtp =
            case mamt of
              Nothing -> ""
              Just ap -> ".pay" <> parens (render_nest $ pretty ap)
          timep =
            case mtime of
              Nothing -> ""
              Just (td, tp) -> ".timeout" <> parens (cm [pretty td, (render_nest $ pretty tp)])
      DLS_FromConsensus _ more ->
        "commit()" <> semi <> hardline <> render_dls more
    where
      ns x = render_nest $ render_dls x
      cm l = parens (hsep $ punctuate comma $ l)

render_dls :: DLStmts -> Doc a
render_dls ss = concatWith (surround hardline) $ fmap pretty ss

instance Pretty DLBlock where
  pretty (DLBlock _at ss da) =
    render_dls ss <> hardline <> "return" <+> pretty da <> semi

instance Pretty InteractEnv where
  pretty (InteractEnv m) = "interact" <+> render_obj m

instance Pretty SLParts where
  pretty (SLParts m) = "parts" <+> render_obj m <> semi

instance Pretty DLProg where
  pretty (DLProg _at sps db) =
    "#lang dl" <> hardline
    <> pretty sps <> hardline <> hardline
    <> pretty db

instance Pretty a => Pretty (LLCommon a) where
  pretty l =
    case l of
      LL_Return _at -> mempty
      LL_Let at dv de k -> help (DLS_Let at dv de) k
      LL_Var _at dv k -> "let" <+> pretty dv <> semi <> hardline <> pretty k
      LL_Set _at dv da k -> pretty dv <+> "=" <+> pretty da <> semi <> hardline <> pretty k
      LL_Claim at f ct a k -> help (DLS_Claim at f ct a) k
      LL_LocalIf _at ca t f k -> prettyIfp ca t f <> hardline <> pretty k
    where
      help d k = pretty d <> hardline <> pretty k

instance Pretty LLLocal where
  pretty (LLL_Com x) = pretty x

instance Pretty LLConsensus where
  pretty s =
    case s of
      LLC_Com x -> pretty x
      LLC_If _a ca t f -> prettyIfp ca t f
      LLC_Transfer at who da k -> help (DLS_Transfer at who da) k
      LLC_FromConsensus _at _ret_at k ->
        "commit()" <> semi <> hardline <> pretty k
    where
      help d k = pretty d <> hardline <> pretty k

instance Pretty LLStep where
  pretty s =
    case s of
      LLS_Com x -> pretty x
      LLS_Stop _at da -> "exit" <> parens (pretty da) <> semi
      LLS_Only _at who onlys k ->
        "only" <> parens (render_sp who) <+> ns (pretty onlys) <> semi <> hardline <> pretty k
      LLS_ToConsensus _at who as vs mamt mtime cons ->
        "publish" <> parens (render_sp who) <> (render_das as) <> (cm $ map pretty vs) <> amtp <> timep <> ns (pretty cons)
        where
          amtp =
            case mamt of
              Nothing -> mempty
              Just (ts, ta) ->
                ".pay" <> parens (render_nest $ (pretty ts) <> hardline <> "return" <+> pretty ta <> semi)
          timep =
            case mtime of
              Nothing -> mempty
              Just (td, tl) -> ".timeout" <> parens (cm [pretty td, (render_nest $ pretty tl)])
    where
      cm l = parens (hsep $ punctuate comma $ l)
      ns = render_nest

instance Pretty LLProg where
  pretty (LLProg _at sps db) =
    "#lang ll" <> hardline
    <> pretty sps <> hardline <> hardline
    <> pretty db
