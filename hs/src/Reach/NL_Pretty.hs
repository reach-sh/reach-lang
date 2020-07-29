module Reach.NL_Pretty where

import qualified Data.Map.Strict as M
import Data.Text.Prettyprint.Doc
import Reach.NL_AST

--- FIXME use Pretty type class
render_dv :: DLVar -> Doc a
render_dv (DLVar _ s t i) = viaShow s <> ":" <> viaShow t <> ":" <> viaShow i

render_da :: DLArg -> Doc a
render_da a =
  case a of
    DLA_Var v -> render_dv v
    DLA_Con c -> viaShow c
    DLA_Array as -> brackets $ render_das as
    DLA_Obj env -> braces $ concatWith (surround (comma <> hardline)) $ map render_p $ M.toList env
      where
        render_p (k, oa) = pretty k <+> ":" <+> render_da oa

render_das :: [DLArg] -> Doc a
render_das as = hcat $ punctuate comma $ map render_da as

render_de :: DLExpr -> Doc a
render_de e =
  case e of
    DLE_PrimOp _ o as -> viaShow o <> parens (render_das as)
    DLE_ArrayRef _ a o -> render_da a <> brackets (render_da o)
    DLE_Interact _ m as -> "interact." <> viaShow m <> parens (render_das as)
    DLE_Digest _ as -> "digest" <> parens (render_das as)

render_sp :: SLPart -> Doc a
render_sp p = viaShow p

render_nest :: Doc a -> Doc a
render_nest inner = nest 2 $ braces (hardline <> inner <> " ")

render_dl :: DLStmt -> Doc a
render_dl d =
  case d of
    DLS_Let _ v e ->
      "let" <+> render_dv v <+> "=" <+> render_de e <> semi
    DLS_Claim _ _ ct a ->
      "claim" <> parens (viaShow ct) <> parens (render_da a) <> semi
    DLS_If _ ca ts fs ->
      "if" <+> render_da ca <+> "then"
        <+> ns ts <> hardline <> "else"
        <+> ns fs <> semi
    DLS_Transfer _ who da ->
      "transfer." <> parens (render_da da) <> ".to" <> parens (render_sp who) <> semi
    DLS_Return _ ret sv ->
      "throw" <> parens (viaShow sv) <> ".to" <> parens (viaShow ret) <> semi
    DLS_Prompt _ ret bodys ->
      "prompt" <> parens (viaShow ret) <+> ns bodys <> semi
    DLS_Only _ who onlys ->
      "only" <> parens (render_sp who) <+> ns onlys <> semi
    DLS_ToConsensus _ who as vs mamt mtime cons ->
      "publish" <> parens (render_sp who) <> (cm $ map render_da as) <> (cm $ map render_dv vs) <> amtp <> timep <> ns cons
      where
        amtp =
          case mamt of
            Nothing -> ""
            Just ap -> ".pay" <> parens (render_nest $ render_dp ap)
        timep =
          case mtime of
            Nothing -> ""
            Just (td, tp) -> ".timeout" <> parens (cm [render_da td, (render_nest $ render_dp tp)])
    DLS_FromConsensus _ more ->
      "commit()" <> semi <> hardline <> render_dls more
  where
    ns x = render_nest $ render_dls x
    cm l = parens (hsep $ punctuate comma $ l)

render_dls :: DLStmts -> Doc a
render_dls ss = concatWith (surround hardline) $ fmap render_dl ss

render_dp :: DLProg -> Doc a
render_dp (DLProg ss da) =
  render_dls ss <> hardline <> "return" <+> render_da da <> semi

render_local :: LLLocal -> Doc a
render_local l =
  case l of
    LLL_LocalStop -> "next()" <> semi
    LLL_Let at dv de k -> help (DLS_Let at dv de) k
    LLL_Var _at dv k -> "var" <+> render_dv dv <> semi <> hardline <> render_local k
    LLL_Set _at dv da k -> render_dv dv <+> "=" <+> render_da da <> semi <> hardline <> render_local k
    LLL_Claim at f ct a k -> help (DLS_Claim at f ct a) k
    LLL_If _at ca t f ->
      "if" <+> render_da ca <+> "then"
        <+> ns t <> hardline <> "else"
        <+> ns f <> semi
  where
    help d k = render_dl d <> hardline <> render_local k
    ns x = render_nest $ render_local x

render_con :: LLConsensus -> Doc a
render_con s =
  case s of
    LLC_ConStop -> mempty
    LLC_Let at dv de k -> help (DLS_Let at dv de) k
    LLC_Var _at dv k -> "var" <+> render_dv dv <> semi <> hardline <> render_con k
    LLC_Set _at dv da k -> render_dv dv <+> "=" <+> render_da da <> semi <> hardline <> render_con k
    LLC_Claim at f ct a k -> help (DLS_Claim at f ct a) k
    LLC_LocalIf _at ca t f k -> do_if ca t f <> hardline <> render_con k
    LLC_If _at ca t f -> do_if ca t f
    LLC_Transfer at who da k -> help (DLS_Transfer at who da) k
    LLC_FromConsensus _at k ->
      "commit()" <> semi <> hardline <> render_step k
  where
    help d k = render_dl d <> hardline <> render_con k
    ns = render_nest
    do_if ca t f =
      "if" <+> render_da ca <+> "then"
        <+> ns (render_con t) <> hardline <> "else"
        <+> ns (render_con f) <> semi

render_step :: LLStep -> Doc a
render_step s =
  case s of
    LLS_Stop da -> "exit" <> parens (render_da da) <> semi
    LLS_LocalStop -> mempty
    LLS_Let at dv de k -> help (DLS_Let at dv de) k
    LLS_Var _at dv k -> "var" <+> render_dv dv <> semi <> hardline <> render_step k
    LLS_Set _at dv da k -> render_dv dv <+> "=" <+> render_da da <> semi <> hardline <> render_step k
    LLS_Claim at f ct a k -> help (DLS_Claim at f ct a) k
    LLS_LocalIf _at ca t f k -> do_if ca t f <> hardline <> render_step k
    LLS_If _at ca t f -> do_if ca t f
    LLS_Only _at who onlys k ->
      "only" <> parens (render_sp who) <+> ns (render_local onlys) <> semi <> hardline <> render_step k
    LLS_ToConsensus _at who as vs mamt mtime cons ->
      "publish" <> parens (render_sp who) <> (cm $ map render_da as) <> (cm $ map render_dv vs) <> amtp <> timep <> ns (render_con cons)
      where
        amtp =
          case mamt of
            Nothing -> ""
            Just (ts, ta) ->
              ".pay" <> parens (render_nest $ (render_local ts) <> hardline <> "return" <+> render_da ta <> semi)
        timep =
          case mtime of
            Nothing -> ""
            Just (td, tl) -> ".timeout" <> parens (cm [render_da td, (render_nest $ render_step tl)])
  where
    help d k = render_dl d <> hardline <> render_step k
    cm l = parens (hsep $ punctuate comma $ l)
    ns = render_nest
    do_if ca t f =
      "if" <+> render_da ca <+> "then"
        <+> ns (render_step t) <> hardline <> "else"
        <+> ns (render_step f) <> semi
