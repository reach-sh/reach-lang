{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Reach.Pretty where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import Reach.Texty

pform :: Doc -> Doc -> Doc
pform f xs = parens $ f <+> xs

pform_ :: Doc -> Doc
pform_ f = pform f mempty

pbrackets :: [Doc] -> Doc
pbrackets xs = render_nest $ vsep $ punctuate comma xs

render_das :: Pretty a => [a] -> Doc
render_das as = hsep $ punctuate comma $ map pretty as

render_nest :: Doc -> Doc
render_nest inner = nest $ braces (hardline <> inner <> " ")

prettyIf :: Pretty c => c -> Doc -> Doc -> Doc
prettyIf ca t f =
  "if" <+> pretty ca <+> "then"
    <+> render_nest t <> hardline <> "else"
    <+> render_nest f <> semi

prettyIfp :: Pretty c => Pretty e => c -> e -> e -> Doc
prettyIfp ca t f = prettyIf ca (pretty t) (pretty f)

prettySwitch :: (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e) => a -> M.Map b (c, d, e) -> Doc
prettySwitch ov csm =
  "switch" <+> parens (pretty ov) <+> render_nest (concatWith (surround hardline) $ map render_p $ M.toList csm)
  where
    render_p (k, (vc, vd, ve)) = "case" <+> pretty k <+> "as" <+> pretty vc <> "/" <> pretty vd <> ":" <+> render_nest (pretty ve)

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

prettyStop :: Doc
prettyStop = "exit" <> parens (emptyDoc) <> semi

prettyMap :: (Pretty a, Pretty b, Pretty c, Pretty d) => a -> [b] -> [a] -> c -> d -> Doc
prettyMap ans xs as i f =
  "map" <+> pretty ans <+> "=" <+> "for" <+>
  parens (withCommas as <> pretty i <+> "in" <+> withCommas xs) <+> braces (nest $ hardline <> pretty f)
    where withCommas vs = L.foldl' (\accum v -> accum <> pretty v <> ",") "" vs

prettyReduce :: (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e, Pretty f, Pretty g) => a -> b -> c -> d -> e -> f -> g -> Doc
prettyReduce ans x z b a i f =
  "reduce" <+> pretty ans <+> "=" <+> "for" <+> parens (pretty b <+> "=" <+> pretty z <> semi <+> pretty a <> "," <> pretty i <+> "in" <+> pretty x)
    <+> braces (nest $ hardline <> pretty f)

prettyToConsensus__ :: (Pretty a, Pretty b, Pretty c, Pretty s, Pretty d, Pretty k2) => c -> M.Map s a -> b -> Maybe (d, k2) -> Doc
prettyToConsensus__ lct send recv mtime =
  "publish" <> parens ("@" <> pretty lct) <> nest (hardline <> mtime' <> send' <> recv')
  where
    mtime' = prettyTimeout mtime
    send' = prettySends send <> hardline
    recv' = pretty recv <> hardline

prettySends :: Pretty a => Pretty b => M.Map a b -> Doc
prettySends send = concatWith (surround hardline) (map go $ M.toList send)
  where
    go (p, s) =
      ".case" <> parens (pretty p) <> pretty s

prettyTimeout :: Pretty d => Pretty k2 => Maybe (d, k2) -> Doc
prettyTimeout = \case
  Nothing -> emptyDoc
  Just (delaya, tbody) ->
    ".timeout" <> parens (hsep $ punctuate comma $ [pretty delaya, render_nest (pretty tbody)]) <> hardline

prettyToConsensus_ :: Pretty c => Pretty d => Pretty s => (a -> Doc) -> (b -> Doc) -> M.Map s (Bool, [d], d, d) -> (c, [c], c, c, a) -> (Maybe (d, b)) -> Doc
prettyToConsensus_ fa fb send (win, msg, amtv, tv, body) mtime =
  prettyToConsensus fa fb send (Nothing, win, msg, amtv, tv, body) mtime

prettyToConsensus :: Pretty c => Pretty d => Pretty s => (a -> Doc) -> (b -> Doc) -> M.Map s (Bool, [d], d, d) -> (Maybe c, c, [c], c, c, a) -> (Maybe (d, b)) -> Doc
prettyToConsensus fa fb send (ltv, win, msg, amtv, tv, body) mtime =
  "publish" <> parens emptyDoc
    <> nest
      (hardline <> mtime'
         <> concatWith (surround hardline) (map go $ M.toList send)
         <> hardline
         <> ".recv"
         <> parens (hsep $ punctuate comma $ [pretty ltv, pretty win, prettyl msg, pretty amtv, pretty tv, render_nest (fa body)])
         <> semi)
  where
    go (p, (isClass, args, amta, whena)) =
      ".case" <> parens (hsep $ punctuate comma $ [pretty p, pretty isClass, prettyl args, pretty amta, pretty whena])
    mtime' = case mtime of
      Nothing -> emptyDoc
      Just (delaya, tbody) ->
        ".timeout" <> parens (hsep $ punctuate comma $ [pretty delaya, render_nest (fb tbody)]) <> hardline

prettyCommit :: Doc
prettyCommit = "commit();"

prettyCom :: Pretty a => Pretty b => a -> b -> Doc
prettyCom x y = pretty x <> hardline <> pretty y

prettyOnly :: Pretty a => Pretty b => a -> b -> Doc
prettyOnly who b =
  "only" <> parens (pretty who) <+> render_nest (pretty b) <> semi

prettyOnlyK :: Pretty a => Pretty b => Pretty c => a -> b -> c -> Doc
prettyOnlyK who onlys k =
  prettyOnly who onlys <> hardline <> pretty k

prettyBlock :: Pretty a => Doc -> a -> Doc
prettyBlock b da = b <> hardline <> "return" <+> pretty da <> semi

prettyBlockP :: Pretty a => Pretty b => a -> b -> Doc
prettyBlockP b da = prettyBlock (pretty b) da

prettyViewIs :: (Pretty a, Pretty b, Pretty c) => a -> b -> c -> Doc
prettyViewIs v k a = "view(" <> pretty v <> ")." <> pretty k <> ".is(" <> pretty a <> ")"
