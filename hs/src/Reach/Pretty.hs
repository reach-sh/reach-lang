{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Reach.Pretty where

import qualified Data.Map.Strict as M
import Reach.Texty

pform :: Doc -> Doc -> Doc
pform f xs = group $ parens $ f <+> xs

pform_ :: Doc -> Doc
pform_ f = pform f mempty

pbrackets :: [Doc] -> Doc
pbrackets xs = group $ render_nest $ vsep $ punctuate comma xs

render_obj :: Pretty k => Pretty v => M.Map k v -> Doc
render_obj env =
  braces $ nest 2 $ hardline <> (concatWith (surround (comma <> hardline)) $ map render_p $ M.toList env)
  where
    render_p (k, oa) = pretty k <+> "=" <+> pretty oa

render_das :: Pretty a => [a] -> Doc
render_das as = hsep $ punctuate comma $ map pretty as

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

prettyTransfer :: Pretty a => a -> a -> Doc
prettyTransfer who da =
  "transfer." <> parens (pretty da) <> ".to" <> parens (pretty who)

prettyStop :: Doc
prettyStop = "exit" <> parens (emptyDoc) <> semi

prettyMap :: Pretty a => Pretty b => Pretty c => a -> b -> a -> c -> Doc
prettyMap ans x a f =
  "map" <+> pretty ans <+> "=" <+> "for" <+> parens (pretty a <+> "in" <+> pretty x)
    <+> braces (nest 2 $ hardline <> pretty f)

prettyReduce :: Pretty a => Pretty b => Pretty c => a -> b -> b -> a -> a -> c -> Doc
prettyReduce ans x z b a f =
  "reduce" <+> pretty ans <+> "=" <+> "for" <+> parens (pretty b <+> "=" <+> pretty z <> semi <+> pretty a <+> "in" <+> pretty x)
    <+> braces (nest 2 $ hardline <> pretty f)

prettyToConsensus_ :: Pretty c => Pretty d => Pretty s => (a -> Doc) -> (b -> Doc) -> M.Map s (Bool, [d], d, d) -> (c, [c], c, c, a) -> (Maybe (d, b)) -> Doc
prettyToConsensus_ fa fb send (win, msg, amtv, tv, body) mtime =
  prettyToConsensus fa fb send (Nothing, win, msg, amtv, tv, body) mtime

prettyToConsensus :: Pretty c => Pretty d => Pretty s => (a -> Doc) -> (b -> Doc) -> M.Map s (Bool, [d], d, d) -> (Maybe c, c, [c], c, c, a) -> (Maybe (d, b)) -> Doc
prettyToConsensus fa fb send (ltv, win, msg, amtv, tv, body) mtime =
  "publish" <> parens emptyDoc
    <> nest
      2
      (hardline <> mtime'
         <> concatWith (surround hardline) (map go $ M.toList send)
         <> hardline
         <> ".recv"
         <> parens (hsep $ punctuate comma $ [pretty ltv, pretty win, prettyl msg, pretty amtv, pretty tv, render_nest (fa body)])
         <> semi)
  where
    go (p, (isClass, args, amta, whena)) =
      ".case" <> parens (hsep $ punctuate comma $ [pretty p, pretty isClass, prettyl args, pretty amta, pretty whena])
    mtime' =
      case mtime of
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
