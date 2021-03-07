module Reach.Texty
  ( Doc (..)
  , render
  , Pretty
  , pretty
  , prettyl
  , (<+>)
  , group --- XXX remove
  , emptyDoc
  , viaShow
  , vsep
  , hcat
  , hsep
  , nest --- XXX remove int arg
  , concatWith --- XXX remove
  , punctuate
  , enclose
  , surround
  , squotes
  , dquotes
  , parens
  , braces
  , brackets
  , hardline --- XXX remove
  , semi
  , comma
  , space
  , render_obj
  )
where

import Control.Monad.Identity
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

data Doc
  = DText LT.Text
  | DNewline
  | DCat Doc Doc
  | DVSep [Doc]
  | DHSep [Doc]
  | DNest Integer Doc

instance Show Doc where
  show = LT.unpack . render

type Env = Integer

type App = ReaderT Env Identity

render_ :: Doc -> App LT.Text
render_ = \case
  DText x -> return x
  DCat x y -> (<>) <$> render_ x <*> render_ y
  DHSep [] -> return ""
  DHSep (x : xs) -> do
    x_ <- render_ x
    xs_ <- render_ (DHSep xs)
    return $ x_ <> " " <> xs_
  DNest dn d -> local (dn +) $ render_ d
  DNewline -> do
    n <- ask
    return $ "\n" <> (LT.replicate (fromIntegral n) " ")
  DVSep [] -> return ""
  DVSep (x : xs) -> do
    x_ <- render_ x
    n_ <- render_ DNewline
    xs_ <- render_ (DVSep xs)
    return $ x_ <> n_ <> xs_

render :: Doc -> LT.Text
render = runIdentity . flip runReaderT 0 . render_

render_obj :: Pretty k => Pretty v => M.Map k v -> Doc
render_obj env =
  braces $ nest 2 $ hardline <> (concatWith (surround (comma <> hardline)) $ map render_p $ M.toAscList env)
  where
    render_p (k, oa) = pretty k <+> "=" <+> pretty oa

class Pretty a where
  pretty :: a -> Doc

instance (Pretty k, Pretty v) => Pretty (M.Map k v) where
  pretty = render_obj

instance (Pretty a, Pretty b) => Pretty (Either a b) where
  pretty = \case
    Left x -> pretty x
    Right x -> pretty x

instance {-# OVERLAPS #-} Pretty String where
  pretty = DText . LT.pack

instance {-# OVERLAPS #-} Pretty B.ByteString where
  pretty = viaShow

instance Pretty a => Pretty (Maybe a) where
  pretty Nothing = "Nothing"
  pretty (Just x) = "Just" <+> pretty x

prettyl :: Pretty a => [a] -> Doc
prettyl l = "[" <> (concatWith (\x y -> x <> ", " <> y) $ map pretty l) <> "]"

instance {-# OVERLAPPABLE #-} Pretty a => Pretty [a] where
  pretty = prettyl

instance Pretty a => Pretty (S.Set a) where
  pretty = pretty . S.toAscList

instance (Pretty a, Pretty b) => Pretty (a, b) where
  pretty (x, y) = "(" <> pretty x <> ", " <> pretty y <> ")"

instance Pretty Bool where
  pretty = viaShow

instance Pretty () where
  pretty = viaShow

instance Pretty Integer where
  pretty = viaShow

instance Pretty Int where
  pretty = viaShow

instance Pretty T.Text where
  pretty = DText . LT.fromStrict

instance Pretty LT.Text where
  pretty = DText

instance Pretty Doc where
  pretty = id

instance Semigroup Doc where
  (<>) = DCat

instance Monoid Doc where
  mempty = ""

instance IsString Doc where
  fromString = DText . LT.pack

infixr 6 <+>

(<+>) :: Doc -> Doc -> Doc
x <+> y = x <> " " <> y

group :: Doc -> Doc
group = id

emptyDoc :: Doc
emptyDoc = mempty

vsep :: [Doc] -> Doc
vsep = DVSep

hsep :: [Doc] -> Doc
hsep = DHSep

hcat :: [Doc] -> Doc
hcat = concatWith (<>)

nest :: Integer -> Doc -> Doc
nest = DNest

concatWith :: Foldable f => (Doc -> Doc -> Doc) -> f Doc -> Doc
concatWith f xs =
  case foldr (\_ _ -> False) True xs of
    True -> mempty
    False -> foldr1 f xs

viaShow :: Show a => a -> Doc
viaShow = DText . LT.pack . show

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p = go
  where
    go [] = []
    go [d] = [d]
    go (d : ds) = (d <> p) : go ds

enclose :: Doc -> Doc -> Doc -> Doc
enclose b a c = b <> c <> a

surround :: Doc -> Doc -> Doc -> Doc
surround x l r = enclose l r x

squotes :: Doc -> Doc
squotes = enclose "'" "'"

dquotes :: Doc -> Doc
dquotes = enclose "\"" "\""

parens :: Doc -> Doc
parens = enclose "(" ")"

braces :: Doc -> Doc
braces = enclose "{" "}"

brackets :: Doc -> Doc
brackets = enclose "[" "]"

hardline :: Doc
hardline = DNewline

semi :: Doc
semi = ";"

comma :: Doc
comma = ","

space :: Doc
space = " "
