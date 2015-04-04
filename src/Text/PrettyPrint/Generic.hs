{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedLists      #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Pretty Printing for Generic

module Text.PrettyPrint.Generic (
  Pretty(..),
  GPretty(..),
  pretty, showPretty,
  ) where

import           GHC.Exts                     (IsList (..))
import           GHC.Generics
import           Text.PrettyPrint.ANSI.Leijen hiding (Pretty (..))

import           Control.Applicative          (Const, WrappedArrow,
                                               WrappedMonad, ZipList)
import qualified Data.ByteString.Char8        as S
import qualified Data.ByteString.Lazy.Char8   as L
import           Data.Functor.Identity        (Identity)
import           Data.Int
import qualified Data.IntMap                  as IntMap
import qualified Data.IntSet                  as IntSet
import qualified Data.Map                     as Map
import           Data.Monoid                  (All, Alt, Any, First, Last,
                                               Product, Sum)
import qualified Data.Sequence                as Seq
import qualified Data.Set                     as Set
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import qualified Data.Text.Lazy               as TL
import           Data.Word

class GPretty f where
  gprettyPrec :: Int -> f a -> [Doc]

instance GPretty V1 where
  gprettyPrec _ _ = error "this never happen"

instance GPretty U1 where
  gprettyPrec _ U1 = []

instance Pretty c => GPretty (Rec0 c) where
  gprettyPrec p (K1 c) = [prettyPrec p c]

instance GPretty f => GPretty (D1 d f) where
  gprettyPrec p (M1 a) = gprettyPrec p a

instance (GPretty f, Constructor c) => GPretty (C1 c f) where
  gprettyPrec p c@(M1 a)
    | conIsRecord c =
      [ con <+> encloseSep (lbrace <> space) (space <> rbrace) (comma <> space) es ]
    | null es      = [ con ]
    | p == 0       = [ con <+> sep es ]
    | otherwise    = [ parens $ con <+> sep es ]
    where
      con = bold (text (conName c))
      es = gprettyPrec (p + 1) a

instance {-# OVERLAPPABLE #-} (GPretty f, Selector s) => GPretty (S1 s f) where
  gprettyPrec _ s@(M1 a) =
    [ underline (text (selName s)) <+> text "=" <+> sep (gprettyPrec 0 a) ]
instance {-# OVERLAPPING #-} GPretty f => GPretty (S1 NoSelector f) where
  gprettyPrec p (M1 a) = gprettyPrec p a

instance (GPretty f, GPretty g) => GPretty (f :+: g) where
  gprettyPrec p (L1 a) = gprettyPrec p a
  gprettyPrec p (R1 a) = gprettyPrec p a

instance (GPretty f, GPretty g) => GPretty (f :*: g) where
  gprettyPrec p (a :*: b) = gprettyPrec p a ++ gprettyPrec p b

class Pretty a where
  prettyPrec :: Int -> a -> Doc
  default prettyPrec :: (Generic a, GPretty (Rep a)) => Int -> a -> Doc
  prettyPrec p = sep . gprettyPrec p . from

pretty :: Pretty a => a -> Doc
pretty = prettyPrec 0

showPretty :: Pretty a => a -> String
showPretty = show . pretty

instance Pretty ()      where prettyPrec _ = text . show
instance Pretty Char    where prettyPrec _ = text . show
instance Pretty Int     where prettyPrec _ = text . show
instance Pretty Integer where prettyPrec _ = text . show
instance Pretty Float   where prettyPrec _ = text . show
instance Pretty Double  where prettyPrec _ = text . show
instance Pretty Bool    where prettyPrec _ = text . show

instance Pretty Word    where prettyPrec _ = text . show
instance Pretty Word8   where prettyPrec _ = text . show
instance Pretty Word16  where prettyPrec _ = text . show
instance Pretty Word32  where prettyPrec _ = text . show
instance Pretty Word64  where prettyPrec _ = text . show

instance Pretty Int8    where prettyPrec _ = text . show
instance Pretty Int16   where prettyPrec _ = text . show
instance Pretty Int32   where prettyPrec _ = text . show
instance Pretty Int64   where prettyPrec _ = text . show

instance {-# OVERLAPPABLE #-} Pretty a => Pretty [a] where
  prettyPrec _ = encloseSep (lbracket <> space) (space <> rbracket) (comma <> space) . map pretty

instance {-# OVERLAPPING #-} Pretty String where
  prettyPrec _ = dquotes . text . prettyString where
    prettyString cs = foldr ((.) . prettyChar) id cs ""
    prettyChar c
      | fromEnum c < 0x80 = showChar c
      | otherwise = (c:)

instance (Pretty a, Pretty b) => Pretty (a, b) where
  prettyPrec _ (a, b) = parens $ pretty a <> comma <+> pretty b

instance (Pretty a, Pretty b, Pretty c) => Pretty (a, b, c) where
  prettyPrec _ (a, b, c) = parens $ pretty a <> comma <+> pretty b <> comma <+> pretty c

instance (Pretty a, Pretty b, Pretty c, Pretty d) => Pretty (a, b, c, d) where
  prettyPrec _ (a, b, c, d) = parens $ pretty a <> comma <+> pretty b <> comma <+> pretty c <> comma <+> pretty d

instance (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e) => Pretty (a, b, c, d, e) where
  prettyPrec _ (a, b, c, d, e) = parens $ pretty a <> comma <+> pretty b <> comma <+> pretty c <> comma <+> pretty d <> comma <+> pretty e

instance (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e, Pretty f) => Pretty (a, b, c, d, e, f) where
  prettyPrec _ (a, b, c, d, e, f) = parens $ pretty a <> comma <+> pretty b <> comma <+> pretty c <> comma <+> pretty d <> comma <+> pretty e <> comma <+> pretty f

instance (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e, Pretty f, Pretty g) => Pretty (a, b, c, d, e, f, g) where
  prettyPrec _ (a, b, c, d, e, f, g) = parens $ pretty a <> comma <+> pretty b <> comma <+> pretty c <> comma <+> pretty d <> comma <+> pretty e <> comma <+> pretty f <> comma <+> pretty g

instance Pretty a => Pretty (Maybe a)
instance (Pretty a, Pretty b) => Pretty (Either a b)
instance Pretty Ordering
instance Pretty Any
instance Pretty All
instance Pretty a => Pretty (First a)
instance Pretty a => Pretty (Last a)
instance Pretty a => Pretty (Sum a)
instance Pretty a => Pretty (Product a)
instance Pretty (f a) => Pretty (Alt f a)
instance Pretty a => Pretty (Identity a)
instance Pretty a => Pretty (Const a b)
instance Pretty a => Pretty (ZipList a)
instance Pretty (m a) => Pretty (WrappedMonad m a)
instance Pretty (a b c) => Pretty (WrappedArrow a b c)

-- bytestrings, texts

instance Pretty S.ByteString where
  prettyPrec _ bs = case T.decodeUtf8' bs of
    Left err -> pretty $ show err
    Right t -> pretty t

instance Pretty T.Text where
  prettyPrec _ = pretty . T.unpack

instance Pretty L.ByteString where
  prettyPrec _ = pretty . L.toStrict

instance Pretty TL.Text where
  prettyPrec _ = pretty . TL.toStrict

-- containers

instance (Pretty a, Ord a) => Pretty (Set.Set a) where
  prettyPrec _ = encloseSep (lbracket <> space) (space <> rbracket) (comma <> space) . map pretty . toList

instance Pretty IntSet.IntSet where
  prettyPrec _ = pretty . Set.fromList . toList

instance (Pretty a, Pretty b, Ord a) => Pretty (Map.Map a b) where
  prettyPrec _ = encloseSep (lbrace <> space) (space <> rbrace) (comma <> space) . map f . toList where
    f (key, val) = pretty key <> colon <+> pretty val

instance Pretty b => Pretty (IntMap.IntMap b) where
  prettyPrec _ = pretty . Map.fromList . toList

instance Pretty a => Pretty (Seq.Seq a) where
  prettyPrec _ = pretty . toList

-- tests

data Foo = Foo { fooA :: Int, fooB :: String } deriving Generic
instance Pretty Foo

data Bar a = Bar { barA :: Foo, barB :: a } deriving Generic
instance Pretty a => Pretty (Bar a)

test :: IO ()
test = do
  putStrLn $ showPretty ()
  putStrLn $ showPretty 'a'
  putStrLn $ showPretty (123 :: Int)
  putStrLn $ showPretty (2^100 :: Integer)
  putStrLn $ showPretty (pi :: Float)
  putStrLn $ showPretty (pi :: Double)
  putStrLn $ showPretty ("Hello" :: String)
  putStrLn $ showPretty True
  putStrLn $ showPretty ([1..5] :: [Int])
  putStrLn $ showPretty ([1..10] :: [Int])

  putStrLn $ showPretty ('a', 'b')
  putStrLn $ showPretty ('a', 'b', 'c')
  putStrLn $ showPretty ('a', 'b', 'c', 'd')
  putStrLn $ showPretty ('a', 'b', 'c', 'd', 'e')
  putStrLn $ showPretty ('a', 'b', 'c', 'd', 'e', 'f')
  putStrLn $ showPretty ('a', 'b', 'c', 'd', 'e', 'f', 'g')

  putStrLn $ showPretty (Nothing :: Maybe Int)
  putStrLn $ showPretty (Just 123 :: Maybe Int)
  putStrLn $ showPretty (Just (Just 1) :: Maybe (Maybe Int))
  putStrLn $ showPretty (Left "Left" :: Either String Double)
  putStrLn $ showPretty (Right pi :: Either String Double)

  putStrLn $ showPretty (T.encodeUtf8 "日本語" :: S.ByteString)
  putStrLn $ showPretty (L.fromStrict $ T.encodeUtf8 "日本語" :: L.ByteString)
  putStrLn $ showPretty ("日本語" :: T.Text)
  putStrLn $ showPretty ("日本語" :: TL.Text)

  putStrLn $ showPretty (["foo", "bar", "baz"] :: Set.Set String)
  putStrLn $ showPretty ([1..5] :: IntSet.IntSet)
  putStrLn $ showPretty ([("foo", 123), ("bar", 456)] :: Map.Map String Int)
  putStrLn $ showPretty ([(123, "foo"), (456, "bar")] :: IntMap.IntMap String)
  putStrLn $ showPretty ([1..5] :: Seq.Seq Int)

  putStrLn $ showPretty (Foo 123 "foo")
  putStrLn $ showPretty (Bar (Foo 123 "foo") (Just True))
