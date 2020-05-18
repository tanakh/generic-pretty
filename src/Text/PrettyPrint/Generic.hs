{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedLists      #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Pretty printing for Generic data types

module Text.PrettyPrint.Generic (
  -- * Type classes
  Pretty(..),
  GPretty(..),

  -- * Utility functions
  pretty,  prettyShow,  prettyPrint,  hPrettyPrint,
  pretty', prettyShow', prettyPrint', hPrettyPrint',
  ) where

import           GHC.Exts                     (IsList (..))
import           GHC.Generics
import           System.IO
import           Text.PrettyPrint.ANSI.Leijen hiding (Pretty (..))

import           Control.Applicative          (Const, WrappedArrow,
                                               WrappedMonad, ZipList)
import qualified Data.ByteString.Char8        as S
import qualified Data.ByteString.Lazy.Char8   as L
import           Data.Complex
import           Data.Functor.Identity        (Identity)
import           Data.Int
import qualified Data.IntMap                  as IntMap
import qualified Data.IntSet                  as IntSet
import qualified Data.Map                     as Map
import           Data.Monoid                  (All, Alt, Any, First, Last,
                                               Product, Sum)
import           Data.Ratio
import qualified Data.Sequence                as Seq
import qualified Data.Set                     as Set
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import qualified Data.Text.Lazy               as TL
import qualified Data.Vector                  as V
import qualified Data.Vector.Primitive        as VP
import qualified Data.Vector.Storable         as VS
import qualified Data.Vector.Unboxed          as VU
import           Data.Word

-- Color scheme

constructorS, numericS, stringS, operatorS, selectorS :: Doc -> Doc
constructorS = underline . bold . dullgreen
numericS     = magenta
stringS      = dullyellow
operatorS    = dullred
selectorS    = blue

-- Generic function

-- | Type class for generic representations
class GPretty f where
  -- | Pretty print a `Generic` value
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
    | p == 0       = [ con <+> align (sep es) ]
    | otherwise    = [ parens $ con <+> align (sep es) ]
    where
      con = constructorS $ text (conName c)
      es = gprettyPrec (p + 1) a

instance {-# OVERLAPPABLE #-} (GPretty f, Selector s) => GPretty (S1 s f) where
  gprettyPrec _ s@(M1 a) =
    [ selectorS (text (selName s)) <+> operatorS (text "=") <+> sep (gprettyPrec 0 a) ]
instance {-# OVERLAPPING #-} GPretty f => GPretty (S1 ('MetaSel 'Nothing su ss ds) f) where
  gprettyPrec p (M1 a) = gprettyPrec p a

instance (GPretty f, GPretty g) => GPretty (f :+: g) where
  gprettyPrec p (L1 a) = gprettyPrec p a
  gprettyPrec p (R1 a) = gprettyPrec p a

instance (GPretty f, GPretty g) => GPretty (f :*: g) where
  gprettyPrec p (a :*: b) = gprettyPrec p a ++ gprettyPrec p b

-- Wrapper function

-- | Type class for pretty printing
class Pretty a where
  -- | Pretty print a value to `Doc`
  prettyPrec :: Int -> a -> Doc
  default prettyPrec :: (Generic a, GPretty (Rep a)) => Int -> a -> Doc
  prettyPrec p = sep . gprettyPrec p . from

-- Utility functions

-- | Pretty print a value with decoration
pretty :: Pretty a => a -> Doc
pretty = prettyPrec 0

-- | Pretty print a value to `String`
prettyShow :: Pretty a => a -> String
prettyShow = show . pretty

-- | Pretty print a value to `stdout`
prettyPrint :: Pretty a => a -> IO ()
prettyPrint = hPrettyPrint stdout

-- | Pretty print a value
hPrettyPrint :: Pretty a => Handle -> a -> IO ()
hPrettyPrint h = hPutDoc h . (<> hardline) . pretty

-- | Pretty print a value without decoration
pretty' :: Pretty a => a -> Doc
pretty' = plain . pretty

-- | Plain version for `prettyShow`
prettyShow' :: Pretty a => a -> String
prettyShow' = show . pretty'

-- | Plain version for `prettyPrint`
prettyPrint' :: Pretty a => a -> IO ()
prettyPrint' = hPrettyPrint' stdout

-- | Plain version for `hPrettyPrint`
hPrettyPrint' :: Pretty a => Handle -> a -> IO ()
hPrettyPrint' h = hPutDoc h . (<> hardline) . pretty'

-- instances

instance Pretty ()      where prettyPrec _ = text . show
instance Pretty Char    where prettyPrec _ = stringS . text . show
instance Pretty Int     where prettyPrec _ = numericS . text . show
instance Pretty Integer where prettyPrec _ = numericS . text . show
instance Pretty Float   where prettyPrec _ = numericS . text . show
instance Pretty Double  where prettyPrec _ = numericS . text . show
instance Pretty Bool    where prettyPrec _ = numericS . text . show

instance Pretty Word    where prettyPrec _ = numericS . text . show
instance Pretty Word8   where prettyPrec _ = numericS . text . show
instance Pretty Word16  where prettyPrec _ = numericS . text . show
instance Pretty Word32  where prettyPrec _ = numericS . text . show
instance Pretty Word64  where prettyPrec _ = numericS . text . show

instance Pretty Int8    where prettyPrec _ = numericS . text . show
instance Pretty Int16   where prettyPrec _ = numericS . text . show
instance Pretty Int32   where prettyPrec _ = numericS . text . show
instance Pretty Int64   where prettyPrec _ = numericS . text . show

instance (Integral a, Pretty a) => Pretty (Ratio a) where
  prettyPrec _ r =
    pretty (numerator r) <+> operatorS (char '%') <+> pretty (denominator r)

instance (Pretty a) => Pretty (Complex a) where
  prettyPrec _ (r :+ i) =
    pretty r <+> operatorS (text ":+") <+> pretty i

instance {-# OVERLAPPABLE #-} Pretty a => Pretty [a] where
  prettyPrec _ = encloseSep (lbracket <> space) (space <> rbracket) (comma <> space) . map pretty

instance {-# OVERLAPPING #-} Pretty String where
  prettyPrec _ = stringS . dquotes . text . prettyString where
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

-- vectors

instance Pretty a => Pretty (V.Vector a) where
  prettyPrec _ = pretty . toList

instance (Pretty a, VP.Prim a) => Pretty (VP.Vector a) where
  prettyPrec _ = pretty . toList

instance (Pretty a, VS.Storable a) => Pretty (VS.Vector a) where
  prettyPrec _ = pretty . toList

instance (Pretty a, VU.Unbox a) => Pretty (VU.Vector a) where
  prettyPrec _ = pretty . toList
