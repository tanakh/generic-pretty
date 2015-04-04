{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Pretty Printing for Generic

module Text.PrettyPrint.Generic (
  Pretty(..),
  GPretty(..),
  showPretty,
  ) where

import           GHC.Generics
import           Text.PrettyPrint.ANSI.Leijen hiding (Pretty (..))

class GPretty f where
  gpretty :: f a -> [Doc]

instance GPretty V1 where
  gpretty _ = error "this never happen"

instance GPretty U1 where
  gpretty U1 = []

instance Pretty c => GPretty (Rec0 c) where
  gpretty (K1 c) = [pretty c]

instance GPretty f => GPretty (D1 d f) where
  gpretty (M1 a) = gpretty a

instance (GPretty f, Constructor c) => GPretty (C1 c f) where
  gpretty c@(M1 a)
    | conIsRecord c =
      [ bold (text (conName c)) <+>
        encloseSep (lbrace <> space) (space <> rbrace) (comma <> space) (gpretty a) ]
    | otherwise =
      [ parens $ bold (text (conName c)) <+> sep (gpretty a) ]

instance {-# OVERLAPPABLE #-} (GPretty f, Selector s) => GPretty (S1 s f) where
  gpretty s@(M1 a) =
    [ underline (text (selName s)) <+> text "=" <+> sep (gpretty a) ]
instance {-# OVERLAPPING #-} GPretty f => GPretty (S1 NoSelector f) where
  gpretty (M1 a) = gpretty a

instance (GPretty f, GPretty g) => GPretty (f :+: g) where
  gpretty (L1 a) = gpretty a
  gpretty (R1 a) = gpretty a

instance (GPretty f, GPretty g) => GPretty (f :*: g) where
  gpretty (a :*: b) = gpretty a ++ gpretty b

class Pretty a where
  pretty :: a -> Doc
  default pretty :: (Generic a, GPretty (Rep a)) => a -> Doc
  pretty = sep . gpretty . from

showPretty :: Pretty a => a -> String
showPretty = show . pretty

instance Pretty ()      where pretty = text . show
instance Pretty Char    where pretty = text . show
instance Pretty Int     where pretty = text . show
instance Pretty Integer where pretty = text . show
instance Pretty Float   where pretty = text . show
instance Pretty Double  where pretty = text . show
instance Pretty Bool    where pretty = text . show

instance {-# OVERLAPPABLE #-} Pretty a => Pretty [a] where
  pretty = encloseSep (lbracket <> space) (space <> rbracket) (comma <> space) . map pretty
instance {-# OVERLAPPING #-} Pretty String where
  pretty = text . show

instance Pretty a => Pretty (Maybe a)
instance (Pretty a, Pretty b) => Pretty (Either a b)

data Foo = Foo { fooA :: Int, fooB :: String } deriving Generic
instance Pretty Foo

test :: IO ()
test = do
  putStrLn $ showPretty ()
  putStrLn $ showPretty 'a'
  putStrLn $ showPretty (123 :: Int)
  putStrLn $ showPretty (2^100 :: Integer)
  putStrLn $ showPretty (pi :: Float)
  putStrLn $ showPretty (pi :: Double)
  putStrLn $ showPretty "Hello"
  putStrLn $ showPretty True
  putStrLn $ showPretty ([1..5] :: [Int])
  putStrLn $ showPretty ([1..10] :: [Int])

  putStrLn $ showPretty (Just 123 :: Maybe Int)
  putStrLn $ showPretty (Just (Just 1) :: Maybe (Maybe Int))
  putStrLn $ showPretty (Left "Left" :: Either String Double)
  putStrLn $ showPretty (Right pi :: Either String Double)

  putStrLn $ showPretty (Foo 123 "foo")
