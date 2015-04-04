{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8      as S
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Complex
import qualified Data.IntMap                as IntMap
import qualified Data.IntSet                as IntSet
import qualified Data.Map                   as Map
import           Data.Ratio
import qualified Data.Sequence              as Seq
import qualified Data.Set                   as Set
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.Text.Lazy             as TL
import qualified Data.Vector                as V
import qualified Data.Vector.Primitive      as VP
import qualified Data.Vector.Storable       as VS
import qualified Data.Vector.Unboxed        as VU
import           GHC.Generics
import           Test.Tasty
import           Test.Tasty.HUnit

import           Text.PrettyPrint.Generic

data Foo = Foo { fooA :: Int, fooB :: String } deriving Generic
instance Pretty Foo

data Bar a = Bar { barA :: Foo, barB :: a } deriving Generic
instance Pretty a => Pretty (Bar a)

data Baz = Baz1 | Baz2 Int | Baz3 Foo (Bar Baz) deriving Generic
instance Pretty Baz

main :: IO ()
main =
  defaultMain $ testGroup "test cases"
    [ testCase "()" $
      prettyShow' () @?= "()"
    , testCase "Char" $
      prettyShow' 'a' @?= "'a'"
    , testCase "Int" $
      prettyShow' (123 :: Int) @?= "123"
    , testCase "Integer" $
      prettyShow' (2^100 :: Integer) @?= "1267650600228229401496703205376"
    , testCase "Float" $
      prettyShow' (pi :: Float) @?= "3.1415927"
    , testCase "Double" $
      prettyShow' (pi :: Double) @?= "3.141592653589793"
    , testCase "String" $
      prettyShow' ("Hello" :: String) @?= "\"Hello\""
    , testCase "Bool" $
      prettyShow' True @?= "True"

    , testCase "Rational" $
      prettyShow' (123 % 456 :: Rational) @?= "41 % 152"
    , testCase "Complex" $
      prettyShow' (123 :+ 456 :: Complex Double) @?= "123.0 :+ 456.0"

    , testCase "List" $
      prettyShow' ([1..5] :: [Int]) @?= "[ 1, 2, 3, 4, 5 ]"

    , testCase "2-tuple" $
      prettyShow' ('a', 'b') @?= "('a', 'b')"
    , testCase "3-tuple" $
      prettyShow' ('a', 'b', 'c') @?= "('a', 'b', 'c')"
    , testCase "4-tuple" $
      prettyShow' ('a', 'b', 'c', 'd') @?= "('a', 'b', 'c', 'd')"
    , testCase "5-tuple" $
      prettyShow' ('a', 'b', 'c', 'd', 'e') @?= "('a', 'b', 'c', 'd', 'e')"
    , testCase "6-tuple" $
      prettyShow' ('a', 'b', 'c', 'd', 'e', 'f') @?= "('a', 'b', 'c', 'd', 'e', 'f')"
    , testCase "7-tuple" $
      prettyShow' ('a', 'b', 'c', 'd', 'e', 'f', 'g') @?= "('a', 'b', 'c', 'd', 'e', 'f', 'g')"

    , testCase "Maybe 1" $
      prettyShow' (Nothing :: Maybe Int) @?= "Nothing"
    , testCase "Maybe 2" $
      prettyShow' (Just 123 :: Maybe Int) @?= "Just 123"
    , testCase "Maybe 3" $
      prettyShow' (Just (Just 123) :: Maybe (Maybe Int)) @?= "Just (Just 123)"
    , testCase "Either 1" $
      prettyShow' (Left "Left" :: Either String Double) @?= "Left \"Left\""
    , testCase "Either 2" $
      prettyShow' (Right pi :: Either String Double) @?= "Right 3.141592653589793"

    , testCase "Strict ByteString" $
      prettyShow' (T.encodeUtf8 "日本語" :: S.ByteString) @?= "\"日本語\""
    , testCase "Lazy ByteString" $
      prettyShow' (L.fromStrict $ T.encodeUtf8 "日本語" :: L.ByteString) @?= "\"日本語\""
    , testCase "Strict Text" $
      prettyShow' ("日本語" :: T.Text) @?= "\"日本語\""
    , testCase "Lazy Text" $
      prettyShow' ("日本語" :: TL.Text) @?= "\"日本語\""

    , testCase "Set" $
      prettyShow' (["foo", "bar", "baz"] :: Set.Set String) @?= "[ \"bar\", \"baz\", \"foo\" ]"
    , testCase "IntSet" $
      prettyShow' ([1..5] :: IntSet.IntSet) @?= "[ 1, 2, 3, 4, 5 ]"
    , testCase "Map" $
      prettyShow' ([("foo", 123), ("bar", 456)] :: Map.Map String Int) @?= "{ \"bar\": 456, \"foo\": 123 }"
    , testCase "IntMap" $
      prettyShow' ([(123, "foo"), (456, "bar")] :: IntMap.IntMap String) @?= "{ 123: \"foo\", 456: \"bar\" }"
    , testCase "Seq" $
      prettyShow' ([1..5] :: Seq.Seq Int) @?= "[ 1, 2, 3, 4, 5 ]"

    , testCase "Vector" $
      prettyShow' ([1..5] :: V.Vector Int) @?= "[ 1, 2, 3, 4, 5 ]"
    , testCase "Primitive Vector" $
      prettyShow' ([1..5] :: VP.Vector Int) @?= "[ 1, 2, 3, 4, 5 ]"
    , testCase "Storable Vector" $
      prettyShow' ([1..5] :: VS.Vector Int) @?= "[ 1, 2, 3, 4, 5 ]"
    , testCase "Unboxed Vector" $
      prettyShow' ([1..5] :: VU.Vector Int) @?= "[ 1, 2, 3, 4, 5 ]"

    , testCase "User defined 1" $
      prettyShow' (Foo 123 "foo") @?= "Foo { fooA = 123, fooB = \"foo\" }"
    , testCase "User defined 2" $
      prettyShow' (Bar (Foo 123 "foo") (Just True)) @?= "Bar { barA = Foo { fooA = 123\n                 , fooB = \"foo\" }\n    , barB = Just True }"
    , testCase "User defined 3" $
      prettyShow' Baz1 @?= "Baz1"
    , testCase "User defined 4" $
      prettyShow' (Baz2 123) @?= "Baz2 123"
    , testCase "User defined 5" $
      prettyShow' (Baz3 (Foo 123 "foo") (Bar (Foo 456 "bar") Baz1)) @?= "Baz3 Foo { fooA = 123\n         , fooB = \"foo\" }\n     Bar { barA = Foo { fooA = 456\n                      , fooB = \"bar\" }\n         , barB = Baz1 }"
    ]
