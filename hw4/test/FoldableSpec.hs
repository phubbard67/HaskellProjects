{-# LANGUAGE NoImplicitPrelude #-}

module FoldableSpec where

import Data.Coerce
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Map.Append as AppendMap
import Data.Map.Append (AppendMap(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty(..))
import Data.List (intersperse)
import Data.Maybe (maybe)
import Data.Monoid (Any(..), Sum(..))
import Prelude hiding (Foldable(..), Applicative(..), any, concat)
import Test.Hspec
import Test.Hspec.QuickCheck
import qualified Test.QuickCheck as QC
import Test.QuickCheck hiding (NonEmpty, elements)
import Test.QuickCheck.Arbitrary.Generic

import Foldables

spec :: Spec
spec = do
  describe "problem 1" $ do
    prop "foldMap meets the specification" $
      conjoin
        [ nonEmptyFoldMapSpec @Int @()
        , nonEmptyFoldMapSpec @Bool @Any
        , nonEmptyFoldMapSpec @String @(Any,String)
        ]

  describe "problem 2" $ do
    prop "length meets the specification" $
      conjoin
        [ lengthSpec @[] @()
        , lengthSpec @Maybe @Int
        , lengthSpec @(Fun Bool) @String
        ]

    prop "findCount meets the specification" $
      conjoin
        [ findCountSpec @[] @()
        , findCountSpec @Maybe @Int
        , findCountSpec @(Fun Bool) @String
        ]

  describe "problem 3" $ do
    prop "finite function foldMap meets the specification" $
      conjoin
        [ finiteFunctionFoldMapSpec @Bool @Int
        , finiteFunctionFoldMapSpec @Coordinate @String
        , finiteFunctionFoldMapSpec @Index @(Any,String)
        ]

  describe "problem 4" $ do
    prop "won function meets the specification" $
      conjoin
        [ wonSpec @Cell
        , wonSpec @Int
        , wonSpec @String
        ]

instance Function Coordinate
instance CoArbitrary Coordinate

instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = genericArbitrary

instance Arbitrary a => Arbitrary (Board a) where
  arbitrary = genericArbitrary

instance Arbitrary Cell where
  arbitrary = genericArbitrary

instance Finite a => Foldable (Fun a) where
  foldMap f = foldMap f . applyFun

extensionally ::
  forall a b.
  ( Show a, Show b, Eq b
  , Arbitrary a
  ) =>
  (b -> b -> Property) ->
  (a -> b) -> (a -> b) -> Property
extensionally p f g = forAll arbitrary $ \x -> p (f x) (g x)

extensionally2 ::
  forall a b c.
  ( Show a, Show b, Eq c
  , Arbitrary a, Arbitrary b
  ) =>
  (c -> c -> Property) ->
  (a -> b -> c) -> (a -> b -> c) -> Property
extensionally2 p f g = forAll arbitrary $ \x y -> p (f x y) (g x y)

nonEmptyFoldMapSpec ::
  forall a b.
  ( Show a, Show b, Eq b
  , Monoid b
  , Function a
  , CoArbitrary a
  , Arbitrary a, Arbitrary b
  ) =>
  Property
nonEmptyFoldMapSpec =
  extensionally2 @(Fun a b) (===)
    (\(Fun _ f) -> foldMap f)
    (\(Fun _ f) -> foldMap f . NonEmpty.toList)

length' :: (Foldable f, Num b) => f a -> b
length' = listLength . toList
  where
    listLength :: Num b => [a] -> b
    listLength [] = 0
    listLength (x:xs) = 1 + listLength xs

lengthSpec ::
  forall f a.
  (Foldable f, Show (f a), Arbitrary (f a)) =>
  Property
lengthSpec = extensionally @(f a) (===) length length'

findCount' :: (Foldable f, Num b) => (a -> Bool) -> f a -> b
findCount' p = listFindCount p . toList
  where
    listFindCount :: Num b => (a -> Bool) -> [a] -> b
    listFindCount p [] = 0
    listFindCount p (x:xs) = listFindCount p xs + if p x then 1 else 0

findCountSpec ::
  forall f a.
  ( Foldable f, Show a, Show (f a), Arbitrary (f a)
  , Function a, CoArbitrary a
  ) =>
  Property
findCountSpec =
  extensionally2 @(Fun a Bool) @(f a) (===)
    (\(Fun _ p) -> findCount p)
    (\(Fun _ p) -> findCount' p)

finiteFunctionToList :: Finite a => (a -> b) -> [b]
finiteFunctionToList f = fmap f elements

finiteFunctionFoldMapSpec ::
  forall a b.
  ( Show a, Show b, Eq b
  , Finite a
  , Function a
  , CoArbitrary a
  , Arbitrary b
  ) =>
  Property
finiteFunctionFoldMapSpec =
  extensionally @(Fun a b) (===)
    (\(Fun _ f) -> foldMap (\x -> [x]) f)
    (\(Fun _ f) -> finiteFunctionToList f)

won' :: Eq a => a -> Board a -> Bool
won' x (Board b) =
  any (all (x ==))
    [ [b (C0,C0), b (C0,C1), b (C0,C2)]
    , [b (C1,C0), b (C1,C1), b (C1,C2)]
    , [b (C2,C0), b (C2,C1), b (C2,C2)]
    , [b (C0,C0), b (C1,C0), b (C2,C0)]
    , [b (C0,C1), b (C1,C1), b (C2,C1)]
    , [b (C0,C2), b (C1,C2), b (C2,C2)]
    , [b (C0,C0), b (C1,C1), b (C2,C2)]
    , [b (C0,C2), b (C1,C1), b (C2,C0)]
    ]

wonSpec ::
  forall a.
  ( Show a, Eq a
  , Arbitrary a
  ) =>
  Property
wonSpec = extensionally2 @a (===) won won'
  
