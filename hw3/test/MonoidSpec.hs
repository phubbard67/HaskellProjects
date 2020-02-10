{-# LANGUAGE NoImplicitPrelude #-}

module MonoidSpec (spec) where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Foldable
import Data.List.NonEmpty (NonEmpty)
import Data.Semigroup (Semigroup, (<>), All(..), Any(..), Sum(..), sconcat)
import Prelude hiding (Monoid(..))
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (Result, NonEmpty)
import Test.QuickCheck.Arbitrary.Generic

import Monoids

spec :: Spec
spec = do
  describe "problem 1" $ do
    prop "mempty for Same is left unit to (<>)" $ leftUnit @Same
    prop "mempty for Same is right unit to (<>)" $ rightUnit @Same

  describe "problem 2" $ do
    prop "mempty for a -> b is left unit to (<>)" $
      conjoin
        [ extensionalLeftUnit @Int @Any
        , extensionalLeftUnit @Int @String
        , extensionalLeftUnit @Int @[(Any,All)]
        ]

    prop "mempty for a -> b is right unit to (<>)" $
      conjoin
        [ extensionalRightUnit @Int @Any
        , extensionalRightUnit @Int @String
        , extensionalRightUnit @Int @[(Any,All)]
        ]

  describe "problem 3" $ do
    prop "(<>) for Result is associative" $
      conjoin
        [ associativity @(Result Any All)
        , associativity @(Result Any (Any,All))
        , associativity @(Result (NonEmpty Int) [String])
        ]

    prop "mempty for Result is left unit to (<>)" $
      conjoin
        [ leftUnit @(Result Any All)
        , leftUnit @(Result Any (Any,All))
        , leftUnit @(Result (NonEmpty Int) [String])
        ]

    prop "mempty for Result is right unit to (<>)" $
      conjoin
        [ rightUnit @(Result Any All)
        , rightUnit @(Result Any (Any,All))
        , rightUnit @(Result (NonEmpty Int) [String])
        ]

    prop "mconcat for Result is correct with all Ok values" $
      conjoin
        [ successResultSpec @[String] @All
        , successResultSpec @(NonEmpty String) @(Any,All)
        , successResultSpec @(Maybe String) @[(Any,All)]
        ]

    prop "mconcat for Result is correct with any Error values" $
      conjoin
        [ successResultSpec @[String] @All
        , successResultSpec @(NonEmpty String) @(Any,All)
        , successResultSpec @(Maybe String) @[(Any,All)]
        ]

  describe "problem 4" $ do
    prop "train meets the specification" trainSpec


instance Arbitrary Same where
  arbitrary = genericArbitrary

instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = genericArbitrary

instance (Arbitrary e, Arbitrary a) => Arbitrary (Result e a) where
  arbitrary = genericArbitrary

associativity ::
  forall a.
  (Show a, Eq a, Semigroup a, Arbitrary a) =>
  Property
associativity =
  forAll arbitrary $ \(x::a) y z ->
    x <> (y <> z) == (x <> y) <> z

leftUnit :: forall a. (Show a, Eq a, Monoid a, Arbitrary a) => Property
leftUnit = forAll arbitrary $ \(x::a) -> mempty <> x == x

rightUnit :: forall a. (Show a, Eq a, Monoid a, Arbitrary a) => Property
rightUnit = forAll arbitrary $ \(x::a) -> x <> mempty == x

extensionalLeftUnit ::
  forall a b.
  ( Show a, Show b, Eq b
  , Monoid b
  , Function a
  , CoArbitrary a
  , Arbitrary a, Arbitrary b
  ) =>
  Property
extensionalLeftUnit =
  forAll arbitrary $ \(Fun _ f :: Fun a b) x ->
    (mempty <> f) x == f x

extensionalRightUnit ::
  forall a b.
  ( Show a, Show b, Eq b
  , Monoid b
  , Function a
  , CoArbitrary a
  , Arbitrary a, Arbitrary b
  ) =>
  Property
extensionalRightUnit =
  forAll arbitrary $ \(Fun _ f :: Fun a b) x ->
    (f <> mempty) x == f x

successResultSpec ::
  forall e a.
  (Show a, Eq a, Monoid a, Arbitrary a, Eq e, Semigroup e) =>
  Property
successResultSpec =
  forAll arbitrary $ \(as :: [a]) ->
    mconcat (map Ok as) == Ok @e (mconcat as)

errorResultSpec ::
  forall e a.
  ( Show e, Show a
  , Eq e, Eq a
  , Semigroup e, Monoid a
  , Arbitrary e, Arbitrary a
  ) =>
  Property
errorResultSpec =
  forAll arbitrary $ \(as :: [a]) (es :: NonEmpty a) ->
    forAll (shuffle (map Ok as ++ map Error (toList es))) $ \rs ->
      mconcat rs == Error (sconcat es)

trainSpec :: Property
trainSpec =
  forAll arbitrary $ \(ws :: [String]) w1 w2 ->
    (Map.lookup w2 =<< Map.lookup w1 (fromFreqMap (train (unwords ws)))) ==
      let count = length (filter ((w1,w2) ==) (bigrams ws)) in
        if count > 0 then Just count else Nothing
