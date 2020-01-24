{-# LANGUAGE NoImplicitPrelude #-}

module SemigroupSpec (spec) where

import Prelude hiding (Semigroup(..))
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic

import Semigroups

instance Arbitrary Any where
  arbitrary = Any <$> arbitrary

instance Arbitrary All where
  arbitrary = All <$> arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (These a b) where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary a => Arbitrary (BinTree a) where
  arbitrary = scale (floor . sqrt . fromIntegral) $ sized go
    where
      go 0 = Leaf <$> arbitrary
      go n = oneof [Node <$> go (n-1) <*> go (n-1), go (n-1)]

  shrink = genericShrink

associativity ::
  forall a.
  (Show a, Eq a, Semigroup a, Arbitrary a) =>
  Property
associativity =
  forAll arbitrary $ \(x::a) y z ->
    x <> (y <> z) == (x <> y) <> z

extensionalAssociativity ::
  forall a b.
  ( Show a, Show b
  , Eq b
  , Semigroup b
  , Function a
  , CoArbitrary a
  , Arbitrary a, Arbitrary b
  ) =>
  Property
extensionalAssociativity =
  forAll arbitrary $ \(Fun _ f :: Fun a b) (Fun _ g) (Fun _ h) x ->
    (f <> (g <> h)) x == ((f <> g) <> h) x

indexwiseInequality ::
  forall a.
  (Show a, Eq a, Arbitrary a) =>
  Property
indexwiseInequality =
  forAll arbitrary $ \(t1 :: BinTree a) t2 ->
    forAll (choose (0, max (leafCount t1) (leafCount t2) - 1)) $ \i ->
      leafAt i t1 /= leafAt i t2 ==> t1 /= t2

distributivity ::
  forall a.
  (Show a, Eq a, Arbitrary a, Semigroup a) =>
  Property
distributivity =
  forAll arbitrary $ \(t1 :: BinTree a) t2 ->
    sconcat (t1 <> t2) == sconcat t1 <> sconcat t2

spec :: Spec
spec = do
  describe "problem 1" $ do
    prop "Semigroup <> for \"Maybe\" is associative" $
      conjoin
        [ associativity @(Maybe ())
        , associativity @(Maybe String)
        , associativity @(Maybe (String, All))
        ]

    prop "Semigroup <> for \"These\" is associative" $
      conjoin
        [ associativity @(These String Any)
        , associativity @(These ([String], All) ())
        , associativity @(These () ([String], All))
        ]

    prop "Semigroup <> for \"a -> b\" is associative" $
      conjoin
        [ extensionalAssociativity @Int @String
        , extensionalAssociativity @String @Any
        ]

  describe "problem 2" $ do
    prop "Semigroup <> for \"BinTree\" is associative" $
      conjoin
        [ associativity @(BinTree ())
        , associativity @(BinTree String)
        , associativity @(BinTree (String, Int))
        ]

    prop "Eq == for \"BinTree\" respects index-wise inequality" $
      conjoin
        [ indexwiseInequality @String
        , indexwiseInequality @(String, Int)
        , indexwiseInequality @[Either [String] [Int]]
        ]

  describe "problem 3" $ do
    prop "sconcat distributes over <>" $
      conjoin
        [ distributivity @(BinTree ())
        , distributivity @(BinTree String)
        , distributivity @(BinTree (String, Int))
        ]
