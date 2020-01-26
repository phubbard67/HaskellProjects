;> {-# LANGUAGE NoImplicitPrelude #-}

> module Semigroups where

This assignment has you work with the Semigroup typeclass.

The Haskell community has a general preference for borrowing names from
abstract mathematics, which is often intimidating to beginners: if the names
"Semigroup" doesn't mean anything to you yet, don't worry! Try not to be scared
of the new terminology: most of this material is much simpler than it appears
at first.

The first thing to keep in mind is that Semigroup, as a typeclass, is like an
adjective that applies to types. When we say "type X is a Semigroup", it should
be interpreted as "it is possible to implement a lawful instance of the
Semigroup typeclass for type X". (We'll review what it means for an instance to
be "lawful" further below.) 

Here's the intuition from a programming perspective:

  A type X is a semigroup if we can "concatenate" a non-empty list of
  elements of type X into a single element by divide-and-conquer recursion.

In the naming tradition of OOP interfaces, Semigroup might be called something
like "Combinable".

The Semigroup typeclass is part of the Prelude, but we'll define it manually
in this file, so this import hides the default definition.

> import Prelude hiding (Semigroup(..))

This import is just for magic test generation stuff, like last time.

> import GHC.Generics

Here's the definition of the Semigroup class. The "infixr 6" line says that
the <> operator is right-associative with precedence 6.

> class Semigroup a where
>   -- associativity: a <> (b <> c) == (a <> b) <> c
>   infixr 6 <>
>   (<>) :: a -> a -> a

You can get information about the Semigroup typeclass in GHCi with the command
":i Semigroup".

The "associativity" comment is a *law*: it says that an instance of Semigroup
is only valid if the given equation holds for all choices of a/b/c. Haskell's
type system isn't strong enough to allow us to statically require that laws
hold, so we have to check them manually with tests and/or pen-and-paper proofs.

(There are languages with type systems that can enforce laws: see Agda, Idris,
Lean, Coq, ATS, ..., plus the LiquidHaskell compiler plugin.)

Notice that the law uses the == operator, which is from the Eq typeclass.
Equality in typeclass laws is actually a little more subtle than this; when a
type has an Eq instance we expect the law to be true according to the ==
operator, but as we'll see below, there are types that have instances of
Semigroup but not Eq.

In general, typeclass laws are expected to hold up to some well-defined
type-specific notion of equality: the two sides may not be *identical*
expressions, but they should represent the "same" value in some way.
In particular, equality over values of function type is handled in a special
way when checking laws, which comes up in problem 1c and is discussed there.


Let's start with the most boring semigroup: the unit type. Recall that the
syntax () at the type level denotes the unit type, and the same syntax at the
term level denotes the unit term:

> unitExample :: ()
> unitExample = ()

There's only one value of the unit type, so there's only one possible
implementation for <>.

> instance Semigroup () where
>   a <> b = ()

We can easily check the associative law with case-by-case equational reasoning:
there's only one possible case.
  
    () <> (() <> ())
  = () <> ()
  = (() <> ()) <> ()


What about Bool? There's more than one lawful choice for the <> operator:
the AND (&&) and OR (||) functions are both asssociative.

(Mathematically, the tuples <Bool,(&&)> and <Bool,(||)> are both semigroups.)

The traditional Haskell way to resolve this issue is to declare two new types
that "wrap" the Bool type, so that we can define two different Semigroup
instances.

The "newtype" keyword is equivalent to the "data" keyword except that it can
only be used for data types with exactly one constructor, which must have
exactly one argument. It's designed for these "wrapper" types: newtypes are
logically identical to types defined with "data", but there is no runtime
overhead to using a newtype versus the "wrapped" type, while types defined with
"data" generally introduce a level of boxing.

> -- booleans with AND
> newtype All = All { unAll :: Bool }
>   deriving (Show, Eq)

Recall that record syntax in a datatype declaration creates both a constructor
and a field extraction function: check the types of the "All" and "unAll"
functions in GHCi.

> instance Semigroup All where
>   All a <> All b = All (a && b)

> -- booleans with OR
> newtype Any = Any { unAny :: Bool }
>   deriving (Show, Eq)

> instance Semigroup Any where
>   Any a <> Any b = Any (a || b)

Note that Haskell will not catch if we mix up the Semigroup definitions for
"All" and "Any". The intended meaning of the wrapper types is given in
comments, and the computational meaning is given by the instance definitions
for the type; the programmer has to make sure these line up.

This isn't very exciting yet, but now we can use the <> operator in slightly
nontrivial expressions: try these out in GHCi!

  Any True <> Any False
  unAny (Any True <> Any False)
  All True <> All False
  unAll (All True <> All False)
  Any True <> All False -- think about why this fails

A pair type is a semigroup when both of its element types are semigroups.
Recall that the double arrow (=>) specifies a *constraint* on this instance:
this declares an instance of Semigroup (a,b) for exactly all pairs of types
a/b where each type has a Semigroup instance.

> instance (Semigroup a, Semigroup b) => Semigroup (a,b) where
>   (a1,b1) <> (a2,b2) = (a1<>a2, b1<>b2)

A list type of any element type is a semigroup, where <> is list append.
Recall that this says there is an instance of Semigroup for the type "[a]"
for any type "a".

> instance Semigroup [a] where
>   (<>) = (++)


*************
* PROBLEM 1 *  3 points
*************

Give a lawful definition for <> in each of the following Semigroup instances.
You may modify the existing definition code to add arguments and cases.
Recall that the associativity law depends on the Eq instance for a type!

a.

> instance Semigroup a => Semigroup (Maybe a) where
>  Just x <> Just y = Just(x <> y)
>  Nothing <> Nothing = Nothing
>  a <> Nothing = a
>  Nothing <> a = a


b.

> data These a b = This a | That b | These a b
>   deriving (Show, Eq, Generic)

  This is the instance that "deriving Eq" generates:

  instance (Eq a, Eq b) => Eq (These a b) where
    This a1 == This a2 = a1 == a2
    That b1 == That b2 = b1 == b2
    These a1 b1 == These a2 b2 = a1 == a2 && b1 == b2
    _ == _ = False

> instance (Semigroup a, Semigroup b) => Semigroup (These a b) where
>   This a1 <> This a2 = This(a1 <> a2)
>   That b1 <> That b2 = That(b1 <> b2)
>   This a1 <> These a2 b1 = These(a1 <> a2) b1
>   That b1 <> These a1 b2 = These a1 (b1 <> b2)
>   That a1 <> This b1 = These b1 a1
>   This b1 <> That a1 = These b1 a1
>   These a1 b1 <> That b2 = These a1 (b1 <> b2) 
>   These a1 b1 <> This a2 = These (a1 <> a2) b1
>   These a1 b1 <> These a2 b2 = These (a1 <> a2) (b1 <> b2)



c.

We can't actually define an Eq instance for function types, so this instance
should satisfy the associativity law when "f == g" over functions f/g means
"for any valid input x, f x == g x".

> 
> instance Semigroup b => Semigroup (a -> b) where
>   f <> g = \x -> f x <> g x
>   



*****************
* END PROBLEM 1 *
*****************


Consider the following type, a binary tree type with data only at the leaves.

> data BinTree a = Leaf a | Node (BinTree a) (BinTree a)
>   deriving Generic -- just for testing magic

Note that a value of type "BinTree a" always contains at least one value of
type "a".

The showTree function can be used to show BinTrees in a more visual format.

> showTree :: Show a => Int -> BinTree a -> String
> showTree indent (Leaf a) = replicate indent ' ' ++ show a
> showTree indent (Node l r) =
>   unlines
>     [ replicate indent ' ' ++ "."
>     , showTree (2+indent) l
>     , showTree (2+indent) r
>     ]

> instance Show a => Show (BinTree a) where
>   show = showTree 0

The leafCount function returns the number of leaves in a BinTree.

> leafCount :: BinTree a -> Int
> leafCount (Leaf x) = 1
> leafCount (Node l r) = leafCount l + leafCount r

The leafAt function retrieves the value of the leaf at the given index,
counting the leaves from left to right.

> leafAt :: Int -> BinTree a -> Maybe a
> leafAt 0 (Leaf x) = Just x
> leafAt _ (Leaf x) = Nothing
> leafAt i (Node l r)
>   | i < lCount = leafAt i l
>   | otherwise = leafAt (i - lCount) r
>   where lCount = leafCount l

The leaves function returns a (nonempty) list of the leaves in a BinTree
ordered from left to right.

> leaves :: BinTree a -> [a]
> leaves (Leaf x) = [x]
> leaves (Node l r) = leaves l ++ leaves r


*************
* PROBLEM 2 *  2 points
*************

This instance of Semigroup for BinTree passes the Haskell typechecker.

> instance Semigroup (BinTree a) where
>   (<>) = Node

It is not necessarily a valid instance, however: recall that the associativity
law depends on the implementation of the Eq typeclass.

Give a definition for == in the following Eq instance that, together with the
Semigroup instance given above, obeys both of these laws:

  - the Semigroup associativity law
  - index-wise inequality:
      for any trees t1/t2,
      for any integer i,
        leafAt i t1 /= leafAt i t2 implies t1 /= t2

You may modify the existing definition code for == to add parameters and cases.
The definition of /= is automatically generated from your definition of ==.
Do not modify the Semigroup instance for BinTree above.
(Also, don't worry about efficiency.)

> instance Eq a => Eq (BinTree a) where
>  Leaf x == Leaf y = x == y
>  Leaf x == Node r y = False
>  Node r y == Leaf x = False
>  Node l x == Node r y = l == r && x == y

*****************
* END PROBLEM 2 *
*****************


We can write definitions that are *parametric* over the choice of semigroup,
meaning they work for any type with a Semigroup instance.

Here's a straightforward example: given three elements of the same semigroup,
we can use <> to concatenate them all together.

Recall that the => operator in a type is a *constraint*: this function takes in
three inputs of the same arbitrary type "a" and returns an output of type "a",
where "a" must have a Semigroup instance.

> sconcat3 :: Semigroup a => a -> a -> a -> a
> sconcat3 a b c = a <> b <> c

(The name "sconcat" is short for "semigroup concatenate".)

What if we try to concatenate a list of elements of a semigroup?

  sconcat :: Semigroup a => [a] -> a
  sconcat [] = error "no default value!"
  sconcat (x:xs) = x <> sconcat xs

We don't know anything about "a" other than that it's a semigroup, and the
Semigroup typeclass doesn't give us any way to produce an element of "a" out of
thin air. We only know how to concatenate *non-empty* semigroup collections!

Fortunately, we already have a non-empty collection type: BinTree.


*************
* PROBLEM 3 *  2 points
*************

Replace the sconcat definition below with a definition that obeys this
*distributivity* law:

  sconcat (t1 <> t2) == sconcat t1 <> sconcat t2

You may modify the existing definition code for sconcat to add parameters and
cases.

> sconcat :: Semigroup a => BinTree a -> a
> sconcat (Node x r) = sconcat x <> sconcat r
> sconcat (Leaf x) = x

*****************
* END PROBLEM 3 *
*****************


The balancedTree function creates a balanced BinTree out of an input list,
where the leaves in the tree are the elements in the list from left to right
and each node in the tree has subtrees whose lengths differ by at most 1.

We can use this to balance an existing tree, by getting the list of its leaves
and then creating a balanced tree from that list.

> balance :: BinTree a -> BinTree a
> balance = balancedTree . leaves
>   where
>     balancedTree :: [a] -> BinTree a
>     balancedTree [] = error "impossible case"
>     balancedTree [x] = Leaf x
>     balancedTree xs =
>       let
>         midpoint = length xs `div` 2
>         (xs1, xs2) = (take midpoint xs, drop midpoint xs)
>       in
>         Node (balancedTree xs1) (balancedTree xs2)

Note that since we know "leaves" always returns a nonempty list, we know
"balancedTree" won't ever fail during a call to "balance". Since "balancedTree"
is only in scope within the definition of "balance", this is fine; the
"impossible case" error is meant to signify that the programmer can verify on
paper that the case is impossible even though the Haskell typechecker can't
tell that.

(If we wanted to implement a version of "balancedTree" that's safe to call with
any well-typed input, we could have it return a Maybe value, or we could change
the input to be a value of the nonempty list type defined in the base
Data.NonEmpty library.)


*************
* PROBLEM 4 *  3 points
*************

Does the following law hold? Justify your answer.

  for any arbitrary lawful semigroup "a",
  for any arbitrary terminating value "t :: BinTree a",
    sconcat t == sconcat (balance t)

Don't just give the name of another law - explain why it's relevant!

If you want to reference an image, submit the image along with this file on D2L
in a standard format and reference it by filename.

Write your answer in the space below this text.


*****************
* END PROBLEM 4 *
*****************
