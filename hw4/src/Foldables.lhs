> {-# LANGUAGE NoImplicitPrelude #-}

This assignment has you work with the Foldable, Functor, and Applicative
typeclasses, along with the Semigroup and Monoid typeclasses from previous
assignments. The main goal is to get hands-on experience with Haskell's
standard higher-kinded typeclasses and to demonstrate how they can help us
avoid a lot of boilerplate code when defining standard functions for custom
"container types".

There are many different sensible formal definitions of the word "container"
when describing parametric types; we would generally expect types like List and
Maybe to be containers in any interpretation, while e.g. certain function types
can be seen as "containers" in some senses and not in others.

Foldable, Functor, and Applicative are all very general typeclasses that have
many instances that might be surprising from a programming perspective, but
we'll be using them in this assignment specifically to classify different
notions of container types. At an intuitive level, if we have a parametric
container type "F :: * -> *":

  - F is foldable if we can "concatenate" a container of type "F a" to a single
    "a" whenever "a" is a monoid; equivalently, F is a foldable if we can
    convert a container of type "F a" to a list of "a" elements that contains
    all of the elements from the container. (The Foldable typeclass and the
    Listable typeclass from lecture are logically equivalent, but the Foldable
    formulation of the concept usually has better performance characteristics.)
  - F is a functor if we can "map" a *single function* over all elements of
    a container of type "F a", obtaining a new container as output.
  - F is an applicative if we can create a "singleton" container given one
    element and we can "zip" multiple containers together with
    multiple-argument functions over elements.

To be clear, not all Functor/Foldable/Applicative instances follow these
intuitions, but they do apply to instances for most types that we would think
of as "container types".

Most of the super common container types (List, Maybe, etc.) implement all
three of Functor/Foldable/Applicative, but some interesting types are
containers in one sense but not another. By definition every Applicative is
also a Functor, so the "feature matrix" that we get from combining these three
typeclasses doesn't include all possible combinations: in total, we get six
sensible combinations of typeclasses, each classifying a different subset of
the functionality we sometimes associate with "containers".

  - no instances - can't concatenate, map, or zip
  - Foldable T - can concatenate but not map or zip
  - Functor T - can map but not concatenate or zip
  - (Foldable T, Functor T) - can map and concatenate but not zip
  - (Functor T, Applicative T) - can map and zip but not concatenate
  - (Foldable T, Functor T, Applicative T) - can map, zip, and concatenate

There are benefits and drawbacks to having this fine-grained hierarchy instead
of a single "Container" typeclass that captures all of this functionality. In
recent versions of GHC, the ConstraintKinds extension allows us to write a type
synonym for a constraint, so we could define this type and use it seamlessly: a
type signature like "foo :: Container f => ..." means that the "f" variable has
instances for all three of these typeclasses.

  type Container f = (Foldable f, Functor f, Applicative f)

But while we will generally be using concrete types that are instances of
this Container type, many functions we write will only require concatenation
*or* mapping, so these functions can be written with more lenient constraints.
This is in keeping with the "principle of least privilege", and in the spirit
of encapsulation via interfaces in object-oriented programming: the less a
function assumes about the concrete types of its input, the harder it is to
write a type-correct but logically incorrect implementation of the function.

Also, there are many more operations we might associate with a container -
should it support removal of elements, sorting, random access by index, ...?
Each additional operation adds functionality to the interface but restricts the
set of types it applies to. This is a problem for a library author trying to
provide a meaningful abstraction that will be valuable in a useful variety of
client code! Different languages and paradigms propose different solutions and
workarounds for this problem; the standard solution in the Haskell Prelude is
to define many small typeclasses that each capture a single concept, letting
the user combine them when needed.

> module Foldables where

We'll be defining simplified versions of the Foldable and Applicative
typeclasses in this file. The Prelude versions have the same operations with
the same types as the ones given in here, but they also allow instances to
override some derived functions with more optimized implementations.

> import Prelude hiding (Foldable(..), Applicative(..), any, concat)

We'll be using some types from the "containers" package, as in the last
assignment. We'll also import the "union map" type instead of defining our own:
the AppendMap type from the "appendmap" package provides the same functionality
as the Union type from the last assignment.

> import Data.Coerce (coerce)
> import qualified Data.Map as Map (foldMapWithKey, singleton)
> import Data.Map (Map)
> import Data.Map.Append (AppendMap(..))
> import Data.List.NonEmpty (NonEmpty(..))
> import Data.List (intersperse)
> import Data.Maybe (maybe)
> import Data.Monoid (Any(..), Sum(..))
> import GHC.Generics (Generic)

Foldable doesn't actually have any laws of its own, which is uncommon for
higher-kinded typeclasses. (There's a Prelude class called Traversable that
generalizes Foldable and does have sensible laws, but we won't cover it here.)

> class Foldable (f :: * -> *) where
>   foldMap :: forall a b. Monoid b => (a -> b) -> f a -> b

Intuitively, the foldMap function "reduces" a container of values by
transforming all of the values into some monoid type and then "concatenating"
all of the values together.

There is one law that relates Foldable and Functor, for any type that has
instances for both:

  foldMap (g . f) == foldMap g . fmap f

This is similar to the functor composition law: intuitively, it says that the
"map" part of the "foldMap" operation must agree with the "fmap" operation from
the Functor instance.

The prototypical foldable type is the list type. Note that "fmap" and "map"
mean the same thing when applied to lists; you can use whichever you want when
working with lists, but the given code in this file will use "fmap" to
emphasize that we're working with the list type as a functor.

> instance Foldable [] where
>   foldMap :: Monoid b => (a -> b) -> [a] -> b
>   foldMap f = mconcat . fmap f

One standard function that we can write over an arbitrary Foldable is the
function that converts a container to a list: we convert each element to a
singleton list, and then concatenate all the lists together.

> toList :: Foldable f => f a -> [a]
> toList = foldMap (\x -> [x])

As mentioned in the introduction, Foldable is actually logically equivalent to
the Listable type from the lecture slides on higher-kinded types, so foldMap
can be seen as having equivalent "power" as toList: we can define either one in
terms of the other.

  foldMap :: (Listable f, Monoid b) => (a -> b) -> f a -> b
  foldMap f = mconcat . fmap f . toList

This implementation of foldMap is often less than optimally efficient, though -
even if all of the space overhead of converting to a list is optimized away, it
still commits to processing elements linearly (one at a time), while some
Foldable instances may have implementations of foldMap in sub-linear time.

The Foldable instance for Maybe relies on the intuition that a value of type
"Maybe a" contains either zero or one values of type "a". If there are zero
values, we return mempty (from the Monoid instance for "b"); if there's one
value, we just apply our transforming function to that value.

This is a very terse definition, but it illustrates that the "maybe" function
acts as a sort of "folding" operation for the Maybe type. Check the type of
"maybe" in GHCi and test it out on some sample inputs if you're not clear on
how it works - we haven't introduced it in class, but by this point I think
you're up to the challenge of deciphering this code!

> instance Foldable Maybe where
>   foldMap :: Monoid b => (a -> b) -> Maybe a -> b
>   foldMap = maybe mempty


*************
* PROBLEM 1 *
*************

Implement a Foldable instance for the non-empty list type from Data.NonEmpty.
Do not use any additional top-level definitions that you add to this file.
You may add arguments and pattern-matching cases to the definition of foldMap.

The Data.List.NonEmpty module exports a toList function - it's not imported
in this file, so you can't use it in your answer, but we will refer to it in
order to define the intended behavior of this Foldable instance. Here is its
definition:
  
  toList :: NonEmpty a -> [a]
  toList (x :| xs) = x : xs

Your implementation of foldMap should obey this law:

  foldMap f == foldMap f . toList

Note that the left "foldMap" in the above equation is from the NonEmpty
instance, and the right "foldMap" is from the list type ("[]") instance.

> instance Foldable NonEmpty where
>   foldMap :: Monoid b => (a -> b) -> NonEmpty a -> b
>   foldMap = undefined

*****************
* END PROBLEM 1 *
*****************


One more Foldable instance that will be useful later in this assignment is the
instance for the Map type from the "containers" package (also used in the last
assignment). The Data.Map module actually has a Foldable implementation
already, but since we avoided importing the Foldable type from the Prelude and
defined it in this module instead, we have to give this instance explicitly.
The implementation just calls the standard foldMap function for the Map type.

> instance Foldable (Map k) where
>   foldMap :: Monoid b => (a -> b) -> Map k a -> b
>   foldMap f = Map.foldMapWithKey (\_ x -> f x)


There are many useful functions we can define over an arbitrary Foldable type.
Since Foldable is equivalent to Listable, this is effectively the same set of
functions that we can define by converting a container to a list and doing some
operation over the list, but foldMap lets us give more direct definitions.

> sum :: (Foldable f, Num a) => f a -> a
> sum = getSum . foldMap Sum

> any :: Foldable f => (a -> Bool) -> f a -> Bool
> any f = getAny . foldMap (Any . f)

> elem :: (Foldable f, Eq a) => a -> f a -> Bool
> elem x = any (x ==)

We also get a generalized version of "mconcat" that works over any Foldable.

> fold :: (Foldable f, Monoid a) => f a -> a
> fold = foldMap id

An interesting special case of the "fold" function is the list concatenation
function: a container of lists can be reduced to a single list, using the
Monoid instance for the list type.

> concat :: Foldable f => f [a] -> [a] -- concat @[] :: [[a]] -> [a]
> concat = fold

Note that we can't have a function with type "Foldable f => [f a] -> f [a]",
because the Foldable interface doesn't give us the ability to combine multiple
containers together into a single container (or to create a new container).
It can be a good exercise to try to write this function and see why you can't!


*************
* PROBLEM 2 *
*************

Give definitions for the following functions over an arbitrary Foldable input.
Do not use any additional top-level definitions that you add to this file, and
don't use the "toList" function defined above - instead of converting the
collection to a list, you should use foldMap along with an appropriate monoid
type to implement each operation.

The operations are specified in terms of toList, and your implementations
should give the same result as these specifications on all inputs.
You may modify the given definitions to add new named arguments.


a.

Specification:

  lengthSpec :: (Foldable f, Num b) => f a -> b
  lengthSpec = listLength . toList
    where
      listLength :: Num b => [a] -> b
      listLength [] = 0
      listLength (x:xs) = 1 + listLength xs

Problem:

> length :: (Foldable f, Num b) => f a -> b
> length = undefined


b.

Specification:

  findCountSpec :: (Foldable f, Num b) => (a -> Bool) -> f a -> b
  findCountSpec p = listFindCount p . toList
    where
      listFindCount :: Num b => (a -> Bool) -> [a] -> b
      listFindCount p [] = 0
      listFindCount p (x:xs) = listFindCount p xs + if p x then 1 else 0

Problem:

> findCount :: (Foldable f, Num b) => (a -> Bool) -> f a -> b
> findCount = undefined

*****************
* END PROBLEM 2 *
*****************


One last Foldable function that will be useful later is this elemCounts
function, which returns a Map indicating how many occurrences of each value are
in the container. The Semigroup and Monoid instances for AppendMap are the same
as for the Union type from the last assignment: the (<>) operator over
AppendMap values takes the union of two maps and joins the values of any
duplicate keys with the (<>) operator from the Semigroup instance for the value
type. The coerce function is used here with the following type:

  coerce :: AppendMap a (Sum Int) -> Map a Int

> elemCounts :: (Foldable f, Ord a) => f a -> Map a Int
> elemCounts = coerce . foldMap singleton
>   where
>     singleton :: a -> AppendMap a (Sum Int)
>     singleton x = AppendMap (Map.singleton x (Sum 1))


We'll take a quick detour here to talk about finite types. The motivation is
that function types with a finite input domain can be seen as containers: for
example, a function "f :: Bool -> A" can be seen as a pair of values of type
"A", specifically "f True :: A" and "f False :: A". We saw this in the first
assignment, where we defined a tic-tac-toe board as a function from the
nine-element Index type to the Cell type; we'll revisit this example later in
this assignment.

There are many equivalent ways to encode "finiteness". This typeclass says that
a type is finite if we can create a list that contains every element of the
type at some determinable index; the "countable" package provides this type
with the same name in the Data.Countable module.

This definition carries a lot of theoretical weight, but keep in mind that
programmatically it just says that a Finite type is one where we have a finite
list containing all of the elements of the type.

> class Finite (a :: *) where
>   -- exists n. length elements == n
>   -- forall x. elem x elements == True
>   elements :: [a]

The first law is just saying that "elements" must be a finite list; we haven't
talked much about infinite lists yet, but they exist in Haskell, so we have to
specify that this is not an infinite list. The second law says that every value
of type "a" can be found somewhere in the list.

As mentioned above, Bool is a finite type: it has exactly two values. It's easy
to see that this definition satisfies the Finite laws.

> instance Finite Bool where
>   elements = [True, False]

The type "Either a b" is finite when both "a" and "b" are finite.

> instance (Finite a, Finite b) => Finite (Either a b) where
>   elements = fmap Left elements ++ fmap Right elements

Similarly, the type "(a,b)" is finite when both "a" and "b" are finite.

> instance (Finite a, Finite b) => Finite (a, b) where
>   elements = allPairs elements elements


*************
* PROBLEM 3 *
*************

Give an instance of Foldable for function types with finite input domains.
Do not use any additional top-level definitions that you add to this file.

As in problem 2, the specification is given in terms of a "toList" function
that you should not use in your answer. Here's the toList function for function
types with finite input domains.

  toList :: Finite a => (a -> b) -> [b]
  toList f = fmap f elements

  -- for example
  toList @Bool f == [f True, f False]

Specification:
  
  foldMap (\x -> [x]) f == toList f

Remember that the syntax "(->) a b" is equivalent to "a -> b", so this
instance is saying that the type "a -> b" can be seen as a foldable container
of values of type "b" whenever "a" is a finite type.

Problem:

> instance Finite a => Foldable ((->) a) where
>   foldMap :: Monoid c => (b -> c) -> (a -> b) -> c
>   foldMap = undefined

*****************
* END PROBLEM 3 *
*****************


Another fundamental higher-kinded typeclass in Haskell is the Applicative
class. With container types, the intuition for Applicative is that we have a
sort of "cross-apply" operation: the (<*>) operator takes a container of
functions and applies those functions to a container of values, producing a new
container as output. The "pure" function creates a container given a single
"seed" element: this might be a container that contains exactly one value, but
it might also be a container with several copies of the one value, or with some
extra structure.

This is a very general abstraction, but we'll be using it in a fairly limited
way in this assignment, so we'll avoid talking about some of the Applicative
laws for now; the ones given here are a subset of the applicative laws that the
Prelude lists in the standard Applicative definition.

> class Functor f => Applicative (f :: * -> *) where
>   pure :: forall a. a -> f a

>   -- pure f <*> xs == fmap f xs
>   -- pure f <*> pure x = pure (f x)
>   infixl 4 <*>
>   (<*>) :: forall a b. f (a -> b) -> f a -> f b

The "<*>" operator is sometimes pronounced "ap" (as in "apply"), and it has
this name in the Prelude, but we'll see a better way to pronounce it in
some expressions below.

The first law says that applying a container that contains just one function to
a container of elements has the same result as using "fmap" to apply the
function to each element in the container.

The second law says that applying a container that contains just one function
to a container that contains just one element results in a container that
contains just one result, that of the function applied to the element.

The Applicative class is definitely best understood through examples. The list
type can actually be given multiple different lawful Applicative instances, but
this is the standard one defined in the Prelude: "pure" creates a
single-element list, and "fs <*> xs" applies each function in "fs" to each
value in "xs" and collects the results.

> instance Applicative [] where
>   pure :: a -> [a]
>   pure x = [x]
> 
>   (<*>) :: [a -> b] -> [a] -> [b]
>   fs <*> xs = [f x | f <- fs, x <- xs]

One of the primary features of the Applicative typeclass, which is probably not
obvious at first, is that it allows us to "map" multiple-argument functions
over multiple containers. We can write a standard function called liftA2 that
acts as a two-argument version of fmap:

> liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
> liftA2 f x y = pure f <*> x <*> y

It's interesting to explore why we can't define liftA2 with just a Functor
constraint, but we'll put that aside for now. This is a subtle definition, and
it's worth spending a minute to try to make sense of it. The (<*>) operator is
left-associative, so this is the same as "(pure f <*> x) <*> y". The types work
out as follows (check for yourself):

  f :: a -> b -> c
  x :: f a
  y :: f b

  pure f             :: f (a -> b -> c)
  pure f <*> x       :: f (b -> c)
  pure f <*> x <*> y :: f c

Similarly, we can write functions liftA3, liftA4, etc. for arbitrary
Applicative types. In general, the expression "pure f <*> x <*> y <*> ..." can
be read as "mapping" the multiple-argument function "f" over multiple container
inputs. This pattern is the most common use of <*>, so we might pronounce the
definition of liftA2 as "map f over the containers x and y", keeping in mind
that "mapping" over multiple arguments involves the Applicative typeclass.

A canonical example of liftA2 over lists is the function that creates a list of
all ordered pairs of elements from two input lists.

> allPairs :: [a] -> [b] -> [(a,b)]
> allPairs = liftA2 (,)

If you trace out the steps of computation involved in an application of
allPairs, it creates a list of functions of the form "\y -> (x,y)" for each
element "x" in the first list, and applies each function in that list to each
element of the second list to produce a list of pairs.

Another standard Applicative type is the Maybe type. There is only one lawful
Applicative instance for Maybe: "pure" creates a non-empty Maybe value (Just)
from a single element, and <*> combines a function with a value if both exist.

> instance Applicative Maybe where
>   pure :: a -> Maybe a
>   pure = Just
> 
>   (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
>   Just f <*> Just x = Just (f x)
>   _ <*> _ = Nothing


Now, let's reintroduce the Index and Board types from the first assignment.

> data Coordinate = C0 | C1 | C2
>   deriving (Generic, Eq, Ord, Show)

This type is clearly finite, with three distinct values.

> instance Finite Coordinate where
>   elements = [C0, C1, C2]

An Index is a pair of coordinates, and has a Finite instance given by the
general instance for pairs above.

> type Index = (Coordinate, Coordinate)

A Board is a container of nine elements. The GeneralizedNewtypeDeriving GHC
extension allows us to automatically derive instances for newtypes whenever the
"wrapped" type has those instances: here we use this to derive several useful
bits of functionality for our Board type, since the type "(->) Index" already
has instances for all of these typeclasses.

> newtype Board a = Board { cell :: Index -> a }
>   deriving (Generic, Functor, Foldable, Semigroup, Monoid)

(You can ignore the Generic instance as usual, I'm just using it for testing.)

To be specific, these are the constraints on the generated instances:

  instance Functor Board where ...
  instance Foldable Board where ...
  instance Semigroup a => Semigroup (Board a) where ...
  instance Monoid a => Monoid (Board a) where ...

The Semigroup instance combines two boards elementwise:

  ( x1 x2 x3 )    ( y1 y2 y3 )   ( (x1 <> y1) (x2 <> y2) (x3 <> y3) )
  ( x4 x5 x6 ) <> ( y4 y5 y6 ) = ( (x4 <> y4) (x5 <> y5) (x6 <> y6) )
  ( x7 x8 x9 )    ( y7 y8 y9 )   ( (x7 <> y7) (x8 <> y8) (x9 <> y9) )

The Monoid instance defines "mempty" as a board full of "mempty" values.

Here's the Show instance from last time, which gives a 2D display of the board.

> instance Show a => Show (Board a) where
>   show b = unlines (fmap showRow rows)
>     where
>       rows = [[(cx,cy) | cx <- elements] | cy <- elements]
>       showRow = concat . intersperse " " . fmap (show . cell b)

Here's a Cell type and an example board to play around with in GHCi.

> data Cell = X | O | E -- E for Empty
>   deriving (Generic, Eq)

> instance Show Cell where
>   show X = "X"
>   show O = "O"
>   show E = "."

> example :: Board Cell
> example = Board grid
>   where
>     grid :: Index -> Cell
>     grid (C0,C0) = X; grid (C1,C0) = O; grid (C2,C0) = E
>     grid (C0,C1) = X; grid (C1,C1) = X; grid (C2,C1) = E
>     grid (C0,C2) = O; grid (C1,C2) = E; grid (C2,C2) = E


The Board type is an applicative.

> instance Applicative Board where
>   pure :: a -> Board a
>   pure x = Board (\i -> x)

>   (<*>) :: Board (a -> b) -> Board a -> Board b
>   fb <*> xb = Board (\i -> cell fb i (cell xb i))

(Incidentally, these are the K and S combinators from combinatory logic.)

The "pure" function creates a Board where every element has the same value.

           ( x x x )
  pure x = ( x x x )
           ( x x x )

The <*> function is elementwise application of a board of functions to a board
of elements.

  ( f1 f2 f3 )     ( x1 x2 x3 )   ( (f1 x1) (f2 x2) (f3 x3) )
  ( f4 f5 f6 ) <*> ( x4 x5 x6 ) = ( (f4 x4) (f5 x5) (f6 x6) )
  ( f7 f8 f9 )     ( x7 x8 x9 )   ( (f7 x7) (f8 x8) (f9 x9) )

A consequence of these two behaviors is that the "liftA2" function is
elementwise application of a single two-argument function to two boards of
elements.

           ( x1 x2 x3 ) ( y1 y2 y3 )   ( (f x1 y1) (f x2 y2) (f x3 y3) )
  liftA2 f ( x4 x5 x6 ) ( y4 y5 y6 ) = ( (f x4 y4) (f x5 y5) (f x6 y6) )
           ( x7 x8 x9 ) ( y7 y8 y9 )   ( (f x7 y7) (f x8 y8) (f x9 y9) )

Now that we have all these high-level operations over the Board type, let's put
them to use! The code below revisits the problem of checking whether some
player has won a game of tic-tac-toe, given the current state of the board.

We'll take a more abstract approach this time, using a modified tic-tac-toe
ruleset that works out to be equivalent to the standard ruleset. Conceptually,
we'll say that every time a player makes a mark on the board, they collect one
"token" for each different way that mark could lead to a win; for example,
placing a mark in the upper-left corner yields one "row 0" token, one "column
0" token, and one "diagonal upper-left to lower-right" token. A player has won
when they've collected three of the same token.

> data Token
>   = Row Coordinate
>   | Column Coordinate
>   | Diag1 -- upper-left to lower-right
>   | Diag2 -- lower-left to upper-right
>   deriving (Eq, Ord, Show)

Let's look at an example with a concrete board state:

  X O X
  X . O   -- "." is an empty space (E)
  X O O

Here are the tokens that each player has collected in this game so far:

  X: [ Row C0, Column C0, Diag1 -- upper-left
     , Row C0, Column C2, Diag2 -- upper-right
     , Row C1, Column C0        -- center-left
     , Row C2, Column C0        -- lower-left
     ]

  O: [ Row C0, Column C1        -- upper-center
     , Row C1, Column C2        -- center-right
     , Row C2, Column C1        -- lower-center
     , Row C2, Column C2, Diag2 -- lower-right
     ]

Player X has won this game because they've collected three "Column C0" tokens.
Player O has not won because they don't have three of any particular token.

We need a procedure to generate these token lists from a given board state.
There are many ways to approach this; we'll start by building a board that has
all possible tokens on it, and then filter it down to only the tokens that
belong to a particular player.

This "rowBoard" definition is a board with all of the Row tokens placed.
Check it out in GHCi - it actually prints nicely!

> rowBoard :: Board [Token]
> rowBoard = Board (\(cx,cy) -> [Row cx])

Similarly, the columnBoard and diagBoard1/2 functions represent the placements
of the other three kinds of tokens.

> columnBoard :: Board [Token]
> columnBoard = Board (\(cx,cy) -> [Column cy])

> diagBoard1 :: Board [Token]
> diagBoard1 = Board (\(cx,cy) -> if cx == cy then [Diag1] else [])

> diagBoard2 :: Board [Token]
> diagBoard2 = Board (\(cx,cy) -> if cx == mirror cy then [Diag2] else [])
>   where
>     mirror :: Coordinate -> Coordinate
>     mirror C0 = C2
>     mirror C1 = C1
>     mirror C2 = C0

We can combine these boards with the Monoid instance for Board: the type
"Board [Token]" is a monoid whose <> action is cellwise list concatenation.

This is the board with all of the tokens placed.

> tokenBoard :: Board [Token]
> tokenBoard = mconcat [rowBoard, columnBoard, diagBoard1, diagBoard2]

Almost there! Now we need a way to filter it down to only the tokens that one
given player owns. We'll do this by taking a game board and the token board and
"zipping" them together with an operation that keeps only the stacks of tokens
in cells corresponding to marks from the given player on the game board.

The "playerFilter" function is going to be the "zipping" function that we
combine the two boards with; we'll use it partially applied with one argument,
so remember this type can also be written "a -> (a -> [Token] -> [Token])".

> playerFilter :: Eq a => a -> a -> [Token] -> [Token]
> playerFilter x x' ys = if x == x' then ys else []

For a given "x", the function "playerFilter x :: a -> [Token] -> [Token]" can
be seen as a function that conditionally empties a list when its first argument
is not "x". Conceptually, this represents the action of removing from the board
all of the stacks of tokens that don't belong to "x" from the board.

Now, if we have a board of pieces of type "a", we can combine it with
"tokenBoard" using "liftA2 (playerFilter x)" to obtain the board with only the
tokens that belong to "x".

> playerBoard :: Eq a => a -> Board a -> Board [Token]
> playerBoard x b = liftA2 (playerFilter x) b tokenBoard


*************
* PROBLEM 4 *
*************

Replace the "undefined" in the function below so that "won" returns a Bool
indicating whether the given player has won on the given board. You may use
other functions that you define in this file, but you may not change any of the
definition of "won" except for the "undefined" part.

Do not use any of the Coordinate, Token, Board, or Cell constructors
explicitly, in this definition or the definitions of any other functions that
you use in this one. Don't use "toList" or any functions that use it either,
and don't add or modify any imports to this file.

This problem can be solved in several different ways by combining a small
number of the Foldable functions introduced in this assignment. (The solution I
have in the solution key is a composition of three functions.) This is a common
pattern in Haskell: many problems involving containers can be solved concisely
with the standard Prelude Foldable functions!

Here's a tip to get you started: the "elemCounts" function might be useful, and
remember that the Map type has a Foldable instance. Also, while you shouldn't
use "toList" in your answer, it might be useful for experimenting in GHCi!

> won :: Eq a => a -> Board a -> Bool
> won x = undefined . playerBoard x

*****************
* END PROBLEM 4 *
*****************
