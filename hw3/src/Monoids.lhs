> {-# LANGUAGE NoImplicitPrelude #-}

This assignment has you work with the Monoid typeclass. The Monoid typeclass
extends the Semigroup typeclass, so let's recall the intuition for semigroups
from the last assignment:

  A type X is a semigroup if we can "concatenate" a non-empty list of
  elements of type X into a single element by divide-and-conquer recursion.

The Monoid typeclass extends this to concatenating any list, not just non-empty
lists. This is achieved by extending the Semigroup typeclass definition with a
"unit of concatenation": a value whose presence in a list does not affect the
result of "concatenating" the list, like theempty string for concatenation over
strings or 0 for addition over numbers. The concatenation of an empty list of
some monoid type is then defined to be just the unit value for that monoid.

> module Monoids where

The Monoid typeclass is also part of the Prelude, but we'll define it manually
in here, along with the mconcat function that concatenates lists.

> import Prelude hiding (Monoid, mempty, mconcat)

We'll be playing with some types from the Prelude and from the "collections"
package, which is made available in this module through Cabal/Stack. These
types will be discussed as they come up, but the imports have to be at the top
of the file in order to make Haskell happy.

The import syntax with parentheses restricts an import statement to only bring
the specified names into scope, instead of every name that the imported module
exports. The "import qualified" syntax brings the exported names in a module
into scope with a required prefix, so e.g. the "singleton" function from
Data.Map must be referred to as "Map.singleton" in this file; this is to avoid
name clashes.

> import Data.Coerce (coerce)
> import qualified Data.List.NonEmpty as NonEmpty
> import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
> import Data.Semigroup (Semigroup, (<>), All(..), Any(..), Sum(..), First(..))
> import qualified Data.Map as Map
> import Data.Map (Map)
> import GHC.Generics (Generic)
> import System.Random

Before we get into monoids, let's talk about one more semigroup. The NonEmpty
type from the Prelude module Data.NonEmpty represents non-empty lists with a
very simple encoding: a nonempty list is an element along with a possibly-empty
list. Check the type of the (:|) constructor in GHCi - it behaves similarly to
the standard (:) list constructor, but not quite the same.

  data NonEmpty a = a :| [a]

We have a function "toList :: NonEmpty a -> [a]", but no (total) function in
the other direction - instead we have "nonEmpty :: [a] -> Maybe (NonEmpty a)",
which returns Nothing if the input list is empty.

We'll use the singleton function a couple times in this assignment: it makes a
non-empty list out of a single element.

> singleton :: a -> NonEmpty a
> singleton x = x :| []

For any type "a", the type "NonEmpty a" is a semigroup but *not* a monoid.
(NonEmpty is a free semigroup, isomorphic to last assignment's BinTree.)

  instance Semigroup (NonEmpty a) where
    (x :| xs) <> ys = x :| (xs ++ toList ys)

It's pretty common in programming to use a list with the assumption that it's
non-empty; this lets us encode that assumption in the type system so that we
get a compile-time error if it's violated.
    

A type is a Monoid when it is a Semigroup and it also has a unit of
concatenation. The syntax with the => symbol here means two things:
  A) If we want to implement an instance of Monoid for a type, Haskell will also
     require us to implement an instance of Semigroup for that type.
  B) If Haskell knows that some type is a Monoid, it also knows that type is a
     Semigroup.

> class Semigroup a => Monoid a where
>   -- mempty <> x == x
>   -- x <> mempty == x
>   mempty :: a

Note that "mempty" is not a function; we sometimes call it a "polymorphic
constant". Check the type of mempty in GHCi.

Since there are no arguments that can be used to infer a concrete type for "a"
for some occurrence of mempty, "a" is inferred by *context* - Haskell usually
knows what type a term *should* have based on its position in some larger term
(e.g. as an argument to a function with a known type), and in these cases it
can use that knowledge to decide which implementation of mempty to use.


Let's look at some simple monoids before we get into more interesting examples.
The All and Any types from the last assignment are both monoids. They're both
imported from the Prelude in this module along with their semigroup instances,
so here are their definitions and Semigroup instances as a reminder:

  newtype All = All { getAll :: Bool }

  instance Semigroup All where
    All x <> All y = All (x && y)

  newtype Any = Any { getAny :: Bool }

  instance Semigroup Any where
    Any x <> Any y = Any (x || y)


The unit for the "AND" operation is True, and the unit for the "OR" operation
is False. Check in GHCi that the unit laws hold with some test values.

> instance Monoid All where
>   mempty = All True

> instance Monoid Any where
>   mempty = Any False

(The Prelude also defines these Monoid instances and most others defined in
this file, but we haven't imported them.)


*************
* PROBLEM 1 *
*************

Give a lawful implementation for the Monoid instance for the Same type. Do not
change the definition of the type or the implementation of the Semigroup
instance.

> newtype Same = Same { getSame :: Bool }
>   deriving (Generic, Show, Eq)

> instance Semigroup Same where
>   Same x <> Same y = Same (x == y)

> instance Monoid Same where
>   mempty = Same True

*****************
* END PROBLEM 1 *
*****************


The mconcat function concatenates a list of values of a monoid type into a
single value. Note that this is just one implementation; as discussed in the
solution file for the last assignment, the associativity law guarantees that
this implementation can be swapped out with a parallelized divide-and-conquer
implementation without affecting the result of the function over any lawful
Monoid type.

> mconcat :: Monoid a => [a] -> a
> mconcat = foldr (<>) mempty


Let's look at the behavior of mconcat over some particular monoids. In
particular, we can represent many operations over lists by mapping the
constructor of some monoid type over a list, using mconcat to reduce the list
to a single element, and then unwrapping the monoid type into some result type.


The Prelude defines a Sum type, which represents the monoid of addition over
any numeric type.
  
  newtype Sum a = Sum { getSum :: a }

  instance Num a => Semigroup a where
    Sum x <> Sum y = Sum (x + y)

> instance Num a => Monoid (Sum a) where
>   mempty = Sum 0

If we map the Sum constructor over a list and then mconcat the values in the
list, we get the sum of the elements in the list.

> -- the Prelude already exports this function with the name "sum"
> sum' :: Num a => [a] -> a
> sum' = getSum . mconcat . map Sum

As discussed in lecture and previous assignments, the newtype keyword is
logically equivalent to the data keyword when defining types, but avoids the
boxing overhead that "data" introduces. Specifically, the Sum constructor and
the getSum function in the definition of sum' are both compiled to no-ops: the
implementation of sum' is computationally equivalent to "foldr (+) 0" under the
definition of mconcat given in this file, except it can be optimized just by
changing the mconcat implementation!


The Maybe type extends a Semigroup to a Monoid by adding a unit in the form of
Nothing. Note that this depends on a specific choice of Semigroup instance for
Maybe in order to be lawful: for example, if "Just x <> Nothing == Nothing",
associativity may hold but the right unit law does not hold. This is the
Semigroup instance for Maybe given in the Prelude.

  instance Semigroup a => Semigroup (Maybe a) where
    Just x <> Just y = Just (x <> y)
    Just x <> _ = Just x
    _ <> Just y = Just y

> instance Semigroup a => Monoid (Maybe a) where
>   mempty = Nothing

The mconcat function over the Maybe monoid combines all of the Just values in a
list, or returns Nothing if all of the values are Nothing. For example, we can
use this along with the NonEmpty semigroup to get a non-empty list of all of
the Just values in a list of Maybe values, or Nothing if there are none.

We'll cover the "fmap" function in depth soon; in the justs function it's used
to "map" over a Maybe value, with the following specialized subterm types.

  fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap singleton :: Maybe a -> Maybe (NonEmpty a)
  map (fmap singleton) :: [Maybe a] -> [Maybe (NonEmpty a)]

> justs :: [Maybe a] -> Maybe (NonEmpty a)
> justs = mconcat . map (fmap singleton)


The First type represents the trivial semigroup where (<>) always chooses its
left argument. The behavior of mconcat over the First semigroup extended to a
monoid with with Maybe is to choose the first element in a list, or return
Nothing if the list is empty.

  newtype First a = { getFirst :: a }

  instance Semigroup (First a) where
    x <> _ = x

A key component of the pattern of monoidal programming in modern Haskell
practice is "safe coercion". The "coerce" function from Data.Coerce is magical:
it can convert between any two types that have the exact same runtime
representation, and you'll get a compile-time type error if you try to use it
to convert between incompatible types. The function is used here with the type
"Maybe (First a) -> Maybe a" - GHC knows that "First a = a" at runtime because
"First" is defined as a newtype, and it knows that if "a = b", then
"Maybe a = Maybe b".

> head' :: [a] -> Maybe a
> head' = coerce . mconcat . map (Just . First)

We could use "fmap getFirst" instead of "coerce" here (and we could use
"coerce" instead of "First" if we used more type annotations); fundamentally,
there is no code that can be written with "coerce" that can't be written
without it. It's usually a matter of taste, but "coerce" does have the
advantage that it makes explicit the fact that the wrapping/unwrapping
operations are no-ops at runtime that just exist to specify behavior at
compile-time.

The First type isn't terribly interesting by itself, but the choice between the
list monoid and the "Maybe First" monoid can represent the choice between
reporting all errors or just the first error that comes up in execution of a
program that reports errors safely, as we'll see later in this file.  


Many polymorphic types also admit monoids, so we can automatically generate
Monoid instances for more complex nested types.

A list is always a monoid, regardless of the type of its elements.

  instance Semigroup [a] where
    (<>) = (++)

> instance Monoid [a] where
>   mempty = []

A pair of monoid types is a monoid.

  instance (Semigroup a, Semigroup b) => Semigroup (a,b) where
    (a1,b1) <> (a2,b2) = (a1<>a2, b1<>b2)

> instance (Monoid a, Monoid b) => Monoid (a,b) where
>   mempty = (mempty, mempty)


*************
* PROBLEM 2 *
*************

The Prelude gives a Semigroup instance for any function type with a semigroup
return type:

  instance Semigroup b => Semigroup (a -> b) where
    f <> g = \x -> f x <> g x

Give a lawful definition for mempty in the Monoid instance below.

> instance Monoid b => Monoid (a -> b) where
>   mempty = \x -> mempty

*****************
* END PROBLEM 2 *
*****************


The Monoid instance from problem 2 above can be used along with the All and Any
types to join lists of predicates.

> allMet :: [a -> Bool] -> a -> Bool
> allMet ps = getAll . mconcat (coerce ps) -- coerce :: [a -> Bool] -> [a -> All]

> anyMet :: [a -> Bool] -> a -> Bool
> anyMet ps = getAny . mconcat (coerce ps) -- coerce :: [a -> Bool] -> [a -> Any]

> -- divides x y = "x evenly divides y"
> divides :: Int -> Int -> Bool
> divides x y = y `mod` x == 0

> prime :: Int -> Bool
> prime x = not (anyMet (map divides [2..x-1]) x)


*************
* PROBLEM 3 *
*************

The Result type is equivalent to the Either type, but we're going to give it a
different Monoid definition than the standard Either monoid defined in the
Prelude.

> data Result e a = Error e | Ok a
>   deriving (Generic, Show, Eq)

Give lawful implementations for the Semigroup and Monoid instances for Result
below. In addition to the Semigroup and Monoid laws, the mconcat implementation
that is derived from your code should have the following properties.

  - If all members of the list "xs :: Result e a" are constructed with Ok,
    "mconcat xs" should return the result of joining all of the Ok values
    together with the Semigroup instance for "a".
  - If any members of the list "xs :: Result e a" are constructed with Error,
    "mconcat xs" should return the result of joining all of the Error values
    together with the Semigroup instance for "e".

You may modify the definition of (<>) to add cases.
Do not change the constraints on either instance.

> instance (Semigroup e, Semigroup a) => Semigroup (Result e a) where
>   Error e <> Error a =  Error (a <> e)
>   Ok e <> Ok a = Ok (e <> a)
>   Error e <> Ok a = Error e
>   Ok a <> Error e = Error e
>
> instance (Semigroup e, Monoid a) => Monoid (Result e a) where
>   mempty = Ok mempty

*****************
* END PROBLEM 3 *
*****************


The allOkSum function uses the Result monoid along with the Sum monoid to get
the sum of all of the Ok values in a list if there are no errors, or returns 0
if there are any errors.  The "forall e." syntax is necessary here along with
the ScopedTypeVariables extension in order to use the type variable "e" in the
type annotation on the expression "coerce rs"; the type annotation is needed to
tell GHC that we don't want the coercion to change the error type.

> allOkSum :: forall e. Semigroup e => [Result e Int] -> Int
> allOkSum rs =
>   case mconcat (coerce rs :: [Result e (Sum Int)]) of
>     Error _ -> 0
>     Ok x -> getSum x

The firstError function uses the Result monoid along with the First monoid to
retrieve the first Error value from a list of Results, or Nothing if there are
no Error values in the list.

> firstError :: forall e a. Monoid a => [Result e a] -> Maybe e
> firstError rs =
>   case mconcat (coerce rs :: [Result (First e) a]) of
>     Error e -> Just (getFirst e)
>     Ok _ -> Nothing

The allErrors function uses the Result monoid along with the list monoid to
retrieve a list of all of the errors in a list of Results. Note that the list
type is not a newtype, so we're not able to use coerce with the type
"[Result e a] -> [Result [e] a]"; instead, we use the mapError function to
create a single-element list out of each error in the list of results.
(Check the types of the subexpressions of allErrors in GHCi!)

> mapError :: (e -> e') -> Result e a -> Result e' a
> mapError f (Error e) = Error (f e)
> mapError f (Ok a) = Ok a

> allErrors :: Monoid a => [Result e a] -> [e]
> allErrors rs =
>   case mconcat (map (mapError (\x -> [x])) rs) of
>     Error es -> es
>     Ok _ -> []

The sumOrErrors function gets the sum of all Ok items in a list if there are no
errors, or the nonempty list of errors if there are any errors. Note that
even without assuming any compiler optimizations, mconcat here acts in a single
pass over the list!

> sumOrErrors :: [Result e Int] -> Result (NonEmpty e) Int
> sumOrErrors =
>   let coerce' = coerce :: [Result e Int] -> [Result e (Sum Int)] in
>     coerce . mconcat . map (mapError singleton) . coerce'

Here are some sample functions that you can use to play around with allOkSum,
firstError, allErrors, and sumOrErrors, to see that they work as advertised.
For example, try "sumOrErrors [safeDiv 1 3, safeMod 1 0]" and similar terms!

> safeDiv :: Int -> Int -> Result String Int
> safeDiv x 0 = Error "can't divide by 0"
> safeDiv x y = Ok (x `div` y)

> safeMod :: Int -> Int -> Result String Int
> safeMod x 0 = Error "can't mod by 0"
> safeMod x y = Ok (x `div` y)


The last part of this assignment will involve the Map type from Data.Map, which
is imported from the "containers" library package. You won't have to use the
Map type directly, but if you're interested in learning more about it, you can
find the API at this URL:

  https://hackage.haskell.org/package/containers-0.6.2.1/docs/Data-Map-Lazy.html

The Map type is a standard type of "mappings", mostly unrelated to the "map"
function in Haskell; almost all modern languages offer a similar type either
built-in or through a library, under names including "hash map", "dictionary",
and "associative array". The type "Map k v" is a map from keys of type "k" to
values of type "v", and it comes with a function for looking up values by key:

  Map.lookup :: Ord k => k -> Map k v -> Maybe v

The "Ord k" constraint is necessary because the Map type is actually
implemented as an ordered tree, with O(log n) average-time lookups that rely on
an ordering between keys. Some of our functions will also need an Ord constraint, since they will use Map functions that have an Ord constraint.

We'll be using a particular implementation of Monoid for the Map type,
different from the default one defined in the Prelude: when two maps are
"concatenated" together with (<>), the result should be the *union* of the two
maps.

> newtype Union a = Union { getUnion :: a }
>   deriving (Generic, Show, Eq)

If there are any duplicate keys between the two maps, the values of the
duplicate keys are combined with the (<>) operator from a Semigroup instance
for the value type. For example, the union of the maps {"x": Sum 1, "y": Sum 2}
and {"x": Sum 2, "z": Sum 4} is the map {"x": Sum 3, "y": Sum 2, "z: Sum 4"}.

> instance (Ord k, Semigroup v) => Semigroup (Union (Map k v)) where
>   Union m1 <> Union m2 = Union (Map.unionWith (<>) m1 m2)

The unit of the operation is the empty map.

> instance (Ord k, Semigroup v) => Monoid (Union (Map k v)) where
>   mempty = Union Map.empty

In the code below, we'll create a very basic text prediction engine. Our
program will first take in a string and compute how often each pair of
sequential words occurs in the string; these sequential pairs are called
"bigrams", and the result of this computation is a (discrete) "Markov
distribution", a particular kind of frequency distribution. For example, if we
input the string "a a b a c b a", we'll get the following distribution:

  {
    "a": {"a": 1, "c": 1},
    "b": {"a": 2},
    "c": {"b": 1}
  }

This says that "a" is followed by "a" 1 time and by "c" 1 time, "b" is followed
by "a" 2 times, and "c" is followed by "b" 1 time. This distribution can be
represented by the type "Map String (Map String Int)". More generally, we'll
avoid committing to a specific type of "words" at this stage:

  type FreqMap a = Map a (Map a Int)

But we will also want to combine frequency distributions: specifically, if we have two maps representing frequency distributions, we to *union* the two maps and *add* the contents of duplicate keys. For example, if we combine the example distribution above with the following distribution:

  {
    "a": {"a": 1, "b": 2},
    "d": {"b": 3, "e": 4}
  }

we should get this following distribution:

  {
    "a": {"a": 2, "b": 2, "c": 1},
    "b": {"a": 2},
    "c": {"b": 1},
    "d": {"b": 3, "e": 4}
  }
    
This is exactly the action of the (<>) operator on the FreqMap type as defined
below.

> type FreqMap a = Union (Map a (Union (Map a (Sum Int))))

Since Union and Sum are both newtypes, we can convert back and forth between
this FreqMap and the more convenient definition for free with coerce.
(Again, we could define these ourselves using the wrapping/unwrapping functions
along with Map.map, but this saves a nontrivial amount of uninteresting code.)

> fromFreqMap :: FreqMap a -> Map a (Map a Int)
> fromFreqMap = coerce

> toFreqMap :: Map a (Map a Int) -> FreqMap a
> toFreqMap = coerce

A "singleton" frequency map is a map that represents a single occurrence of a bigram. For example:

  singletonFreqMap "a" "b" = {"a": {"b": 1}}

> singletonFreqMap :: Eq a => a -> a -> FreqMap a
> singletonFreqMap x y = toFreqMap (Map.singleton x (Map.singleton y 1))

This little function generates the list of bigrams in a given input list.
Try it out in GHCi!

> bigrams :: [a] -> [(a,a)]
> bigrams [] = []
> bigrams (x:xs) = zip (x:xs) xs

Now we can generate the frequency map of bigrams in an input list by
transforming it into a list of bigrams, transforming each bigram into a
singleton frequency map, and combining all of the frequency maps together.
(Try it out in GHCi!)

> bigramFreqMap :: Ord a => [a] -> FreqMap a
> bigramFreqMap = mconcat . map (\(x,y) -> singletonFreqMap x y) . bigrams


*************
* PROBLEM 4 *
*************

Replace the "undefined" in the function below so that "train" returns the
frequency map that is the combination of the word bigram frequency maps for
each line in the input string. For example, given the following string as
input (where "\n" is a newline):

  "a b \n c d"

the frequency map returned should be the following, with no entry for the
bigram ("b","c"):
  
  {
    "a": {"b": 1},
    "c": {"d": 1},
  }

You should use the Prelude function "words :: String -> [String]" to split a
line into a list of words. You may use other functions that you define in this
file, but you may not change any of the definition of "train" except for the
"undefined" part.

> train :: String -> FreqMap String
> train = mconcat . map bigramFreqMap . map words . lines

*************
* PROBLEM 4 *
*************


The functions below implement a pure interface for randomly generating strings
using a FreqMap. The StdGen type from System.Random represents a pseudorandom
number generator, with a primitive operation that takes in a generator and
returns a pseudorandom number and a new generator:

  random :: Random a => StdGen -> (a, StdGen)

The Random typeclass classifies types that can be pseudorandomly generated.
The "random" function will return the same output every time it's given the
same input generator, but the output generator can be used to get new input,
and an arbitrary number of random values can be generated by "threading" 
generators in and out of these computations.  

The Main.hs file in this project uses a random seed for the pseudorandom number
generator (probably the current millisecond, depending on your platform), but
you can also create deterministic StdGen values with the mkStdGen function,
which takes an Int as input. Try calling "walk" with some sample inputs in GHCi:

  walk 100 "a" (train "a b c a a b a b c c a c b") (mkStdGen 1)

You'll get the same result every time you evaluate that expression, but if you
change the argument to mkStdGen you'll get a different pseudorandom result.

> choices :: Ord a => a -> FreqMap a -> [a]
> choices x m =
>   case Map.lookup x (fromFreqMap m) of
>     Nothing -> []
>     Just m' -> concatMap (\(x,c) -> replicate c x) (Map.toList m')

> randomElem :: [a] -> StdGen -> Maybe (a, StdGen)
> randomElem [] g = Nothing
> randomElem xs g = let (i,g') = random g in Just (xs !! (i `mod` length xs), g')

> -- n is the maximum number of words to output
> walk :: Ord a => Int -> a -> FreqMap a -> StdGen -> [a]
> walk 0 x m g = []
> walk n x m g =
>   case randomElem (choices x m) g of
>     Just (y,g') -> y : walk (n-1) y m g'
>     Nothing -> []
