-- | This module implements an Iterator structure built on top of JavaScript generator functions. They are as useful as lazy lists but without the memory overhead or possibility of call stack exhaustion.
module Data.Iterator (Iterator, next, iterate, iterate', forEach, isDone, empty, singleton, concat, concat', take, range, range', fromArray, toArray, repeat, (..)) where

import Prelude
import Data.Foldable (class Foldable)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Test.QuickCheck (class Arbitrary, arbitrary)

-- |  Generic iterator type. An `Iterator a` produces elements of type `a` finitely or infinitely many times.
foreign import data Iterator :: Type -> Type

-- | Checks, without consuming elements, whether the iterator is done.
foreign import isDone :: forall a. Iterator a -> Boolean

-- | Return an empty iterator, such that `isDone empty == true`.
foreign import empty :: forall a. Iterator a

-- | Return a singleton iterator, one that produces the value `a` once before finishing.
foreign import singleton :: forall a. a -> Iterator a

-- | Infinitely chain the input function, producing the output of it at every step. The seed value is necessary to initially call the function and does not appear in the iterator.
foreign import iterate :: forall a. (a -> a) -> a -> Iterator a

-- | Performs an effectful computation for each value in the iterator. The return value of the effectful computation is ignored.
foreign import forEach :: forall m a. (a -> m Unit) -> Iterator a -> m Unit

-- | Zips two iterators, returning a tuple of each value. The length of the new iterator is the minimum of the two input iterators.
foreign import zip :: forall a b. Iterator a -> Iterator b -> Iterator (Tuple a b)

-- | Concatenates an array of iterators into an iterators that uses each iterator of the array, in order, using `(<>)`.
foreign import concat' :: forall a. Array (Iterator a) -> Iterator a

-- | Concatenates nested iterators using `(<>)` on each of them.
foreign import concat :: forall a. Iterator (Iterator a) -> Iterator a

-- | Takes *at most* the `n` first values of the iterators. The iterator finishes when either the iterator is finished or the right number of values have been produced.
foreign import take :: forall a. Int -> Iterator a -> Iterator a

-- | Produces an iterator of values over the specified range, starting at the lower bound (included) and producing every result of the successor function until (but not including) the value that equals the upper bound.
-- |
-- | #### Notes
-- |
-- | - This is **not** the equivalent of the `(..)` operator, as the operator only works over types that are `Semiring`.
-- | - The successor function must eventually return the upper bound for the equality check to terminate the iterator, otherwise the iterator will be infinite.
foreign import range :: forall a. Eq a => (a -> a) -> a -> a -> Iterator a

-- | Consumes elements of an array, returning an iterator that produces the elements of that array.
foreign import fromArray :: forall a. Array a -> Iterator a

-- | Consumes an iterator, returning an array with the elements produced by the iterator.
-- |
-- | #### Warning
-- |
-- | This function crashes if the iterator is infinite. Should you not be certain of the iterator finiteness, it is best to *not* use this function, or at the very least, to catch any OOM exception that might occur.
foreign import toArray :: forall a. Iterator a -> Array a

-- | Infinitely produce the input value.
foreign import repeat :: forall a. a -> Iterator a

-- | Returns the next element in the iterator, as well as the iterator if it isn't already done.
next :: forall a. Iterator a -> Tuple a (Maybe (Iterator a))
next iter = Tuple value $ if done then Nothing else Just iter
  where
  Tuple value done = nextImpl iter

-- | Iterate over a `Monoid`, starting from its empty value as returned by `mempty`.
iterate' :: forall a. Monoid a => (a -> a) -> Iterator a
iterate' pred = iterate pred mempty

-- | Iterate over a range of `Semiring`, starting at the lower bound and incrementing it (`x' = x + one`) until it reaches the upper bound.
range' :: forall a. Eq a => Semiring a => a -> a -> Iterator a
range' = range ((+) one)

infixl 8 range' as ..

instance functorIterator :: Functor Iterator where
  map = mapImpl

instance semigroupIterator :: Semigroup (Iterator a) where
  append i1 i2
    | isDone i1 || isDone i2 = empty
    | otherwise = appendImpl i1 i2

instance monoidIterator :: Monoid (Iterator a) where
  mempty = empty

instance applyIterator :: Apply Iterator where
  apply i1 i2 = (\(Tuple f x) -> f x) <$> zip i1 i2

instance applicativeIterator :: Applicative Iterator where
  pure = singleton

instance bindIterator :: Bind Iterator where
  bind x f = concat $ f <$> x

instance monadIterator :: Monad Iterator

instance foldableIterator :: Foldable Iterator where
  foldl = foldlImpl
  foldr = foldrImpl
  foldMap map = foldlImpl (\acc x -> acc <> map x) mempty

instance arbitraryIterator :: Arbitrary a => Arbitrary (Iterator a) where
  arbitrary = do
    x <- arbitrary
    (cond :: Boolean) <- arbitrary
    if cond then do
      xs <- arbitrary
      pure $ singleton x <> xs
    else
      pure $ singleton x

foreign import nextImpl :: forall a. Iterator a -> Tuple a Boolean

foreign import mapImpl :: forall a b. (a -> b) -> Iterator a -> Iterator b

foreign import appendImpl :: forall a. Iterator a -> Iterator a -> Iterator a

foreign import foldlImpl :: forall a b. (b -> a -> b) -> b -> Iterator a -> b

foreign import foldrImpl :: forall a b. (a -> b -> b) -> b -> Iterator a -> b
