module Data.Iterator (Iterator, next, iterate, iterate', forEach, isDone, empty, singleton, concat, concat', take, range, fromArray, toArray) where

import Prelude
import Data.Foldable (class Foldable)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Test.QuickCheck (class Arbitrary, arbitrary)

foreign import data Iterator :: Type -> Type

foreign import isDone :: forall a. Iterator a -> Boolean

foreign import empty :: forall a. Iterator a

foreign import singleton :: forall a. a -> Iterator a

foreign import iterate :: forall a. (a -> a) -> a -> Iterator a

foreign import forEach :: forall m a. (a -> m Unit) -> Iterator a -> m Unit

foreign import zip :: forall a b. Iterator a -> Iterator b -> Iterator (Tuple a b)

foreign import concat' :: forall a. Array (Iterator a) -> Iterator a

foreign import concat :: forall a. Iterator (Iterator a) -> Iterator a

foreign import take :: forall a. Int -> Iterator a -> Iterator a

foreign import range :: forall a. Eq a => (a -> a) -> a -> a -> Iterator a

foreign import fromArray :: forall a. Array a -> Iterator a

foreign import toArray :: forall a. Iterator a -> Array a

next :: forall a. Iterator a -> Tuple a (Maybe (Iterator a))
next iter = Tuple value $ if done then Nothing else Just iter
  where
  Tuple value done = nextImpl iter

iterate' :: forall a. Monoid a => (a -> a) -> Iterator a
iterate' pred = iterate pred mempty

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
