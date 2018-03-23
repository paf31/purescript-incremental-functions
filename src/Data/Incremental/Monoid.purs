module Data.Incremental.Monoid
  ( WrappedMonoid(..)
  , appending
  ) where

import Prelude

import Data.Incremental (class Patch, Change, toChange)
import Data.Monoid (class Monoid)
import Data.Newtype (class Newtype)

-- | A change structure for any monoid, acting on itself by appending on the right.
newtype WrappedMonoid a = WrappedMonoid a

derive newtype instance semigroupWrappedMonoid :: Semigroup a => Semigroup (WrappedMonoid a)
derive newtype instance monoidWrappedMonoid :: Monoid a => Monoid (WrappedMonoid a)

derive instance eqWrappedMonoid :: Eq a => Eq (WrappedMonoid a)
derive instance ordWrappedMonoid :: Ord a => Ord (WrappedMonoid a)
derive instance newtypeWrappedMonoid :: Newtype (WrappedMonoid a) _

instance showWrappedMonoid :: Show a => Show (WrappedMonoid a) where
  show (WrappedMonoid a) = "(WrappedMonoid " <> show a <> ")"

instance patchWrappedMonoid :: Monoid a => Patch (WrappedMonoid a) (WrappedMonoid a) where
  patch (WrappedMonoid x) (WrappedMonoid y) = WrappedMonoid (x <> y)

-- | Change by appending a value on the right.
appending :: forall a. Monoid a => a -> Change (WrappedMonoid a)
appending = toChange <<< WrappedMonoid
