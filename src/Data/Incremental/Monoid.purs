module Data.Incremental.Monoid
  ( Left(..)
  , appendLeft
  , Right(..)
  , appendRight
  ) where

import Prelude

import Data.Incremental (class Patch, Change, toChange)
import Data.Monoid (class Monoid)
import Data.Monoid.Dual (Dual(..))
import Data.Newtype (class Newtype)

-- | A change structure for any monoid, with the `Dual` monoid acting by
-- | appending on the left.
newtype Left a = Left a

derive instance eqLeft :: Eq a => Eq (Left a)
derive instance ordLeft :: Ord a => Ord (Left a)
derive instance newtypeLeft :: Newtype (Left a) _

instance showLeft :: Show a => Show (Left a) where
  show (Left a) = "(Left " <> show a <> ")"

instance patchLeft :: Monoid a => Patch (Left a) (Dual a) where
  patch (Left x) (Dual y) = Left (y <> x)

-- | Change by appending a value on the left.
appendLeft :: forall a. Monoid a => a -> Change (Left a)
appendLeft = toChange <<< Dual

-- | A change structure for any monoid, acting on itself by appending on the right.
newtype Right a = Right a

derive newtype instance semigroupRight :: Semigroup a => Semigroup (Right a)
derive newtype instance monoidRight :: Monoid a => Monoid (Right a)

derive instance eqRight :: Eq a => Eq (Right a)
derive instance ordRight :: Ord a => Ord (Right a)
derive instance newtypeRight :: Newtype (Right a) _

instance showRight :: Show a => Show (Right a) where
  show (Right a) = "(Right " <> show a <> ")"

instance patchRight :: Monoid a => Patch (Right a) (Right a) where
  patch (Right x) (Right y) = Right (x <> y)

-- | Change by appending a value on the right.
appendRight :: forall a. Monoid a => a -> Change (Right a)
appendRight = toChange <<< Right
