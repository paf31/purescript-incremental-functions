module Data.Incremental.WrappedGroup
  ( WrappedGroup(..)
  ) where

import Prelude
import Data.Group (class CommutativeGroup, class Group, ginverse)
import Data.Incremental (class ChangeStructure)
import Data.Monoid (class Monoid)
import Data.Newtype (class Newtype)

-- | A change structure for any abelian group.
newtype WrappedGroup g = WrappedGroup g

derive newtype instance semigroupWrappedGroup :: Semigroup g => Semigroup (WrappedGroup g)
derive newtype instance monoidWrappedGroup :: Monoid g => Monoid (WrappedGroup g)
derive newtype instance groupWrappedGroup :: Group g => Group (WrappedGroup g)
derive newtype instance commutativeGroupWrappedGroup :: CommutativeGroup g => CommutativeGroup (WrappedGroup g)

derive instance eqWrappedGroup :: Eq g => Eq (WrappedGroup g)
derive instance ordWrappedGroup :: Ord g => Ord (WrappedGroup g)

derive instance newtypeWrappedGroup :: Newtype (WrappedGroup g) _

instance showWrappedGroup :: Show g => Show (WrappedGroup g) where
  show (WrappedGroup g) = "(WrappedGroup " <> show g <> ")"

instance changeStructureWrappedGroup :: CommutativeGroup g => ChangeStructure (WrappedGroup g) (WrappedGroup g) where
  diff (WrappedGroup x) (WrappedGroup y) = WrappedGroup (x <> ginverse y)
  patch (WrappedGroup x) (WrappedGroup y) = WrappedGroup (x <> y)
