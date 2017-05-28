module Data.Incremental.Abelian
  ( class Group
  , inverse
  , subtract
  , class Abelian
  , WrappedAbelian(..)
  ) where

import Prelude
import Data.Incremental (class ChangeStructure)
import Data.Monoid (class Monoid)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (class Newtype)

-- | A `Group` is a `Monoid` with inverses.
-- |
-- | Laws:
-- |
-- | - _Left inverse_: `inverse x <> x = mempty`
-- | - _Right inverse_: `x <> inverse x = mempty`
class Monoid g <= Group g where
  inverse :: g -> g

instance groupAdditive :: Ring a => Group (Additive a) where
  inverse (Additive a) = Additive (negate a)

-- | An `Abelian` group is a `Group` whose `append` operation also satisfies the
-- | _commutativity law_:
-- |
-- | - _Commutativity_ `x <> y = y <> x`
class Group g <= Abelian g

instance abelianAdditive :: Ring a => Abelian (Additive a)

-- | Subtraction in a group.
subtract :: forall g. Group g => g -> g -> g
subtract x y = x <> inverse y

-- | A change structure for any abelian group.
newtype WrappedAbelian g = WrappedAbelian g

derive newtype instance semigroupWrappedAbelian :: Semigroup g => Semigroup (WrappedAbelian g)
derive newtype instance monoidWrappedAbelian :: Monoid g => Monoid (WrappedAbelian g)
derive newtype instance groupWrappedAbelian :: Group g => Group (WrappedAbelian g)
derive newtype instance abelianWrappedAbelian :: Abelian g => Abelian (WrappedAbelian g)

derive instance eqWrappedAbelian :: Eq g => Eq (WrappedAbelian g)
derive instance ordWrappedAbelian :: Ord g => Ord (WrappedAbelian g)

derive instance newtypeWrappedAbelian :: Newtype (WrappedAbelian g) _

instance showWrappedAbelian :: Show g => Show (WrappedAbelian g) where
  show (WrappedAbelian g) = "(WrappedAbelian " <> show g <> ")"

instance changeStructureWrappedAbelian :: Abelian g => ChangeStructure (WrappedAbelian g) (WrappedAbelian g) where
  diff (WrappedAbelian x) (WrappedAbelian y) = WrappedAbelian (x `subtract` y)
  patch (WrappedAbelian x) (WrappedAbelian y) = WrappedAbelian (x <> y)
