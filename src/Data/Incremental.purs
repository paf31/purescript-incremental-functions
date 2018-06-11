-- | Incremental computation, based on
-- |
-- | > "A Theory of Changes for Higher-Order Languages" by
-- | > Cai, Giarrusso, Rendel and Ostermann.
-- |
-- | This module also defines a HOAS-style interface for working with
-- | function changes.

module Data.Incremental
  ( class Patch
  , patch
  , class Diff
  , diff
  , Change
  , fromChange
  , toChange
  , Jet
  , constant
  , change
  ) where

import Prelude

import Data.Tuple (Tuple(..))
import Unsafe.Coerce (unsafeCoerce)

-- | The monoid `d` of changes acts on values of type `a`.
class Monoid d <= Patch a d | a -> d where
  patch :: a -> d -> a

class Patch a d <= Diff a d | a -> d where
  diff :: a -> a -> d

instance patchUnit :: Patch Unit Unit where
  patch _ _ = unit

instance diffUnit :: Diff Unit Unit where
  diff _ _ = unit

instance patchTuple :: (Patch a da, Patch b db) => Patch (Tuple a b) (Tuple da db) where
  patch (Tuple a b) (Tuple c d) = Tuple (patch a c) (patch b d)

instance diffTuple :: (Diff a da, Diff b db) => Diff (Tuple a b) (Tuple da db) where
  diff (Tuple a b) (Tuple c d) = Tuple (diff a c) (diff b d)

-- | A type level function which maps a type to the type of its change structure.
-- |
-- | Uniqueness of instances makes the coercions `fromChange` and `toChange` safe,
-- | since the functional dependency makes the change structure type unique.
data Change a

instance semigroupChange :: (Patch a da, Semigroup da) => Semigroup (Change a) where
  append x y = toChange (fromChange x <> fromChange y)

instance monoidChange :: (Patch a da, Monoid da) => Monoid (Change a) where
  mempty = toChange mempty

fromChange :: forall a da. Patch a da => Change a -> da
fromChange = unsafeCoerce

toChange :: forall a da. Patch a da => da -> Change a
toChange = unsafeCoerce

-- | A value (`position`) paired with a change (`velocity`).
-- |
-- | We can think of these modified terms as conceptually similar to dual
-- | numbers.
-- |
-- | We can use functions of type `Jet a -> Jet b` as incremental
-- | functions from `a` to `b`, which gives us a HOAS-style DSL for working
-- | with jets.
type Jet a =
  { position :: a
  , velocity :: Change a
  }

-- | A constant term
constant :: forall a da. Patch a da => a -> Jet a
constant position = { position, velocity: mempty }

-- | Create a function which applies a patch to its input
change :: forall a da. Patch a da => Change a -> Jet a -> Jet a
change c { position, velocity } =
  { position: position `patch` fromChange c
  , velocity
  }
