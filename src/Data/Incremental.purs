-- | Incremental computation, based on
-- |
-- | > "A Theory of Changes for Higher-Order Languages" by
-- | > Cai, Gairrusso, Rendel and Ostermann.
-- |
-- | This module also defines a HOAS-style interface for working with
-- | function changes.

module Data.Incremental
  ( class ChangeStructure
  , diff
  , patch
  , FunctionChange
  , runFunctionChange
  , Change
  , fromChange
  , toChange
  , D1(..)
  , valueOf
  , changeOf
  , lam
  , app
  , constant
  , applyPatch
  ) where

import Prelude

import Data.Monoid (class Monoid, mempty)
import Unsafe.Coerce (unsafeCoerce)

-- | A "change structure" on `a` consists of a monoid `d` of changes, together with
-- | diff and patch functions.
class Monoid d <= ChangeStructure a d | a -> d where
  diff :: a -> a -> d
  patch :: a -> d -> a

-- | A change structure for functions
newtype FunctionChange a da db = FunctionChange (a -> da -> db)

runFunctionChange :: forall a da db. FunctionChange a da db -> a -> da -> db
runFunctionChange (FunctionChange df) = df

instance semigroupFunctionChange :: Semigroup db => Semigroup (FunctionChange a da db) where
  append (FunctionChange f) (FunctionChange g) = FunctionChange \a da -> f a da <> g a da

instance monoidFunctionChange :: Monoid db => Monoid (FunctionChange a da db) where
  mempty = FunctionChange \_ _-> mempty

instance diffFunctionChange :: (ChangeStructure a da, ChangeStructure b db) => ChangeStructure (a -> b) (FunctionChange a da db) where
  diff f g = FunctionChange \a da -> f (a `patch` da) `diff` g a
  patch f (FunctionChange df) a = f a `patch` df a (mempty :: da)

-- | A type level function which maps a type to the type of its change structure.
-- |
-- | Uniqueness of instances makes the coercions `fromChange` and `toChange` safe,
-- | since the functional dependency makes the change structure type unique.
data Change a

fromChange :: forall a da. ChangeStructure a da => Change a -> da
fromChange = unsafeCoerce

toChange :: forall a da. ChangeStructure a da => da -> Change a
toChange = unsafeCoerce

-- | A term paired with its rate of change.
-- |
-- | We can think of these modified terms as conceptually similar to dual numbers.
data D1 a = D1 a (Change a)

valueOf :: forall a. D1 a -> a
valueOf (D1 a _) = a

changeOf :: forall a. D1 a -> Change a
changeOf (D1 _ da) = da

-- | Lambda abstraction
lam :: forall a b da db. (ChangeStructure a da, ChangeStructure b db) => (D1 a -> D1 b) -> D1 (a -> b)
lam f =
  D1 (\a -> valueOf (f (D1 a (toChange (mempty :: da)))))
     (toChange (FunctionChange \a da -> fromChange (changeOf (f (D1 a (toChange da))))))

-- | Function application
app :: forall a b da db. (ChangeStructure a da, ChangeStructure b db) => D1 (a -> b) -> D1 a -> D1 b
app (D1 f df) (D1 a da) = D1 (f a) (toChange (runFunctionChange (fromChange df) a (fromChange da)))

-- | A constant term
constant :: forall a da. ChangeStructure a da => a -> D1 a
constant a = D1 a (toChange (mempty :: da))

-- | Create a function which applies a patch to its input
applyPatch :: forall a da. ChangeStructure a da => da -> D1 (a -> a)
applyPatch da = D1 (_ `patch` da) (toChange (FunctionChange \_ c -> c <> da))
