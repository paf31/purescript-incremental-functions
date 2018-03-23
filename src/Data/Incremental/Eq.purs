module Data.Incremental.Eq
  ( WrappedEq(..)
  , replace
  ) where

import Prelude

import Data.Incremental (class Patch, class Diff, Change, toChange)
import Data.Maybe (Maybe(..))
import Data.Maybe.Last (Last(..))
import Data.Newtype (class Newtype)

-- | A change structure for any type with equality.
newtype WrappedEq a = WrappedEq a

derive instance eqWrappedEq :: Eq a => Eq (WrappedEq a)
derive instance ordWrappedEq :: Ord a => Ord (WrappedEq a)
derive instance newtypeWrappedEq :: Newtype (WrappedEq a) _

instance showWrappedEq :: Show a => Show (WrappedEq a) where
  show (WrappedEq a) = "(WrappedEq " <> show a <> ")"

instance patchWrappedEq :: Patch (WrappedEq a) (Last a) where
  patch x (Last Nothing) = x
  patch _ (Last (Just y)) = WrappedEq y

instance diffWrappedEq :: Eq a => Diff (WrappedEq a) (Last a) where
  diff (WrappedEq x) (WrappedEq y)
    | x == y = Last Nothing
    | otherwise = Last (Just y)

-- | Change by replacing the current value.
replace :: forall a. a -> Change (WrappedEq a)
replace a = toChange (Last (Just a))
