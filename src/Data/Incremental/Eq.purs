module Data.Incremental.Eq
  ( WrappedEq(..)
  ) where

import Prelude

import Data.Incremental (class ChangeStructure)
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

instance changeStructureWrappedEq :: Eq a => ChangeStructure (WrappedEq a) (Last a) where
  diff (WrappedEq x) (WrappedEq y)
    | x == y = Last Nothing
    | otherwise = Last (Just y)
  patch x (Last Nothing) = x
  patch _ (Last (Just y)) = WrappedEq y
