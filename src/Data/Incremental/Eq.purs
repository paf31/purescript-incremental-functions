module Data.Incremental.Eq
  ( Atomic(..)
  , replace
  , mapAtomic
  ) where

import Prelude

import Data.Incremental (class Diff, class Patch, Change, Jet, fromChange, toChange)
import Data.Maybe (Maybe(..))
import Data.Maybe.Last (Last(..))
import Data.Newtype (class Newtype, unwrap, wrap)

-- | A change structure for any type with equality.
newtype Atomic a = Atomic a

derive instance eqAtomic :: Eq a => Eq (Atomic a)
derive instance ordAtomic :: Ord a => Ord (Atomic a)
derive instance newtypeAtomic :: Newtype (Atomic a) _

instance showAtomic :: Show a => Show (Atomic a) where
  show (Atomic a) = "(Atomic " <> show a <> ")"

instance patchAtomic :: Patch (Atomic a) (Last a) where
  patch x (Last Nothing) = x
  patch _ (Last (Just y)) = Atomic y

instance diffAtomic :: Eq a => Diff (Atomic a) (Last a) where
  diff (Atomic x) (Atomic y)
    | x == y = Last Nothing
    | otherwise = Last (Just y)

-- | Change by replacing the current value.
replace :: forall a. a -> Change (Atomic a)
replace a = toChange (Last (Just a))

-- | Change an `Atomic` value using a regular function.
mapAtomic
  :: forall a b
   . (a -> b)
  -> Jet (Atomic a)
  -> Jet (Atomic b)
mapAtomic f { position, velocity } =
    { position: wrap (f (unwrap position))
    , velocity: toChange (map f (fromChange velocity))
    }
