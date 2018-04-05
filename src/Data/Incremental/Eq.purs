module Data.Incremental.Eq
  ( Atomic(..)
  , replace
  , map
  , lift2
  , apply
  , mapAtomic
  ) where

import Prelude hiding (map, apply)
import Prelude as Prelude

import Data.Incremental (class Diff, class Patch, Change, Jet, fromChange, toChange)
import Data.Maybe (Maybe(..), fromMaybe)
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
-- |
-- | This alias for `map` will be removed in a future version.
mapAtomic
  :: forall a b
   . (a -> b)
  -> Jet (Atomic a)
  -> Jet (Atomic b)
mapAtomic f { position, velocity } =
  { position: wrap (f (unwrap position))
  , velocity: toChange (Prelude.map f (fromChange velocity))
  }

-- | Change an `Atomic` value using a regular function.
map
  :: forall a b
   . (a -> b)
  -> Jet (Atomic a)
  -> Jet (Atomic b)
map = mapAtomic

-- | Combine two `Atomic` values using a regular function.
-- |
-- | _Note_: The result will change (entirely) if either argument
-- | changes. If changes should be independent, consider using a `Tuple`
-- | instead.
lift2
  :: forall a b c
   . (a -> b -> c)
  -> Jet (Atomic a)
  -> Jet (Atomic b)
  -> Jet (Atomic c)
lift2 f a b =
    { position: wrap (f (unwrap a.position) (unwrap b.position))
    , velocity: toChange (combine f (fromChange a.velocity) (fromChange b.velocity))
    }
  where
    combine _ (Last Nothing) (Last Nothing) =
      Last Nothing
    combine g (Last a')      (Last b')      =
      Last (Just (g (fromMaybe (unwrap a.position) a')
                    (fromMaybe (unwrap b.position) b')))

-- | Combine two `Atomic` values in an applicative style.
apply
  :: forall a b
   . Jet (Atomic (a -> b))
  -> Jet (Atomic a)
  -> Jet (Atomic b)
apply = lift2 ($)
