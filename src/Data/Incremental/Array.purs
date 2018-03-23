module Data.Incremental.Array
  ( WrappedArray(..)
  , ArrayChange(..)
  , insertAt
  , deleteAt
  , modifyAt
  , map
  , singleton
  , static
  ) where

import Prelude hiding (map)

import Data.Array (foldl, mapWithIndex, zipWith)
import Data.Array as Array
import Data.Incremental (class Patch, Change, Jet, constant, fromChange, patch, toChange)
import Data.Maybe (fromMaybe)
import Data.Newtype (class Newtype, wrap)
import Prelude as Prelude

newtype WrappedArray a = WrappedArray (Array a)

derive instance eqWrappedArray :: Eq a => Eq (WrappedArray a)

instance showWrappedArray :: Show a => Show (WrappedArray a) where
  show (WrappedArray xs) = "(WrappedArray " <> show xs <> ")"

derive instance newtypeWrappedArray :: Newtype (WrappedArray a) _

data ArrayChange a da
  = InsertAt Int a
  | DeleteAt Int
  | ModifyAt Int da

derive instance eqArrayChange :: (Eq a, Eq da) => Eq (ArrayChange a da)

instance showArrayChange :: (Show a, Show da) => Show (ArrayChange a da) where
  show (InsertAt i a) = "(InsertAt " <> show i <> " " <> show a <> ")"
  show (DeleteAt i) = "(DeleteAt " <> show i <> ")"
  show (ModifyAt i da) = "(ModifyAt " <> show i <> " " <> show da <> ")"

instance patchWrappedArray
    :: Patch a da
    => Patch (WrappedArray a) (Array (ArrayChange a da)) where
  patch (WrappedArray xs) = WrappedArray <<< foldl patchOne xs where
    patchOne xs_ (InsertAt i x)   = fromMaybe xs_ (Array.insertAt i x xs)
    patchOne xs_ (DeleteAt i)     = fromMaybe xs_ (Array.deleteAt i xs)
    patchOne xs_ (ModifyAt i da)  = fromMaybe xs_ (Array.modifyAt i (_ `patch` da) xs)

insertAt :: forall a da. Patch a da => Int -> a -> Change (WrappedArray a)
insertAt i v = toChange [InsertAt i v]

deleteAt :: forall a da. Patch a da => Int -> Change (WrappedArray a)
deleteAt i = toChange [DeleteAt i]

modifyAt :: forall a da. Patch a da => Int -> Change a -> Change (WrappedArray a)
modifyAt i c = toChange [ModifyAt i (fromChange c)]

-- | Construct an array from a single element.
singleton :: forall a da. Patch a da => Jet a -> Jet (WrappedArray a)
singleton { position, velocity } =
  { position: wrap [position]
  , velocity: toChange [ModifyAt 0 (fromChange velocity)]
  }

-- | Construct an array whose elements can change but whose length is fixed,
-- | from an array of jets.
static
  :: forall a da
   . Patch a da
  => Array (Jet a)
  -> Jet (WrappedArray a)
static xs =
  { position: wrap (Prelude.map _.position xs)
  , velocity: toChange (mapWithIndex (\i -> ModifyAt i <<< fromChange <<< _.velocity) xs)
  }

-- | Modify each array element by applying the specified function.
map
  :: forall a b da db
   . Patch a da
  => Patch b db
  => (Jet a -> Jet b)
  -> Jet (WrappedArray a)
  -> Jet (WrappedArray b)
map f { position: WrappedArray xs, velocity: dxs } =
    { position: WrappedArray (Prelude.map (_.position <<< f <<< constant) xs)
    , velocity: toChange (zipWith go xs (fromChange dxs))
    }
  where
    go _ (InsertAt i a)   = InsertAt i (f (constant a)).position
    go _ (DeleteAt i)     = DeleteAt i
    go a (ModifyAt i da)  = ModifyAt i (fromChange j.velocity) where j = f { position: a, velocity: toChange da }
