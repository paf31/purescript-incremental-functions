module Data.Incremental.Array
  ( IArray(..)
  , ArrayChange(..)
  , insertAt
  , deleteAt
  , modifyAt
  , length
  , map
  , singleton
  , static
  ) where

import Prelude hiding (map)

import Data.Array (foldl, mapMaybe, mapWithIndex, null, (!!))
import Data.Array as Array
import Data.Foldable (foldMap)
import Data.Incremental (class Patch, Change, Jet, constant, fromChange, patch, toChange)
import Data.Incremental.Eq (Atomic)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Maybe.Last (Last(..))
import Data.Monoid (mempty)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Prelude as Prelude

newtype IArray a = IArray (Array a)

derive instance eqIArray :: Eq a => Eq (IArray a)

instance showIArray :: Show a => Show (IArray a) where
  show (IArray xs) = "(IArray " <> show xs <> ")"

derive instance newtypeIArray :: Newtype (IArray a) _

data ArrayChange a da
  = InsertAt Int a
  | DeleteAt Int
  | ModifyAt Int da

derive instance eqArrayChange :: (Eq a, Eq da) => Eq (ArrayChange a da)

instance showArrayChange :: (Show a, Show da) => Show (ArrayChange a da) where
  show (InsertAt i a) = "(InsertAt " <> show i <> " " <> show a <> ")"
  show (DeleteAt i) = "(DeleteAt " <> show i <> ")"
  show (ModifyAt i da) = "(ModifyAt " <> show i <> " " <> show da <> ")"

instance patchIArray
    :: Patch a da
    => Patch (IArray a) (Array (ArrayChange a da)) where
  patch (IArray xs) = IArray <<< foldl patchOne xs where
    patchOne xs_ (InsertAt i x)   = fromMaybe xs_ (Array.insertAt i x xs_)
    patchOne xs_ (DeleteAt i)     = fromMaybe xs_ (Array.deleteAt i xs_)
    patchOne xs_ (ModifyAt i da)  = fromMaybe xs_ (Array.modifyAt i (_ `patch` da) xs_)

insertAt :: forall a da. Patch a da => Int -> a -> Change (IArray a)
insertAt i v = toChange [InsertAt i v]

deleteAt :: forall a da. Patch a da => Int -> Change (IArray a)
deleteAt i = toChange [DeleteAt i]

modifyAt :: forall a da. Patch a da => Int -> Change a -> Change (IArray a)
modifyAt i c = toChange [ModifyAt i (fromChange c)]

-- | Construct an array from a single element.
singleton :: forall a da. Patch a da => Jet a -> Jet (IArray a)
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
  -> Jet (IArray a)
static xs =
  { position: wrap (Prelude.map _.position xs)
  , velocity: toChange (mapWithIndex (\i -> ModifyAt i <<< fromChange <<< _.velocity) xs)
  }

-- | Compute the length of the array incrementally.
length
  :: forall a da
   . Patch a da
  => Jet (IArray a)
  -> Jet (Atomic Int)
length { position, velocity } =
    { position: wrap (Array.length (unwrap position))
    , velocity: toChange (additiveToLast (foldMap go (fromChange velocity)))
    }
  where
    go (InsertAt _ _) = Additive 1
    go (DeleteAt _) | not (null (unwrap position)) = Additive (-1)
    go _ = mempty

    additiveToLast (Additive 0) = mempty
    additiveToLast (Additive n) = Last (Just (Array.length (unwrap position) + n))

-- | Modify each array element by applying the specified function.
-- |
-- | _Note_: The function itself must not change over time.
map
  :: forall a b da db
   . Patch a da
  => Patch b db
  => (Jet a -> Jet b)
  -> Jet (IArray a)
  -> Jet (IArray b)
map f { position: IArray xs, velocity: dxs } =
    { position: IArray (Prelude.map (_.position <<< f <<< constant) xs)
    , velocity: toChange (mapMaybe go (fromChange dxs))
    }
  where
    go (InsertAt i a)   = Just (InsertAt i (f (constant a)).position)
    go (DeleteAt i)     = Just (DeleteAt i)
    go (ModifyAt i da)  = (xs !! i) <#> \a ->
      let j = f { position: a, velocity: toChange da }
       in ModifyAt i (fromChange j.velocity)
