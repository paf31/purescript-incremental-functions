module Data.Incremental.Array
  ( IArray(..)
  , ArrayChange(..)
  , insertAt
  , deleteAt
  , modifyAt
  , length
  , map
  , mapWithIndex
  , singleton
  , static
  , withIndex
  ) where

import Prelude hiding (map)

import Data.Array ((:), (!!))
import Data.Array as Array
import Data.Enum (enumFromTo)
import Data.Foldable (foldl)
import Data.Incremental (class Patch, Change, Jet, constant, fromChange, patch, toChange)
import Data.Incremental.Eq (Atomic(..))
import Data.Incremental.Tuple (uncurry)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Traversable (mapAccumL)
import Data.Tuple (Tuple(..))
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
  , velocity: toChange (Array.mapWithIndex (\i -> ModifyAt i <<< fromChange <<< _.velocity) xs)
  }

-- | Compute the length of the array incrementally.
length
  :: forall a da
   . Patch a da
  => Jet (IArray a)
  -> Jet (Atomic Int)
length { position, velocity } =
    { position: wrap len0
    , velocity: toChange (pure (foldl go len0 (fromChange velocity)))
    }
  where
    len0 = Array.length (unwrap position)

    go len (InsertAt _ _) = len + 1
    go len (DeleteAt _) | len > 0 = len - 1
    go len _ = len

-- | Modify each array element by applying the specified function.
map
  :: forall a b da db
   . Patch a da
  => Patch b db
  => (Jet a -> Jet b)
  -> Jet (IArray a)
  -> Jet (IArray b)
map f { position: IArray xs, velocity: dxs } =
    { position: IArray (Prelude.map f0 xs)
    , velocity: toChange (f_updates <> xs_updates)
    }
  where
    f0 = _.position <<< f <<< constant
    f1 position velocity = (f { position, velocity }).velocity

    -- Changes originating from changes in f
    f_updates :: Array (ArrayChange b db)
    f_updates = Array.mapWithIndex (\i a -> ModifyAt i (fromChange (f (constant a)).velocity)) xs

    -- Changes originating from changes in xs
    xs_updates :: Array (ArrayChange b db)
    xs_updates = Array.catMaybes (mapAccumL go xs (fromChange dxs)).value

    go :: Array a -> ArrayChange a da -> { accum :: Array a, value :: Maybe (ArrayChange b db) }
    go xs_ (InsertAt i a) =
      { accum: fromMaybe xs_ (Array.insertAt i a xs_)
      , value: Just (InsertAt i (f (constant a)).position)
      }
    go xs_ (DeleteAt i) =
      { accum: fromMaybe xs_ (Array.deleteAt i xs_)
      , value: Just (DeleteAt i)
      }
    go xs_ (ModifyAt i da) =
      { accum: fromMaybe xs_ (Array.modifyAt i (_ `patch` da) xs_)
      , value: (xs_ !! i) <#> \a ->
          ModifyAt i (fromChange (f1 a (toChange da)))
      }

-- | Annotate an array with the indices of its elements.
-- |
-- | _Note_: Insertions or removals in the middle of an array will result
-- | in a cascade of modifications to the tail of the result.
withIndex
  :: forall a da
   . Patch a da
  => Jet (IArray a)
  -> Jet (IArray (Tuple (Atomic Int) a))
withIndex { position, velocity } =
    { position: wrap (Array.mapWithIndex (Tuple <<< Atomic) (unwrap position))
    , velocity: toChange (Array.fold (mapAccumL go len0 (fromChange velocity)).value)
    }
  where
    len0 = Array.length (unwrap position)

    go len (InsertAt i a) =
      { accum: len + 1
      , value: InsertAt i (Tuple (Atomic i) a)
                 : Prelude.map
                     (\j -> ModifyAt j (Tuple (pure j) mempty))
                     (enumFromTo (i + 1) len)
      }
    go len (DeleteAt i) =
      { accum: len - 1
      , value: DeleteAt i
                 : Prelude.map
                     (\j -> ModifyAt j (Tuple (pure j) mempty))
                     (enumFromTo i (len - 2))
      }
    go len (ModifyAt i da) =
      { accum: len
      , value: [ModifyAt i (Tuple mempty da)]
      }

-- | Modify each array element by applying the specified function, taking the
-- | index of each element into account.
-- |
-- | _Note_: Insertions or removals in the middle of an array will result
-- | in a cascade of modifications to the tail of the result.
mapWithIndex
  :: forall a da b db
   . Patch a da
  => Patch b db
  => (Jet (Atomic Int) -> Jet a -> Jet b)
  -> Jet (IArray a)
  -> Jet (IArray b)
mapWithIndex f = withIndex >>> map (uncurry f)
