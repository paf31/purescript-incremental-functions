-- | A change structure for maps, and helper functions.

module Data.Incremental.Map
  ( IMap(..)
  , MapChanges(..)
  , MapChange(..)
  , insert
  , remove
  , updateAt
  , static
  , singleton
  , map
  , modifyAt
  , size
  , zip
  , toIArray
  ) where

import Prelude hiding (map)

import Data.Bifoldable (biany)
import Data.Bifunctor (lmap)
import Data.Filterable (filterMap)
import Data.Foldable (sum)
import Data.Incremental (class Diff, class Patch, Change, Jet, constant, diff, fromChange, patch, toChange)
import Data.Incremental.Array (ArrayChange(..), IArray)
import Data.Incremental.Eq (Atomic(..))
import Data.List (mapMaybe)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Maybe.Last (Last)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.These (These(..))
import Data.Traversable (mapAccumL)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import Prelude as Prelude

-- | A change structure for `Map` which tracks changes for each key.
newtype IMap k v = IMap (Map k v)

derive instance eqIMap :: (Eq k, Eq v) => Eq (IMap k v)

instance showIMap :: (Show k, Show v) => Show (IMap k v) where
  show (IMap m) = "(IMap " <> show m <> ")"

derive instance newtypeIMap :: Newtype (IMap k v) _

-- | A change for a single key is an addition, removal, or update.
data MapChange v dv
  = Add v
  | Remove
  | Update dv

derive instance eqMapChange :: (Eq v, Eq dv) => Eq (MapChange v dv)

instance showMapChange :: (Show v, Show dv) => Show (MapChange v dv) where
  show (Add v) = "(Add " <> show v <> ")"
  show Remove = "Remove"
  show (Update dv) = "(Update " <> show dv <> ")"

-- | A change for each possible key.
newtype MapChanges k v dv = MapChanges (Map k (MapChange v dv))

derive instance eqMapChanges :: (Eq k, Eq v, Eq dv) => Eq (MapChanges k v dv)
derive instance newtypeMapChanges :: Newtype (MapChanges k v dv) _

instance showMapChanges :: (Show k, Show v, Show dv) => Show (MapChanges k v dv) where
  show (MapChanges m) = "(MapChanges " <> show m <> ")"

instance semigroupMapChanges :: (Ord k, Patch v dv) => Semigroup (MapChanges k v dv) where
  append (MapChanges m1) (MapChanges m2) = MapChanges (Map.unionWith combine m1 m2) where
    combine _ (Add v) = Add v
    combine _ Remove = Remove
    combine (Add v) (Update dv) = Add (patch v dv)
    combine Remove (Update _) = Remove
    combine (Update dv1) (Update dv2) = Update (dv1 <> dv2)

instance monoidMapChanges :: (Ord k, Patch v dv) => Monoid (MapChanges k v dv) where
  mempty = MapChanges Map.empty

instance patchIMap
    :: (Ord k, Patch v dv)
    => Patch (IMap k v) (MapChanges k v dv) where
  patch (IMap m1) (MapChanges m2) =
    IMap <<< Map.fromFoldable <<< mapMaybe (\(Tuple k v) -> Tuple k <$> v) <<< Map.toUnfoldable $ align m1 m2 <#>
      case _ of
        This x -> Just x
        That (Add v) -> Just v
        Both _ (Add v) -> Just v
        Both v (Update dv) -> Just (patch v dv)
        _ -> Nothing

instance diffIMap
    :: (Ord k, Diff v dv)
    => Diff (IMap k v) (MapChanges k v dv) where
  diff (IMap m1) (IMap m2) = MapChanges $ align m1 m2 <#>
    case _ of
      This x -> Remove
      That y -> Add y
      Both x y -> Update (diff x y)

insert :: forall k v dv. Ord k => Patch v dv => k -> v -> Change (IMap k v)
insert k v = toChange (wrap (Map.singleton k (Add v)))

remove :: forall k v dv. Ord k => Patch v dv => k -> Change (IMap k v)
remove k = toChange (wrap (Map.singleton k Remove))

updateAt :: forall k v dv. Ord k => Patch v dv => k -> Change v -> Change (IMap k v)
updateAt k c = toChange (wrap (Map.singleton k (Update (fromChange c))))

-- | Construct a map from a key/value pair.
singleton :: forall k v dv. Ord k => Patch v dv => k -> Jet v -> Jet (IMap k v)
singleton k v = static (Map.singleton k v)

-- | Construct a map whose values can change but whose keys are fixed.
static
  :: forall k v dv
   . Ord k
  => Patch v dv
  => Map.Map k (Jet v)
  -> Jet (IMap k v)
static xs =
  { position: wrap (Prelude.map _.position xs)
  , velocity: toChange (MapChanges (Prelude.map (Update <<< fromChange <<< _.velocity) xs))
  }

-- | Update a single key by applying a function.
modifyAt
  :: forall k v dv
   . Ord k
  => Patch v dv
  => k
  -> (Jet v -> Jet v)
  -> Jet (IMap k v)
  -> Jet (IMap k v)
modifyAt k f { position: IMap m, velocity: dm } =
    { position: IMap (Map.update (Just <<< _.position <<< f <<< constant) k m)
    , velocity: toChange (MapChanges (Map.update (Just <<< go (Map.lookup k m)) k (unwrap (fromChange dm))))
    }
  where
    go _        (Add v)     = Add (patch v (fromChange j.velocity)) where j = f (constant v)
    go _        Remove      = Remove
    go (Just v) (Update dv) = Update (fromChange j.velocity) where j = f { position: v, velocity: toChange dv }
    go _        (Update _)  = Update mempty

-- | Update every key by applying a function.
map
  :: forall k a da b db
   . Ord k
  => Patch a da
  => Patch b db
  => (Jet a -> Jet b)
  -> Jet (IMap k a)
  -> Jet (IMap k b)
map f { position: IMap m, velocity: dm } =
    { position: IMap (Prelude.map (_.position <<< f <<< constant) m)
    , velocity: toChange (MapChanges (go <$> align m (unwrap (fromChange dm))))
    }
  where
    go :: These a (MapChange a da) -> MapChange b db
    go (This v)             = Update (fromChange j.velocity) where j = f (constant v)
    go (That (Add v))       = Add (patch j.position (fromChange j.velocity)) where j = f (constant v)
    go (Both _ Remove)      = Remove
    go (Both v (Update dv)) = Update (fromChange j.velocity) where j = f { position: v, velocity: toChange dv }
    go _                    = Update mempty

-- | Compute the size of an `IMap`, incrementally.
size
  :: forall k a da
   . Ord k
  => Patch a da
  => Jet (IMap k a)
  -> Jet (Atomic Int)
size { position: IMap m, velocity: dm } =
    { position: wrap cur
    , velocity: toChange (pure (cur + sum (Prelude.map sizeOf (align m (unwrap (fromChange dm))))))
    }
  where
    cur = Map.size m

    sizeOf :: These a (MapChange a da) -> Int
    sizeOf (Both _ Remove) = -1
    sizeOf (That (Add _)) = 1
    sizeOf _ = 0

-- | Zip two maps, keeping those keys which are common to _both_ input maps.
zip
  :: forall k a da b db
   . Ord k
  => Patch a da
  => Patch b db
  => Jet (IMap k a)
  -> Jet (IMap k b)
  -> Jet (IMap k (Tuple a b))
zip { position: IMap m1, velocity: dm1 } { position: IMap m2, velocity: dm2 } =
  let z = zipMap m1 m2

      isRemove :: forall x dx. MapChange x dx -> Boolean
      isRemove Remove = true
      isRemove _ = false

      go :: These (Tuple a b) (These (MapChange a da) (MapChange b db))
         -> Maybe (MapChange (Tuple a b) (Tuple da db))
      go (That (Both (Add a) (Add b)))           = Just (Add (Tuple a b))
      go (Both _ e) | biany isRemove isRemove e  = Just Remove
      go (Both _ (This (Update da)))             = Just (Update (Tuple da mempty))
      go (Both _ (That (Update db)))             = Just (Update (Tuple mempty db))
      go (Both _ (Both (Update da) (Update db))) = Just (Update (Tuple da db))
      go _                                       = Nothing
   in { position: IMap z
      , velocity: toChange (MapChanges (filterMap go (z `align` (unwrap (fromChange dm1) `align` unwrap (fromChange dm2)))))
      }

-- | Convert an `IMap` into an `IArray` of tuples of keys and values, in order,
-- | incrementally.
toIArray
  :: forall k a da
   . Ord k
  => Patch a da
  => Jet (IMap k a)
  -> Jet (IArray (Tuple (Atomic k) a))
toIArray { position, velocity } =
    { position: wrap (Prelude.map (lmap Atomic) (Map.toUnfoldable (unwrap position)))
    , velocity: toChange (mapAccumL go 0 (Map.toUnfoldable (unwrap (fromChange velocity)))).value
    }
  where
    indexOf :: forall x. k -> Map k x -> Int
    indexOf k m = unwrap (Map.foldSubmap Nothing (Just k) (\_ _ -> Additive 1) m) - 1

    go :: Int
       -> Tuple k (MapChange a da)
       -> { accum :: Int
          , value :: ArrayChange (Tuple (Atomic k) a) (Tuple (Last k) da)
          }
    go n (Tuple k (Add a)) =
      { accum: n + 1
      , value: InsertAt (n + indexOf k (unwrap position) + 1) (Tuple (Atomic k) a)
      }
    go n (Tuple k Remove) =
      { accum: n - 1
      , value: DeleteAt (n + indexOf k (unwrap position))
      }
    go n (Tuple k (Update da)) =
      { accum: n
      , value: ModifyAt (indexOf k (unwrap position)) (Tuple mempty da)
      }

-- Helpers

align :: forall k a b. Ord k => Map k a -> Map k b -> Map k (These a b)
align xs ys =
  Map.unionWith
    (unsafePartial \(This x) (That y) -> Both x y)
    (Prelude.map This xs)
    (Prelude.map That ys)

zipMap
  :: forall k a b
   . Ord k
  => Map k a
  -> Map k b
  -> Map k (Tuple a b)
zipMap m1 m2 = filterMap go (align m1 m2) where
  go (Both a b) = Just (Tuple a b)
  go _ = Nothing
