-- | A change structure for maps, and helper functions.

module Data.Incremental.Map
  ( WrappedMap(..)
  , MapChanges(..)
  , MapChange(..)
  , insert
  , remove
  , updateAt
  , map
  , modifyAt
  ) where

import Prelude hiding (map)

import Data.Incremental (class Diff, class Patch, Change, Jet, constant, diff, fromChange, patch, toChange)
import Data.List (mapMaybe)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.These (These(..))
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import Prelude as Prelude

align :: forall k a b. Ord k => Map k a -> Map k b -> Map k (These a b)
align xs ys =
  Map.unionWith
    (unsafePartial \(This x) (That y) -> Both x y)
    (Prelude.map This xs)
    (Prelude.map That ys)

-- | A change structure for `Map` which tracks changes for each key.
newtype WrappedMap k v = WrappedMap (Map k v)

derive instance eqWrappedMap :: (Eq k, Eq v) => Eq (WrappedMap k v)

instance showWrappedMap :: (Show k, Show v) => Show (WrappedMap k v) where
  show (WrappedMap m) = "(WrappedMap " <> show m <> ")"

derive instance newtypeWrappedMap :: Newtype (WrappedMap k v) _

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

instance patchWrappedMap
    :: (Ord k, Patch v dv)
    => Patch (WrappedMap k v) (MapChanges k v dv) where
  patch (WrappedMap m1) (MapChanges m2) =
    WrappedMap <<< Map.fromFoldable <<< mapMaybe (\(Tuple k v) -> Tuple k <$> v) <<< Map.toUnfoldable $ align m1 m2 <#>
      case _ of
        This x -> Just x
        That (Add v) -> Just v
        Both _ (Add v) -> Just v
        Both v (Update dv) -> Just (patch v dv)
        _ -> Nothing

instance diffWrappedMap
    :: (Ord k, Diff v dv)
    => Diff (WrappedMap k v) (MapChanges k v dv) where
  diff (WrappedMap m1) (WrappedMap m2) = MapChanges $ align m1 m2 <#>
    case _ of
      This x -> Remove
      That y -> Add y
      Both x y -> Update (diff x y)

insert :: forall k v dv. Ord k => Patch v dv => k -> v -> Change (WrappedMap k v)
insert k v = toChange (wrap (Map.singleton k (Add v)))

remove :: forall k v dv. Ord k => Patch v dv => k -> Change (WrappedMap k v)
remove k = toChange (wrap (Map.singleton k Remove))

updateAt :: forall k v dv. Ord k => Patch v dv => k -> Change v -> Change (WrappedMap k v)
updateAt k c = toChange (wrap (Map.singleton k (Update (fromChange c))))

-- | Update a single key by applying a function.
modifyAt
  :: forall k v dv
   . Ord k
  => Patch v dv
  => k
  -> (Jet v -> Jet v)
  -> Jet (WrappedMap k v)
  -> Jet (WrappedMap k v)
modifyAt k f { position: WrappedMap m, velocity: dm } =
    { position: WrappedMap (Map.update (Just <<< _.position <<< f <<< constant) k m)
    , velocity: toChange (MapChanges (Map.update (Just <<< go (Map.lookup k m)) k (unwrap (fromChange dm))))
    }
  where
    go _        (Add v)     = Add (patch v (fromChange j.velocity)) where j = f (constant v)
    go _        Remove      = Remove
    go (Just v) (Update dv) = Update (fromChange j.velocity) where j = f { position: v, velocity: toChange dv }
    go _        (Update _)  = Update mempty

-- | Update every key by applying a function.
map
  :: forall k v dv
   . Ord k
  => Patch v dv
  => (Jet v -> Jet v)
  -> Jet (WrappedMap k v)
  -> Jet (WrappedMap k v)
map f { position: WrappedMap m, velocity: dm } =
    { position: WrappedMap (Prelude.map (_.position <<< f <<< constant) m)
    , velocity: toChange (MapChanges (go <$> align m (unwrap (fromChange dm))))
    }
  where
    go (This v)             = Update (fromChange j.velocity) where j = f (constant v)
    go (That (Add v))       = Add (patch v (fromChange j.velocity)) where j = f (constant v)
    go (That Remove)        = Remove
    go (That (Update dv))   = Update dv
    go (Both _ (Add v))     = Add (patch v (fromChange j.velocity)) where j = f (constant v)
    go (Both _ Remove)      = Remove
    go (Both v (Update dv)) = Update (fromChange j.velocity) where j = f { position: v, velocity: toChange dv }
