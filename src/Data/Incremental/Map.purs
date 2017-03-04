-- | A change structure for maps, and helper functions.

module Data.Incremental.Map
  ( WrappedMap(..)
  , MapChange(..)
  , MapChanges(..)
  , key
  , each
  ) where

import Prelude
import Data.Map as Map
import Data.Incremental (class ChangeStructure, Change, D1(..), diff, fromChange, lam, patch, runFunctionChange, toChange)
import Data.List (mapMaybe)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype, unwrap)
import Data.These (These(..))
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)

-- TODO: this really belongs in `purescript-these`.
align :: forall k a b. Ord k => Map k a -> Map k b -> Map k (These a b)
align xs ys =
  Map.unionWith
    (unsafePartial \(This x) (That y) -> Both x y)
    (map This xs)
    (map That ys)

-- | A change structure for `Map` which tracks changes for each key.
newtype WrappedMap k v = WrappedMap (Map k v)

instance showWrappedMap :: (Show k, Show v) => Show (WrappedMap k v) where
  show (WrappedMap m) = "(WrappedMap " <> show m <> ")"

derive instance newtypeWrappedMap :: Newtype (WrappedMap k v) _

-- | A change for a single key is an addition, removal, or update.
data MapChange v dv
  = Add v
  | Remove
  | Update dv

instance showMapChange :: (Show v, Show dv) => Show (MapChange v dv) where
  show (Add v) = "(Add " <> show v <> ")"
  show Remove = "Remove"
  show (Update dv) = "(Update " <> show dv <> ")"

-- | A change for each possible key.
newtype MapChanges k v dv = MapChanges (Map k (MapChange v dv))

derive instance newtypeMapChanges :: Newtype (MapChanges k v dv) _

instance showMapChanges :: (Show k, Show v, Show dv) => Show (MapChanges k v dv) where
  show (MapChanges m) = "(MapChanges " <> show m <> ")"

instance semigroupMapChanges :: (Ord k, ChangeStructure v dv) => Semigroup (MapChanges k v dv) where
  append (MapChanges m1) (MapChanges m2) = MapChanges (Map.unionWith combine m1 m2) where
    combine _ (Add v) = Add v
    combine _ Remove = Remove
    combine (Add v) (Update dv) = Add (patch v dv)
    combine Remove (Update _) = Remove
    combine (Update dv1) (Update dv2) = Update (dv1 <> dv2)

instance monoidMapChanges :: (Ord k, ChangeStructure v dv) => Monoid (MapChanges k v dv) where
  mempty = MapChanges Map.empty

instance changeStructureMap
  :: (Ord k, ChangeStructure v dv)
  => ChangeStructure (WrappedMap k v) (MapChanges k v dv) where
  diff (WrappedMap m1) (WrappedMap m2) = MapChanges $ align m1 m2 <#>
    case _ of
      This x -> Remove
      That y -> Add y
      Both x y -> Update (diff x y)
  patch (WrappedMap m1) (MapChanges m2) =
    WrappedMap <<< Map.fromFoldable <<< mapMaybe (\(Tuple k v) -> Tuple k <$> v) <<< Map.toUnfoldable $ align m1 m2 <#>
      case _ of
        This x -> Just x
        That (Add v) -> Just v
        Both _ (Add v) -> Just v
        Both v (Update dv) -> Just (patch v dv)
        _ -> Nothing

-- | Update a single key by applying a function.
key :: forall k v dv. (Ord k, ChangeStructure v dv) => k -> D1 ((v -> v) -> WrappedMap k v -> WrappedMap k v)
key k =
  lam \(D1 f df) ->
    lam \(D1 (WrappedMap m) dm) ->
      D1 (WrappedMap (Map.update (Just <<< f) k m))
         (toChange (MapChanges (Map.update (\dv -> map (\v -> changeMap df v dv) (Map.lookup k m)) k (unwrap (fromChange dm)))))

-- | Update every key by applying a function.
each :: forall k v dv. (Ord k, ChangeStructure v dv) => D1 ((v -> v) -> WrappedMap k v -> WrappedMap k v)
each =
    lam \(D1 f df) ->
      lam \(D1 (WrappedMap m) dm) ->
        D1 (WrappedMap (map f m))
           (toChange (MapChanges (applyChange (fromChange df) <$> align m (unwrap (fromChange dm)))))
  where
    applyChange df (This v) = Update (runFunctionChange df v mempty)
    applyChange df (That (Add v)) = Add (patch v (runFunctionChange df v mempty))
    applyChange df (That Remove) = Remove
    applyChange df (That (Update dv)) = Update dv
    applyChange df (Both _ (Add v)) = Add (patch v (runFunctionChange df v mempty))
    applyChange df (Both _ Remove) = Remove
    applyChange df (Both v (Update dv)) = Update (runFunctionChange df v dv)

changeMap :: forall v dv. ChangeStructure v dv => Change (v -> v) -> v -> MapChange v dv -> MapChange v dv
changeMap df _ (Add v) = Add (patch v (runFunctionChange (fromChange df) v mempty))
changeMap df _ Remove = Remove
changeMap df v (Update dv) = Update (runFunctionChange (fromChange df) v dv)
