## Module Data.Incremental.Map

A change structure for maps, and helper functions.

#### `WrappedMap`

``` purescript
newtype WrappedMap k v
  = WrappedMap (Map k v)
```

A change structure for `Map` which tracks changes for each key.

##### Instances
``` purescript
(Show k, Show v) => Show (WrappedMap k v)
Newtype (WrappedMap k v) _
(Ord k, ChangeStructure v dv) => ChangeStructure (WrappedMap k v) (MapChanges k v dv)
```

#### `MapChange`

``` purescript
data MapChange v dv
  = Add v
  | Remove
  | Update dv
```

A change for a single key is an addition, removal, or update.

##### Instances
``` purescript
(Show v, Show dv) => Show (MapChange v dv)
```

#### `MapChanges`

``` purescript
newtype MapChanges k v dv
  = MapChanges (Map k (MapChange v dv))
```

A change for each possible key.

##### Instances
``` purescript
Newtype (MapChanges k v dv) _
(Show k, Show v, Show dv) => Show (MapChanges k v dv)
(Ord k, ChangeStructure v dv) => Semigroup (MapChanges k v dv)
(Ord k, ChangeStructure v dv) => Monoid (MapChanges k v dv)
(Ord k, ChangeStructure v dv) => ChangeStructure (WrappedMap k v) (MapChanges k v dv)
```

#### `key`

``` purescript
key :: forall k v dv. Ord k => ChangeStructure v dv => k -> D1 ((v -> v) -> WrappedMap k v -> WrappedMap k v)
```

Update a single key by applying a function.

#### `each`

``` purescript
each :: forall k v dv. Ord k => ChangeStructure v dv => D1 ((v -> v) -> WrappedMap k v -> WrappedMap k v)
```

Update every key by applying a function.


