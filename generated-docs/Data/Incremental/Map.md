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
(Eq k, Eq v) => Eq (WrappedMap k v)
(Show k, Show v) => Show (WrappedMap k v)
Newtype (WrappedMap k v) _
(Ord k, Patch v dv) => Patch (WrappedMap k v) (MapChanges k v dv)
(Ord k, Diff v dv) => Diff (WrappedMap k v) (MapChanges k v dv)
```

#### `MapChanges`

``` purescript
newtype MapChanges k v dv
  = MapChanges (Map k (MapChange v dv))
```

A change for each possible key.

##### Instances
``` purescript
(Eq k, Eq v, Eq dv) => Eq (MapChanges k v dv)
Newtype (MapChanges k v dv) _
(Show k, Show v, Show dv) => Show (MapChanges k v dv)
(Ord k, Patch v dv) => Semigroup (MapChanges k v dv)
(Ord k, Patch v dv) => Monoid (MapChanges k v dv)
(Ord k, Patch v dv) => Patch (WrappedMap k v) (MapChanges k v dv)
(Ord k, Diff v dv) => Diff (WrappedMap k v) (MapChanges k v dv)
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
(Eq v, Eq dv) => Eq (MapChange v dv)
(Show v, Show dv) => Show (MapChange v dv)
```

#### `insert`

``` purescript
insert :: forall k v dv. Ord k => Patch v dv => k -> v -> Change (WrappedMap k v)
```

#### `remove`

``` purescript
remove :: forall k v dv. Ord k => Patch v dv => k -> Change (WrappedMap k v)
```

#### `updateAt`

``` purescript
updateAt :: forall k v dv. Ord k => Patch v dv => k -> Change v -> Change (WrappedMap k v)
```

#### `map`

``` purescript
map :: forall k v dv. Ord k => Patch v dv => (Jet v -> Jet v) -> Jet (WrappedMap k v) -> Jet (WrappedMap k v)
```

Update every key by applying a function.

#### `modifyAt`

``` purescript
modifyAt :: forall k v dv. Ord k => Patch v dv => k -> (Jet v -> Jet v) -> Jet (WrappedMap k v) -> Jet (WrappedMap k v)
```

Update a single key by applying a function.


