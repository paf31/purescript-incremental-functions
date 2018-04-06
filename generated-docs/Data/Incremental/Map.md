## Module Data.Incremental.Map

A change structure for maps, and helper functions.

#### `IMap`

``` purescript
newtype IMap k v
  = IMap (Map k v)
```

A change structure for `Map` which tracks changes for each key.

##### Instances
``` purescript
(Eq k, Eq v) => Eq (IMap k v)
(Show k, Show v) => Show (IMap k v)
Newtype (IMap k v) _
(Ord k, Patch v dv) => Patch (IMap k v) (MapChanges k v dv)
(Ord k, Diff v dv) => Diff (IMap k v) (MapChanges k v dv)
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
(Ord k, Patch v dv) => Patch (IMap k v) (MapChanges k v dv)
(Ord k, Diff v dv) => Diff (IMap k v) (MapChanges k v dv)
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
insert :: forall k v dv. Ord k => Patch v dv => k -> v -> Change (IMap k v)
```

#### `remove`

``` purescript
remove :: forall k v dv. Ord k => Patch v dv => k -> Change (IMap k v)
```

#### `updateAt`

``` purescript
updateAt :: forall k v dv. Ord k => Patch v dv => k -> Change v -> Change (IMap k v)
```

#### `static`

``` purescript
static :: forall k v dv. Ord k => Patch v dv => Map k (Jet v) -> Jet (IMap k v)
```

Construct a map whose values can change but whose keys are fixed.

#### `singleton`

``` purescript
singleton :: forall k v dv. Ord k => Patch v dv => k -> Jet v -> Jet (IMap k v)
```

Construct a map from a key/value pair.

#### `map`

``` purescript
map :: forall k a da b db. Ord k => Patch a da => Patch b db => (Jet a -> Jet b) -> Jet (IMap k a) -> Jet (IMap k b)
```

Update every key by applying a function.

#### `modifyAt`

``` purescript
modifyAt :: forall k v dv. Ord k => Patch v dv => k -> (Jet v -> Jet v) -> Jet (IMap k v) -> Jet (IMap k v)
```

Update a single key by applying a function.

#### `size`

``` purescript
size :: forall k a da. Ord k => Patch a da => Jet (IMap k a) -> Jet (Atomic Int)
```

Compute the size of an `IMap`, incrementally.

#### `zip`

``` purescript
zip :: forall k a da b db. Ord k => Patch a da => Patch b db => Jet (IMap k a) -> Jet (IMap k b) -> Jet (IMap k (Tuple a b))
```

Zip two maps, keeping those keys which are common to _both_ input maps.

#### `toIArray`

``` purescript
toIArray :: forall k a da. Ord k => Patch a da => Jet (IMap k a) -> Jet (IArray (Tuple (Atomic k) a))
```

Convert an `IMap` into an `IArray` of tuples of keys and values, in order,
incrementally.


