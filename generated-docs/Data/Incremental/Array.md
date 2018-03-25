## Module Data.Incremental.Array

#### `WrappedArray`

``` purescript
newtype WrappedArray a
  = WrappedArray (Array a)
```

##### Instances
``` purescript
(Eq a) => Eq (WrappedArray a)
(Show a) => Show (WrappedArray a)
Newtype (WrappedArray a) _
(Patch a da) => Patch (WrappedArray a) (Array (ArrayChange a da))
```

#### `ArrayChange`

``` purescript
data ArrayChange a da
  = InsertAt Int a
  | DeleteAt Int
  | ModifyAt Int da
```

##### Instances
``` purescript
(Eq a, Eq da) => Eq (ArrayChange a da)
(Show a, Show da) => Show (ArrayChange a da)
(Patch a da) => Patch (WrappedArray a) (Array (ArrayChange a da))
```

#### `insertAt`

``` purescript
insertAt :: forall a da. Patch a da => Int -> a -> Change (WrappedArray a)
```

#### `deleteAt`

``` purescript
deleteAt :: forall a da. Patch a da => Int -> Change (WrappedArray a)
```

#### `modifyAt`

``` purescript
modifyAt :: forall a da. Patch a da => Int -> Change a -> Change (WrappedArray a)
```

#### `map`

``` purescript
map :: forall a b da db. Patch a da => Patch b db => (Jet a -> Jet b) -> Jet (WrappedArray a) -> Jet (WrappedArray b)
```

Modify each array element by applying the specified function.

#### `singleton`

``` purescript
singleton :: forall a da. Patch a da => Jet a -> Jet (WrappedArray a)
```

Construct an array from a single element.

#### `static`

``` purescript
static :: forall a da. Patch a da => Array (Jet a) -> Jet (WrappedArray a)
```

Construct an array whose elements can change but whose length is fixed,
from an array of jets.


