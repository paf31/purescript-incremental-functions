## Module Data.Incremental.Array

#### `IArray`

``` purescript
newtype IArray a
  = IArray (Array a)
```

##### Instances
``` purescript
(Eq a) => Eq (IArray a)
(Show a) => Show (IArray a)
Newtype (IArray a) _
(Patch a da) => Patch (IArray a) (Array (ArrayChange a da))
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
(Patch a da) => Patch (IArray a) (Array (ArrayChange a da))
```

#### `insertAt`

``` purescript
insertAt :: forall a da. Patch a da => Int -> a -> Change (IArray a)
```

#### `deleteAt`

``` purescript
deleteAt :: forall a da. Patch a da => Int -> Change (IArray a)
```

#### `modifyAt`

``` purescript
modifyAt :: forall a da. Patch a da => Int -> Change a -> Change (IArray a)
```

#### `length`

``` purescript
length :: forall a da. Patch a da => Jet (IArray a) -> Jet (Atomic Int)
```

Compute the length of the array incrementally.

#### `map`

``` purescript
map :: forall a b da db. Patch a da => Patch b db => (Jet a -> Jet b) -> Jet (IArray a) -> Jet (IArray b)
```

Modify each array element by applying the specified function.

_Note_: The function itself must not change over time.

#### `singleton`

``` purescript
singleton :: forall a da. Patch a da => Jet a -> Jet (IArray a)
```

Construct an array from a single element.

#### `static`

``` purescript
static :: forall a da. Patch a da => Array (Jet a) -> Jet (IArray a)
```

Construct an array whose elements can change but whose length is fixed,
from an array of jets.


