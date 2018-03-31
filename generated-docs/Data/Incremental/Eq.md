## Module Data.Incremental.Eq

#### `Atomic`

``` purescript
newtype Atomic a
  = Atomic a
```

A change structure for any type with equality.

##### Instances
``` purescript
(Eq a) => Eq (Atomic a)
(Ord a) => Ord (Atomic a)
Newtype (Atomic a) _
(Show a) => Show (Atomic a)
Patch (Atomic a) (Last a)
(Eq a) => Diff (Atomic a) (Last a)
```

#### `replace`

``` purescript
replace :: forall a. a -> Change (Atomic a)
```

Change by replacing the current value.

#### `mapAtomic`

``` purescript
mapAtomic :: forall a b. (a -> b) -> Jet (Atomic a) -> Jet (Atomic b)
```

Change an `Atomic` value using a regular function.


