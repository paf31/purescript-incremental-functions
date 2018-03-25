## Module Data.Incremental.Eq

#### `WrappedEq`

``` purescript
newtype WrappedEq a
  = WrappedEq a
```

A change structure for any type with equality.

##### Instances
``` purescript
(Eq a) => Eq (WrappedEq a)
(Ord a) => Ord (WrappedEq a)
Newtype (WrappedEq a) _
(Show a) => Show (WrappedEq a)
Patch (WrappedEq a) (Last a)
(Eq a) => Diff (WrappedEq a) (Last a)
```

#### `replace`

``` purescript
replace :: forall a. a -> Change (WrappedEq a)
```

Change by replacing the current value.

#### `mapEq`

``` purescript
mapEq :: forall a b. (a -> b) -> Jet (WrappedEq a) -> Jet (WrappedEq b)
```


