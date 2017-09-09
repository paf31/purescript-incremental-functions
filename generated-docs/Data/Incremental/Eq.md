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
(Eq a) => ChangeStructure (WrappedEq a) (Last a)
```


