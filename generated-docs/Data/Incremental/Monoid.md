## Module Data.Incremental.Monoid

#### `Left`

``` purescript
newtype Left a
  = Left a
```

A change structure for any monoid, with the `Dual` monoid acting by
appending on the left.

##### Instances
``` purescript
(Eq a) => Eq (Left a)
(Ord a) => Ord (Left a)
Newtype (Left a) _
(Show a) => Show (Left a)
(Monoid a) => Patch (Left a) (Dual a)
```

#### `appendLeft`

``` purescript
appendLeft :: forall a. Monoid a => a -> Change (Left a)
```

Change by appending a value on the left.

#### `Right`

``` purescript
newtype Right a
  = Right a
```

A change structure for any monoid, acting on itself by appending on the right.

##### Instances
``` purescript
(Semigroup a) => Semigroup (Right a)
(Monoid a) => Monoid (Right a)
(Eq a) => Eq (Right a)
(Ord a) => Ord (Right a)
Newtype (Right a) _
(Show a) => Show (Right a)
(Monoid a) => Patch (Right a) (Right a)
```

#### `appendRight`

``` purescript
appendRight :: forall a. Monoid a => a -> Change (Right a)
```

Change by appending a value on the right.


