## Module Data.Incremental.Monoid

#### `WrappedMonoid`

``` purescript
newtype WrappedMonoid a
  = WrappedMonoid a
```

A change structure for any monoid, acting on itself by appending on the right.

##### Instances
``` purescript
(Semigroup a) => Semigroup (WrappedMonoid a)
(Monoid a) => Monoid (WrappedMonoid a)
(Eq a) => Eq (WrappedMonoid a)
(Ord a) => Ord (WrappedMonoid a)
Newtype (WrappedMonoid a) _
(Show a) => Show (WrappedMonoid a)
(Monoid a) => Patch (WrappedMonoid a) (WrappedMonoid a)
```

#### `appending`

``` purescript
appending :: forall a. Monoid a => a -> Change (WrappedMonoid a)
```

Change by appending a value on the right.


