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

#### `map`

``` purescript
map :: forall a b. (a -> b) -> Jet (Atomic a) -> Jet (Atomic b)
```

Change an `Atomic` value using a regular function.

#### `lift2`

``` purescript
lift2 :: forall a b c. (a -> b -> c) -> Jet (Atomic a) -> Jet (Atomic b) -> Jet (Atomic c)
```

Combine two `Atomic` values using a regular function.

_Note_: The result will change (entirely) if either argument
changes. If changes should be independent, consider using a `Tuple`
instead.

#### `apply`

``` purescript
apply :: forall a b. Jet (Atomic (a -> b)) -> Jet (Atomic a) -> Jet (Atomic b)
```

Combine two `Atomic` values in an applicative style.

#### `mapAtomic`

``` purescript
mapAtomic :: forall a b. (a -> b) -> Jet (Atomic a) -> Jet (Atomic b)
```

Change an `Atomic` value using a regular function.

This alias for `map` will be removed in a future version.


