## Module Data.Incremental.Abelian

#### `Group`

``` purescript
class (Monoid g) <= Group g  where
  inverse :: g -> g
```

A `Group` is a `Monoid` with inverses.

Laws:

- _Left inverse_: `inverse x <> x = mempty`
- _Right inverse_: `x <> inverse x = mempty`

##### Instances
``` purescript
(Ring a) => Group (Additive a)
(Group g) => Group (WrappedAbelian g)
```

#### `Abelian`

``` purescript
class (Group g) <= Abelian g 
```

An `Abelian` group is a `Group` whose `append` operation also satisfies the
_commutativity law_:

- _Commutativity_ `x <> y = y <> x`

##### Instances
``` purescript
(Ring a) => Abelian (Additive a)
(Abelian g) => Abelian (WrappedAbelian g)
```

#### `subtract`

``` purescript
subtract :: forall g. Group g => g -> g -> g
```

Subtraction in a group.

#### `WrappedAbelian`

``` purescript
newtype WrappedAbelian g
  = WrappedAbelian g
```

A change structure for any abelian group.

##### Instances
``` purescript
(Semigroup g) => Semigroup (WrappedAbelian g)
(Monoid g) => Monoid (WrappedAbelian g)
(Group g) => Group (WrappedAbelian g)
(Abelian g) => Abelian (WrappedAbelian g)
(Eq g) => Eq (WrappedAbelian g)
(Ord g) => Ord (WrappedAbelian g)
Newtype (WrappedAbelian g) _
(Show g) => Show (WrappedAbelian g)
(Abelian g) => ChangeStructure (WrappedAbelian g) (WrappedAbelian g)
```


