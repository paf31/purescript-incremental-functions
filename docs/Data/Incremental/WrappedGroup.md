## Module Data.Incremental.WrappedGroup

#### `WrappedGroup`

``` purescript
newtype WrappedGroup g
  = WrappedGroup g
```

A change structure for any abelian group.

##### Instances
``` purescript
(Semigroup g) => Semigroup (WrappedGroup g)
(Monoid g) => Monoid (WrappedGroup g)
(Group g) => Group (WrappedGroup g)
(CommutativeGroup g) => CommutativeGroup (WrappedGroup g)
(Eq g) => Eq (WrappedGroup g)
(Ord g) => Ord (WrappedGroup g)
Newtype (WrappedGroup g) _
(Show g) => Show (WrappedGroup g)
(CommutativeGroup g) => ChangeStructure (WrappedGroup g) (WrappedGroup g)
```


