## Module Data.Incremental.Record

#### `WrappedRecord`

``` purescript
newtype WrappedRecord r
  = WrappedRecord ({  | r })
```

##### Instances
``` purescript
Newtype (WrappedRecord a) _
(RowToList r rl, SemigroupRL rl r) => Semigroup (WrappedRecord r)
(RowToList r rl, MonoidRL rl r) => Monoid (WrappedRecord r)
(RowToList r rl, RowToList d dl, MonoidRL dl d, PatchRL r rl d dl) => Patch (WrappedRecord r) (WrappedRecord d)
```

#### `SemigroupRL`

``` purescript
class SemigroupRL (rl :: RowList) r | rl -> r where
  appendRL :: RLProxy rl -> {  | r } -> {  | r } -> {  | r }
```

##### Instances
``` purescript
SemigroupRL Nil ()
(IsSymbol l, Semigroup a, SemigroupRL rl r1, RowCons l a r1 r2, RowLacks l r1, RowLacks l r2) => SemigroupRL (Cons l a rl) r2
```

#### `MonoidRL`

``` purescript
class (SemigroupRL rl r) <= MonoidRL (rl :: RowList) r | rl -> r where
  memptyRL :: RLProxy rl -> {  | r }
```

##### Instances
``` purescript
MonoidRL Nil ()
(IsSymbol l, Monoid a, MonoidRL rl r1, RowCons l a r1 r2, RowLacks l r1, RowLacks l r2) => MonoidRL (Cons l a rl) r2
```

#### `PatchRL`

``` purescript
class (MonoidRL dl d) <= PatchRL r (rl :: RowList) d (dl :: RowList) | rl -> r, dl -> d, rl -> dl where
  patchRL :: RLProxy rl -> RLProxy dl -> {  | r } -> {  | d } -> {  | r }
```

##### Instances
``` purescript
PatchRL () Nil () Nil
(IsSymbol l, Patch a m, PatchRL r1 rl d1 dl, RowCons l a r1 r2, RowCons l m d1 d2, RowLacks l r1, RowLacks l r2, RowLacks l d1, RowLacks l d2) => PatchRL r2 (Cons l a rl) d2 (Cons l m dl)
```

#### `get`

``` purescript
get :: forall l a da r rl rest1 rest2 d dl. IsSymbol l => RowCons l a rest1 r => RowCons l da rest2 d => RowToList r rl => RowToList d dl => PatchRL r rl d dl => Patch a da => SProxy l -> Jet (WrappedRecord r) -> Jet a
```

An incremental property accessor function

#### `update`

``` purescript
update :: forall l a da r rl rest1 rest2 d dl. IsSymbol l => RowCons l a rest1 r => RowCons l da rest2 d => RowToList r rl => RowToList d dl => PatchRL r rl d dl => Patch a da => SProxy l -> Change a -> Change (WrappedRecord r)
```

An incremental property update function


