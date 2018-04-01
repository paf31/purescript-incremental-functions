## Module Data.Incremental.Record

#### `IRecord`

``` purescript
newtype IRecord r
  = IRecord ({  | r })
```

##### Instances
``` purescript
Newtype (IRecord a) _
```

#### `get`

``` purescript
get :: forall l a da r rl rest1 rest2 d dl. IsSymbol l => RowCons l a rest1 r => RowCons l da rest2 d => RowToList r rl => RowToList d dl => PatchRL r rl d dl => Patch a da => SProxy l -> Jet (IRecord r) -> Jet a
```

An incremental property accessor function

#### `update`

``` purescript
update :: forall l a da r rl rest1 rest2 d dl. IsSymbol l => RowCons l a rest1 r => RowCons l da rest2 d => RowToList r rl => RowToList d dl => PatchRL r rl d dl => Patch a da => SProxy l -> Change a -> Change (IRecord r)
```

An incremental property update function

#### `PatchRL`

``` purescript
class (MonoidRL dl d) <= PatchRL r (rl :: RowList) d (dl :: RowList) | rl -> r, dl -> d, rl -> dl where
  patchRL :: RLProxy rl -> RLProxy dl -> {  | r } -> {  | d } -> {  | r }
```

##### Instances
``` purescript
PatchRL () Nil () Nil
(IsSymbol l, Patch a m, PatchRL r1 rl d1 dl, RowCons l a r1 r2, RowCons l m d1 d2, RowLacks l r1, RowLacks l d1) => PatchRL r2 (Cons l a rl) d2 (Cons l m dl)
```


