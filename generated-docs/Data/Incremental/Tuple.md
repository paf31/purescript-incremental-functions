## Module Data.Incremental.Tuple

#### `fst`

``` purescript
fst :: forall a da b db. Patch a da => Patch b db => Jet (Tuple a b) -> Jet a
```

Extract the first component of a `Tuple`, incrementally.

#### `snd`

``` purescript
snd :: forall a da b db. Patch a da => Patch b db => Jet (Tuple a b) -> Jet b
```

Extract the second component of a `Tuple`, incrementally.

#### `tuple`

``` purescript
tuple :: forall a da b db. Patch a da => Patch b db => Jet a -> Jet b -> Jet (Tuple a b)
```

Construct a `Tuple`, incrementally.

#### `uncurry`

``` purescript
uncurry :: forall a da b db c. Patch a da => Patch b db => (Jet a -> Jet b -> Jet c) -> Jet (Tuple a b) -> Jet c
```

Uncurry an incremental function.

#### `curry`

``` purescript
curry :: forall a da b db c. Patch a da => Patch b db => (Jet (Tuple a b) -> Jet c) -> Jet a -> Jet b -> Jet c
```

Curry an incremental function.


