## Module Data.Incremental

Incremental computation, based on

> "A Theory of Changes for Higher-Order Languages" by
> Cai, Giarrusso, Rendel and Ostermann.

This module also defines a HOAS-style interface for working with
function changes.

#### `Patch`

``` purescript
class (Monoid d) <= Patch a d | a -> d where
  patch :: a -> d -> a
```

The monoid `d` of changes acts on values of type `a`.

##### Instances
``` purescript
Patch Unit Unit
(Patch a da, Patch b db) => Patch (Tuple a b) (Tuple da db)
```

#### `Diff`

``` purescript
class (Patch a d) <= Diff a d | a -> d where
  diff :: a -> a -> d
```

##### Instances
``` purescript
Diff Unit Unit
(Diff a da, Diff b db) => Diff (Tuple a b) (Tuple da db)
```

#### `Change`

``` purescript
data Change a
```

A type level function which maps a type to the type of its change structure.

Uniqueness of instances makes the coercions `fromChange` and `toChange` safe,
since the functional dependency makes the change structure type unique.

##### Instances
``` purescript
(Patch a da, Semigroup da) => Semigroup (Change a)
(Patch a da, Monoid da) => Monoid (Change a)
```

#### `fromChange`

``` purescript
fromChange :: forall a da. Patch a da => Change a -> da
```

#### `toChange`

``` purescript
toChange :: forall a da. Patch a da => da -> Change a
```

#### `Jet`

``` purescript
type Jet a = { position :: a, velocity :: Change a }
```

A value (`position`) paired with a change (`velocity`).

We can think of these modified terms as conceptually similar to dual
numbers.

We can use functions of type `Jet a -> Jet b` as incremental
functions from `a` to `b`, which gives us a HOAS-style DSL for working
with jets.

#### `constant`

``` purescript
constant :: forall a da. Patch a da => a -> Jet a
```

A constant term

#### `change`

``` purescript
change :: forall a da. Patch a da => Change a -> Jet a -> Jet a
```

Create a function which applies a patch to its input


