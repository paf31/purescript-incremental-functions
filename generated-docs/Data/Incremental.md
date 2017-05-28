## Module Data.Incremental

Incremental computation, based on

> "A Theory of Changes for Higher-Order Languages" by
> Cai, Gairrusso, Rendel and Ostermann.

This module also defines a HOAS-style interface for working with
function changes.

#### `ChangeStructure`

``` purescript
class (Monoid d) <= ChangeStructure a d | a -> d where
  diff :: a -> a -> d
  patch :: a -> d -> a
```

A "change structure" on `a` consists of a monoid `d` of changes, together with
diff and patch functions.

##### Instances
``` purescript
(ChangeStructure a da, ChangeStructure b db) => ChangeStructure (a -> b) (FunctionChange a da db)
```

#### `FunctionChange`

``` purescript
newtype FunctionChange a da db
```

A change structure for functions

##### Instances
``` purescript
(Semigroup db) => Semigroup (FunctionChange a da db)
(Monoid db) => Monoid (FunctionChange a da db)
(ChangeStructure a da, ChangeStructure b db) => ChangeStructure (a -> b) (FunctionChange a da db)
```

#### `runFunctionChange`

``` purescript
runFunctionChange :: forall a da db. FunctionChange a da db -> a -> da -> db
```

#### `Change`

``` purescript
data Change a
```

A type level function which maps a type to the type of its change structure.

Uniqueness of instances makes the coercions `fromChange` and `toChange` safe,
since the functional dependency makes the change structure type unique.

#### `fromChange`

``` purescript
fromChange :: forall a da. ChangeStructure a da => Change a -> da
```

#### `toChange`

``` purescript
toChange :: forall a da. ChangeStructure a da => da -> Change a
```

#### `D1`

``` purescript
data D1 a
  = D1 a (Change a)
```

A term paired with its rate of change.

We can think of these modified terms as conceptually similar to dual numbers.

#### `valueOf`

``` purescript
valueOf :: forall a. D1 a -> a
```

#### `changeOf`

``` purescript
changeOf :: forall a. D1 a -> Change a
```

#### `lam`

``` purescript
lam :: forall a b da db. ChangeStructure a da => ChangeStructure b db => (D1 a -> D1 b) -> D1 (a -> b)
```

Lambda abstractionion

#### `app`

``` purescript
app :: forall a b da db. ChangeStructure a da => ChangeStructure b db => D1 (a -> b) -> D1 a -> D1 b
```

Function application

#### `constant`

``` purescript
constant :: forall a da. ChangeStructure a da => a -> D1 a
```

A constant term

#### `applyPatch`

``` purescript
applyPatch :: forall a da. ChangeStructure a da => da -> D1 (a -> a)
```

Create a function which applies a patch to its input


