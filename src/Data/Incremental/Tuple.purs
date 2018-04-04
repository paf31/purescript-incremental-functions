module Data.Incremental.Tuple where

import Data.Incremental (class Patch, Jet, fromChange, toChange)
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple

-- | Extract the first component of a `Tuple`, incrementally.
fst :: forall a da b db. Patch a da => Patch b db => Jet (Tuple a b) -> Jet a
fst { position, velocity } =
  { position: Tuple.fst position
  , velocity: toChange (Tuple.fst (fromChange velocity))
  }

-- | Extract the second component of a `Tuple`, incrementally.
snd :: forall a da b db. Patch a da => Patch b db => Jet (Tuple a b) -> Jet b
snd { position, velocity } =
  { position: Tuple.snd position
  , velocity: toChange (Tuple.snd (fromChange velocity))
  }

-- | Construct a `Tuple`, incrementally.
tuple :: forall a da b db. Patch a da => Patch b db => Jet a -> Jet b -> Jet (Tuple a b)
tuple a b =
  { position: Tuple a.position b.position
  , velocity: toChange (Tuple (fromChange a.velocity) (fromChange b.velocity))
  }

-- | Uncurry an incremental function.
uncurry
  :: forall a da b db c
   . Patch a da
  => Patch b db
  => (Jet a -> Jet b -> Jet c)
  -> Jet (Tuple a b)
  -> Jet c
uncurry f t = f (fst t) (snd t)

-- | Curry an incremental function.
curry
  :: forall a da b db c
   . Patch a da
  => Patch b db
  => (Jet (Tuple a b) -> Jet c)
  -> Jet a
  -> Jet b
  -> Jet c
curry f a b = f (tuple a b)
