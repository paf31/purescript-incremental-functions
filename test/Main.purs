module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Incremental (Jet, constant, fromChange, toChange)
import Data.Incremental.Eq as IEq
import Data.Incremental.Array as IArray
import Data.Incremental.Map as IMap
import Data.Map as Map
import Data.Maybe.Last (Last(..))
import Data.Monoid (mempty)
import Data.Newtype (unwrap, wrap)
import Data.Tuple (Tuple(..))
import Test.Assert (ASSERT, assert)

-- | The function \x -> x * 2, together with its derivative
times2 :: Jet (IEq.WrappedEq Int) -> Jet (IEq.WrappedEq Int)
times2 { position, velocity } =
  { position: wrap (unwrap position * 2)
  , velocity: toChange (map (_ * 2) (fromChange velocity))
  }

-- | The `double` function iterated three times
times8 :: Jet (IEq.WrappedEq Int) -> Jet (IEq.WrappedEq Int)
times8 = times2 >>> times2 >>> times2

main :: Eff (assert :: ASSERT) Unit
main = do
  let t1 = times8 (constant (wrap 1))
  assert (t1.position == wrap 8)
  assert (fromChange t1.velocity == mempty)

  let t2 = times8 { position: wrap 1, velocity: IEq.replace 2 }
  assert (t2.position == wrap 8)
  assert (fromChange t2.velocity == Last (pure 16))

  let t3 = IMap.modifyAt 1 times8 (constant (wrap (Map.fromFoldable [Tuple 1 (wrap 1), Tuple 2 (wrap 2)])))
  assert (unwrap t3.position == Map.fromFoldable [Tuple 1 (wrap 8), Tuple 2 (wrap 2)])

  let t4 = IMap.modifyAt 1 times8
             { position: wrap (Map.fromFoldable [Tuple 1 (wrap 1), Tuple 2 (wrap 2)])
             , velocity: IMap.updateAt 1 (IEq.replace 2)
             }
  assert (unwrap t4.position == Map.fromFoldable [Tuple 1 (wrap 8), Tuple 2 (wrap 2)])
  assert (fromChange t4.velocity == fromChange (IMap.updateAt 1 (IEq.replace 16)))

  let t5 = IArray.map times8
             { position: wrap [wrap 1, wrap 2]
             , velocity: IArray.modifyAt 1 (IEq.replace 3)
             }
  assert (unwrap t5.position == [wrap 8, wrap 16])
  assert (fromChange t5.velocity == fromChange (IArray.modifyAt 1 (IEq.replace 24)))

  let t6 = IArray.map (IArray.map times8)
             { position: wrap [wrap [wrap 1, wrap 2], wrap [wrap 3]]
             , velocity: IArray.modifyAt 0 (IArray.modifyAt 1 (IEq.replace 4))
             }
  assert (unwrap t6.position == [wrap [wrap 8, wrap 16], wrap [wrap 24]])
  assert (fromChange t6.velocity == fromChange (IArray.modifyAt 0 (IArray.modifyAt 1 (IEq.replace 32))))
