module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Incremental (Jet, constant, fromChange, patch)
import Data.Incremental.Array as IArray
import Data.Incremental.Eq (Atomic(..), mapAtomic, replace)
import Data.Incremental.Map as IMap
import Data.Incremental.Record as IRecord
import Data.Map as Map
import Data.Maybe.Last (Last(..))
import Data.Monoid (mempty)
import Data.Newtype (unwrap, wrap)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Test.Assert (ASSERT, assert)

-- | The function \x -> x * 2, together with its derivative
times2 :: Jet (Atomic Int) -> Jet (Atomic Int)
times2 = mapAtomic (_ * 2)

-- | The `double` function iterated three times
times8 :: Jet (Atomic Int) -> Jet (Atomic Int)
times8 = times2 >>> times2 >>> times2

main :: Eff (assert :: ASSERT) Unit
main = do
  let t1 = times8 (constant (wrap 1))
  assert (t1.position == wrap 8)
  assert (fromChange t1.velocity == mempty)

  let t2 = times8 { position: wrap 1, velocity: replace 2 }
  assert (t2.position == wrap 8)
  assert (fromChange t2.velocity == Last (pure 16))

  let t3 = IMap.modifyAt 1 times8 (constant (wrap (Map.fromFoldable [Tuple 1 (wrap 1), Tuple 2 (wrap 2)])))
  assert (unwrap t3.position == Map.fromFoldable [Tuple 1 (wrap 8), Tuple 2 (wrap 2)])

  let t4 = IMap.modifyAt 1 times8
             { position: wrap (Map.fromFoldable [Tuple 1 (wrap 1), Tuple 2 (wrap 2)])
             , velocity: IMap.updateAt 1 (replace 2)
             }
  assert (unwrap t4.position == Map.fromFoldable [Tuple 1 (wrap 8), Tuple 2 (wrap 2)])
  assert (fromChange t4.velocity == fromChange (IMap.updateAt 1 (replace 16)))

  let t5 = IArray.map times8
             { position: wrap [wrap 1, wrap 2]
             , velocity: IArray.modifyAt 1 (replace 3)
             }
  assert (unwrap t5.position == [wrap 8, wrap 16])
  assert (fromChange t5.velocity == fromChange (IArray.modifyAt 1 (replace 24)))

  let t6 = IArray.map (IArray.map times8)
             { position: wrap [wrap [wrap 1, wrap 2], wrap [wrap 3]]
             , velocity: IArray.modifyAt 0 (IArray.modifyAt 1 (replace 4))
             }
  assert (unwrap t6.position == [wrap [wrap 8, wrap 16], wrap [wrap 24]])
  assert (fromChange t6.velocity == fromChange (IArray.modifyAt 0 (IArray.modifyAt 1 (replace 32))))

  let testRecord :: IRecord.IRecord (foo :: Atomic Int, bar :: Atomic Char)
      testRecord = IRecord.IRecord { foo: wrap 0, bar: wrap 'a' }
      t7 = IRecord.get (SProxy :: SProxy "foo")
             { position: testRecord
             , velocity: IRecord.update (SProxy :: SProxy "foo") (replace 42)
             }
  assert (unwrap t7.position == 0)
  assert (fromChange t7.velocity == fromChange (replace 42))

  let t8 = IMap.size $ IMap.zip
             (constant (wrap (Map.fromFoldable [Tuple 1 (Atomic 1), Tuple 2 (Atomic 2)])))
             { position: wrap (Map.fromFoldable [Tuple 1 (Atomic 'a'), Tuple 2 (Atomic 'b')])
             , velocity: IMap.remove 1
             }
  assert (unwrap t8.position == 2)
  assert (fromChange t8.velocity == Last (pure 1))

  let t9 = IArray.withIndex
             { position: wrap [Atomic 'a', Atomic 'b', Atomic 'c']
             , velocity:
                 IArray.insertAt 1 (Atomic 'x')
                 <> IArray.deleteAt 2
                 <> IArray.modifyAt 2 (replace 'y')
             }
  assert $ unwrap t9.position ==
    [ Tuple (Atomic 0) (Atomic 'a')
    , Tuple (Atomic 1) (Atomic 'b')
    , Tuple (Atomic 2) (Atomic 'c')
    ]
  assert $ unwrap (patch t9.position (fromChange t9.velocity)) ==
    [ Tuple (Atomic 0) (Atomic 'a')
    , Tuple (Atomic 1) (Atomic 'x')
    , Tuple (Atomic 2) (Atomic 'y')
    ]
