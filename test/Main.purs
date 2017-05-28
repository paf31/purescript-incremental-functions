module Test.Main where

import Prelude
import Data.Map as Map
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.Incremental (D1(..), app, changeOf, fromChange, lam, patch, runFunctionChange, toChange, valueOf)
import Data.Incremental.Abelian (WrappedAbelian(..))
import Data.Incremental.Map (MapChange(..), MapChanges(..), WrappedMap(..), key)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (unwrap, wrap)

-- | Integers with a change structure given by group under addition
type Int' = WrappedAbelian (Additive Int)

-- | The function \x -> x * 2, together with its derivative
double :: D1 (Int' -> Int')
double = lam \(D1 (WrappedAbelian (Additive a)) da) ->
  D1 (WrappedAbelian (Additive (a * 2)))
     (toChange (WrappedAbelian (Additive (unwrap (unwrap (fromChange da)) * 2))))

-- | The `double` function iterated three times
times8 :: D1 (Int' -> Int')
times8 = lam \x -> double `app` (double `app` (double `app` x))

main :: Eff (console :: CONSOLE) Unit
main = do
  log "Integer example"
  log ""

  log "times8 n ="
  let n = wrap (wrap 2)
  logShow (valueOf times8 n)

  log "times8 (n + dn) ="
  let dn = wrap (wrap 1)
  logShow (valueOf times8 (patch n dn))

  log "d_times8 n 1 ="
  let d_times8 = fromChange (changeOf times8)
  logShow (runFunctionChange d_times8 n dn)

  log ""
  log "Map example"
  log ""

  log "key 1 times8 m ="
  let m = WrappedMap (Map.singleton 1 (wrap (wrap 1)) <> Map.singleton 2 (wrap (wrap 1)))
      f = key 1 `app` times8
  logShow (valueOf f m)

  let dm = MapChanges (Map.singleton 1 (Update (wrap (wrap 1))))
  log "key 1 times8 (m + dm) ="
  logShow (valueOf f (patch m dm))

  log "d_(key 1) times8 dm ="
  let df = fromChange (changeOf f)
  logShow (runFunctionChange df m dm)
