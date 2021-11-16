module Test.Utils (
  throwError,
  next,
  wait,
  concatPredicates,
) where

import PlutusTx.Prelude hiding (fromInteger)
import Prelude qualified as Hask -- (String, fromInteger)

import Data.Functor (void)
import Data.List (foldl1')
import Plutus.Contract.Test (TracePredicate, (.&&.))
import Plutus.Trace.Emulator qualified as Trace

-- | Throws error to emulator trace.
throwError :: Hask.String -> Trace.EmulatorTrace a
throwError msg = Trace.throwError (Trace.GenericError $ "Generic Error:" <> msg)

-- | Wait for one slot.
next :: Trace.EmulatorTrace ()
next = void Trace.nextSlot

-- | Wait given amount of slots.
wait :: Integer -> Trace.EmulatorTrace ()
wait = void . Trace.waitNSlots . Hask.fromInteger

concatPredicates :: [TracePredicate] -> TracePredicate
concatPredicates = foldl1' (.&&.)
