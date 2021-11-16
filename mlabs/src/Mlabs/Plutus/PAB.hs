module Mlabs.Plutus.PAB (
  call,
  waitForLast,
  printBalance,
) where

-- Note: should this module be modified to Plutus Prelude version?
-- import PlutusTx.Prelude
import Prelude qualified as Hask

import Data.Aeson (FromJSON, Result (..), fromJSON)
import Data.Functor (void)
import Data.Monoid (Last (..))
import Mlabs.Utils.Wallet (walletFromNumber)
import Plutus.Contract (ContractInstanceId)
import Plutus.PAB.Effects.Contract.Builtin (Builtin)
import Plutus.PAB.Simulator (Simulation, callEndpointOnInstance, valueAt, waitForState, waitNSlots)
import Wallet.Emulator.Wallet qualified as Wallet

import Mlabs.Plutus.Contract (IsEndpoint, endpointName)
import Mlabs.System.Console.Utils (logBalance)

call :: IsEndpoint a => ContractInstanceId -> a -> Simulation (Builtin schema) ()
call cid input = do
  void Hask.$ callEndpointOnInstance cid (endpointName input) input
  void Hask.$ waitNSlots 2

{- | Waits for the given value to be written to the state of the service.
 We use it to share data between endpoints. One endpoint can write parameter to state with tell
 and in another endpoint we wait for the state-change.
-}
waitForLast :: FromJSON a => ContractInstanceId -> Simulation t a
waitForLast cid =
  Hask.flip waitForState cid Hask.$ \json -> case fromJSON json of
    Success (Last (Hask.Just x)) -> Hask.Just x
    _ -> Hask.Nothing

printBalance :: Hask.Integer -> Simulation (Builtin schema) ()
printBalance n =
  logBalance ("WALLET " Hask.<> Hask.show n) Hask.=<< 
    (valueAt Hask.. Wallet.walletAddress Hask.$ walletFromNumber n)
