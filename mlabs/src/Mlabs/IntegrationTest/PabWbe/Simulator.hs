module Mlabs.IntegrationTest.PabWbe.Simulator (
  handlers,
  runSimulator,
) where

-- import PlutusTx.Prelude
import Prelude

import Control.Monad (void)
import Control.Monad.Freer
import Control.Monad.IO.Class (MonadIO (..))
import Data.Default (Default (def))

import Plutus.PAB.Effects.Contract.Builtin (Builtin, BuiltinHandler (..))
import Plutus.PAB.Effects.Contract.Builtin qualified as Builtin
import Plutus.PAB.Simulator (SimulatorEffectHandlers, logString)
import Plutus.PAB.Simulator qualified as Simulator
import Plutus.PAB.Webserver.Server qualified as PAB.Server
-- import Wallet.Emulator.Types (knownWallet, walletPubKeyHash)
-- import Ledger.Crypto (pubKeyHash)

import Mlabs.IntegrationTest.PabWbe.Pab (TestContracts(..))
-- import Mlabs.IntegrationTest.PabWbe.Contracts.Mint (Mint(..))

-- | Start PAB simulator for NFT contracts
runSimulator :: IO ()
runSimulator = void $
  Simulator.runSimulationWith handlers $ do
    logString @(Builtin TestContracts)
      "Starting PAB demo webserver. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug
    -- let wallet = knownWallet 1
    --     walletPkh = walletPubKeyHash wallet
    --     mintContract = MintContract $ Mint walletPkh ("TestToken", 10)
    -- _  <- Simulator.activateContract wallet mintContract
    _ <- liftIO getLine
    shutdown

-- | Simulator handlers for NFT contracts
handlers :: SimulatorEffectHandlers (Builtin TestContracts)
handlers =
  Simulator.mkSimulatorHandlers def def $
    interpret (contractHandler Builtin.handleBuiltin)
