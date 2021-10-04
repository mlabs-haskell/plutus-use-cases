{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- | Simulator demo for Governance
module Main (
  main,
) where

import PlutusTx.Prelude
import Prelude (IO, getLine, show)

import Control.Monad (forM)
import Control.Monad.Freer (Eff)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Functor (void)

import Mlabs.Governance.Contract.Api (Deposit (..), QueryBalance (..), Withdraw (..))
import Mlabs.Governance.Contract.Simulator.Handler (GovernanceContracts (..))
import Mlabs.Governance.Contract.Simulator.Handler qualified as Handler
import Mlabs.Governance.Contract.Validation (AssetClassGov (..))

import Ledger (PubKeyHash, pubKeyHash)

import Plutus.PAB.Core (PABEffects)
import Plutus.PAB.Effects.Contract.Builtin (Builtin)
import Plutus.PAB.Simulator (Simulation, payToWallet)
import Plutus.PAB.Simulator qualified as Simulator
import Plutus.PAB.Webserver.Server qualified as PWS
import Wallet.Emulator.Types (Wallet (..), walletPubKey)
import Wallet.Emulator.Wallet (walletAddress)
import Wallet.Types (ContractInstanceId)

import Mlabs.Plutus.PAB (call, waitForLast)
import Mlabs.System.Console.PrettyLogger (logNewLine)
import Mlabs.System.Console.Utils (logAction, logBalance)

-- | Main function to run simulator
main :: IO ()
main = void $
  Simulator.runSimulationWith Handler.handlers $ do
    Simulator.logString @(Builtin GovernanceContracts) "Starting Governance PAB webserver"
    shutdown <- PWS.startServerDebug
    let simWallets = Handler.wallets
        (wallet1 : wallet2 : wallet3 : _) = simWallets
    (cids, _gov) <-
      subscript
        "Initializing contracts\nWallet 1 mints and distributes initial GOV tokens"
        simWallets
        (initializeContracts wallet1)
    let [_, wCid2, wCid3] = cids

    subscript_
      "Wallet 2 deposits 55 GOV (xGOV tokens being minted as result) "
      simWallets
      $ deposit wCid2 55

    subscript_
      "Wallet 2 queries amount of GOV deposited"
      simWallets
      $ getBalance wCid2 wallet2

    subscript_
      "Wallet 2 deposits 10 more GOV"
      simWallets
      $ deposit wCid2 10

    subscript_
      "Wallet 2 queries amount of GOV deposited"
      simWallets
      $ getBalance wCid2 wallet2

    subscript_
      "Wallet 2 withdraws 60 GOV"
      simWallets
      $ withdraw wCid2 wallet2 60

    subscript_
      "Wallet 2 queries amount of GOV deposited"
      simWallets
      $ getBalance wCid2 wallet2

    subscript_
      "Wallet 3 queries amount of GOV deposited"
      simWallets
      $ getBalance wCid3 wallet3

    Simulator.logString @(Builtin GovernanceContracts) "Scripted part is over\nPress Enter to stop and exit"
    void $ liftIO getLine
    shutdown
  where
    subscript_ msg wallets simulation = void $ subscript msg wallets simulation
    subscript msg wallets simulation = do
      logAction msg
      next
      res <- simulation
      Simulator.waitNSlots 1
      mapM_ printBalance wallets
      next
      return res

    next = do
      logNewLine
      void $ Simulator.waitNSlots 5

initializeContracts :: Wallet -> Eff (PABEffects _ _) ([ContractInstanceId], AssetClassGov)
-- shortcut for Governance initialization
initializeContracts admin = do
  cidInit <- Simulator.activateContract admin Bootstrap
  govCs <- waitForLast cidInit
  void $ Simulator.waitUntilFinished cidInit
  let gov = AssetClassGov govCs Handler.govTokenName
  cids <- forM Handler.wallets $ \w -> Simulator.activateContract w (Governance gov)
  return (cids, gov)

-- shortcits for endpoint calls
deposit :: ContractInstanceId -> Integer -> Simulation _ ()
deposit cid amount = call cid $ Deposit amount

withdraw :: ContractInstanceId -> Wallet -> Integer -> Simulation _ ()
withdraw cid wallet amount = call cid $ Withdraw [(walletPKH wallet, amount)]

getBalance :: ContractInstanceId -> Wallet -> Eff (PABEffects _ _) ()
getBalance cid wallet = do
  call cid $ QueryBalance $ walletPKH wallet
  govBalance :: Integer <- waitForLast cid
  logAction $ "Balance is " ++ show govBalance

printBalance :: Wallet -> Simulation (Builtin schema) ()
printBalance wallet = do
  v <- Simulator.valueAt $ walletAddress wallet
  logBalance ("WALLET " <> show wallet) v

walletPKH :: Wallet -> PubKeyHash
walletPKH = pubKeyHash . walletPubKey

-- cfg =
--   BootstrapCfg
--     { wallets = Wallet <$> [1 .. 3] -- wallets participating, wallet #1 is admin
--     , govTokenName = "GOVToken" -- name of GOV token to be paid in exchange of xGOV tokens
--     , govAmount = 100 -- GOV amount each wallet gets on start
--     }
