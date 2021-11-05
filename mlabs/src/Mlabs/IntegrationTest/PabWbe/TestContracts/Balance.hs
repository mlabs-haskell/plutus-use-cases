{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:debug-context #-}

module Mlabs.IntegrationTest.PabWbe.TestContracts.Balance (
  BalanceAndSignSchema,
  endpoints,
) where

import Control.Monad (forever)

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)

import Ledger.Constraints (mkTx)
import Ledger.Constraints qualified as Constraints

import Mlabs.IntegrationTest.PabWbe.Types
import Mlabs.IntegrationTest.Types

import Plutus.Contract (
  Contract,
  Endpoint,
  Promise,
  balanceTx,
  endpoint,
  logInfo,
  ownPubKeyHash,
  selectList,
  submitBalancedTx,
  throwError,
 )
import Plutus.V1.Ledger.Ada qualified as Ada

import PlutusTx.Prelude

import Prelude qualified as Hask

type BalanceAndSignSchema = Endpoint "run-balance" ()

endpoints :: Contract () BalanceAndSignSchema Text ()
endpoints = forever . selectList $ [balanceAndSign]

balanceAndSign :: Promise () BalanceAndSignSchema Text PabWbeResult
balanceAndSign = endpoint @"run-balance" $ \() -> do
  ownPkh <- ownPubKeyHash
  logInfo @Hask.String $ "Running balance with wallet PKH " <> Hask.show ownPkh

  let toPkh =
        decodePkh
          "{\"getPubKeyHash\" : \"d19278d36a31eec98aca5d1cc226fcf5aee6451bb9d0123bb60c1b5b\"}"
      txC = Constraints.mustPayToPubKey toPkh $ Ada.adaValueOf 5
      etx = mkTx @Void Hask.mempty txC

  case etx of
    Left e -> throwError . Text.pack $ Hask.show e
    Right unbTx -> do
      balanced <- balanceTx unbTx
      logInfo @Hask.String $ Hask.show balanced
      submitted <- submitBalancedTx balanced
      logInfo @Hask.String $ Hask.show submitted
      -- FIXME
      error ()
