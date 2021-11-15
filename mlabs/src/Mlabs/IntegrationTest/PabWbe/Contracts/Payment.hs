module Mlabs.IntegrationTest.PabWbe.Contracts.Payment (
  Payment (..),
  PaymentSchema,
  payFromTo,
) where

import Control.Monad (void)

import Data.Aeson (FromJSON, ToJSON)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Void (Void)

import GHC.Generics (Generic)

import Ledger (
  ChainIndexTxOut (..),
  PubKeyHash,
  getCardanoTxId,
  pubKeyHashAddress,
 )
import Ledger.Ada qualified as Ada
import Ledger.Constraints qualified as Constraints

import Plutus.Contract (
  Endpoint,
  Promise,
  awaitTxConfirmed,
  endpoint,
  logInfo,
  submitTxConstraintsWith,
 )

import PlutusTx.Prelude

import Schema (ToSchema)

import Prelude qualified as Hask

data Payment = Payment
  { fromPkh :: PubKeyHash
  , toPkh :: PubKeyHash
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

type PaymentSchema = Endpoint "payment" Payment

payFromTo :: Promise () PaymentSchema Text ()
payFromTo = endpoint @"payment" $ \Payment {..} -> do
  let cs =
        mconcat
          [ Constraints.mustSpendPubKeyOutput oref
          , Constraints.mustPayToPubKey toPkh $ Ada.adaValueOf 5
          ]

      ls = Constraints.unspentOutputs utxos

      utxos = Map.singleton oref txOut

      oref = error ()

      txOut =
        PublicKeyChainIndexTxOut
          (pubKeyHashAddress fromPkh)
          (Ada.adaValueOf 10)

  txId <- getCardanoTxId <$> submitTxConstraintsWith @Void ls cs
  logInfo @Hask.String $ "Awaiting tx completion: " <> Hask.show txId
  void $ awaitTxConfirmed txId
  logInfo @Hask.String "Tx confirmed, payment complete"
