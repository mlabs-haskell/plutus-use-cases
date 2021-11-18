module Mlabs.IntegrationTest.PabWbe.Contracts.Payment (
  Payment (..),
  PaymentSchema,
  payFromTo,
) where

import Control.Monad (void)

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi.Schema qualified as Schema
import Data.Text (Text)
import Data.Void (Void)

import GHC.Generics (Generic)

import Ledger (
  PubKeyHash,
  getCardanoTxId,
 )
import Ledger.Ada qualified as Ada
import Ledger.Constraints qualified as Constraints

import Playground.Contract qualified as Playground

import Plutus.Contract (
  Contract,
  Endpoint,
  awaitTxConfirmed,
  logInfo,
  submitTxConstraintsWith,
 )

import PlutusTx.Prelude

import Prelude qualified as Hask

newtype Payment = Payment
  { toPkh :: PubKeyHash
  }
  deriving stock (Hask.Eq, Hask.Ord, Hask.Show, Generic)
  deriving anyclass (FromJSON, ToJSON, Schema.ToSchema, Playground.ToSchema)

type PaymentSchema = Endpoint "payment" Payment

payFromTo :: Payment -> Contract () PaymentSchema Text ()
payFromTo Payment {..} = do
  txId <-
    getCardanoTxId
      <$> submitTxConstraintsWith @Void Hask.mempty constraint
  logInfo @Hask.String $ "Awaiting tx completion: " <> Hask.show txId
  void $ awaitTxConfirmed txId
  logInfo @Hask.String "Tx confirmed, payment complete"
  where
    constraint = Constraints.mustPayToPubKey toPkh $ Ada.adaValueOf 5
