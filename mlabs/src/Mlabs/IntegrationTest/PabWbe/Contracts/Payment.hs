module Mlabs.IntegrationTest.PabWbe.Contracts.Payment (
  Payment (..),
  PaymentSchema,
  payFromTo,
) where

import Control.Monad (void)

import Data.Aeson (FromJSON, ToJSON)
import Data.Map qualified as Map
import Data.OpenApi.Schema qualified as Schema
import Data.Text (Text)
import Data.Void (Void)

import GHC.Generics (Generic)

import Mlabs.IntegrationTest.Utils

import Ledger (
  ChainIndexTxOut (..),
  PubKeyHash,
  TxId,
  TxOutRef (TxOutRef),
  getCardanoTxId,
  pubKeyHashAddress,
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

data Payment = Payment
  { fromPkh :: PubKeyHash
  , toPkh :: PubKeyHash
  }
  deriving stock (Hask.Eq, Hask.Ord, Hask.Show, Generic)
  deriving anyclass (FromJSON, ToJSON, Schema.ToSchema, Playground.ToSchema)

type PaymentSchema = Endpoint "payment" Payment

payFromTo :: Payment -> Contract () PaymentSchema Text ()
payFromTo Payment {..} = do
  let constraints =
        mconcat
          [ Constraints.mustSpendPubKeyOutput oref
          , Constraints.mustPayToPubKey toPkh $ Ada.adaValueOf 5
          ]

      lookups =
        Constraints.unspentOutputs $
          Map.singleton oref txOut

      oref = TxOutRef refId 2

      txOut =
        PublicKeyChainIndexTxOut
          (pubKeyHashAddress fromPkh)
          (Ada.adaValueOf 10)

  txId <-
    getCardanoTxId
      <$> submitTxConstraintsWith @Void lookups constraints
  logInfo @Hask.String $ "Awaiting tx completion: " <> Hask.show txId
  void $ awaitTxConfirmed txId
  logInfo @Hask.String "Tx confirmed, payment complete"
  where
    refId :: TxId
    refId =
      unsafeDecode
        "TxId"
        "{\"getTxId\" : \"206e4a7e1b8a8004d41546ae28089cc4c00853d4f285c45ca62ba8a81b271f41\"}"
