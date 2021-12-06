{-# LANGUAGE  NamedFieldPuns #-}

-- | Simple pay to wallet contract. No scripts used.
module Mlabs.Roundtrip.RountripP2W(
    runDemo
    , PayToWalletParams(..)
    , DemoSchema
    , ContractArgs(..)
    ) where

import Prelude qualified as Hask
import PlutusTx.Prelude

import Control.Lens
import Control.Monad (forever)
import Data.Aeson (FromJSON, ToJSON)
import Data.Void (Void)
import Data.Text (Text)
import GHC.Generics (Generic)
import Schema (ToSchema)
import Data.OpenApi.Schema qualified as OpenApi

import Cardano.Api (AsType (AsAddressInEra, AsAlonzoEra))
import Cardano.Api qualified
import Plutus.Contract.CardanoAPI (FromCardanoError, fromCardanoAddress, toCardanoAddress)

import Ledger (TxOut(..), TxOutRef, PubKeyHash, Value, Address, toPubKeyHash, outputs)
import Plutus.V1.Ledger.Ada qualified as Ada
import Ledger.Constraints.OffChain (tx)
import Ledger.Constraints (UnbalancedTx, adjustUnbalancedTx, mustPayToPubKey)
import Plutus.Contract (ContractError(..), Endpoint, Contract, Promise, endpoint, 
  mkTxConstraints, yieldUnbalancedTx, logWarn, logInfo, runError, throwError)


import Mlabs.Roundtrip.PreBalance (PrebalancedTx(..), preBalanceTxFrom)
import Mlabs.Plutus.Contract (selectForever)

-- |Contract activation args
data ContractArgs = ContractArgs
  { ownAddress :: Text
  }
  deriving (Hask.Eq, Hask.Ord, Hask.Show, Generic)
  deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema)

-- Contract API
data PayToWalletParams =
    PayToWalletParams
        { lovelaceAmount:: Integer
        , receiverAddress :: Text
        , collateralRef :: TxOutRef
        }
        deriving stock (Hask.Eq, Hask.Show, Generic)
        deriving anyclass (ToJSON, FromJSON, ToSchema)

type DemoSchema = Endpoint "call-demo" PayToWalletParams

-- | Off-chain
runDemo :: ContractArgs -> Contract () DemoSchema ContractError ()
runDemo cArgs = selectForever [endpoint @"call-demo" (runDemo_ cArgs)]

runDemo_ :: ContractArgs -> PayToWalletParams -> Contract () DemoSchema ContractError ()
runDemo_ (ContractArgs ownAddress) ps = do
    runError (run ps) >>= \case
      Left err -> logWarn @Hask.String (Hask.show @ContractError err)
      Right () -> pure ()
  where  
    run PayToWalletParams{lovelaceAmount, receiverAddress, collateralRef} = do
      let paymentValue = Ada.lovelaceValueOf lovelaceAmount
      senderAddr <- parseAddress ownAddress
      receiverAddr <- parseAddress receiverAddress
      {- this won't work
         funds will go to payment key which could be not address we want
      -}
      -- receiverPKH <- getPKH receiverAddr
      -- utx <- mkTxConstraints @Void (Hask.mempty) (mustPayToPubKey receiverPKH paymentValue)
      -- maybe this
      utx <- withPaymentToReceiver receiverAddr paymentValue 
                <$> mkTxConstraints @Void (Hask.mempty) (Hask.mempty)
      PrebTx pUtx <- preBalanceTxFrom senderAddr collateralRef utx
      logInfo @Hask.String $ "Yielding tx"
      yieldUnbalancedTx pUtx

withPaymentToReceiver :: Address -> Value -> UnbalancedTx -> UnbalancedTx
withPaymentToReceiver addr v utx = 
  over (tx . outputs) (paymentOut :) utx
  where 
    paymentOut = TxOut addr v Nothing


getPKH :: Address -> Contract () DemoSchema ContractError PubKeyHash
getPKH addr = do
  case toPubKeyHash addr of
    Just p -> pure p
    Nothing -> throwError $ OtherError "Can not get PKH from Address"

-- parseAddress :: Text -> Maybe (Either FromCardanoError Address)
parseAddress :: Text -> Contract () DemoSchema ContractError Address
parseAddress addr = 
  case fmap fromCardanoAddress $ Cardano.Api.deserialiseAddress (AsAddressInEra AsAlonzoEra) addr of
    Just (Right a) -> pure a
    _ -> throwError $ OtherError "Can not parse address"
