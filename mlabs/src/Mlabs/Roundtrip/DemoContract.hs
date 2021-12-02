{-# LANGUAGE  NamedFieldPuns #-}
module Mlabs.Roundtrip.DemoContract(
    runDemo
    , PayToWalletParams(..)
    , DemoSchema
    , ContractArgs(..)
    ) where

import Prelude qualified as Hask
import PlutusTx.Prelude

import Data.Aeson (FromJSON, ToJSON)
import Data.Void (Void)
import Data.Text (Text)
import GHC.Generics (Generic)
import Schema (ToSchema)
import Data.OpenApi.Schema qualified as OpenApi

import Cardano.Api (AsType (AsAddressInEra, AsAlonzoEra))
import Cardano.Api qualified
import Plutus.Contract.CardanoAPI (FromCardanoError, fromCardanoAddress, toCardanoAddress)

import Ledger (TxOutRef, PubKeyHash, Value, Address, toPubKeyHash)
import Plutus.V1.Ledger.Ada qualified as Ada
import Ledger.Constraints (adjustUnbalancedTx, mustPayToPubKey)
import Plutus.Contract (ContractError(..), Endpoint, Contract, Promise, endpoint, 
  mkTxConstraints, yieldUnbalancedTx, logWarn, logInfo, runError, throwError)


import Mlabs.Roundtrip.PreBalance (PrebalancedTx(..), preBalanceTxFrom)

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
runDemo :: ContractArgs -> Promise () DemoSchema ContractError ()
runDemo (ContractArgs ownAddress) = endpoint @"call-demo" $ \ps -> do
    runError (run ps) >>= \case
      Left err -> logWarn @Hask.String (Hask.show @ContractError err)
      Right () -> pure ()
  where  
    run PayToWalletParams{lovelaceAmount, receiverAddress, collateralRef} = do
      addr <- parseAddress ownAddress
      receiverPKH <- parseAddress receiverAddress >>= getPKH
      utx <- mkTxConstraints @Void (Hask.mempty) (mustPayToPubKey receiverPKH $ Ada.lovelaceValueOf lovelaceAmount)
      PrebTx pUtx <- preBalanceTxFrom addr collateralRef utx
      logInfo @Hask.String $ "Yielding tx"
      yieldUnbalancedTx pUtx


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
