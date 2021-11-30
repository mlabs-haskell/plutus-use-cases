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
import GHC.Generics (Generic)
import Schema (ToSchema)
import Data.OpenApi.Schema qualified as OpenApi

import Ledger (PubKeyHash, Value)
import Ledger.Constraints (adjustUnbalancedTx, mustPayToPubKey)
import Plutus.Contract (ContractError, Endpoint, Promise, endpoint, mkTxConstraints, yieldUnbalancedTx, logWarn, logInfo, runError)


import Mlabs.Demo.PreBalance (PrebalancedTx(..), preBalanceTxFrom)

-- |Contract activation args
data ContractArgs = ContractArgs
  { ownPkh :: PubKeyHash
  }
  deriving (Hask.Eq, Hask.Ord, Hask.Show, Generic)
  deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema)

-- Contract API
data PayToWalletParams =
    PayToWalletParams
        { amount :: Value
        , pkh    :: PubKeyHash
        }
        deriving stock (Hask.Eq, Hask.Show, Generic)
        deriving anyclass (ToJSON, FromJSON, ToSchema)

type DemoSchema = Endpoint "call-demo" PayToWalletParams

-- | Off-chain
runDemo :: ContractArgs -> Promise () DemoSchema ContractError ()
runDemo (ContractArgs opkh) = endpoint @"call-demo" $ \ps -> do
    runError (run ps) >>= \case
      Left err -> logWarn @Hask.String (Hask.show @ContractError err)
      Right () -> pure ()
  where  
    run PayToWalletParams{amount, pkh} = do
      utx <- mkTxConstraints @Void Hask.mempty (mustPayToPubKey pkh amount)
      PrebTx pUtx <- preBalanceTxFrom opkh  utx
      logInfo @Hask.String $ "Yielding tx"
      yieldUnbalancedTx pUtx
