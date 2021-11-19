module Mlabs.IntegrationTest.PabWbe.Contracts.Mint (
  Mint (..),
  MintSchema,
  mintToken,
) where

import Control.Monad (void)

import Data.Aeson (FromJSON, ToJSON)
import Data.Map qualified as Map
import Data.OpenApi qualified as Schema
import Data.Text (Text)

import GHC.Generics (Generic)

import Ledger (PubKeyHash, TokenName, TxOutRef (TxOutRef))
import Ledger.Ada qualified as Ada
import Ledger.Constraints qualified as Constraints
import Ledger.Tx (getCardanoTxId)
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Scripts (unitRedeemer)

import Mlabs.IntegrationTest.Utils

import Playground.Contract qualified as Playground

import Plutus.Contract (Contract, Endpoint, awaitTxConfirmed, logInfo, submitTxConstraintsWith, throwError)
import Plutus.Contracts.Currency (
  OneShotCurrency (..),
  curPolicy,
  mintedValue,
 )
import Plutus.Contracts.PubKey qualified as PubKey

import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Prelude

import Prelude qualified as Hask

data Mint = Mint
  { pkh :: PubKeyHash
  , amounts :: [(TokenName, Integer)]
  }
  deriving stock (Hask.Eq, Hask.Ord, Hask.Show, Generic)
  deriving anyclass (FromJSON, ToJSON, Schema.ToSchema, Playground.ToSchema)

type MintSchema = Endpoint "mint" Mint

mintToken :: Mint -> Contract () MintSchema Text ()
mintToken Mint {..} = do
  (txOutRef, mciTxOut, pkScript) <-
    mapTShowableErr @PubKey.PubKeyError
      . PubKey.pubKeyContract pkh
      $ Ada.adaValueOf 10
  case mciTxOut of
    Nothing -> throwError "No 'ChainIndexTxOut's found for minting"
    Just citxOut -> do
      let theCurrency = mkCurrency txOutRef amounts
          utxos = Map.singleton txOutRef citxOut
          curVali = curPolicy theCurrency
          lookups = 
            Hask.mconcat 
              [ Constraints.mintingPolicy curVali
              , Constraints.unspentOutputs utxos
              , Constraints.otherScript  (Scripts.validatorScript pkScript)
              ]
          mintTx =
            Constraints.mustSpendScriptOutput txOutRef unitRedeemer
              Hask.<> Constraints.mustMintValue (mintedValue theCurrency)
      txId <-
        getCardanoTxId
          <$> submitTxConstraintsWith @Scripts.Any lookups mintTx
      logInfo @Hask.String $ "Awaiting tx completion: " <> Hask.show txId
      void . awaitTxConfirmed $ txId
      logInfo @Hask.String "Tx confirmed, minting complete"
  where
    -- Taken from Plutus.Contracts.Currency.hs in plutus-use-cases
    -- (not exported)
    mkCurrency :: TxOutRef -> [(TokenName, Integer)] -> OneShotCurrency
    mkCurrency (TxOutRef h i) amts =
      OneShotCurrency
        { curRefTransactionOutput = (h, i)
        , curAmounts = AssocMap.fromList amts
        }
