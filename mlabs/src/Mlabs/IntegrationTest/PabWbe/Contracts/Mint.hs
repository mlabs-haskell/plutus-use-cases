module Mlabs.IntegrationTest.PabWbe.Contracts.Mint (
  Mint (..),
  MintSchema,
  runMintTest,
) where

import Control.Lens ((^.))
import Control.Monad (void)

import Data.Aeson (FromJSON, ToJSON)
import Data.Map qualified as Map
import Data.OpenApi qualified as Schema
import Data.Text (Text)

import GHC.Generics (Generic)

import Ledger (
  PubKeyHash,
  TokenName,
  TxOutRef (TxOutRef),
  ciTxOutValue,
  pubKeyHashAddress,
 )
import Ledger.Ada qualified as Ada
import Ledger.Constraints qualified as Constraints
import Ledger.Scripts (unitRedeemer)
import Ledger.Tx (getCardanoTxId)
import Ledger.Typed.Scripts qualified as Scripts

import Mlabs.IntegrationTest.Checks
import Mlabs.IntegrationTest.Types
import Mlabs.IntegrationTest.Utils

import Playground.Contract qualified as Playground

import Plutus.Contract (
  Contract,
  Endpoint,
  awaitTxConfirmed,
  logInfo,
  submitTxConstraintsWith,
  throwError,
  utxosAt, ownPubKeyHash
 )
import Plutus.Contracts.Currency (
  OneShotCurrency (..),
  curPolicy,
  currencySymbol,
  mintedValue,
 )
import Plutus.Contracts.PubKey qualified as PubKey

import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Prelude hiding (check)

import Prelude qualified as Hask

data Mint = Mint
  { pkh :: PubKeyHash
  , amount :: (TokenName, Integer)
  }
  deriving stock (Hask.Eq, Hask.Ord, Hask.Show, Generic)
  deriving anyclass (FromJSON, ToJSON, Schema.ToSchema, Playground.ToSchema)

type MintSchema = Endpoint "mint" Mint

runMintTest :: Mint -> Contract () MintSchema Text ()
runMintTest mint@Mint {..} = do
  symbol <- currencySymbol <$> mintToken mint
  totalUtxosValue <-
    mconcat
      . fmap (^. ciTxOutValue)
      . Map.elems
      <$> utxosAt (pubKeyHashAddress pkh)
  logInfo @Text . report . currencyMustBeMinted $ MintInfo {..}

mintToken :: Mint -> Contract () MintSchema Text OneShotCurrency
mintToken Mint {..} = do
  ownPkh <- ownPubKeyHash
  (txOutRef, mciTxOut, pkScript) <-
    mapTShowableErr @PubKey.PubKeyError
      . PubKey.pubKeyContract pkh
      $ Ada.adaValueOf 10
  case mciTxOut of
    Nothing -> throwError "No 'ChainIndexTxOut's found for minting"
    Just citxOut -> do
      let theCurrency = mkCurrency txOutRef [amount]
          utxos = Map.singleton txOutRef citxOut
          curVali = curPolicy theCurrency
          lookups =
            Hask.mconcat
              [ Constraints.mintingPolicy curVali
              , Constraints.unspentOutputs utxos
              , Constraints.otherScript $ Scripts.validatorScript pkScript
              ]
          mintTx =
            Hask.mconcat
              [ Constraints.mustSpendScriptOutput txOutRef unitRedeemer
              , Constraints.mustMintValue $ mintedValue theCurrency
              , Constraints.mustBeSignedBy ownPkh
              ]
      txId <-
        getCardanoTxId
          <$> submitTxConstraintsWith @Scripts.Any lookups mintTx
      logInfo @Hask.String $ "Awaiting tx completion: " <> Hask.show txId
      void . awaitTxConfirmed $ txId
      logInfo @Hask.String "Tx confirmed, minting complete"
      pure theCurrency
  where
    -- Taken from Plutus.Contracts.Currency.hs in plutus-use-cases
    -- (not exported)
    mkCurrency :: TxOutRef -> [(TokenName, Integer)] -> OneShotCurrency
    mkCurrency (TxOutRef h i) amts =
      OneShotCurrency
        { curRefTransactionOutput = (h, i)
        , curAmounts = AssocMap.fromList amts
        }
