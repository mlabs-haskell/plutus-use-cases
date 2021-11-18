module Mlabs.IntegrationTest.PabWbe.TxInfo (
  analyzeBalanced,
  analyzeSigned,
  getTxAndWitnesses,
  getSomeCardanoApiTx,
) where

import Cardano.Api qualified as C

import Control.Lens ((^.))

import Data.Text (Text)

import Ledger (
  CardanoTx,
  ChainIndexTxOut,
  SomeCardanoApiTx (..),
  ciTxOutValue,
 )

import Mlabs.IntegrationTest.Types
import Mlabs.IntegrationTest.Utils

import Plutus.ChainIndex (ChainIndexTx, citxCardanoTx)
import Plutus.Contract (Contract, throwError)
import Plutus.Contract.CardanoAPI (fromCardanoTx)

import PlutusTx.Prelude

import Prelude qualified as Hask

analyzeBalanced ::
  -- Tx before balancing
  ChainIndexTx ->
  -- Tx after balancing
  ChainIndexTx ->
  -- All tx outs from balanced tx inputs
  [ChainIndexTxOut] ->
  -- Body content from balanced tx
  TxBodyContent ->
  BalanceInfo
analyzeBalanced initial balanced citxOuts bodyContent =
  BalanceInfo
    { fee = feeFromBodyContent bodyContent
    , totalOutsValue = chainIndexTxVal balanced
    , totalInsValue = mconcat $ (^. ciTxOutValue) <$> citxOuts
    , unbalancedInsOuts = getInsOuts initial
    , balancedInsOuts = getInsOuts balanced
    , txInFromWallet = inputsDifference initial balanced
    }

analyzeSigned :: ChainIndexTx -> ChainIndexTx -> Either TestError SignInfo
analyzeSigned balanced signed =
  maybe (Left conversionFailed) Right $
    mkSignInfo <$> getTx balanced <*> getTx signed
  where
    conversionFailed :: TestError
    conversionFailed =
      ConversionError "Failed to convert 'ChainIndexTx's to 'Tx AlonzoEra's"

    getTx :: ChainIndexTx -> Maybe (C.Tx C.AlonzoEra)
    getTx = (getCardanoTx =<<) . (^. citxCardanoTx)

    getCardanoTx :: SomeCardanoApiTx -> Maybe (C.Tx C.AlonzoEra)
    getCardanoTx (SomeTx tx C.AlonzoEraInCardanoMode) = Just tx
    getCardanoTx _ = Nothing

getTxAndWitnesses ::
  SomeCardanoApiTx ->
  Contract w s Text (TxBodyContent, ChainIndexTx)
getTxAndWitnesses = \case
  SomeTx tx@(C.Tx _ _) mode@C.AlonzoEraInCardanoMode -> case C.getTxBody tx of
    C.TxBody txbc -> case fromCardanoTx mode tx of
      Right t -> pure (txbc, t)
      Left e -> throwError $ "Failed to get 'ChainIndexTx': " Hask.<> tshow e
  _ -> throwError "Failed to get tx information"

getSomeCardanoApiTx :: CardanoTx -> Contract w s Text SomeCardanoApiTx
getSomeCardanoApiTx = either pure throwE -- need the 'Left'
  where
    throwE =
      const $
        throwError "Expected 'SomeCardanoApiTx', got 'Tx' instead"
