-- |
module Mlabs.IntegrationTest.PabWbe.TxInfo (
  analyzeBalanced,
  analyzeSigned,
  getBodyAndWitnesses,
  getSomeCardanoApiTx,
) where

import Cardano.Api qualified as C
import Cardano.Ledger.Coin (Coin (Coin))

import Control.Lens ((^.))

import Data.Text (Text)
import Data.Text qualified as Text

import Ledger (CardanoTx, SomeCardanoApiTx (..))

import Mlabs.IntegrationTest.PabWbe.Types
import Mlabs.IntegrationTest.Types
import Mlabs.IntegrationTest.Utils

import Plutus.ChainIndex (
  ChainIndexTx,
  citxCardanoTx,
 )
import Plutus.Contract (Contract, throwError)
import Plutus.Contract.CardanoAPI (fromCardanoTx)

import PlutusTx.Prelude

import Prelude qualified as Hask

analyzeBalanced ::
  ChainIndexTx ->
  ChainIndexTx ->
  TxBodyContent ->
  PabBalanceInfo
analyzeBalanced initial balanced C.TxBodyContent {..} =
  PabBalanceInfo
    { fee = balancedTxFee
    , totalOutsValue = chainIndexTxVal balanced
    , unbalancedInsOuts = getInsOuts initial
    , balancedInsOuts = getInsOuts balanced
    }
  where
    balancedTxFee :: Maybe Coin
    balancedTxFee = case txFee of
      C.TxFeeExplicit _ (C.Lovelace ll) -> Just $ Coin ll
      C.TxFeeImplicit _ -> Nothing

analyzeSigned :: ChainIndexTx -> ChainIndexTx -> Maybe SignInfo
analyzeSigned balanced signed = mkSignInfo <$> getTx balanced <*> getTx signed
  where
    getTx :: ChainIndexTx -> Maybe (C.Tx C.AlonzoEra)
    getTx = (getCardanoTx =<<) . (^. citxCardanoTx)

    getCardanoTx :: SomeCardanoApiTx -> Maybe (C.Tx C.AlonzoEra)
    getCardanoTx (SomeTx tx C.AlonzoEraInCardanoMode) = Just tx
    getCardanoTx _ = Nothing

getBodyAndWitnesses ::
  SomeCardanoApiTx ->
  Contract w s Text (TxBodyContent, ChainIndexTx, [C.KeyWitness C.AlonzoEra])
getBodyAndWitnesses = \case
  SomeTx tx@(C.Tx _ wits) mode@C.AlonzoEraInCardanoMode -> case C.getTxBody tx of
    C.TxBody txbc -> case fromCardanoTx mode tx of
      Right t -> pure (txbc, t, wits)
      Left e ->
        throwError $
          "Failed to get 'ChainIndexTx': "
            Hask.<> Text.pack (Hask.show e)
  _ -> throwError "Failed to get tx information"

getSomeCardanoApiTx :: CardanoTx -> Contract w s Text SomeCardanoApiTx
getSomeCardanoApiTx = either pure throwE -- need the 'Left'
  where
    throwE =
      const $
        throwError "Expected 'SomeCardanoApiTx', got 'Tx' instead"
