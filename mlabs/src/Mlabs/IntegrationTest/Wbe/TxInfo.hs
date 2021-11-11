module Mlabs.IntegrationTest.Wbe.TxInfo (
  UTXOGetter,
  analyseBalanced,
  analyseSigned,
) where

import Cardano.Api qualified as C

import Control.Lens ((^.))
import Control.Monad.Error.Class (liftEither)
import Control.Monad.IO.Class (liftIO)

import Data.Bifunctor (second)
import Data.Map (Map)
import Data.Set (Set)

import Mlabs.IntegrationTest.Types
import Mlabs.IntegrationTest.Utils
import Mlabs.IntegrationTest.Wbe.TxRead
import Mlabs.IntegrationTest.Wbe.Types

import Ledger (SomeCardanoApiTx (SomeTx), TxIn, TxOut, Value)

import Plutus.ChainIndex (ChainIndexTx (..), citxCardanoTx,)
import Plutus.Contract.Wallet (ExportTx (..))

import Prelude

type UTXOGetter = Set TxIn -> IO (Either TestError (Map TxIn TxOut))

analyseBalanced ::
  UTXOGetter ->
  WbeExportTx ->
  WbeTx 'Balanced ->
  WbeT BalanceInfo
analyseBalanced utxosGetter (ExportTx apiTx _ _) wtx = do
  initial <- liftEither $ toChainIndexTx apiTx
  balanced <- liftEither $ parseTx wtx

  let fromWallet = inputsDifference balanced initial

  _ <- liftEither =<< liftIO (utxosGetter fromWallet)

  pure $
    BalanceInfo
      { totalOutsValue = chainIndexTxVal balanced
      , totalInsValue = error "FIXME"
      , txInFromWallet = fromWallet
      , fee = balancedTxFee balanced
      , unbalancedInsOuts = getInsOuts initial
      , balancedInsOuts = getInsOuts balanced
      , ..
      }

analyseSigned :: WbeTx 'Balanced -> WbeTx 'Signed -> Either TestError SignInfo
analyseSigned btx stx =
  second (uncurry mkSignInfo) $
    (,) <$> parseApiTx btx <*> parseApiTx stx

balancedTxFee :: ChainIndexTx -> Maybe Value
balancedTxFee balanced = case balanced ^. citxCardanoTx of
  Just (SomeTx tx C.AlonzoEraInCardanoMode) -> case C.getTxBody tx of
    C.TxBody bodyContent -> feeFromBodyContent bodyContent
  _ -> Nothing
