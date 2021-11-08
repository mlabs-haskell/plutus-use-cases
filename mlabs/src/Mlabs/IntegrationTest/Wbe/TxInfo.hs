module Mlabs.IntegrationTest.Wbe.TxInfo (
  UTXOGetter,
  analyseBalanced,
  analyseSigned,
) where

import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.Alonzo.TxBody qualified as C
import Cardano.Ledger.Coin (Coin)

import Control.Lens ((^.))
import Control.Monad.Error.Class (liftEither)
import Control.Monad.IO.Class (liftIO)

import Data.Bifunctor (second)
import Data.Function (on)
import Data.List ((\\))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

import Mlabs.IntegrationTest.Types
import Mlabs.IntegrationTest.Wbe.TxRead
import Mlabs.IntegrationTest.Wbe.Types

import Ledger (SomeCardanoApiTx (SomeTx), TxIn, TxOut, Value, outValue)

import Plutus.ChainIndex (
  ChainIndexTx (..),
  ChainIndexTxOutputs (..),
  citxCardanoTx,
  citxInputs,
  citxOutputs,
 )
import Plutus.Contract.Wallet (ExportTx (..))

import Prelude

type UTXOGetter = Set TxIn -> IO (Either WbeError (Map TxIn TxOut))

analyseBalanced ::
  UTXOGetter ->
  WbeExportTx ->
  WbeTx 'Balanced ->
  WbeT BalanceInfo
analyseBalanced utxosGetter (WbeExportTx (ExportTx apiTx _ _)) wtx = do
  initial <- liftEither $ toChainIndexTx apiTx
  balanced <- liftEither $ parseTx wtx
  -- debug print
  -- liftIO $ print "-------------------"
  -- liftIO $ print "init"
  -- liftIO $ print "-------------------"
  -- liftIO $ print (initial ^. citxInputs)
  -- liftIO $ print "-------------------"
  -- liftIO $ print (initial ^. citxOutputs)
  -- liftIO $ print "-------------------"
  -- liftIO $ print "balanced"
  -- liftIO $ print "-------------------"
  -- liftIO $ print (balanced ^. citxInputs)
  -- liftIO $ print "-------------------"
  -- liftIO $ print (balanced ^. citxOutputs)
  -- liftIO $ print "-------------------"

  let fromWallet = addedByWallet balanced initial

  utxosFoundInWallet <- liftEither =<< liftIO (utxosGetter fromWallet)

  pure $
    BalanceInfo
      { fromWalletTotalValue = utxosVal utxosFoundInWallet
      , totalOutsValue = chainIndexTxVal balanced
      , txInFromWallet = fromWallet
      , fee = balancedTxFee balanced
      , unbalancedIsOuts = getInsOuts initial
      , balancedIsOuts = getInsOuts balanced
      , ..
      }
  where
    addedByWallet :: ChainIndexTx -> ChainIndexTx -> Set TxIn
    addedByWallet = Set.difference `on` (^. citxInputs)

    chainIndexTxVal :: ChainIndexTx -> Value
    chainIndexTxVal citx = case citx ^. citxOutputs of
      InvalidTx -> mempty
      ValidTx vs -> mconcat $ (^. outValue) <$> vs

    utxosVal :: Map TxIn TxOut -> Value
    utxosVal = mconcat . fmap (^. outValue) . Map.elems

analyseSigned :: WbeTx 'Balanced -> WbeTx 'Signed -> Either WbeError SignInfo
analyseSigned btx stx =
  second (uncurry mkSignInfo) $
    (,) <$> parseApiTx btx <*> parseApiTx stx
  where
    mkSignInfo :: C.Tx C.AlonzoEra -> C.Tx C.AlonzoEra -> SignInfo
    mkSignInfo balanced signed =
      SignInfo
        { witnessDiff = balancedWitnesses \\ signedWitnesses
        , ..
        }
      where
        balancedWitnesses = C.getTxWitnesses balanced
        signedWitnesses = C.getTxWitnesses signed

balancedTxFee :: ChainIndexTx -> Maybe Coin
balancedTxFee balanced = case balanced ^. citxCardanoTx of
  Just (SomeTx (C.Tx txBody _) C.AlonzoEraInCardanoMode) -> case txBody of
    C.ShelleyTxBody _ body _ _ _ _ -> Just $ C.txfee body
  _ -> Nothing

getInsOuts :: ChainIndexTx -> (Set TxIn, ChainIndexTxOutputs)
getInsOuts = (,) <$> (^. citxInputs) <*> (^. citxOutputs)
