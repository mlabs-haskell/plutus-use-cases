module Mlabs.WbeTest.TxInfo (
  BalanceInfo (..),
  SignInfo (..),
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
import Data.Either (rights)
import Data.Function (on)
import Data.List ((\\))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

import GHC.Generics (Generic)

import Mlabs.WbeTest.TxRead
import Mlabs.WbeTest.Types

import Ledger (SomeCardanoApiTx (SomeTx), TxIn, TxOut, Value, outValue)

import Plutus.ChainIndex (
  ChainIndexTx (..),
  ChainIndexTxOutputs (..),
  citxCardanoTx,
  citxInputs,
  citxOutputs,
 )
import Plutus.Contract.CardanoAPI qualified as C
import Plutus.Contract.Wallet (ExportTx (..), ExportTxInput (..))

import Prelude

data BalanceInfo = BalanceInfo
  { fromWalletTotalValue :: Value
  , txInFromWallet :: Set TxIn
  , fee :: Maybe Coin
  , lookupsTotalValue :: Value
  , totalOutsValue :: Value
  , unbalancedIsOuts :: (Set TxIn, ChainIndexTxOutputs)
  , balancedIsOuts :: (Set TxIn, ChainIndexTxOutputs)
  }
  deriving stock (Show, Eq, Generic)

data SignInfo = SignInfo
  { balancedWitnesses :: [C.KeyWitness C.AlonzoEra]
  , signedWitnesses :: [C.KeyWitness C.AlonzoEra]
  , witnessDiff :: [C.KeyWitness C.AlonzoEra]
  }
  deriving stock (Show, Eq, Generic)

type UTXOGetter = Set TxIn -> IO (Either WbeError (Map TxIn TxOut))

analyseBalanced ::
  UTXOGetter ->
  WbeExportTx ->
  WbeTx 'Balanced ->
  WbeT BalanceInfo
analyseBalanced utxosGetter (WbeExportTx (ExportTx apiTx lookups _)) wtx = do
  initial <- liftEither $ toChainIndexTx apiTx
  balanced <- liftEither $ parseTx wtx

  let fromWallet = addedByWallet balanced initial

  utxoInputs <- liftEither =<< liftIO (utxosGetter fromWallet)

  pure $
    BalanceCheck
      { lookupsTotalValue = lookupsVal lookups
      , fromWalletTotalValue = utxosVal utxosFoundInWallet
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

    lookupsVal :: [ExportTxInput] -> Value
    lookupsVal =
      mconcat
        . fmap (^. outValue)
        . rights
        . fmap (C.fromCardanoTxOut . txOut)

checkSigned :: WbeTx 'Balanced -> WbeTx 'Signed -> Either WbeError SignCheck
checkSigned btx stx =
  second (uncurry mkSignCheck) $
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
getInsOuts = liftA2 (,) (^. citxInputs) (^. citxOutputs)
