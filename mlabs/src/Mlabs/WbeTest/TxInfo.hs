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

import System.Exit (die)

data BalanceInfo = BalanceInfo
  { utxoInputs :: Map TxIn TxOut
  , txInFromWallet :: Set TxIn
  , fee :: Maybe Coin
  , lookupsTotalValue :: Value
  , utxosTotalValue :: Value
  , totalOutsValue :: Value
  }
  deriving stock (Show, Eq, Generic)

data SignInfo = SignInfo
  { balancedWitnesses :: [C.KeyWitness C.AlonzoEra]
  , signedWitnesses :: [C.KeyWitness C.AlonzoEra]
  , witnessDiff :: [C.KeyWitness C.AlonzoEra]
  }
  deriving stock (Show, Eq, Generic)

type UTXOGetter = Set TxIn -> IO (Either String (Map TxIn TxOut))

analyseBalanced ::
  UTXOGetter ->
  WbeExportTx ->
  WbeTx 'Balanced ->
  IO BalanceInfo
analyseBalanced utxosGetter (WbeExportTx (ExportTx apiTx lookups _)) wtx =
  case (,) <$> toChainIndexTx apiTx <*> parseTx wtx of
    Left e -> die e
    Right (initial, balanced) -> do
      print $ "Added from Wallet:\n" ++ show fromWallet
      utxoInputs <- either die pure =<< utxosGetter fromWallet
      print $ "TXOs from Wallet:\n" ++ show utxoInputs
      pure $
        BalanceInfo
          { lookupsTotalValue = lookupsVal lookups
          , utxosTotalValue = utxosVal utxoInputs
          , totalOutsValue = chainIndexTxVal balanced
          , txInFromWallet = fromWallet
          , fee = balancedTxFee balanced
          , ..
          }
      where
        fromWallet :: Set TxIn
        fromWallet = addedByWallet balanced initial 

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

analyseSigned :: WbeTx 'Balanced -> WbeTx 'Signed -> Either String SignInfo
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
