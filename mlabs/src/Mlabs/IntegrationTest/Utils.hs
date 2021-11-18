{-# LANGUAGE NamedFieldPuns #-}

module Mlabs.IntegrationTest.Utils (
  chainIndexTxVal,
  getInsOuts,
  unsafeDecodePkh,
  mkSignInfo,
  toChainIndexTx,
  inputsDifference,
  convertUTxOs,
  feeFromBodyContent,
  mapTShowableErr,
  tshow,
  getExportTxId,
  unsafeDecode,
  utxosVal,
) where

import Prelude

import Cardano.Api qualified as C

import Control.Lens ((^.))

import Data.Aeson (FromJSON, decode)
import Data.Bifunctor (first)
import Data.ByteString.Lazy (ByteString)
import Data.Function (on)
import Data.List ((\\))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text

import Ledger (
  ChainIndexTxOut,
  PubKeyHash,
  TxIn (TxIn),
  TxOut,
  TxOutRef,
  Value,
  outValue,
  toTxOut,
 )
import Ledger.Ada qualified as Ada

import Mlabs.IntegrationTest.Types

import Plutus.ChainIndex (
  ChainIndexTx,
  ChainIndexTxOutputs (..),
  citxInputs,
  citxOutputs,
 )
import Plutus.Contract (Contract)
import Plutus.Contract qualified as Contract
import Plutus.Contract.CardanoAPI (fromCardanoTx)
import Plutus.Contract.Wallet (ExportTx, partialTx)

chainIndexTxVal :: ChainIndexTx -> Value
chainIndexTxVal citx = case citx ^. citxOutputs of
  InvalidTx -> mempty
  ValidTx vs -> mconcat $ (^. outValue) <$> vs

getInsOuts :: ChainIndexTx -> (Set TxIn, ChainIndexTxOutputs)
getInsOuts = (,) <$> (^. citxInputs) <*> (^. citxOutputs)

unsafeDecodePkh :: ByteString -> PubKeyHash
unsafeDecodePkh = unsafeDecode "PKH"

mkSignInfo :: C.Tx C.AlonzoEra -> C.Tx C.AlonzoEra -> SignInfo
mkSignInfo balanced signed =
  SignInfo
    { witnessDiff = balancedWitnesses \\ signedWitnesses
    , ..
    }
  where
    balancedWitnesses = C.getTxWitnesses balanced
    signedWitnesses = C.getTxWitnesses signed

toChainIndexTx :: C.Tx C.AlonzoEra -> Either TestError ChainIndexTx
toChainIndexTx =
  first (DecoderError . show)
    . fromCardanoTx C.AlonzoEraInCardanoMode

inputsDifference :: ChainIndexTx -> ChainIndexTx -> Set TxIn
inputsDifference = Set.difference `on` (^. citxInputs)

utxosVal :: Map TxIn TxOut -> Value
utxosVal = mconcat . fmap (^. outValue) . Map.elems

getExportTxId :: ExportTx -> C.TxId
getExportTxId = C.getTxId . C.getTxBody . partialTx

convertUTxOs :: Map TxOutRef ChainIndexTxOut -> Map TxIn TxOut
convertUTxOs = Map.mapKeys (`TxIn` Nothing) . Map.map toTxOut

feeFromBodyContent :: C.TxBodyContent b e -> Maybe Value
feeFromBodyContent C.TxBodyContent {txFee} = case txFee of
  C.TxFeeExplicit _ (C.Lovelace ll) ->
    Just $
      Ada.lovelaceValueOf ll
  C.TxFeeImplicit _ -> Nothing

mapTShowableErr :: Show e => Contract w s e a -> Contract w s Text a
mapTShowableErr = Contract.mapError tshow

tshow :: Show a => a -> Text
tshow = Text.pack . show

unsafeDecode :: FromJSON a => String -> ByteString -> a
unsafeDecode name = fromMaybe theImpossible . decode
  where
    theImpossible =
      error $
        "The impossible happened: failed to decode "
          <> name
