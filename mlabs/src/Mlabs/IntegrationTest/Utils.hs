module Mlabs.IntegrationTest.Utils (
  chainIndexTxVal,
  getInsOuts,
  decodePkh,
  mkSignInfo,
) where

import Prelude

import Cardano.Api qualified as C

import Control.Lens ((^.))

import Data.Aeson (decode)
import Data.ByteString.Lazy (ByteString)
import Data.List ((\\))
import Data.Maybe (fromMaybe)
import Data.Set (Set)

import Ledger (PubKeyHash, TxIn, Value, outValue)

import Mlabs.IntegrationTest.Types

import Plutus.ChainIndex (ChainIndexTx, ChainIndexTxOutputs (..), citxInputs, citxOutputs)

chainIndexTxVal :: ChainIndexTx -> Value
chainIndexTxVal citx = case citx ^. citxOutputs of
  InvalidTx -> mempty
  ValidTx vs -> mconcat $ (^. outValue) <$> vs

getInsOuts :: ChainIndexTx -> (Set TxIn, ChainIndexTxOutputs)
getInsOuts = (,) <$> (^. citxInputs) <*> (^. citxOutputs)

decodePkh :: ByteString -> PubKeyHash
decodePkh = fromMaybe theImpossible . decode
  where
    theImpossible = error "The impossible happened: failed to decode PKH"

mkSignInfo :: C.Tx C.AlonzoEra -> C.Tx C.AlonzoEra -> SignInfo
mkSignInfo balanced signed =
  SignInfo
    { witnessDiff = balancedWitnesses \\ signedWitnesses
    , ..
    }
  where
    balancedWitnesses = C.getTxWitnesses balanced
    signedWitnesses = C.getTxWitnesses signed
