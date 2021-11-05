module Mlabs.IntegrationTest.Types (
  BalanceInfo (..),
  SignInfo (..),
  decodePkh,
) where

import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.Coin (Coin)

import Data.Aeson (decode)
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (fromMaybe)
import Data.Set (Set)

import GHC.Generics (Generic)

import Ledger (TxIn, Value)

import Plutus.ChainIndex (
  ChainIndexTxOutputs (..),
 )

import Ledger.Crypto (PubKeyHash)
import Prelude

data BalanceInfo = BalanceInfo
  { fromWalletTotalValue :: Value
  , txInFromWallet :: Set TxIn
  , fee :: Maybe Coin
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

decodePkh :: ByteString -> PubKeyHash
decodePkh = fromMaybe theImpossible . decode
  where
    theImpossible = error "The impossible happened: failed to decode PKH"
