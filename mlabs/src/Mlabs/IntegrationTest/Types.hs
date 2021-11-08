module Mlabs.IntegrationTest.Types (
  BalanceInfo (..),
  SignInfo (..),
  TxBodyContent,
) where

import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.Coin (Coin)

import Data.Set (Set)

import GHC.Generics (Generic)

import Ledger (TxIn, Value)

import Plutus.ChainIndex (
  ChainIndexTxOutputs (..),
 )

import Prelude

type TxBodyContent = C.TxBodyContent C.ViewTx C.AlonzoEra

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
