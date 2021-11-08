module Mlabs.IntegrationTest.PabWbe.Types (
  PabBalanceInfo (..),
  PabWbeResult (..),
) where

import Prelude

import Cardano.Ledger.Coin (Coin)

import Data.Set (Set)

import GHC.Generics (Generic)

import Mlabs.IntegrationTest.Types

import Ledger (TxIn, Value)

import Plutus.ChainIndex (ChainIndexTxOutputs)

data PabBalanceInfo = PabBalanceInfo
  { fee :: Maybe Coin
  , totalOutsValue :: Value
  , unbalancedInsOuts :: (Set TxIn, ChainIndexTxOutputs)
  , balancedInsOuts :: (Set TxIn, ChainIndexTxOutputs)
  }
  deriving stock (Show, Eq, Generic)

data PabWbeResult = PabWbeResult
  { balanced :: PabBalanceInfo
  , signed :: SignInfo
  }
  deriving stock (Generic)
