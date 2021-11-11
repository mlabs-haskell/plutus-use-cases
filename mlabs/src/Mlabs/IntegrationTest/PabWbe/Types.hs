module Mlabs.IntegrationTest.PabWbe.Types (
  PabResults (..),
) where

import GHC.Generics (Generic)

import Mlabs.IntegrationTest.Types

import Plutus.Contract.Wallet (ExportTx)

data PabResults = PabResults
  { balancedInfo :: BalanceInfo
  , signedInfo :: SignInfo
  , exportTx :: ExportTx
  }
  deriving stock (Generic)
