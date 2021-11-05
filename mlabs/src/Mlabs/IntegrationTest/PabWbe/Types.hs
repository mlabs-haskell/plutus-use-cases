module Mlabs.IntegrationTest.PabWbe.Types (
  PabWbeResult (..),
) where

import GHC.Generics (Generic)

import Mlabs.IntegrationTest.Types

data PabWbeResult = PabWbeResult
  { balanced :: BalanceInfo
  , signed :: SignInfo
  }
  deriving stock (Generic)
