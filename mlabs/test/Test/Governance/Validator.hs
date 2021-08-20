{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Governance.Validator (
  test,
) where

import PlutusTx.Prelude

import PlutusTx (fromBuiltinData)
import Ledger
import Plutus.Contract.Test
import Ledger.Ada
import Ledger.Value as Value

import Mlabs.Governance.Contract.Validation (AssetClassGov(..),GovernanceRedeemer(..), GovernanceDatum(..), mkValidator)

import Test.Tasty ( TestTree, defaultMain, testGroup, TestName )
import Test.Tasty.HUnit
import Test.ValidatorTestFramework
-- import Test.TestHelper

-- import Control.Applicative (liftA2)
import qualified Debug.Trace as Tr



acGOV :: AssetClassGov
acGOV  = AssetClassGov "ff" "GOVToken"

red = GRDeposit 2
dat = GovernanceDatum (PubKeyHash "ff") (CurrencySymbol "aa")

test :: TestTree
test = 
  testGroup "Validator" 
    [shouldFail "DD" acGOV $ builderRedeem red (Value.singleton "ff" "GOVToken" 1) dat
    ]


shouldFail :: TestName -> AssetClassGov -> TestContextBuilder -> TestTree
shouldFail testName acGOV tstCtx = 
  -- testCase testName $ False @?= False
  testCase testName (executeSpendContext _mkValidator tstCtx always @?= False)
  where
    _mkValidator dat rdmr ctx = 
      case liftA2 (,) (fromBuiltinData dat) (fromBuiltinData rdmr) of
      Just (dat', rdmr') -> mkValidator acGOV dat' rdmr' ctx
      _                  -> False 


