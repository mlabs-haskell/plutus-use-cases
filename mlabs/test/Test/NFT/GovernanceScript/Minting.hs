module Test.NFT.GovernanceScript.Minting (
  testMinting,
) where

import Data.Semigroup ((<>))
import Test.Tasty (TestTree, localOption)
import PlutusTx.Prelude hiding ((<>))
import Test.NFT.Script.Values as TestValues
import Mlabs.NFT.Governance.Types qualified as NFT
import Mlabs.NFT.Governance.Validation qualified as NFT
import PlutusTx qualified
import Ledger qualified
import Test.Tasty.Plutus.Context
import Test.Tasty.Plutus.Script.Unit

testMinting :: TestTree
testMinting =
  localOption (TestCurrencySymbol TestValues.nftCurrencySymbol) $
  withMintingPolicy "Test NFT-Gov minting policy" nftGovMintPolicy $ do
    shouldValidate "Valid init" validInitData validInitCtx

validInitCtx :: ContextBuilder 'ForMinting
validInitCtx =
  mintsWithSelf TestValues.testTokenName 1

validInitData :: TestData 'ForMinting
validInitData = MintingTest NFT.InitialiseGov

nftGovMintPolicy :: Ledger.MintingPolicy
nftGovMintPolicy =
  Ledger.mkMintingPolicyScript $
    $$(PlutusTx.compile [||go||])
      `PlutusTx.applyCode` ( $$(PlutusTx.compile [||NFT.mkGovMintPolicy||])
                              `PlutusTx.applyCode` PlutusTx.liftCode TestValues.appInstance
                           )
  where
    go = toTestMintingPolicy
