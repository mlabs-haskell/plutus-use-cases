module Test.NFT.GovernanceScript.Minting (
  testMinting,
) where

import Data.Semigroup ((<>))
import Ledger qualified
import Mlabs.NFT.Governance.Types qualified as NFT
import Mlabs.NFT.Governance.Validation qualified as NFT
import PlutusTx qualified
import PlutusTx.Prelude hiding ((<>))
import Test.NFT.GovernanceScript.Values as TestValues
import Test.Tasty (TestTree, localOption)
import Test.Tasty.Plutus.Context
import Test.Tasty.Plutus.Script.Unit

testMinting :: TestTree
testMinting =
  localOption (TestCurrencySymbol TestValues.nftCurrencySymbol) $
    withMintingPolicy "Test NFT-Gov minting policy" nftGovMintPolicy $ do
      shouldValidate "Valid init" validInitData validInitCtx
      shouldValidate "Valid mint gov" validMintData validMintCtx
      shouldn'tValidate "Can't mint with Proof redeemer" proofRedeemerData proofRedeemerCtx

testGovHead :: NFT.GovLList
testGovHead = NFT.HeadLList (NFT.GovLHead (1 % 100)) Nothing

validInitCtx :: ContextBuilder 'ForMinting
validInitCtx =
  mintsWithSelf TestValues.testTokenName 1
    <> paysSelf TestValues.oneProofToken ()
    <> paysSelf TestValues.oneUniqueToken (NFT.GovDatum testGovHead)
    <> (input $ Input (PubKeyType TestValues.authorPkh) TestValues.oneUniqueToken)

validInitData :: TestData 'ForMinting
validInitData = MintingTest NFT.InitialiseGov

validMintCtx :: ContextBuilder 'ForMinting
validMintCtx =
  paysSelf TestValues.oneUniqueToken (NFT.GovDatum testGovHead)

validMintData :: TestData 'ForMinting
validMintData = MintingTest NFT.MintGov

proofRedeemerCtx :: ContextBuilder 'ForMinting
proofRedeemerCtx = mintsWithSelf TestValues.testTokenName 1

proofRedeemerData :: TestData 'ForMinting
proofRedeemerData = MintingTest NFT.Proof

nftGovMintPolicy :: Ledger.MintingPolicy
nftGovMintPolicy =
  Ledger.mkMintingPolicyScript $
    $$(PlutusTx.compile [||go||])
      `PlutusTx.applyCode` ( $$(PlutusTx.compile [||NFT.mkGovMintPolicy||])
                              `PlutusTx.applyCode` PlutusTx.liftCode TestValues.appInstance
                           )
  where
    go = toTestMintingPolicy
