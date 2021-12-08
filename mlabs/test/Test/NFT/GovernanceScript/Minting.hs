module Test.NFT.GovernanceScript.Minting (
  testMinting,
) where

import Data.Maybe (fromJust)
import Data.Semigroup ((<>))
import Ledger qualified
import Mlabs.NFT.Governance.Types qualified as NFT
import Mlabs.NFT.Governance.Validation qualified as NFT
import Mlabs.NFT.Types (UserId (..))
import PlutusTx qualified
import PlutusTx.Prelude hiding ((<>))
import Test.NFT.GovernanceScript.Values as TestValues
import Test.Tasty (TestTree, localOption)
import Test.Tasty.Plutus.Context
import Test.Tasty.Plutus.Script.Unit

testMinting :: TestTree
testMinting =
  localOption (TestCurrencySymbol TestValues.nftCurrencySymbol) $
    -- localOption (TestValidatorHash govMintValidatorHash) $
    withMintingPolicy "Test NFT-Gov minting policy" nftGovMintPolicy $ do
      shouldValidate "Valid init" validInitData validInitCtx
      shouldn'tValidate "Init: missing list head" validInitData initMissingHeadCtx
      shouldn'tValidate "Init: not minting proof token" validInitData initNoProofTokenCtx
      -- (nodeCanBeInserted && nodeInsertedWithCorrectValues))
      shouldValidate "Valid mint gov (first new node case)" validMintData validMintCtx
      shouldn'tValidate "Can't mint with Proof redeemer" proofRedeemerData proofRedeemerCtx

testGovHead :: NFT.GovLList
testGovHead = NFT.HeadLList (NFT.GovLHead (1 % 100) "") Nothing

validInitCtx :: ContextBuilder 'ForMinting
validInitCtx =
  -- mintsWithSelf TestValues.testTokenName 1
  paysSelf TestValues.uniqueAndProofTokens (NFT.GovDatum testGovHead)
    -- list head with unique token
    <> spendsFromOther govMintValidatorHash TestValues.oneUniqueToken (NFT.GovDatum testGovHead)

initMissingHeadCtx :: ContextBuilder 'ForMinting
initMissingHeadCtx =
  paysSelf TestValues.uniqueAndProofTokens (NFT.GovDatum testGovHead)

initNoProofTokenCtx :: ContextBuilder 'ForMinting
initNoProofTokenCtx =
  paysSelf TestValues.oneUniqueToken (NFT.GovDatum testGovHead)
    -- list head with unique token
    <> spendsFromOther govMintValidatorHash TestValues.oneUniqueToken (NFT.GovDatum testGovHead)

validInitData :: TestData 'ForMinting
validInitData = MintingTest NFT.InitialiseGov

testGovNode :: NFT.GovLList
testGovNode = NFT.NodeLList (UserId TestValues.userOnePkh) NFT.GovLNode Nothing

testGovNewHead :: NFT.GovLList
testGovNewHead = NFT.HeadLList (NFT.GovLHead (1 % 100) "") (Just (UserId TestValues.userOnePkh))

validMintCtx :: ContextBuilder 'ForMinting
validMintCtx =
  -- new head
  paysSelf TestValues.uniqueAndProofTokensPlus1Ada (NFT.GovDatum testGovNewHead)
    -- old head
    <> spendsFromOther govMintValidatorHash TestValues.uniqueAndProofTokens (NFT.GovDatum testGovHead)
    -- inserting the first node
    <> paysOther govMintValidatorHash TestValues.listGovTokens (NFT.GovDatum testGovNode)
    -- freeGov tokens paid to user
    <> paysToPubKey TestValues.userOnePkh TestValues.freeGovTokens
    -- fee
    <> paysToPubKey "" TestValues.oneAda

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
