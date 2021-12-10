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
    withMintingPolicy "Test NFT-Gov minting policy" nftGovMintPolicy $ do
      shouldValidate "Valid init" validInitData validInitCtx
      shouldn'tValidate "Init: missing list head" validInitData initMissingHeadCtx
      shouldn'tValidate "Init: not minting proof token" validInitData initNoProofTokenCtx
      -- (nodeCanBeInserted && nodeInsertedWithCorrectValues))
      shouldValidate "Valid mint gov (first new node case)" validMintData validMintCtx
      shouldValidate "Valid mint gov (new node next to head)" validMintData validMintCtx2
      -- TODO: uncomment after validator is fixed
      -- shouldValidate "Valid mint gov (second node)" validMintData validMintCtx3
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

testGovNode2User1 :: NFT.GovLList
testGovNode2User1 = NFT.NodeLList (UserId TestValues.userOnePkh) NFT.GovLNode Nothing

testGovNode2User2 :: NFT.GovLList
testGovNode2User2 = NFT.NodeLList (UserId TestValues.userTwoPkh) NFT.GovLNode (Just (UserId TestValues.userOnePkh))

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

validMintCtx2 :: ContextBuilder 'ForMinting
validMintCtx2 =
  -- new head
  paysSelf TestValues.uniqueAndProofTokensPlus1Ada (NFT.GovDatum testGovNewHead)
    -- old head
    <> spendsFromOther govMintValidatorHash TestValues.uniqueAndProofTokens (NFT.GovDatum testGovNewHead)
    -- Inserting a new node next to the head, while the head already has one
    -- Adding a new node to head's next. It doesn't matter that head primarily had a next or not.
    <> paysOther govMintValidatorHash TestValues.listGovTokens (NFT.GovDatum testGovNode)
    -- freeGov tokens paid to user
    <> paysToPubKey TestValues.userOnePkh TestValues.freeGovTokens
    -- fee
    <> paysToPubKey "" TestValues.oneAda

validMintCtx3 :: ContextBuilder 'ForMinting
validMintCtx3 =
  -- new head
  paysSelf TestValues.uniqueAndProofTokensPlus1Ada (NFT.GovDatum testGovNewHead)
    -- old head
    <> spendsFromOther govMintValidatorHash TestValues.uniqueAndProofTokens (NFT.GovDatum testGovNewHead)
    -- old node
    <> spendsFromOther govMintValidatorHash TestValues.listGovTokensUser1 (NFT.GovDatum testGovNode)
    -- new node 1, user 2
    <> paysOther govMintValidatorHash TestValues.listGovTokensUser2 (NFT.GovDatum testGovNode2User2)
    -- new node 2, user 1 (from old node)
    <> paysOther govMintValidatorHash TestValues.listGovTokensUser1 (NFT.GovDatum testGovNode2User1)
    -- freeGov tokens paid to user 1
    <> paysToPubKey TestValues.userOnePkh TestValues.freeGovTokensUser1
    -- freeGov tokens paid to user 2
    <> paysToPubKey TestValues.userTwoPkh TestValues.freeGovTokensUser2
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
