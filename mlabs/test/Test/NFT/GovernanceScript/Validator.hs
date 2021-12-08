module Test.NFT.GovernanceScript.Validator (
  testScriptValidator,
) where

import Data.Semigroup ((<>))
import Ledger qualified
import Mlabs.NFT.Governance.Types qualified as NFT
import Mlabs.NFT.Governance.Validation qualified as NFT
import Mlabs.NFT.Types (UserId (..))

import PlutusTx.Prelude hiding ((<>))

import Mlabs.NFT.Governance
import PlutusTx qualified
import Test.NFT.GovernanceScript.Values as TestValues
import Test.Tasty (TestTree, localOption)
import Test.Tasty.Plutus.Context
import Test.Tasty.Plutus.Script.Unit

testScriptValidator :: TestTree
testScriptValidator =
  localOption (TestValidatorHash govMintValidatorHash) $
    withValidator "Test NFT-Gov script validator" scriptValidator $ do
      shouldValidate "Valid init" validInitData validInitContext
      shouldn'tValidate "Init: missing unique token" initMissingUniqueTokenData validInitContext
      -- TODO: ?? recheck this
      shouldn'tValidate "Init: no proof token in outputs" validInitData initNoProofTokenOutputContext
      shouldn'tValidate "Init: missing head in outputs" validInitData initMissingHeadContext

govHeadDatum :: NFT.GovDatum
govHeadDatum = NFT.GovDatum $ NFT.HeadLList (NFT.GovLHead (5 % 1000) "") Nothing

govNodeDatum :: NFT.GovDatum
govNodeDatum = NFT.GovDatum $ NFT.NodeLList (UserId TestValues.userOnePkh) NFT.GovLNode Nothing

validInitData :: TestData 'ForSpending
validInitData = SpendingTest dtm redeemer val
  where
    dtm = govHeadDatum

    redeemer = NFT.InitialiseGov

    val = TestValues.oneUniqueToken

validInitContext :: ContextBuilder 'ForSpending
validInitContext =
  paysOther TestValues.govMintValidatorHash TestValues.uniqueAndProofTokens govHeadDatum

-- TODO: this fails with PT8 cek error
-- paysOther TestValues.govMintValidatorHash TestValues.oneProofToken govHeadDatum

initNoProofTokenOutputContext :: ContextBuilder 'ForSpending
initNoProofTokenOutputContext =
  paysOther TestValues.govMintValidatorHash TestValues.oneUniqueToken govHeadDatum

initMissingHeadContext :: ContextBuilder 'ForSpending
initMissingHeadContext =
  -- fails with PT8 cek error
  paysOther TestValues.govMintValidatorHash TestValues.uniqueAndProofTokens govNodeDatum

initMissingUniqueTokenData :: TestData 'ForSpending
initMissingUniqueTokenData = SpendingTest dtm redeemer val
  where
    dtm = govHeadDatum

    redeemer = NFT.InitialiseGov

    val = TestValues.adaValue 0

scriptValidator :: Ledger.Validator
scriptValidator =
  Ledger.mkValidatorScript $
    $$(PlutusTx.compile [||wrap||])
      `PlutusTx.applyCode` ($$(PlutusTx.compile [||NFT.mkGovScript||]) `PlutusTx.applyCode` PlutusTx.liftCode TestValues.uniqueAsset)
  where
    wrap ::
      (NFT.GovDatum -> NFT.GovAct -> Ledger.ScriptContext -> Bool) ->
      (BuiltinData -> BuiltinData -> BuiltinData -> ())
    wrap = toTestValidator
