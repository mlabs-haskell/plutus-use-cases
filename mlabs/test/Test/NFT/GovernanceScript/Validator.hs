module Test.NFT.GovernanceScript.Validator (
  testScriptValidator,
) where

import Data.Semigroup ((<>))
import Ledger qualified
import Mlabs.NFT.Governance.Types qualified as NFT
import Mlabs.NFT.Governance.Validation qualified as NFT

import PlutusTx.Prelude hiding ((<>))

import Mlabs.NFT.Governance
import PlutusTx qualified
import Test.NFT.Script.Values as TestValues
import Test.Tasty (TestTree)
import Test.Tasty.Plutus.Context
import Test.Tasty.Plutus.Script.Unit

testScriptValidator :: TestTree
testScriptValidator = withValidator "Test NFT GOV script validator" scriptValidator $ do
  shouldValidate "Valid init" validInitData validInitContext

validInitData :: TestData 'ForSpending
validInitData = SpendingTest dtm redeemer val
  where
    dtm = validInitDatum

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
