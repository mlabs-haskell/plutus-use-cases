-- TODO: split & move to other modules
module Test.NFT.Test where

import qualified Ledger
import qualified Mlabs.NFT.Validation          as NFT
import           PlutusTx.Prelude hiding ((<>))
import qualified PlutusTx.Prelude as PlutusPrelude
import           Test.NFT.Values               as TestValues
import           Test.Tasty                    (TestTree, testGroup, localOption)
import           Test.Tasty.Plutus.Context
import           Test.Tasty.Plutus.Script.Unit
import qualified PlutusTx
import Data.Semigroup ((<>))

test :: TestTree
test =
  testGroup
    "NFT rewrite script tests"
    [testMinting]

testMinting :: TestTree
testMinting = localOption (TestCurrencySymbol (Ledger.scriptCurrencySymbol nftPolicy)) $
  withMintingPolicy "Test NFT minting policy" nftMintPolicy $ do
      shouldValidate "valid case" validData validContext
      shouldn'tValidate "not minting" validData (baseCtx <> paysToTxScriptCtx)
      shouldn'tValidate "no payee" validData (baseCtx <> mintingCtx)
      shouldn'tValidate "pays wrong amount" validData paysWrongAmountCtx

baseCtx :: ContextBuilder 'ForMinting
baseCtx =
  -- FIXME: hacky way to pass "UTXO not consumed"
  input $ Input (PubKeyType TestValues.authorPkh) TestValues.oneAda

mintingCtx :: ContextBuilder 'ForMinting
mintingCtx = mintsWithSelf TestValues.testTokenName 1

paysToTxScriptCtx :: ContextBuilder 'ForMinting
paysToTxScriptCtx = paysOther NFT.txValHash TestValues.oneNft TestValues.testNftId

paysToWrongScriptCtx :: ContextBuilder 'ForMinting
paysToWrongScriptCtx = paysOther NFT.txValHash TestValues.oneNft TestValues.testNftId

paysWrongAmountCtx :: ContextBuilder 'ForMinting
paysWrongAmountCtx = baseCtx <> mintingCtx
  <> paysOther NFT.txValHash
               (TestValues.oneNft PlutusPrelude.<> TestValues.oneNft)
               TestValues.testNftId

validContext :: ContextBuilder 'ForMinting
validContext = baseCtx <> mintingCtx <> paysToTxScriptCtx

validData :: TestData 'ForMinting
validData = MintingTest ()

nonMintingCtx :: ContextBuilder 'ForMinting
nonMintingCtx = (paysOther NFT.txValHash TestValues.oneNft TestValues.testNftId)
             <> (input $ Input (PubKeyType TestValues.authorPkh) TestValues.oneAda)

nftMintPolicy :: Ledger.MintingPolicy
nftMintPolicy =
  Ledger.mkMintingPolicyScript $
    $$(PlutusTx.compile [||wrap||])
    `PlutusTx.applyCode` ( $$(PlutusTx.compile [||NFT.mkMintPolicy||])
                    `PlutusTx.applyCode` PlutusTx.liftCode TestValues.testStateAddr
                    `PlutusTx.applyCode` PlutusTx.liftCode TestValues.testOref
                    `PlutusTx.applyCode` PlutusTx.liftCode TestValues.testNftId
                         )
  where
    wrap :: (() -> Ledger.ScriptContext -> Bool) ->
           (BuiltinData -> BuiltinData -> ())
    wrap = toTestMintingPolicy
