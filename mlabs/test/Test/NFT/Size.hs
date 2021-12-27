module Test.NFT.Size (test) where

import Plutus.V1.Ledger.Scripts (Script, fromCompiledCode)
import PlutusTx qualified
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Plutus.Script.Size (fitsOnChain)

import Mlabs.NFT.Validation (mkTxPolicy)
import Mlabs.NFT.Governance.Validation (mkGovScript)

test :: TestTree
test = testGroup "Size" [testNftFitsOnChain, testGovFitsOnChain]

testNftFitsOnChain :: TestTree
testNftFitsOnChain = fitsOnChain "NFT marketplace" scriptNft

testGovFitsOnChain :: TestTree
testGovFitsOnChain = fitsOnChain "Governance" scriptGov

scriptNft :: Script
scriptNft = fromCompiledCode $$(PlutusTx.compile [||mkTxPolicy||])

scriptGov :: Script
scriptGov = fromCompiledCode $$(PlutusTx.compile [||mkGovScript||])
