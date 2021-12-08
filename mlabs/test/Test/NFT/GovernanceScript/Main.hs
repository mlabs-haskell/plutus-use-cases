module Test.NFT.GovernanceScript.Main where

import Test.NFT.GovernanceScript.Minting (testMinting)
import Test.NFT.GovernanceScript.Validator (testScriptValidator)
import Test.Tasty (TestTree, testGroup)

test :: TestTree
test =
  testGroup
    "Governance script"
    [ testMinting
    , testScriptValidator
    ]
