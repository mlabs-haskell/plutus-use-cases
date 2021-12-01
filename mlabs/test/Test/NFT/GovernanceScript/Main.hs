module Test.NFT.GovernanceScript.Main where

import Test.NFT.GovernanceScript.Minting
import Test.Tasty (TestTree, testGroup)

test :: TestTree
test =
  testGroup
    "Governance script"
    [ testMinting
    ]
