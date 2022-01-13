module Test.EfficientNFT.Script.TokenMint (test) where

import Ledger (
  MintingPolicy,
  PaymentPubKeyHash (unPaymentPubKeyHash),
  TxOutRef (txOutRefId),
  mkMintingPolicyScript,
 )
import PlutusTx qualified

import PlutusTx.Prelude hiding (mconcat, mempty, (<>))
import Prelude (mconcat)

import Test.Tasty (TestTree, localOption)
import Test.Tasty.Plutus.Context
import Test.Tasty.Plutus.Options
import Test.Tasty.Plutus.Script.Unit
import Test.Tasty.Plutus.TestData
import Test.Tasty.Plutus.WithScript

import Mlabs.EfficientNFT.Types (
  MintAct (MintToken),
  OwnerData (OwnerData),
  PlatformConfig (PlatformConfig, pcMarketplacePkh, pcMarketplaceShare),
 )

import Mlabs.EfficientNFT.Token (mkPolicy)

import Test.EfficientNFT.Script.Values qualified as TestValues

--- debug imports
import Plutus.V1.Ledger.Ada qualified as Value

test :: TestTree
test =
  localOption (TestTxId $ txOutRefId TestValues.mintTxOutRef) $
    withMintingPolicy "Token policy" testTokenPolicy $ do
      shouldValidate "valid mint" validData validCtx

validData :: TestData ( 'ForMinting MintAct)
validData = MintingTest redeemer $ token TestValues.tokenName 1
  where
    redeemer = MintToken $ OwnerData TestValues.authorPkh TestValues.nftPrice

validCtx :: ContextBuilder ( 'ForMinting r)
validCtx =
  mconcat
    [ input $ Input (PubKeyType $ unPaymentPubKeyHash TestValues.authorPkh) (Value.lovelaceValueOf 1000000)
    ]

-- TODO: move to values ?
testTokenPolicy :: MintingPolicy
testTokenPolicy =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||go||])
      `PlutusTx.applyCode` ( $$(PlutusTx.compile [||mkPolicy||])
                              `PlutusTx.applyCode` PlutusTx.liftCode TestValues.mintTxOutRef
                              `PlutusTx.applyCode` PlutusTx.liftCode TestValues.authorPkh
                              `PlutusTx.applyCode` PlutusTx.liftCode price
                              `PlutusTx.applyCode` PlutusTx.liftCode platformCfg
                              `PlutusTx.applyCode` PlutusTx.liftCode contentHash
                           )
  where
    go = toTestMintingPolicy
    price = toEnum 3
    platformCfg =
      PlatformConfig -- TODO: move to values
        { pcMarketplacePkh = TestValues.platformPkh
        , pcMarketplaceShare = TestValues.nftPrice
        }
    contentHash = "aaa" -- TODO: move to values
