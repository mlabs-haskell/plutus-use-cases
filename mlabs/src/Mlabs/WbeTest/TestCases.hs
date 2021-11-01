module Mlabs.WbeTest.TestCases (
  getTestTxs
) where

import Prelude
import PlutusTx.Prelude ((%))

import Data.Map qualified as Map
import Data.Void (Void)
import Data.Maybe (fromJust)
import Data.Aeson (decode)

import Control.Monad.Trans.Except (except)

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C

import Ledger hiding (value)
import Ledger.Constraints qualified as Constraints
import Plutus.V1.Ledger.Ada (adaValueOf, lovelaceValueOf)

import Mlabs.NFT.Types (Content (..), MintParams (..), NftId (..), UserId (..), Title(..))
import Mlabs.WbeTest.Types
import Mlabs.WbeTest.TxBuilder

-- todo need some convenient way to get list of test transactions
getTestTxs params connInfo = 
  mapM (except . \f -> f params connInfo)
    [ 
      -- txWalletToWallet
    -- , 
    txWithEnoughInput
    -- , txWithNotEnoughInput
    -- , 
    , nftContractMintTx
    ]


-- inputs should be added
txWalletToWallet  params connInfo = 
  WbeExportTx 
    <$> buildTx @Void (C.localNodeNetworkId connInfo) params mempty txC
  where
    pkh = fromJust $ decode @PubKeyHash
      "{\"getPubKeyHash\" : \"5030c2607444fdf06cdd6da1da0c3d5f95f40d5b7ffc61a23dd523d2\"}"
    txC = Constraints.mustPayToPubKey pkh (adaValueOf 5)

-- inputs should NOT be added
txWithEnoughInput  params connInfo = WbeExportTx 
              <$> buildTx @Void (C.localNodeNetworkId connInfo) params withInputL withInputC
  where
    pkh = fromJust $ decode @PubKeyHash
      "{\"getPubKeyHash\" : \"5030c2607444fdf06cdd6da1da0c3d5f95f40d5b7ffc61a23dd523d2\"}"

    refId = fromJust . decode
           $ "{\"getTxId\" : \"206e4a7e1b8a8004d41546ae28089cc4c00853d4f285c45ca62ba8a81b271f41\"}"

    oref :: TxOutRef = TxOutRef refId 2
    txOut = PublicKeyChainIndexTxOut 
              (pubKeyHashAddress pkh)
              (adaValueOf 10)
    utxos = Map.singleton oref txOut
    withInputC = 
      mconcat 
        [ Constraints.mustSpendPubKeyOutput oref
        ]
    withInputL = Constraints.unspentOutputs utxos


-- inputs should be added
txWithNotEnoughInput  params connInfo = WbeExportTx 
              <$> buildTx @Void (C.localNodeNetworkId connInfo) params withInputL withInputC
  where
    pkh = fromJust $ decode @PubKeyHash
      "{\"getPubKeyHash\" : \"5030c2607444fdf06cdd6da1da0c3d5f95f40d5b7ffc61a23dd523d2\"}"

    refId = fromJust . decode
           $ "{\"getTxId\" : \"206e4a7e1b8a8004d41546ae28089cc4c00853d4f285c45ca62ba8a81b271f41\"}"

    oref :: TxOutRef = TxOutRef refId 2
    txOut = PublicKeyChainIndexTxOut 
              (pubKeyHashAddress pkh)
              (adaValueOf 10)
    utxos = Map.singleton oref txOut
    
    withInputC = 
      mconcat 
        [ Constraints.mustSpendPubKeyOutput oref
        , Constraints.mustPayToPubKey pkh (lovelaceValueOf 9999998)
        ]
    withInputL = Constraints.unspentOutputs utxos

nftContractMintTx ps ni = WbeExportTx <$> buildMintTx ps ni mintBuilder
  where
    pkh = fromJust $ decode @PubKeyHash
      "{\"getPubKeyHash\" : \"5030c2607444fdf06cdd6da1da0c3d5f95f40d5b7ffc61a23dd523d2\"}"

    refId = fromJust . decode
           $ "{\"getTxId\" : \"206e4a7e1b8a8004d41546ae28089cc4c00853d4f285c45ca62ba8a81b271f41\"}"

    oref :: TxOutRef = TxOutRef refId 2
    txOut = PublicKeyChainIndexTxOut 
              (pubKeyHashAddress pkh)
              (adaValueOf 3)
    utxos = Map.singleton oref txOut

    mintBuilder = MintBuilder 
      { params = MintParams
          { mp'content = Content "A painting."
          , mp'title = Title "Fiona Lisa"
          , mp'share = 1 % 10
          , mp'price = Just 5
          }
      , user = UserId pkh
      , ..
      }