{-# LANGUAGE GADTs #-}

module Mlabs.IntegrationTest.Wbe.TestCase (
  TestCase (..),
  Test (..),
  AnyCheck (..),
  getTestCases,
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C

import Control.Monad.Except (liftEither)

import Data.Aeson (decode)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Void (Void)

import Ledger hiding (value)
import Ledger.Constraints qualified as Constraints

import Mlabs.IntegrationTest.Types
import Mlabs.IntegrationTest.Utils
import Mlabs.IntegrationTest.Wbe.CardanoAPI
import Mlabs.IntegrationTest.Wbe.Checks hiding (Balanced)
import Mlabs.IntegrationTest.Wbe.TxBuilder
import Mlabs.IntegrationTest.Wbe.TxInfo
import Mlabs.IntegrationTest.Wbe.Types
import Mlabs.IntegrationTest.Wbe.WbeClient qualified as WbeClient

import Plutus.V1.Ledger.Ada (adaValueOf)

import Prelude

data TestCase = TestCase
  { description :: Text
  , test :: WbeT Test
  }

data Test = Test
  { transaction :: WbeExportTx
  , checks :: [AnyCheck]
  }

data WbeResults = WbeResults
  { balanced :: WbeTx 'Balanced
  , balancedInfo :: BalanceInfo
  , signed :: WbeTx 'Signed
  , signedInfo :: SignInfo
  }

data AnyCheck where
  AnyCheck :: forall a. Reportable (Check a) => Check a -> AnyCheck

getTestCases ::
  C.LocalNodeConnectInfo C.CardanoMode ->
  C.ProtocolParameters ->
  [TestCase]
getTestCases connInfo params =
  (\f -> f connInfo params)
    <$> [ testWallet2Wallet
        , testWallet2WalletEnoughInputs
        , testWallet2WalletNotEnoughInputs
        ]

getWbeResults ::
  C.LocalNodeConnectInfo C.CardanoMode ->
  WbeExportTx ->
  WbeT WbeResults
getWbeResults connInfo exportTx = do
  balanced <- WbeClient.balance exportTx
  balancedInfo <- analyseBalanced (getUTXOs connInfo) exportTx balanced
  signed <- WbeClient.sign balanced
  signedInfo <- liftEither $ analyseSigned balanced signed
  pure WbeResults {..}

-- CASES --
testWallet2Wallet ::
  C.LocalNodeConnectInfo C.CardanoMode ->
  C.ProtocolParameters ->
  TestCase
testWallet2Wallet connInfo params =
  TestCase
    "Transaction from wallet to wallet w/o inputs"
    $ do
      tx <- liftEither mkTx
      WbeResults {..} <- getWbeResults connInfo tx
      pure . Test tx $
        [ AnyCheck . mustBeBalanced $ balancedInfo
        , AnyCheck . feeMustBeAdded $ balancedInfo
        , AnyCheck . witnessesMustBeAdded $ signedInfo
        , AnyCheck . inputsMustBeAdded $ balancedInfo
        , AnyCheck . unbalancedInsOutsShouldNotChange $ balancedInfo
        ]
  where
    mkTx =
      WbeExportTx
        <$> buildTx @Void (C.localNodeNetworkId connInfo) params mempty txC
    pkh =
      decodePkh
        "{\"getPubKeyHash\" : \"5030c2607444fdf06cdd6da1da0c3d5f95f40d5b7ffc61a23dd523d2\"}"
    txC = Constraints.mustPayToPubKey pkh (adaValueOf 5)

testWallet2WalletEnoughInputs ::
  C.LocalNodeConnectInfo C.CardanoMode ->
  C.ProtocolParameters ->
  TestCase
testWallet2WalletEnoughInputs connInfo params =
  TestCase
    "Transaction from wallet to wallet with input that can covers outputs"
    $ do
      tx <- liftEither mkTx
      WbeResults {..} <- getWbeResults connInfo tx
      pure . Test tx $
        [ AnyCheck . mustBeBalanced $ balancedInfo
        , AnyCheck . feeMustBeAdded $ balancedInfo
        , AnyCheck . witnessesMustBeAdded $ signedInfo
        , AnyCheck . cNot . inputsMustBeAdded $ balancedInfo
        , AnyCheck . unbalancedInsOutsShouldNotChange $ balancedInfo
        ]
  where
    mkTx =
      WbeExportTx
        <$> buildTx @Void (C.localNodeNetworkId connInfo) params withInputL withInputC
    pkh =
      decodePkh
        "{\"getPubKeyHash\" : \"5030c2607444fdf06cdd6da1da0c3d5f95f40d5b7ffc61a23dd523d2\"}"

    pkhTo =
      decodePkh
        "{\"getPubKeyHash\" : \"5030c2607444fdf06cdd6da1da0c3d5f95f40d5b7ffc61a23dd523d4\"}"

    refId =
      fromJust . decode $
        "{\"getTxId\" : \"206e4a7e1b8a8004d41546ae28089cc4c00853d4f285c45ca62ba8a81b271f41\"}"

    oref :: TxOutRef = TxOutRef refId 2
    txOut =
      PublicKeyChainIndexTxOut
        (pubKeyHashAddress pkh)
        (adaValueOf 10)
    utxos = Map.singleton oref txOut
    withInputC =
      mconcat
        [ Constraints.mustSpendPubKeyOutput oref
        , Constraints.mustPayToPubKey pkhTo (adaValueOf 5)
        ]
    withInputL = Constraints.unspentOutputs utxos

testWallet2WalletNotEnoughInputs ::
  C.LocalNodeConnectInfo C.CardanoMode ->
  C.ProtocolParameters ->
  TestCase
testWallet2WalletNotEnoughInputs connInfo params =
  TestCase
    "Transaction from wallet to wallet with some inputs, but not enough to cover outputs"
    $ do
      tx <- liftEither mkTx
      WbeResults {..} <- getWbeResults connInfo tx
      pure . Test tx $
        [ AnyCheck . mustBeBalanced $ balancedInfo
        , AnyCheck . feeMustBeAdded $ balancedInfo
        , AnyCheck . witnessesMustBeAdded $ signedInfo
        , AnyCheck . inputsMustBeAdded $ balancedInfo
        , AnyCheck . unbalancedInsOutsShouldNotChange $ balancedInfo
        ]
  where
    mkTx =
      WbeExportTx
        <$> buildTx @Void (C.localNodeNetworkId connInfo) params withInputL withInputC
    pkh =
      decodePkh
        "{\"getPubKeyHash\" : \"5030c2607444fdf06cdd6da1da0c3d5f95f40d5b7ffc61a23dd523d2\"}"

    pkhTo =
      decodePkh
        "{\"getPubKeyHash\" : \"5030c2607444fdf06cdd6da1da0c3d5f95f40d5b7ffc61a23dd523d4\"}"

    refId =
      fromJust . decode $
        "{\"getTxId\" : \"206e4a7e1b8a8004d41546ae28089cc4c00853d4f285c45ca62ba8a81b271f41\"}"

    oref = TxOutRef refId 2
    txOut =
      PublicKeyChainIndexTxOut
        (pubKeyHashAddress pkh)
        (adaValueOf 10)
    utxos = Map.singleton oref txOut
    withInputC =
      mconcat
        [ Constraints.mustSpendPubKeyOutput oref
        , Constraints.mustPayToPubKey pkhTo (adaValueOf 15)
        ]
    withInputL = Constraints.unspentOutputs utxos
