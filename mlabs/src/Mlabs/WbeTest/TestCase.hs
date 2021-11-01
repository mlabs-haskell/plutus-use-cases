{-# LANGUAGE GADTs #-}

module Mlabs.WbeTest.TestCase (
  TestCase (..),
  Test (..),
  AnyCheck (..),
  getTestCases,
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C

import Data.Text (Text)

import Mlabs.WbeTest.CardanoAPI
import Mlabs.WbeTest.Checks hiding (Balanced)
import Mlabs.WbeTest.TxInfo
import Mlabs.WbeTest.Types
import Mlabs.WbeTest.WbeClient qualified as WbeClient

import Control.Monad.Except (liftEither)
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
getTestCases connInfo _ = [testWallet2Wallet]
  where
    testWallet2Wallet :: TestCase
    testWallet2Wallet =
      TestCase
        "Transaction from wallet to wallet w/o inputs"
        $ do
          WbeResults {..} <- getWbeResults connInfo undefined
          error "TODO"

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
