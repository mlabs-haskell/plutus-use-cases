module Mlabs.IntegrationTest.PabWbe.TestStand (
  runTests,
) where

import Data.Foldable (for_, traverse_)
import Data.Row (Empty)
import Data.Text (Text)

import Mlabs.IntegrationTest.Checks
import Mlabs.IntegrationTest.PabWbe.TestCase
import Mlabs.IntegrationTest.Utils

import Plutus.Contract (
  Contract,
  handleError,
  logError,
  logInfo,
  ownPubKeyHash,
 )

import Prelude

runTests :: Contract () Empty Text ()
runTests = do
  pkh <- ownPubKeyHash
  logInfo @Text $ "Running tests contracts with PKH: " <> tshow pkh

  handleError testCaseFailed $
    getTestCases >>= traverse_ runTestCase
  where
    testCaseFailed :: Text -> Contract () Empty Text ()
    testCaseFailed err = logError @Text $ "Test failed with: " <> err

    runTestCase :: TestCase -> Contract () Empty Text ()
    runTestCase TestCase {test = Test {..}, ..} = do
      logInfo @Text $ "Running test: " <> description
      logInfo @Text $ "Check for Tx [id]:" <> exTxId
      for_ checks $ \(AnyCheck check) -> logInfo @Text $ report check
      where
        exTxId :: Text
        exTxId = tshow $ getExportTxId transaction
