module Mlabs.IntegrationTest.Wbe.TestStand (
  run,
  runAll,
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C

import Control.Exception (throw)
import Control.Monad.Except (liftEither)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)

import Data.Aeson (eitherDecodeFileStrict)
import Data.Bifunctor (first)
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import Data.Text.IO qualified as TextIO

import Mlabs.IntegrationTest.Checks
import Mlabs.IntegrationTest.Types
import Mlabs.IntegrationTest.Utils
import Mlabs.IntegrationTest.Wbe.TestCase
import Mlabs.IntegrationTest.Wbe.Types

import Prelude

run :: IO ()
run = runAll Nothing

-- Main entry point
runAll :: Maybe FilePath -> IO ()
runAll mpath = do
  cfg <-
    either throw id
      <$> loadWbeConfig (fromMaybe "./src/Mlabs/IntegrationTest/Wbe/debug.yaml" mpath)
  runWbeT cfg setupTests >>= \case
    Left e -> throw e
    Right (connInfo, params) ->
      for_ (getTestCases connInfo params) $ \TestCase {..} -> do
        TextIO.putStrLn $ "\nRunning test: " <> description
        runWbeT cfg test >>= \case
          Left e -> putStrLn $ "Test failed with: " <> show e
          -- TODO get the tx ID
          Right (Test exportTx checks) -> do
            putStrLn $ "\nCheck for Tx [id]:" <> show (getExportTxId exportTx)
            for_ checks $
              \(AnyCheck check) -> do
                TextIO.putStrLn $ report check
  where
    setupTests ::
      WbeT (C.LocalNodeConnectInfo C.CardanoMode, C.ProtocolParameters)
    setupTests = (,) <$> getConnectionInfo <*> getProtocolParams
      where
        getConnectionInfo :: WbeT (C.LocalNodeConnectInfo C.CardanoMode)
        getConnectionInfo = asks connectionInfoFromConfig

        getProtocolParams :: WbeT C.ProtocolParameters
        getProtocolParams =
          liftEither
            . first DecoderError
            =<< liftIO . eitherDecodeFileStrict @C.ProtocolParameters
            =<< asks networkParamsPath
