module Mlabs.WbeTest.TestStand (
  run,
  runSimple,
) where

import Prelude

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Except (ExceptT (ExceptT), except, runExceptT)

import Data.Aeson
import Data.Bifunctor (first)
import Data.Void (Void)

import Ledger hiding (value)
import Ledger.Constraints qualified as Constraints

import Mlabs.WbeTest.CardanoAPI
import Mlabs.WbeTest.TxBuilder
import Mlabs.WbeTest.TxCheck
import Mlabs.WbeTest.TxRead
import Mlabs.WbeTest.Types
import Mlabs.WbeTest.WbeClient as WbeClient

import Plutus.Contract.Wallet (ExportTx (..))
import Plutus.V1.Ledger.Ada (adaValueOf)

import System.Environment (lookupEnv)

type BalanceCall = WbeClientCfg -> WbeExportTx -> IO (Either WbeError (WbeTx 'Balanced))

-- Main entry point
run :: IO ()
run = do
  runExceptT runBalance >>= \case
    Left e -> putStrLn $ "Balance check failed with: " <> show e
    Right _ -> pure ()

  runExceptT runSign >>= \case
    Left e -> putStrLn $ "Sign check failed with: " <> e
    Right _ -> pure ()
  where
    runBalance =
      runBalanceTest
        doFakeBalance
        (const $ error "FIXME")
        clientCfg
        . WbeExportTx
        $ error "FIXME"

    runSign = runSignTest

runBalanceTest ::
  BalanceCall ->
  UTXOGetter ->
  WbeClientCfg ->
  WbeExportTx ->
  ExceptT WbeError IO ()
runBalanceTest call utxosGetter cfg exportTx = do
  wbeTx <- ExceptT $ call cfg exportTx
  checked <- checkBalanced utxosGetter exportTx wbeTx

  liftIO $ do
    putStrLn "Balanced API Tx:"
    print $ parseApiTx wbeTx
    putStrLn "\nCorresponging balanced ChainIndexTx:"
    print $ parseTx wbeTx
    putStrLn "\nCheck:"
    print checked

runSignTest :: ExceptT String IO ()
runSignTest = liftIO $ putStrLn "TODO: WBE sign test"

runSimple :: ExceptT WbeError IO ()
runSimple = do
  params <-
    ExceptT $
      first DecoderError
        <$> eitherDecodeFileStrict @C.ProtocolParameters
          "./src/Mlabs/WbeTest/network_params.json"
  socket <-
    ExceptT $
      maybe
        (Left $ ConfigurationError "CARDANO_NODE_SOCKET_PATH has not been set")
        Right
        <$> lookupEnv "CARDANO_NODE_SOCKET_PATH"
  pkh <-
    except
      . first DecoderError
      $ eitherDecode @PubKeyHash
        "{\"getPubKeyHash\" : \"5030c2607444fdf06cdd6da1da0c3d5f95f40d5b7ffc61a23dd523d2\"}"

  let value = adaValueOf 5
      txC = Constraints.mustPayToPubKey pkh value
      netId = C.Testnet $ C.NetworkMagic 8
      connectionInfo =
        C.LocalNodeConnectInfo
          (C.CardanoModeParams (C.EpochSlots 21600))
          (C.Testnet $ C.NetworkMagic 8)
          socket

  tx@(WbeExportTx (ExportTx apiTx _ _)) <-
    except $
      WbeExportTx
        <$> buildTx @Void netId params mempty txC
  citx <- except $ toChainIndexTx apiTx

  liftIO $ do
    putStrLn "Submitted for balancing:"
    print citx
    putStrLn ""

  runBalanceTest
    WbeClient.balance
    (getUTXOs connectionInfo)
    clientCfg
    tx

doFakeBalance :: BalanceCall
doFakeBalance _ _ =
  return . Right $
    WbeTx "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jMAEAMAISABEiEjMAEAMAISABEhIiMAMAQRIiACESIgARIAEhIiIwBABSEiIjADAFISIiMAIAUhIiIwAQBSABISIiIiMAcAgiEiIiIjMAYAkAghIiIiIwBQCBIiIiIAQSIiIiADIhIiIiIzACAJAIIhIiIiIzABAJAIIAEhIjACADIiEiMzABAFAEADIAEhIjACADISIwAQAyABESIyABMgATVQFyJTNTUAUAEQAyITMAYAIwBAAREiACEiEiMwAQBAAxIAEREiISMzABAEADACERIAEiMzVzRm4cAIAEA4A0TNQBSJTNTALACEAEQDACxIzUAMiMzU1AGADIgAgAgATU1AEABIgARIhIzABADACEgARIhIzABADACEgASIhIzMAEAQAMAIgASJTNTADMzVzRm481MAYAIiACNTAGABIgAgBQBBMzVzRm4c1MAYAIiABNTAGABIgAQBQBBAEEiACEiABIAEiEjMAEAMAIgAREjIwAQASIzADMAIAIAEzMiIzMiIzMiIzIjMAIwB0iRxOqXb9lAYWy6fAFRTpu0Amz/Wgh5h9VZXzrQqlAFAKIhIzABADACIAEhIjACADIiEiMzABAFAEADIAEhIjACADISIwAQAyABEhIjACADESIAESABMyIzACSIEgaGEG9RJi77gQ2ZMR07vkeLpau5K1vi4flIjVZiLK9vQASAEIhIzABADACIAEzIjMiMzACSIBCkZpb25hIExpc2EASIEg+LuqWXkNICqHmZXDjlPVmIPFIK4zW5X8qe4s0kqHSVgAMwBEiSBoYQb1EmLvuBDZkxHTu+R4ulq7krW+Lh+UiNVmIsr29ABIAQiISMzABAEADACIAEiEjMAEAMAIgAUEgdh5n9h5n9h5n0pGaW9uYSBMaXNh/1gg+LuqWXkNICqHmZXDjlPVmIPFIK4zW5X8qe4s0kqHSVjYeZ/YeZ9YIGhhBvUSYu+4ENmTEdO75Hi6WruStb4uH5SI1WYiyvb0/wL//9h5nwEK/9h5n1gcoJbVHahcPqq+Jxi+e1n1EpGXmTWtd7jetGIvo//YeZ9YHKCW1R2oXD6qvicYvntZ9RKRl5k1rXe43rRiL6P/2HmfBf//9fY="

testnetWalletId :: WalletId
testnetWalletId = "01f9f1dda617eb8bff71468c702afceee5b1ccbf"

clientCfg :: WbeClientCfg
clientCfg = defaultWbeClientCfg testnetWalletId
