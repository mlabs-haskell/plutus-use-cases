{-# LANGUAGE NamedFieldPuns #-}

module Mlabs.WbeTest.TestStand (
  run,
) where

import Prelude



import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Except (liftEither)
import Control.Monad.Reader (asks)

import Data.Aeson (eitherDecodeFileStrict)
import Data.Bifunctor (first)

import Data.Map qualified as Map




import Mlabs.WbeTest.CardanoAPI

import Mlabs.WbeTest.TxInfo
import Mlabs.WbeTest.Checks
import Mlabs.WbeTest.TxRead
import Mlabs.WbeTest.Types
import Mlabs.WbeTest.TestCases
import Mlabs.WbeTest.WbeClient as WbeClient

import Plutus.V1.Ledger.Ada (adaValueOf)

-- Main entry point
-- todo not quite sure how to handle errors here, if one of tests fail we still need to run rest
-- idea: split this into two separate actions, traverse list of [WbeT ()] and use runWbeT in each
-- to collect [Either WbeError ()]. run should have the type WbeT () when this is implemented
run :: WbeConfig -> IO (Either WbeError ())
run cfg = runWbeT cfg $ do
  params <-
    liftEither
      . first DecoderError
      =<< liftIO . eitherDecodeFileStrict @C.ProtocolParameters
      =<< asks networkParamsPath
  connInfo <- asks connectionInfoFromConfig

  testTransactions <- getTestTxs params connInfo

  forM_ testTransactions $ \tx -> do
    balanced <- testBalance connInfo tx
    _ <- testSign balanced
    pure ()
  where
    testBalance connInfo exportTx = do
      balanced <- WbeClient.balance exportTx
      info <- analyseBalanced (getUTXOs connInfo) exportTx balanced
      liftIO $ do
        -- print $ let (WbeExportTx (ExportTx apiTx _ _)) = exportTx in apiTx
        -- print $ encode exportTx
        putStrLn "\nCheck for Tx [id]:" --todo probably some Tx id should be here
        mapM_ putStrLn 
          [ report $ mustBeBalanced info
          , report $ feeMustBeAdded info
          , report $ cNot $ inputsMustBeAdded info
          , report $ unbalancedInsOutsShouldNotChange info
          ]
      return balanced

    testSign _ = runSignTest

runSignTest :: WbeT ()
runSignTest = liftIO $ putStrLn "TODO: WBE sign test"

doFakeBalance :: WbeT (WbeTx 'Balanced)
doFakeBalance =
  pure $
    WbeTx "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jMAEAMAISABEiEjMAEAMAISABEhIiMAMAQRIiACESIgARIAEhIiIwBABSEiIjADAFISIiMAIAUhIiIwAQBSABISIiIiMAcAgiEiIiIjMAYAkAghIiIiIwBQCBIiIiIAQSIiIiADIhIiIiIzACAJAIIhIiIiIzABAJAIIAEhIjACADIiEiMzABAFAEADIAEhIjACADISIwAQAyABESIyABMgATVQFyJTNTUAUAEQAyITMAYAIwBAAREiACEiEiMwAQBAAxIAEREiISMzABAEADACERIAEiMzVzRm4cAIAEA4A0TNQBSJTNTALACEAEQDACxIzUAMiMzU1AGADIgAgAgATU1AEABIgARIhIzABADACEgARIhIzABADACEgASIhIzMAEAQAMAIgASJTNTADMzVzRm481MAYAIiACNTAGABIgAgBQBBMzVzRm4c1MAYAIiABNTAGABIgAQBQBBAEEiACEiABIAEiEjMAEAMAIgAREjIwAQASIzADMAIAIAEzMiIzMiIzMiIzIjMAIwB0iRxOqXb9lAYWy6fAFRTpu0Amz/Wgh5h9VZXzrQqlAFAKIhIzABADACIAEhIjACADIiEiMzABAFAEADIAEhIjACADISIwAQAyABEhIjACADESIAESABMyIzACSIEgaGEG9RJi77gQ2ZMR07vkeLpau5K1vi4flIjVZiLK9vQASAEIhIzABADACIAEzIjMiMzACSIBCkZpb25hIExpc2EASIEg+LuqWXkNICqHmZXDjlPVmIPFIK4zW5X8qe4s0kqHSVgAMwBEiSBoYQb1EmLvuBDZkxHTu+R4ulq7krW+Lh+UiNVmIsr29ABIAQiISMzABAEADACIAEiEjMAEAMAIgAUEgdh5n9h5n9h5n0pGaW9uYSBMaXNh/1gg+LuqWXkNICqHmZXDjlPVmIPFIK4zW5X8qe4s0kqHSVjYeZ/YeZ9YIGhhBvUSYu+4ENmTEdO75Hi6WruStb4uH5SI1WYiyvb0/wL//9h5nwEK/9h5n1gcoJbVHahcPqq+Jxi+e1n1EpGXmTWtd7jetGIvo//YeZ9YHKCW1R2oXD6qvicYvntZ9RKRl5k1rXe43rRiL6P/2HmfBf//9fY="

-- todo need some convenient way to get list of test transactions
getTestTxs ::
  C.ProtocolParameters -> C.LocalNodeConnectInfo mode -> WbeT [WbeExportTx]
getTestTxs params connInfo = mapM liftEither [tx1]
    where
      -- I don't think we need to hadle parsing error here, it's not quite part of business logic
      -- it's just ugly way to get PKH and if we can't do it this way, better just `die` - it should not happen;
      -- need to find a way to parse it w/o json decodeing
      pkh = fromJust $ decode @PubKeyHash
        "{\"getPubKeyHash\" : \"5030c2607444fdf06cdd6da1da0c3d5f95f40d5b7ffc61a23dd523d2\"}"
      txC = Constraints.mustPayToPubKey pkh (adaValueOf 5)

-- TODO this should be replicated in/replaced by a debug.yaml
debugConfig :: FilePath -> WbeConfig
debugConfig socketPath = WbeConfig
  { socketPath
  , networkParamsPath = "./src/Mlabs/WbeTest/network_params.json"
  , epochSlots = 21600
  , networkId = WbeNetworkId $ debugNetId
  , wbeClientCfg = debugClientCfg
  }

debugWalletId :: WalletId
debugWalletId = "01f9f1dda617eb8bff71468c702afceee5b1ccbf"

debugClientCfg :: WbeClientCfg
debugClientCfg = defaultWbeClientCfg debugWalletId

debugNetId :: C.NetworkId
debugNetId = C.Testnet $ C.NetworkMagic 8

debugConnectionInfo =
  C.LocalNodeConnectInfo
    (C.CardanoModeParams (C.EpochSlots 21600))
    debugNetId
