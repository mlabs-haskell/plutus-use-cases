{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Cardano.Api qualified as C
import Cardano.Api.NetworkId.Extra (NetworkIdWrapper (NetworkIdWrapper))
import Cardano.BM.Backend.EKGView qualified as EKG
import Cardano.BM.Data.Severity (Severity (Notice))
import Cardano.BM.Data.Tracer (HasPrivacyAnnotation, HasSeverityAnnotation)
import Cardano.BM.Plugin (loadPlugin)
import Cardano.BM.Setup (setupTrace_)
import Cardano.BM.Tracing (
  HasSeverityAnnotation (getSeverityAnnotation),
  Severity (Debug, Info),
 )
import Cardano.CLI (
  LogOutput (LogToFile, LogToStdStreams),
  Port,
  ekgEnabled,
  getEKGURL,
  getPrometheusURL,
  withLoggingNamed,
 )
import Cardano.ChainIndex.Types qualified as PAB.CI
import Cardano.Launcher.Node (nodeSocketFile)
import Cardano.Mnemonic (SomeMnemonic (SomeMnemonic))
import Cardano.Node.Types (
  MockServerConfig (mscNetworkId, mscNodeMode, mscSocketPath),
  NodeMode (AlonzoNode),
 )
import Cardano.Startup (
  installSignalHandlers,
  setDefaultFilePermissions,
  withUtf8Encoding,
 )
import Cardano.Wallet.Api.Client qualified as WalletClient
import Cardano.Wallet.Api.Server (Listen (ListenOnPort))
import Cardano.Wallet.Api.Types (
  ApiMnemonicT (ApiMnemonicT),
  ApiT (ApiT),
  ApiWallet (ApiWallet),
  EncodeAddress (encodeAddress),
  WalletOrAccountPostData (WalletOrAccountPostData),
  WalletPostData,
  postData,
 )
import Cardano.Wallet.Api.Types qualified as Wallet.Types
import Cardano.Wallet.Logging (stdoutTextTracer, trMessageText)
import Cardano.Wallet.Primitive.AddressDerivation (
  NetworkDiscriminant (Mainnet),
  Passphrase (Passphrase),
 )
import Cardano.Wallet.Primitive.SyncProgress (SyncTolerance (SyncTolerance))
import Cardano.Wallet.Primitive.Types (WalletName (WalletName))
import Cardano.Wallet.Primitive.Types.Coin (Coin (Coin))
import Cardano.Wallet.Shelley (
  SomeNetworkDiscriminant (SomeNetworkDiscriminant),
  serveWallet,
  setupTracers,
  tracerSeverities,
 )
import Cardano.Wallet.Shelley.Launch (withSystemTempDir)
import Cardano.Wallet.Shelley.Launch.Cluster (
  ClusterLog,
  Credential (KeyCredential),
  RunningNode (RunningNode),
  localClusterConfigFromEnv,
  moveInstantaneousRewardsTo,
  oneMillionAda,
  sendFaucetAssetsTo,
  sendFaucetFundsTo,
  testMinSeverityFromEnv,
  tokenMetadataServerFromEnv,
  walletMinSeverityFromEnv,
  withCluster,
 )
import Cardano.Wallet.Types (WalletUrl (WalletUrl))
import Cardano.Wallet.Types qualified as Wallet.Config

import Control.Concurrent.Async (async)
import Control.Lens (contramap, (&), (.~), (^.))
import Control.Monad (void, when)
import Control.Tracer (Tracer, traceWith)

import Data.Bifunctor (first)
import Data.Default (Default (def))
import Data.Proxy (Proxy (Proxy))
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Class (ToText (toText))
import Data.Text.Extras (tshow)

import Mlabs.NFT.PAB.MarketplaceContract

import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Network.URI (URI)

import Plutus.ChainIndex.App qualified as ChainIndex
import Plutus.ChainIndex.ChainIndexLog (ChainIndexLog)
import Plutus.ChainIndex.Config qualified as CI
import Plutus.ChainIndex.Logging qualified as ChainIndex.Logging
import Plutus.PAB.App (StorageBackend (BeamSqliteBackend))
import Plutus.PAB.Effects.Contract.Builtin (handleBuiltin)
import Plutus.PAB.Run qualified as PAB.Run
import Plutus.PAB.Run.Command (ConfigCommand (Migrate, PABWebserver))
import Plutus.PAB.Run.CommandParser (
  AppOpts (
    AppOpts,
    cmd,
    configPath,
    logConfigPath,
    minLogLevel,
    runEkgServer,
    storageBackend
  ),
 )
import Plutus.PAB.Run.CommandParser qualified as PAB.Command
import Plutus.PAB.Types (
  Config (
    chainIndexConfig,
    dbConfig,
    nodeServerConfig,
    walletServerConfig
  ),
  DbConfig (dbConfigFile),
 )
import Plutus.PAB.Types qualified as PAB.Config

import Prelude

import Servant.Client (
  BaseUrl (
    BaseUrl,
    baseUrlHost,
    baseUrlPath,
    baseUrlPort,
    baseUrlScheme
  ),
  ClientEnv,
  ClientError,
  Scheme (Http),
  mkClientEnv,
  runClientM,
 )

import System.Directory (createDirectory)
import System.FilePath ((</>))
import System.Time.Extra (sleep)

import Test.Integration.Faucet (
  genRewardAccounts,
  maryIntegrationTestAssets,
  mirMnemonics,
  shelleyIntegrationTestFunds,
 )
import Test.Integration.Faucet qualified as Faucet
import Test.Integration.Framework.DSL (fixturePassphrase)

data LogOutputs = LogOutputs
  { cluster :: [LogOutput]
  , wallet :: [LogOutput]
  }

newtype ClusterPort = ClusterPort Int

newtype ClusterHost = ClusterHost String

data TestsLog
  = MsgBaseUrl Text Text Text -- wallet url, ekg url, prometheus url
  | MsgSettingUpFaucet
  | MsgCluster ClusterLog
  deriving (Show)

instance ToText TestsLog where
  toText = \case
    MsgBaseUrl walletUrl ekgUrl prometheusUrl ->
      mconcat
        [ "Wallet url: "
        , walletUrl
        , ", EKG url: "
        , ekgUrl
        , ", Prometheus url:"
        , prometheusUrl
        ]
    MsgSettingUpFaucet -> "Setting up faucet..."
    MsgCluster msg -> toText msg

instance HasPrivacyAnnotation TestsLog
instance HasSeverityAnnotation TestsLog where
  getSeverityAnnotation = \case
    MsgSettingUpFaucet -> Notice
    MsgBaseUrl {} -> Notice
    MsgCluster msg -> getSeverityAnnotation msg

main :: IO ()
main = withLocalClusterSetup $ \dir lo@LogOutputs {cluster} ->
  withLoggingNamed "cluster" cluster $ \(_, (_, trCluster)) -> do
    let tr' = contramap MsgCluster $ trMessageText trCluster

    clusterCfg <- localClusterConfigFromEnv
    withCluster
      tr'
      dir
      clusterCfg
      (setupFaucet dir (trMessageText trCluster))
      (whenReady dir (trMessageText trCluster) lo)
  where
    setupFaucet :: FilePath -> Tracer IO TestsLog -> RunningNode -> IO ()
    setupFaucet dir trCluster (RunningNode socketPath _ _) = do
      traceWith trCluster MsgSettingUpFaucet

      let trCluster' = contramap MsgCluster trCluster
          encodeAddresses = fmap (first (T.unpack . encodeAddress @ 'Mainnet))
          accts = KeyCredential <$> concatMap genRewardAccounts mirMnemonics
          rewards = (,Coin $ fromIntegral oneMillionAda) <$> accts

      sendFaucetFundsTo trCluster' socketPath dir $
        encodeAddresses shelleyIntegrationTestFunds
      sendFaucetAssetsTo trCluster' socketPath dir 20
        . encodeAddresses
        $ maryIntegrationTestAssets (Coin 1_000_000_000)
      moveInstantaneousRewardsTo trCluster' socketPath dir rewards

    whenReady ::
      FilePath ->
      Tracer IO TestsLog ->
      LogOutputs ->
      RunningNode ->
      IO ()
    whenReady
      dir
      trCluster
      LogOutputs {wallet}
      rn@(RunningNode socketPath block0 (gp, vData)) = do
        withLoggingNamed "cardano-wallet" wallet $ \(sb, (cfg, tr)) -> do
          let walletHost = "127.0.0.1"
              walletPort = 46493
              tracers = setupTracers (tracerSeverities (Just Debug)) tr
              db = dir </> "wallets"

          setupPABServices
            (ClusterHost walletHost)
            (ClusterPort walletPort)
            dir
            rn

          ekgEnabled
            >>= flip when (EKG.plugin cfg tr sb >>= loadPlugin sb)

          createDirectory db

          tokenMetadataServer <- tokenMetadataServerFromEnv
          prometheusUrl <-
            maybe "none" (uncurry (mkUrl @(Port "Prometheus")))
              <$> getPrometheusURL
          ekgUrl <-
            maybe "none" (uncurry (mkUrl @(Port "EKG")))
              <$> getEKGURL

          void $
            serveWallet
              (SomeNetworkDiscriminant $ Proxy @ 'Mainnet)
              tracers
              (SyncTolerance 10)
              (Just db)
              Nothing
              (fromString walletHost)
              (ListenOnPort walletPort)
              Nothing
              Nothing
              tokenMetadataServer
              socketPath
              block0
              (gp, vData)
              (traceWith' ekgUrl prometheusUrl)
        where
          mkUrl :: forall a. ToText a => String -> a -> Text
          mkUrl h p = T.pack h <> ":" <> toText p

          traceWith' :: Text -> Text -> URI -> IO ()
          traceWith' ekgUrl prometheusUrl u =
            traceWith trCluster $
              MsgBaseUrl (tshow u) ekgUrl prometheusUrl

-- Do all the program setup required for running the local cluster, create a
-- temporary directory, log output configurations, and pass these to the given
-- main action.
withLocalClusterSetup ::
  (FilePath -> LogOutputs -> IO a) ->
  IO a
withLocalClusterSetup action = do
  putStrLn $
    mconcat
      [ "Starting PAB local cluster. Please make sure the `SHELLEY_TEST_DATA` "
      , "environment variable is set to "
      , "`mlabs-local-cluster/cluster-data/cardano-node-shelley` "
      , "in the `mlabs-plutus-use-cases` repository."
      ]

  -- Handle SIGTERM properly
  installSignalHandlers $ putStrLn "Terminated"

  -- Ensure key files have correct permissions for cardano-cli
  setDefaultFilePermissions

  -- Set UTF-8, regardless of user locale
  withUtf8Encoding
    .
    -- This temporary directory will contain logs, and all other data
    -- produced by the local test cluster.
    withSystemTempDir stdoutTextTracer "test-cluster"
    $ \dir -> do
      let logOutputs name minSev =
            [ LogToFile (dir </> name) (min minSev Info)
            , LogToStdStreams minSev
            ]

      lops <-
        LogOutputs
          <$> (logOutputs "cluster.log" <$> testMinSeverityFromEnv)
          <*> (logOutputs "wallet.log" <$> walletMinSeverityFromEnv)

      action dir lops

setupPABServices ::
  ClusterHost ->
  ClusterPort ->
  FilePath ->
  RunningNode ->
  IO ()
setupPABServices walletHost walletPort dir rn =
  void
    . async
    $ do
      walletUrl <- restoreWallets walletHost walletPort
      chainIndexPort <- launchChainIndex dir rn
      launchPAB fixturePassphrase dir walletUrl rn chainIndexPort

-- | Launch the chain index in a separate thread.
launchChainIndex :: FilePath -> RunningNode -> IO ClusterPort
launchChainIndex dir (RunningNode socketPath _ _) = do
  config <- ChainIndex.Logging.defaultConfig
  (trace, _) <- setupTrace_ @IO @ChainIndexLog config "chain-index"
  let dbPath = dir </> "chain-index.db"
      chainIndexConfig =
        CI.defaultConfig
          & CI.socketPath .~ nodeSocketFile socketPath
          & CI.dbPath .~ dbPath
          & CI.networkId .~ C.Mainnet
  void . async . void $ ChainIndex.runMain trace chainIndexConfig
  pure . ClusterPort $ chainIndexConfig ^. CI.port

-- | Launch the PAB in a separate thread.
launchPAB ::
  -- | Passphrase
  Text ->
  -- | Temp directory
  FilePath ->
  -- | wallet url
  BaseUrl ->
  -- | Socket path
  RunningNode ->
  -- | Port of the chain index
  ClusterPort ->
  IO ()
launchPAB
  passPhrase
  dir
  walletUrl
  (RunningNode socketPath _ _)
  (ClusterPort chainIndexPort) = do
    let opts =
          AppOpts
            { minLogLevel = Nothing
            , logConfigPath = Nothing
            , configPath = Nothing
            , runEkgServer = False
            , storageBackend = BeamSqliteBackend
            , cmd = PABWebserver
            , PAB.Command.passphrase = Just passPhrase
            }
        networkID = NetworkIdWrapper C.Mainnet
        config =
          PAB.Config.defaultConfig
            { nodeServerConfig =
                def
                  { mscSocketPath = nodeSocketFile socketPath
                  , mscNodeMode = AlonzoNode
                  , mscNetworkId = networkID
                  }
            , dbConfig =
                def
                  { dbConfigFile = T.pack $ dir </> "plutus-pab.db"
                  }
            , chainIndexConfig =
                def
                  { PAB.CI.ciBaseUrl =
                      PAB.CI.ChainIndexUrl $
                        BaseUrl Http "localhost" chainIndexPort ""
                  }
            , walletServerConfig =
                def
                  & (Wallet.Config.walletSettingsL . Wallet.Config.baseUrlL)
                  .~ WalletUrl walletUrl
            }
    -- TODO: For some reason this has to be async - program terminates if it's done synchronously???
    void
      . async
      $ PAB.Run.runWithOpts @MarketplaceContracts
        handleBuiltin
        (Just config)
        opts {cmd = Migrate}
    sleep 2
    void
      . async
      $ PAB.Run.runWithOpts @MarketplaceContracts
        handleBuiltin
        (Just config)
        opts {cmd = PABWebserver}

-- | Set up wallets
restoreWallets :: ClusterHost -> ClusterPort -> IO BaseUrl
restoreWallets host@(ClusterHost walletHost) port@(ClusterPort walletPort) = do
  sleep 15
  manager <- newManager defaultManagerSettings
  runClient manager >>= \case
    Left err -> do
      putStrLn "restoreWallet failed"
      putStrLn $ "Error: " <> show err
      putStrLn "restoreWallet: trying again in 30s"
      sleep 15
      restoreWallets host port
    Right (ApiWallet (ApiT i) _ _ _ _ _ _ _ _) -> do
      putStrLn $ "Restored wallet: " <> show i
      putStrLn $ "Passphrase: " <> T.unpack fixturePassphrase
      pure baseUrl
  where
    runClient :: Manager -> IO (Either ClientError ApiWallet)
    runClient manager =
      flip runClientM (clientEnv manager) $
        WalletClient.postWallet WalletClient.walletClient walletAcc

    baseUrl :: BaseUrl
    baseUrl =
      BaseUrl
        { baseUrlScheme = Http
        , baseUrlHost = walletHost
        , baseUrlPort = walletPort
        , baseUrlPath = ""
        }

    clientEnv :: Manager -> ClientEnv
    clientEnv manager = mkClientEnv manager baseUrl

    mnemonic :: ApiMnemonicT '[15, 18, 21, 24]
    mnemonic = ApiMnemonicT . SomeMnemonic $ head Faucet.seqMnemonics

    wpData :: WalletPostData
    wpData =
      Wallet.Types.WalletPostData
        Nothing
        mnemonic
        Nothing
        (ApiT $ WalletName "plutus-wallet")
        (ApiT . Passphrase . fromString $ T.unpack fixturePassphrase)

    walletAcc :: WalletOrAccountPostData
    walletAcc = WalletOrAccountPostData {postData = Left wpData}
