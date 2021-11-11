module Mlabs.IntegrationTest.Wbe.Types (
  WbeT (..),
  runWbeT,
  WbeConfig (..),
  loadWbeConfig,
  WbeClientCfg (..),
  defaultWbeClientCfg,
  WbeNetworkId (..),
  WbeExportTx,
  WalletId (..),
  Passphrase (..),
  WbeStage (..),
  WbeTx (..),
  WbeTxSubmitted (..),
  MintBuilder (..),
  connectionInfoFromConfig,
) where

import Prelude

import Cardano.Api qualified as C

import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT))

import Data.Aeson (
  FromJSON (..),
  Options (fieldLabelModifier),
  ToJSON (..),
  defaultOptions,
  genericParseJSON,
  genericToJSON,
  withText,
 )
import Data.Bifunctor (first)
import Data.Map (Map)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word32, Word64)
import Data.Yaml (decodeFileEither)

import GHC.Generics (Generic)

import Ledger (ChainIndexTxOut, TxOutRef)

import Mlabs.IntegrationTest.Types
import Mlabs.NFT.Types

import Network.HTTP.Req qualified as Req

import Plutus.Contract.Wallet (ExportTx (..))

import Text.Read (readMaybe)

newtype WbeT a = WbeT (ReaderT WbeConfig (ExceptT TestError IO) a)
  deriving stock (Generic)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader WbeConfig
    , MonadError TestError
    )

instance Req.MonadHttp WbeT where
  handleHttpException = throwError . HttpError

runWbeT :: WbeConfig -> WbeT a -> IO (Either TestError a)
runWbeT cfg (WbeT a) = runExceptT $ runReaderT a cfg

data WbeConfig = WbeConfig
  { socketPath :: FilePath
  , networkParamsPath :: FilePath
  , epochSlots :: Word64
  , networkId :: WbeNetworkId
  , wbeClientCfg :: WbeClientCfg
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)

loadWbeConfig :: FilePath -> IO (Either TestError WbeConfig)
loadWbeConfig = fmap (first YamlError) . decodeFileEither

connectionInfoFromConfig :: WbeConfig -> C.LocalNodeConnectInfo C.CardanoMode
connectionInfoFromConfig WbeConfig {..} =
  C.LocalNodeConnectInfo
    (C.CardanoModeParams $ C.EpochSlots epochSlots)
    (unWbeNetworkId networkId)
    socketPath

-- | Wrapper for 'NetworkId', which has no 'FromJSON' instance
newtype WbeNetworkId = WbeNetworkId
  { unWbeNetworkId :: C.NetworkId
  }
  deriving stock (Show, Generic)
  deriving newtype (Eq)

instance FromJSON WbeNetworkId where
  parseJSON = withText "WbeNetworkId" $ \t -> case Text.splitOn " " t of
    ["mainnet"] -> pure $ WbeNetworkId C.Mainnet
    ["testnet-magic", n] ->
      maybe
        (fail "Unrecognized network magic value")
        (pure . WbeNetworkId . C.Testnet . C.NetworkMagic)
        . readMaybe @Word32
        $ Text.unpack n
    _ -> fail "Unrecognized network ID"

data WbeClientCfg = WbeClientCfg
  { host :: Text
  , port :: Int
  , walletId :: WalletId
  , passphrase :: Passphrase
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)

defaultWbeClientCfg :: WalletId -> Passphrase -> WbeClientCfg
defaultWbeClientCfg = WbeClientCfg "localhost" 8090

type WbeExportTx = ExportTx

newtype WalletId = WalletId
  { unWalletId :: Text
  }
  deriving stock (Show, Generic)
  deriving newtype (Eq, FromJSON, ToJSON, IsString)

newtype Passphrase = Passphrase
  { unPassphrase :: Text
  }
  deriving stock (Show, Generic)
  deriving newtype (Eq, FromJSON, ToJSON, IsString)

data WbeStage = Balanced | Signed

newtype WbeTx (a :: WbeStage) = WbeTx
  { -- | Base64 representation returned by WBE
    transaction :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype WbeTxSubmitted = WbeTxSubmitted
  { -- | ID of the completed transaction
    txId :: Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON WbeTxSubmitted where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = wbeTxSubmittedModifier
        }

instance ToJSON WbeTxSubmitted where
  toJSON =
    genericToJSON
      defaultOptions
        { fieldLabelModifier = wbeTxSubmittedModifier
        }

wbeTxSubmittedModifier :: String -> String
wbeTxSubmittedModifier = \case
  "txId" -> "id"
  s -> s

-- | Components to manually build an NFT-minting tx
data MintBuilder = MintBuilder
  { params :: MintParams
  , user :: UserId
  , utxos :: Map TxOutRef ChainIndexTxOut
  }
  deriving stock (Show, Eq)
