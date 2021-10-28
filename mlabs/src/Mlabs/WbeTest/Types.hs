{-# LANGUAGE NamedFieldPuns #-}

module Mlabs.WbeTest.Types
( WbeConfig(..),
  loadWbeConfig,
  WbeClientCfg(..),
  defaultWbeClientCfg,
  WbeNetworkId(..),
  WbeExportTx(..),
  WalletId(..),
  Passphrase(..),
  WbeStage(..),
  WbeTx(..),
  WbeTxSubmitted(..),
  MintBuilder(..),
  WbeError(..),
  connectionInfoFromConfig,
) where

import Prelude qualified as Hask

import GHC.Generics (Generic)
import Data.Aeson (
  FromJSON (..),
  (.=),
  Options (fieldLabelModifier),
  ToJSON (..),
  defaultOptions,
  genericParseJSON,
  genericToJSON,
  object,
  withText,
 )
import Data.Text (Text)
import Plutus.Contract.Wallet (ExportTx (..), ExportTxInput (..))
import qualified Cardano.Api as C
import qualified Ledger.Value as Value
import PlutusTx.Prelude
import Mlabs.NFT.Types (MintParams(..), UserId (..))
import Data.Map (Map)
import Ledger (TxOutRef, ChainIndexTxOut)
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString.Base16 as Base16
import Data.String (IsString)
import qualified Plutus.Contract.CardanoAPI as C
import Ledger.Constraints (MkTxError)
import Prettyprinter (pretty)
import qualified Network.HTTP.Req as Req
import Data.Yaml (ParseException, decodeFileEither)
import Data.Bifunctor (first)
import Data.Word (Word64, Word32)
import qualified Data.Text as Text
import Text.Read (readMaybe)

data WbeConfig = WbeConfig
  { socketPath :: Hask.FilePath
  , networkParamsPath :: Hask.FilePath
  , epochSlots :: Word64
  , networkId :: WbeNetworkId
  , wbeClientCfg :: WbeClientCfg
  }
  deriving stock (Hask.Show, Hask.Eq, Generic)
  deriving anyclass (FromJSON)

loadWbeConfig :: Hask.FilePath -> Hask.IO (Either WbeError WbeConfig)
loadWbeConfig = Hask.fmap (first YamlError) . decodeFileEither

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
  deriving stock (Hask.Show, Generic)
  deriving newtype (Hask.Eq)

instance FromJSON WbeNetworkId where
  parseJSON = withText "WbeNetworkId" $ \t -> case Text.splitOn " " t of
    ["mainnet"] -> Hask.pure $ WbeNetworkId C.Mainnet
    ["testnet-magic", n] ->
      maybe
        (Hask.fail "Unrecognized network magic value")
        (Hask.pure . WbeNetworkId . C.Testnet . C.NetworkMagic)
        . readMaybe @Word32
        $ Text.unpack n
    _ -> Hask.fail "Unrecognized network ID"

data WbeClientCfg = WbeClientCfg
  { host :: Text
  , port :: Hask.Int
  , walletId :: WalletId
  , passphrase :: Passphrase
  }
  deriving stock (Hask.Show, Hask.Eq, Generic)
  deriving anyclass (FromJSON)

defaultWbeClientCfg :: WalletId -> Passphrase -> WbeClientCfg
defaultWbeClientCfg = WbeClientCfg "localhost" 8090

{- | Wrapper around 'ExportTx', whose 'ToJSON' instance does not match the format
 expected by the WBE (this should be unecessary after upgrading Plutus to the next
version, where the serialization mismatch is fixed)
-}
newtype WbeExportTx = WbeExportTx ExportTx
  deriving stock (Generic)

instance ToJSON WbeExportTx where
  toJSON (WbeExportTx ExportTx {..}) =
    object
      [ "transaction" .= Text.decodeUtf8 (Base16.encode teRawCBOR)
      , "inputs" .= (inputsForWbe <$> lookups)
      , "signatories" .= signatories
      , "redeemers" .= signatories
      ]
    where
      C.TextEnvelope {teRawCBOR} = C.serialiseToTextEnvelope Nothing partialTx

      inputsForWbe (ExportTxInput (C.TxIn txId txIx) (C.TxOut addr val dat)) =
        object $
          Hask.mconcat
            [
              [ "id" .= txId
              , "index" .= txIx
              , "address" .= addr
              , "assets" .= mempty @[Value.Value] -- Hard-coded for UTxO with only Ada
              ]
            , case val of
                C.TxOutAdaOnly _ ll -> mkAmt ll
                C.TxOutValue _ v -> case C.valueToList v of
                  [(C.AdaAssetId, qt)] -> mkAmt qt
                  _ -> mempty
            , case dat of
                C.TxOutDatumHash _ h -> ["datum" .= h]
                C.TxOutDatumHashNone -> mempty
            ]
        where
          mkAmt v =
            [ "amount"
                .= object
                  [ "unit" .= ("lovelace" :: Text)
                  , "quantity" .= v
                  ]
            ]


-- data WbeExportTx  = WbeExportTx -- placeholder for real one

-- data ReqRedeemer = 
--   ReqRedeemer
--   deriving stock (Hask.Show, Generic)
--   deriving anyclass (ToJSON, FromJSON)
  
-- data ReqInput =
--   ReqInput
--   deriving stock (Hask.Show, Generic)
--   deriving anyclass (ToJSON, FromJSON)
-- -- WbeExportTx types - END

newtype WalletId = WalletId
  { unWalletId :: Text
  }
  deriving stock (Hask.Show, Generic)
  deriving newtype (Hask.Eq, FromJSON, ToJSON, IsString)

newtype Passphrase = Passphrase
  { unPassphrase :: Text
  }
  deriving stock (Hask.Show, Generic)
  deriving newtype (Hask.Eq, FromJSON, ToJSON, IsString)

data WbeStage = Balanced | Signed

newtype WbeTx (a :: WbeStage) = WbeTx
  { -- | Base64 representation returned by WBE
    transaction :: Text
  }
  deriving stock (Hask.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype WbeTxSubmitted = WbeTxSubmitted
  { -- | ID of the completed transaction
    txId :: Text
  }
  deriving stock (Hask.Show, Hask.Eq, Generic)

instance FromJSON WbeTxSubmitted where
  parseJSON = genericParseJSON
    defaultOptions
    { fieldLabelModifier = wbeTxSubmittedModifier
    }

instance ToJSON WbeTxSubmitted where
  toJSON = genericToJSON
    defaultOptions
    { fieldLabelModifier = wbeTxSubmittedModifier
    }

wbeTxSubmittedModifier :: Hask.String -> Hask.String
wbeTxSubmittedModifier = \case
  "txId" -> "id"
  s -> s

-- | Components to manually build an NFT-minting tx
data MintBuilder = MintBuilder
  { params :: MintParams
  , user :: UserId
  , utxos :: Map TxOutRef ChainIndexTxOut
  }
  deriving stock (Hask.Show, Hask.Eq)

data WbeError
  = HttpError Req.HttpException
  | DecoderError Hask.String
  | YamlError ParseException
  | ConfigurationError Hask.String
  | CardanoError C.ToCardanoError
  | TxError MkTxError
  -- HACK these errors come from @queryNodeLocalState@ and friends
  -- Should find a better way to represent them
  | NodeError Hask.String
  deriving stock (Generic)

instance Hask.Show WbeError where
  show = \case
    HttpError err -> Hask.show err
    DecoderError err -> err
    YamlError err -> Hask.show err
    ConfigurationError err -> err
    CardanoError err -> Hask.show $ pretty err
    TxError err -> Hask.show $ pretty err
    NodeError err -> err
