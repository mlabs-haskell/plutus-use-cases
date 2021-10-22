{-# LANGUAGE NamedFieldPuns #-}

module Mlabs.WbeTest.Types
( WbeExportTx(..),
  WalletId(..),
  WbeStage(..),
  WbeTx(..),
  WbeTxSubmitted(..),
  MintBuilder(..),
) where

import Prelude qualified as Hask

import GHC.Generics (Generic)
import Data.Aeson (
  FromJSON (..),
  ToJSON (..),
  KeyValue ((.=)),
  Options (fieldLabelModifier),
  object,
  genericParseJSON,
  defaultOptions,
  genericToJSON
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
