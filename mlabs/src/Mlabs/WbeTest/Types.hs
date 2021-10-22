{-# LANGUAGE NamedFieldPuns #-}

module Mlabs.WbeTest.Types
( WbeExportTx(..),
  WalletId,
  Balanced,
  Signed, 
  WbeTx(..),
  MintBuilder(..),
) where

import Prelude qualified as Hask


import GHC.Generics (Generic)
import Data.Aeson ( FromJSON, ToJSON (toJSON), object, KeyValue ((.=)))
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


type WalletId = Text


data Balanced
data Signed

newtype WbeTx a = WbeTx
  { transaction :: Text -- Base64 represenatation returned by WBE
  }
  deriving stock (Hask.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Components to manually build an NFT-minting tx
data MintBuilder = MintBuilder
  { params :: MintParams
  , user :: UserId
  , utxos :: Map TxOutRef ChainIndexTxOut
  }
  deriving stock (Hask.Show, Hask.Eq)
