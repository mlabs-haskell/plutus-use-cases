{-# LANGUAGE NamedFieldPuns #-}

module Mlabs.Extract.TxBuilder (
  WbeExportTx (..),
  MintBuilder (..),
  buildTx,
  buildWbeTx,
  buildMintTx,
) where

import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as C

import Data.Aeson (KeyValue ((.=)), ToJSON (toJSON), object)
import Data.Bifunctor (first)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)

import GHC.Generics (Generic)

import Mlabs.NFT.Types (Content (..), MintParams (..), NftId (..), UserId (..))
import Mlabs.NFT.Validation (
  DatumNft (..),
  NftTrade,
  mintPolicy,
  txPolicy,
  txScrAddress,
 )

import Ledger (ChainIndexTxOut, TxOutRef, scriptCurrencySymbol)
import qualified Ledger.Constraints as Constraints
import Ledger.Constraints.OffChain (MkTxError (..), UnbalancedTx, mkTx)
import Ledger.Typed.Scripts.Validators (ValidatorTypes (..), validatorScript)
import qualified Ledger.Value as Value

import Plutus.Contract.Wallet (ExportTx (..), ExportTxInput (..), export)

import PlutusTx (FromData, ToData)
import PlutusTx.Prelude

import qualified Prelude as Hask

import Prettyprinter (pretty)

-- | Components to manually build an NFT-minting tx
data MintBuilder = MintBuilder
  { params :: MintParams
  , user :: UserId
  , utxos :: Map TxOutRef ChainIndexTxOut
  }
  deriving stock (Hask.Show, Hask.Eq)

{- | Wrapper around 'ExportTx', whose 'ToJSON' instance does not match the format
 expected by the WBE (this should be unecessary after upgrading Plutus to the next
version, where the serialization mismatch is fixed)
-}
newtype WbeExportTx = WbeExportTx ExportTx
  deriving stock (Generic)

instance ToJSON WbeExportTx where
  toJSON (WbeExportTx ExportTx {..}) =
    object
      [ "transactions" .= C.serialiseToTextEnvelope Nothing partialTx
      , "inputs" .= (inputsForWbe <$> lookups)
      , "signatories" .= signatories
      ]
    where
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

-- | Build an 'WbeExportTx' from arbitrary lookups and transactions constraints
buildWbeTx ::
  ( FromData (DatumType a)
  , ToData (DatumType a)
  , ToData (RedeemerType a)
  ) =>
  C.NetworkId ->
  C.ProtocolParameters ->
  Constraints.ScriptLookups a ->
  Constraints.TxConstraints (RedeemerType a) (DatumType a) ->
  Either [Hask.Char] WbeExportTx
buildWbeTx netId pparams lookups = fmap WbeExportTx . buildTx netId pparams lookups

-- | Build an 'ExportTx' from arbitrary lookups and transactions constraints
buildTx ::
  forall a.
  ( FromData (DatumType a)
  , ToData (DatumType a)
  , ToData (RedeemerType a)
  ) =>
  C.NetworkId ->
  C.ProtocolParameters ->
  Constraints.ScriptLookups a ->
  Constraints.TxConstraints (RedeemerType a) (DatumType a) ->
  Either [Hask.Char] ExportTx
buildTx netId protoParams lookups = buildTxFrom netId protoParams . mkTx @a lookups

-- | Attempts to construct an 'ExportTx' from a 'MintBuilder'
buildMintTx ::
  C.NetworkId ->
  C.ProtocolParameters ->
  MintBuilder ->
  Either [Hask.Char] ExportTx
buildMintTx netId pparams = buildTxFrom netId pparams . unbalancedMint
  where
    unbalancedMint MintBuilder {..} = case Map.toList utxos of
      [] -> Left CannotSatisfyAny
      (oref, _) : _ -> mkTx @NftTrade lookups tx
        where
          lookups =
            Hask.mconcat
              [ Constraints.unspentOutputs utxos
              , Constraints.mintingPolicy nftPolicy
              , Constraints.typedValidatorLookups txPolicy
              , Constraints.otherScript $ validatorScript txPolicy
              ]

          tx =
            Hask.mconcat
              [ Constraints.mustMintValue val
              , Constraints.mustSpendPubKeyOutput oref
              , Constraints.mustPayToTheScript nft val
              ]

          val = Value.singleton (scriptCurrencySymbol nftPolicy) nftId'token 1

          nftPolicy = mintPolicy txScrAddress oref nftId

          nft@DatumNft {dNft'id = nftId@NftId {..}} = nftInit params user oref

buildTxFrom ::
  C.NetworkId ->
  C.ProtocolParameters ->
  Either MkTxError UnbalancedTx ->
  Either [Hask.Char] ExportTx
buildTxFrom netId protoParams unbalanced =
  first Hask.show $
    either (Left . pretty) exp unbalanced
  where
    exp = first pretty . export protoParams netId

nftInit :: MintParams -> UserId -> TxOutRef -> DatumNft
nftInit mps@MintParams {mp'share} user oref =
  DatumNft
    { dNft'id = nftId
    , dNft'share = mp'share
    , dNft'author = user
    , dNft'owner = user
    , dNft'price = mp'price mps
    }
  where
    nftId = nftIdInit mps oref

nftIdInit :: MintParams -> TxOutRef -> NftId
nftIdInit MintParams {mp'content, mp'title} oref =
  NftId
    { nftId'title = mp'title
    , nftId'token = Value.TokenName $ hashData mp'content
    , nftId'outRef = oref
    }
  where
    hashData (Content b) = sha2_256 b
