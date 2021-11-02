{-# LANGUAGE NamedFieldPuns #-}

module Mlabs.WbeTest.TxBuilder (
  buildTx,
  buildWbeTx,
  buildMintTx,
  simpleAdaToWallet,
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C

import Data.Aeson (decode)
import Data.Bifunctor (first)
import Data.Fixed (Micro)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Void (Void)

import Mlabs.NFT.Types (Content (..), MintParams (..), NftId (..), UserId (..))
import Mlabs.NFT.Validation (
  DatumNft (..),
  NftTrade,
  mintPolicy,
  txPolicy,
  txScrAddress,
 )
import Mlabs.WbeTest.Types (MintBuilder (..), WbeError (..), WbeExportTx (..))

import Ledger (TxOutRef, scriptCurrencySymbol)
import Ledger.Constraints qualified as Constraints
import Ledger.Constraints.OffChain (MkTxError (..), UnbalancedTx, mkTx)
import Ledger.Crypto (PubKeyHash (..))
import Ledger.Typed.Scripts.Validators (ValidatorTypes (..), validatorScript)
import Ledger.Value qualified as Value

import Plutus.V1.Ledger.Ada (adaValueOf)
import Plutus.Contract.Wallet (ExportTx (..), export)

import PlutusTx (FromData, ToData)
import PlutusTx.Prelude

import Prelude qualified as Hask

-- Shortcuts for creating transactions --
simpleAdaToWallet ::
  C.NetworkId ->
  C.ProtocolParameters ->
  Micro ->
  Either WbeError WbeExportTx
simpleAdaToWallet netId params ada =
  WbeExportTx <$> buildTx @Void netId params Hask.mempty txC
  where
    pkh :: PubKeyHash =
      -- TODO check if we can do it w/o JSON decoding (was broken in earlier versions)
      fromJust $
        decode
          -- TODO maybe we'll need more general one too one with PKH in arguments
          "{\"getPubKeyHash\" : \"5030c2607444fdf06cdd6da1da0c3d5f95f40d5b7ffc61a23dd523d2\"}"
    value = adaValueOf ada
    txC = Constraints.mustPayToPubKey pkh value

-- Building transactions --

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
  Either WbeError WbeExportTx
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
  Either WbeError ExportTx
buildTx netId protoParams lookups = buildTxFrom netId protoParams . mkTx @a lookups

-- | Attempts to construct an 'ExportTx' from a 'MintBuilder'
buildMintTx ::
  C.ProtocolParameters ->
  C.LocalNodeConnectInfo C.CardanoMode ->
  MintBuilder ->
  Either WbeError ExportTx
buildMintTx pparams connInfo =
  buildTxFrom (C.localNodeNetworkId connInfo) pparams . unbalancedMint
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
  Either WbeError ExportTx
buildTxFrom netId protoParams =
  either (Left . TxError) (first CardanoError . exp)
  where
    exp = export protoParams netId

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
