module Mlabs.IntegrationTest.Wbe.TxBuilder (
  buildTx,
  buildWbeTx,
  simpleAdaToWallet,
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C

import Data.Aeson (decode)
import Data.Bifunctor (first)
import Data.Fixed (Micro)
import Data.Maybe (fromJust)
import Data.Void (Void)

import Mlabs.IntegrationTest.Wbe.Types (WbeError (..), WbeExportTx (..))

import Ledger.Constraints qualified as Constraints
import Ledger.Constraints.OffChain (MkTxError (..), UnbalancedTx, mkTx)
import Ledger.Crypto (PubKeyHash (..))
import Ledger.Typed.Scripts.Validators (ValidatorTypes (..))

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

buildTxFrom ::
  C.NetworkId ->
  C.ProtocolParameters ->
  Either MkTxError UnbalancedTx ->
  Either WbeError ExportTx
buildTxFrom netId protoParams =
  either (Left . TxError) (first CardanoError . exp)
  where
    exp = export protoParams netId
