module Mlabs.IntegrationTest.Wbe.TxRead (
  parseTx,
  parseApiTx,
) where

import Cardano.Api qualified as C

import Data.Bifunctor (first)
import Data.ByteArray.Encoding (Base (Base64), convertFromBase)
import Data.ByteString (ByteString)
import Data.Proxy (Proxy (..))
import Data.Text.Encoding as TE (encodeUtf8)

import Mlabs.IntegrationTest.Types
import Mlabs.IntegrationTest.Utils
import Mlabs.IntegrationTest.Wbe.Types

import Plutus.ChainIndex (ChainIndexTx)

import PlutusTx.Prelude

import Prelude qualified as Hask

parseTx :: WbeTx a -> Either TestError ChainIndexTx
parseTx wbeTx = parseApiTx wbeTx >>= toChainIndexTx

parseApiTx :: WbeTx a -> Either TestError (C.Tx C.AlonzoEra)
parseApiTx WbeTx {..} = base64ToBinary transaction >>= parse
  where
    parse =
      first (DecoderError . Hask.show)
        . C.deserialiseFromCBOR (C.proxyToAsType Proxy)
    base64ToBinary =
      first (DecoderError . Hask.show)
        . convertFromBase @ByteString @ByteString Base64
        . TE.encodeUtf8
