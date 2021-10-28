module Mlabs.WbeTest.TxRead (parseTx, parseApiTx, toChainIndexTx) where

import Prelude qualified as Hask
import PlutusTx.Prelude
import Plutus.ChainIndex (ChainIndexTx)
import Plutus.Contract.CardanoAPI ( fromCardanoTx) 

import Data.ByteArray.Encoding (Base (Base64), convertFromBase)
import Data.ByteString (ByteString)
import           Data.Proxy                  (Proxy (..))
import Data.Text.Encoding as TE (encodeUtf8 )

import qualified Cardano.Api                 as C

import Mlabs.WbeTest.Types
import Data.Bifunctor (first)


-- runCompare :: Hask.FilePath -> Hask.FilePath -> Hask.IO ()
-- runCompare exportedTx wbeTx = do
--   readExported exportedTx >>= Hask.print . fmap partialTx

-- readExported :: Hask.FilePath -> Hask.IO (Maybe ExportTx)
-- readExported exportFile =
--   LB.readFile exportFile >>= return . decode

parseTx  :: WbeTx a -> Either WbeError ChainIndexTx
parseTx wbeTx = parseApiTx wbeTx >>= toChainIndexTx

parseApiTx :: WbeTx a -> Either WbeError (C.Tx C.AlonzoEra)
parseApiTx WbeTx{..} = base64ToBinary transaction >>= parse
  where
    parse =
      first (DecoderError . Hask.show)
        . C.deserialiseFromCBOR (C.proxyToAsType Proxy)
    base64ToBinary =
      first (DecoderError . Hask.show)
        . convertFromBase @ByteString @ByteString Base64
        . TE.encodeUtf8

toChainIndexTx :: C.Tx C.AlonzoEra -> Either WbeError ChainIndexTx
toChainIndexTx =
  first (DecoderError . Hask.show)
    . fromCardanoTx C.AlonzoEraInCardanoMode
