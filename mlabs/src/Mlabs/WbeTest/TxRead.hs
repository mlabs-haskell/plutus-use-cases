module Mlabs.WbeTest.TxRead (parseTx) where

import Prelude qualified as Hask
import PlutusTx.Prelude

import Control.Arrow

import Data.ByteArray.Encoding (Base (Base64), convertFromBase)
import Data.ByteString (ByteString)
import           Data.Proxy                  (Proxy (..))
import Data.Text.Encoding as TE (encodeUtf8 )

import qualified Cardano.Api                 as C

import Mlabs.WbeTest.Types


-- runCompare :: Hask.FilePath -> Hask.FilePath -> Hask.IO ()
-- runCompare exportedTx wbeTx = do
--   readExported exportedTx >>= Hask.print . fmap partialTx

-- readExported :: Hask.FilePath -> Hask.IO (Maybe ExportTx)
-- readExported exportFile =
--   LB.readFile exportFile >>= return . decode


parseTx :: WbeTx a -> Either Hask.String  (C.Tx C.AlonzoEra)
parseTx WbeTx{..} =
  base64ToBinary transaction >>= parse
  where
    parse = left Hask.show . C.deserialiseFromCBOR (C.proxyToAsType Proxy)
    base64ToBinary = 
      convertFromBase @ByteString @ByteString Base64 
      . TE.encodeUtf8 
