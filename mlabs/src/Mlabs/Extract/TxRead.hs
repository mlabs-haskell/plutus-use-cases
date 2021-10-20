module Mlabs.Extract.TxRead (runCompare, readExported) where

import Prelude qualified as Hask
import PlutusTx.Prelude

import Data.Aeson (decode)
import Data.ByteString.Lazy qualified as LB
import Data.Text.Prettyprint.Doc   (Pretty (..))

import Plutus.Contract.Wallet (ExportTx(..))


runCompare :: Hask.FilePath -> Hask.FilePath -> Hask.IO ()
runCompare exportedTx wbeTx = do
  readExported exportedTx >>= Hask.print . fmap partialTx

readExported :: Hask.FilePath -> Hask.IO (Maybe ExportTx)
readExported exportFile =
  LB.readFile exportFile >>= return . decode