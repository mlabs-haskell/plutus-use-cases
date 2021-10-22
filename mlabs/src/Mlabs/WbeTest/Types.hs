module Mlabs.WbeTest.Types
( WbeExportTx(..),
  WalletId,
  Balanced,
  Signed, 
  WbeTx(..),
) where

import Prelude qualified as Hask


import GHC.Generics (Generic)
import Data.Aeson ( FromJSON, ToJSON)
import Data.Text (Text)


-- TODO Dummy one, need to be substituted with real one
data WbeExportTx = WbeExportTx
  deriving stock (Hask.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

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