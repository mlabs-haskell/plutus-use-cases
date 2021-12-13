module Mlabs.NFTBuySetPrice.Types (
  BSPBuyRequestUser(..)
) where

import PlutusTx.Prelude
import Prelude qualified as Hask
import PlutusTx qualified

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Schema (ToSchema)

import Mlabs.NFT.Types

data BSPBuyRequestUser = BSPBuyRequestUser
  { -- | nftId to Buy
    bsur'nftContent :: Content
  , -- | price to buy, in Lovelace.
    bsur'price :: Integer
  , -- | new price for NFT (Nothing locks NFT), in Lovelace.
    bsur'newPrice :: Maybe Integer
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.makeLift ''BSPBuyRequestUser
PlutusTx.unstableMakeIsData ''BSPBuyRequestUser

instance Eq BSPBuyRequestUser where
  {-# INLINEABLE (==) #-}
  (BSPBuyRequestUser content1 price1 newPrice1) == (BSPBuyRequestUser content2 price2 newPrice2) =
    content1 == content2 && price1 == price2 && newPrice1 == newPrice2