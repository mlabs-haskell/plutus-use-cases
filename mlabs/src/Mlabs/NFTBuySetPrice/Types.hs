module Mlabs.NFTBuySetPrice.Types (
  BSPBuyRequestUser(..),
  BSPSetPriceParams(..)
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

data BSPSetPriceParams = BSPSetPriceParams
  { -- | NFTid of the token which price is set.
    bssp'nftContent :: Content
  , -- | New price, in Lovelace.
    bssp'price :: Maybe Integer 
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

instance Eq BSPSetPriceParams where
  {-# INLINEABLE (==) #-}
  (BSPSetPriceParams nftId1 price1) == (BSPSetPriceParams nftId2 price2) =
    nftId1 == nftId2 && price1 == price2