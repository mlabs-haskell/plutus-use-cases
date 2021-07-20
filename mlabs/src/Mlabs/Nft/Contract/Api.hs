{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Contract API for Lendex application
module Mlabs.Nft.Contract.Api(
    Buy(..)
  , CurrentOwner(..)
  , SetPrice(..)
  , StartParams(..)
  , UserSchema
  , AuthorSchema
  , IsUserAct(..)
) where

import GHC.Generics (Generic)
import Playground.Contract (ToSchema, ToJSON, FromJSON)
import Plutus.Contract (type (.\/), BlockchainActions)
import PlutusTx.Prelude ( Show, Integer, Maybe, ByteString )
import qualified Prelude as Hask

import Mlabs.Data.Ray (Ray)
import Mlabs.Nft.Logic.Types ( UserAct(BuyAct, CurrentOwnerAct, SetPriceAct) )
import Mlabs.Plutus.Contract ( Call, IsEndpoint(..) )

----------------------------------------------------------------------
-- NFT endpoints

-- user endpoints

-- | User buys NFT
data Buy = Buy
  { buy'price     :: Integer
  , buy'newPrice  :: Maybe Integer
  }
  deriving stock (Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- | User sets new price for NFT
data SetPrice = SetPrice
  { setPrice'newPrice :: Maybe Integer
  }
  deriving stock (Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- jozef: Does not work without foo, cannot derive ToSchema.
newtype CurrentOwner = CurrentOwner ()
  deriving stock (Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
-- author endpoints

-- | Parameters to init NFT
data StartParams = StartParams
  { sp'content :: ByteString      -- ^ NFT content
  , sp'share   :: Ray             -- ^ author share [0, 1] on reselling of the NFT
  , sp'price   :: Maybe Integer   -- ^ current price of NFT, if it's nothing then nobody can buy it.
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

----------------------------------------------------------------------
-- schemas

-- | User schema. Owner can set the price and the buyer can try to buy.
type UserSchema =
  BlockchainActions
    .\/ Call Buy
    .\/ Call SetPrice
    .\/ Call CurrentOwner

-- | Schema for the author of NFT
type AuthorSchema =
  BlockchainActions
    .\/ Call StartParams

----------------------------------------------------------------------
-- classes

class IsUserAct a where
  toUserAct :: a -> UserAct

instance IsUserAct Buy      where { toUserAct Buy{..} = BuyAct buy'price buy'newPrice }
instance IsUserAct SetPrice where { toUserAct SetPrice{..} = SetPriceAct setPrice'newPrice }
instance IsUserAct CurrentOwner where { toUserAct CurrentOwner{} = CurrentOwnerAct }

instance IsEndpoint Buy where
  type EndpointSymbol Buy = "buy-nft"

instance IsEndpoint SetPrice where
  type EndpointSymbol SetPrice = "set-price-for-nft"

instance IsEndpoint StartParams where
  type EndpointSymbol StartParams = "start-nft"

instance IsEndpoint CurrentOwner where
  type EndpointSymbol CurrentOwner = "get-current-owner-for-nft"

