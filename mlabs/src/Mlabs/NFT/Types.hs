{-# LANGUAGE UndecidableInstances #-}


module Mlabs.NFT.Types where

import PlutusTx.Prelude
import Prelude qualified as Hask

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

import Ledger (PubKeyHash, TokenName, TxOutRef)
import PlutusTx qualified
import Schema (ToSchema)


-- ON-CHAIN TYPES --
newtype Content = Content {getContent :: BuiltinByteString}
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''Content
PlutusTx.makeLift ''Content

newtype Title = Title {getTitle :: BuiltinByteString}
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''Title
PlutusTx.makeLift ''Title

newtype UserId = UserId {getUserId :: PubKeyHash}
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''UserId
PlutusTx.makeLift ''UserId

{- | Unique identifier of NFT.
 The NftId contains a human readable title, the hashed information of the
 content and the utxo ref included when minting the token.
-}
data NftId = NftId
  { -- | Content Title.
    nftId'title :: Title
  , -- | token name is identified by content of the NFT (it's hash of it)
    nftId'token :: TokenName
  , -- | TxOutRef which was used to mint current NFT
    nftId'outRef :: TxOutRef
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''NftId
PlutusTx.makeLift ''NftId

{- | Type representing the data that gets hashed when the token is minted. The
 tile and author are included for proof of authenticity in the case the same
 artwork is hashed more than once.
-}
data NftContent = NftContent
  { -- | Content Title.
    ct'title :: Title
  , -- | data (NftContent, audio, photo, etc)
    ct'data :: Content
  , -- | author
    ct'author :: UserId
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
  deriving (Eq)
PlutusTx.unstableMakeIsData ''NftContent
PlutusTx.makeLift ''NftContent

-- ENDPOINTS PARAMETERS --
-- | Parameters that need to be submitted when minting a new NFT.
data MintParams = MintParams
  { -- | Content to be minted.
    mp'content :: Content
  , -- | Title of content.
    mp'title :: Title
  , -- | Shares retained by author.
    mp'share :: Rational
  , -- | Listing price of the NFT.
    mp'price :: Maybe Integer
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
  -- deriving (Eq)

-- PlutusTx.makeLift ''MintParams
-- PlutusTx.unstableMakeIsData ''MintParams

data SetPriceParams = SetPriceParams
  { sp'nftId :: NftId
  , sp'price :: Maybe Integer -- todo maybe Natural? are they available here?
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data BuyRequestUser = BuyRequestUser
  { -- | nftId to Buy
    ur'nftId :: NftId
  , -- | price to buy
    ur'price :: Integer
  , -- | new price for NFT (Nothing locks NFT)
    ur'newPrice :: Maybe Integer
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
  -- deriving (Eq)
-- PlutusTx.makeLift ''BuyRequestUser
-- PlutusTx.unstableMakeIsData ''BuyRequestUser