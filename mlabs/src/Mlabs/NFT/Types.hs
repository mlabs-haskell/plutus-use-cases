{-# LANGUAGE UndecidableInstances #-}

module Mlabs.NFT.Types (
  UserId (..),
  QueryResponse (..),
  NftId (..),
  BuyRequestUser (..),
  MintParams (..),
  SetPriceParams (..),
  Content (..),
  Title (..),
  AuctionBid (..),
  AuctionState (..),
  AuctionOpenParams (..),
  AuctionBidParams (..),
  AuctionCloseParams (..),
) where

import PlutusTx.Prelude
import Prelude qualified as Hask

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

import Ledger (POSIXTime, PubKeyHash, TokenName, TxOutRef)
import PlutusTx qualified
import Schema (ToSchema)

-- ON-CHAIN TYPES --
newtype Content = Content {getContent :: BuiltinByteString}
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''Content
PlutusTx.makeLift ''Content

instance Eq Content where
  {-# INLINEABLE (==) #-}
  (Content c1) == (Content c2) = c1 == c2

newtype Title = Title {getTitle :: BuiltinByteString}
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''Title
PlutusTx.makeLift ''Title

instance Eq Title where
  {-# INLINEABLE (==) #-}
  (Title t1) == (Title t2) = t1 == t2

newtype UserId = UserId {getUserId :: PubKeyHash}
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''UserId
PlutusTx.makeLift ''UserId

instance Eq UserId where
  {-# INLINEABLE (==) #-}
  (UserId u1) == (UserId u2) = u1 == u2

data AuctionBid = AuctionBid
  { -- | Bid in Lovelace
    ab'bid :: Integer
  , -- | Bidder's wallet pubkey
    ab'bidder :: UserId
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''AuctionBid
PlutusTx.makeLift ''AuctionBid

instance Eq AuctionBid where
  {-# INLINEABLE (==) #-}
  (AuctionBid bid1 bidder1) == (AuctionBid bid2 bidder2) =
    bid1 == bid2 && bidder1 == bidder2

data AuctionState = AuctionState
  { -- | Highest bid
    as'highestBid :: Maybe AuctionBid
  , -- | Deadline
    as'deadline :: POSIXTime
  , -- | Minimum bid amount
    as'minBid :: Integer
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''AuctionState
PlutusTx.makeLift ''AuctionState

instance Eq AuctionState where
  {-# INLINEABLE (==) #-}
  (AuctionState bid1 deadline1 minBid1) == (AuctionState bid2 deadline2 minBid2) =
    bid1 == bid2 && deadline1 == deadline2 && minBid1 == minBid2

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

instance Eq NftId where
  {-# INLINEABLE (==) #-}
  (NftId title1 token1 outRef1) == (NftId title2 token2 outRef2) =
    title1 == title2 && token1 == token2 && outRef1 == outRef2

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

PlutusTx.unstableMakeIsData ''NftContent
PlutusTx.makeLift ''NftContent

instance Eq NftContent where
  {-# INLINEABLE (==) #-}
  (NftContent title1 data1 author1) == (NftContent title2 data2 author2) =
    title1 == title2 && data1 == data2 && author1 == author2

-- ENDPOINTS PARAMETERS --

-- | Parameters that need to be submitted when minting a new NFT.
data MintParams = MintParams
  { -- | Content to be minted.
    mp'content :: Content
  , -- | Title of content.
    mp'title :: Title
  , -- | Shares retained by author.
    mp'share :: Rational
  , -- | Listing price of the NFT, in Lovelace.
    mp'price :: Maybe Integer
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

PlutusTx.makeLift ''MintParams
PlutusTx.unstableMakeIsData ''MintParams

instance Eq MintParams where
  {-# INLINEABLE (==) #-}
  (MintParams content1 title1 share1 price1) == (MintParams content2 title2 share2 price2) =
    content1 == content2 && title1 == title2 && share1 == share2 && price1 == price2

data SetPriceParams = SetPriceParams
  { -- | NFTid of the token which price is set.
    sp'nftId :: NftId
  , -- | New price, in Lovelace.
    sp'price :: Maybe Integer -- todo maybe Natural? are they available here?
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

instance Eq SetPriceParams where
  {-# INLINEABLE (==) #-}
  (SetPriceParams nftId1 price1) == (SetPriceParams nftId2 price2) =
    nftId1 == nftId2 && price1 == price2

data AuctionOpenParams = AuctionOpenParams
  { -- | TODO
    op'nftId :: NftId
  , -- | TODO
    op'deadline :: POSIXTime
  , -- | TODO
    op'minBid :: Integer
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''AuctionOpenParams
PlutusTx.makeLift ''AuctionOpenParams

instance Eq AuctionOpenParams where
  {-# INLINEABLE (==) #-}
  (AuctionOpenParams nftId1 deadline1 minBid1) == (AuctionOpenParams nftId2 deadline2 minBid2) =
    nftId1 == nftId2 && deadline1 == deadline2 && minBid1 == minBid2

data AuctionBidParams = AuctionBidParams
  { -- | TODO
    op'bidAmount :: Integer
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''AuctionBidParams
PlutusTx.makeLift ''AuctionBidParams

instance Eq AuctionBidParams where
  {-# INLINEABLE (==) #-}
  (AuctionBidParams bid1) == (AuctionBidParams bid2) =
    bid1 == bid2

data AuctionCloseParams = AuctionCloseParams
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON) --, ToSchema)
PlutusTx.unstableMakeIsData ''AuctionCloseParams
PlutusTx.makeLift ''AuctionCloseParams

instance Eq AuctionCloseParams where
  {-# INLINEABLE (==) #-}
  AuctionCloseParams == AuctionCloseParams = True

data BuyRequestUser = BuyRequestUser
  { -- | nftId to Buy
    ur'nftId :: NftId
  , -- | price to buy, in Lovelace.
    ur'price :: Integer
  , -- | new price for NFT (Nothing locks NFT), in Lovelace.
    ur'newPrice :: Maybe Integer
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.makeLift ''BuyRequestUser
PlutusTx.unstableMakeIsData ''BuyRequestUser

instance Eq BuyRequestUser where
  {-# INLINEABLE (==) #-}
  (BuyRequestUser nftId1 price1 newPrice1) == (BuyRequestUser nftId2 price2 newPrice2) =
    nftId1 == nftId2 && price1 == price2 && newPrice1 == newPrice2

-- | A datatype used by the QueryContract to return a response
data QueryResponse
  = QueryCurrentOwner UserId
  | QueryCurrentPrice (Maybe Integer)
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON)
