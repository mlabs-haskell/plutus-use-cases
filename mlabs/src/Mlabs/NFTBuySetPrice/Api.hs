module Mlabs.NFTBuySetPrice.Api (
  NFTBySetPriceSchema,
  buySetEndpoints,
) where

import Data.Monoid (Last (..))
import Data.Text (Text)

import Control.Monad (void)

import Playground.Contract (mkSchemaDefinitions)
import Plutus.Contract (Contract, Endpoint, Promise, endpoint, type (.\/))
import Prelude as Hask

import Mlabs.NFT.Contract.BidAuction (bidAuction)
import Mlabs.NFT.Contract.Buy (buy)
import Mlabs.NFT.Contract.CloseAuction (closeAuction)
import Mlabs.NFT.Contract.Init (initApp)
import Mlabs.NFT.Contract.Mint (mint)
import Mlabs.NFT.Contract.OpenAuction (openAuction)
import Mlabs.NFT.Contract.Query (queryContent, queryCurrentOwner, queryCurrentPrice, queryListNfts)
import Mlabs.NFT.Contract.SetPrice (setPrice)
import Mlabs.NFT.Types (
  AdminContract,
  AuctionBidParams (..),
  AuctionCloseParams (..),
  AuctionOpenParams (..),
  BuyRequestUser (..),
  Content,
  InitParams (..),
  MintParams (..),
  NftAppInstance (..),
  NftId (..),
  SetPriceParams (..),
  UniqueToken,
  UserContract,
  UserWriter,
 )
import Mlabs.Plutus.Contract (selectForever)

-- | A common App schema works for now.
type NFTBySetPriceSchema =
    Endpoint "buy" BuyRequestUser
    .\/ Endpoint "set-price" SetPriceParams

buySetEndpoints = Hask.undefined