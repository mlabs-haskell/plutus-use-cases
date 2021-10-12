module Mlabs.NFT.Api (
  NFTAppSchema,
  schemas,
  endpoints,
  queryEndpoints,
) where

import Data.Monoid (Last (..))
import Data.Text (Text)

import Playground.Contract (mkSchemaDefinitions)
import Plutus.Contract (Contract, Endpoint, endpoint, type (.\/))

import Mlabs.NFT.Contract qualified as NFTContract
import Mlabs.NFT.Types (BuyRequestUser (..), MintParams (..), NftId (..), QueryResponse (..), SetPriceParams (..))
import Mlabs.Plutus.Contract (selectForever)

-- | A common App schema works for now.
type NFTAppSchema =
  -- Author Endpoint
  Endpoint "mint" MintParams
    -- User Action Endpoints
    .\/ Endpoint "buy" BuyRequestUser
    .\/ Endpoint "set-price" SetPriceParams
    -- Query Endpoints
    .\/ Endpoint "query-current-owner" NftId
    .\/ Endpoint "query-current-price" NftId
    .\/ Endpoint "query-authentic-nft" NftId

mkSchemaDefinitions ''NFTAppSchema

-- ENDPOINTS --

type ApiUserContract a = Contract (Last NftId) NFTAppSchema Text a
type ApiQueryContract a = Contract QueryResponse NFTAppSchema Text a

-- | User Endpoints .
endpoints :: ApiUserContract ()
endpoints =
  selectForever
    [ endpoint @"mint" NFTContract.mint
    , endpoint @"buy" NFTContract.buy
    , endpoint @"set-price" NFTContract.setPrice
    , endpoint @"query-authentic-nft" NFTContract.queryAuthenticNFT
    ]

-- Query Endpoints are used for Querying, with no on-chain tx generation.
queryEndpoints :: ApiQueryContract ()
queryEndpoints =
  selectForever
    [ endpoint @"query-current-price" NFTContract.queryCurrentPrice
    , endpoint @"query-current-owner" NFTContract.queryCurrentOwner
    ]
