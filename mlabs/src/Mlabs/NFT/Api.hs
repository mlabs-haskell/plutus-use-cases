module Mlabs.NFT.Api (
  NFTAppSchema,
  schemas,
  endpoints,
  queryEndpoints,
) where

import PlutusTx.Prelude hiding (mconcat, (<>))
import Prelude (mconcat, (<>))
import Prelude qualified as Hask

import Control.Lens (at, filtered, to, traversed, (^.), (^..), (^?), _Just, _Right)
import Control.Monad (join, void)
import Data.List qualified as L
import Data.Map qualified as Map
import Data.Monoid (Last (..))
import Data.Text (Text, pack)

import Text.Printf (printf)

import Plutus.ChainIndex.Tx (
  ChainIndexTx,
  citxData,
 )
import Plutus.Contract (Contract, Endpoint, endpoint, utxosTxOutTxAt, type (.\/))
import Plutus.Contract qualified as Contract
import Plutus.V1.Ledger.Value (symbols)
import PlutusTx qualified

import Ledger (
  Address,
  ChainIndexTxOut,
  CurrencySymbol,
  Datum (..),
  Redeemer (..),
  TxOutRef,
  ciTxOutDatum,
  ciTxOutValue,
  getDatum,
  pubKeyAddress,
  pubKeyHash,
  scriptCurrencySymbol,
  txId,
 )

import Ledger.Constraints qualified as Constraints
import Ledger.Typed.Scripts (validatorScript)
import Ledger.Value as Value (TokenName (..), singleton, unAssetClass, valueOf)

import Playground.Contract (mkSchemaDefinitions)

import Mlabs.NFT.Contract (
  QueryContract,
  UserContract,
 )

import Mlabs.NFT.Types (
  BuyRequestUser (..),
  Content (..),
  MintAct (..),
  MintParams (..),
  NftId (..),
  QueryResponse (..),
  SetPriceParams (..),
  UserId (..),
 )

import Mlabs.NFT.Contract qualified as NFTContract

import Mlabs.NFT.Validation (
  DatumNft (..),
  NftTrade,
  UserAct (..),
  asRedeemer,
  calculateShares,
  mintPolicy,
  nftAsset,
  nftCurrency,
  priceNotNegative,
  txPolicy,
  txScrAddress,
 )

import Mlabs.Plutus.Contract (readDatum', selectForever)

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
