module Mlabs.NFTBuySetPrice.Api (
  NFTBySetPriceSchema,
  buySetEndpoints,
) where

import Data.Text (Text)

import Mlabs.Plutus.Contract (selectForever)
import Mlabs.NFT.Types (UniqueToken, UserWriter)
import Mlabs.NFTBuySetPrice.Types
import Mlabs.NFTBuySetPrice.Contract as NFTBSP

import Plutus.Contract (Contract, Endpoint, endpoint, type (.\/))

-- | Schema for NFT buy-set-price dApp
type NFTBySetPriceSchema =
    Endpoint "buy" BSPBuyRequestUser
    .\/ Endpoint "set-price" BSPSetPriceParams

buySetEndpoints :: UniqueToken -> Contract UserWriter NFTBySetPriceSchema Text ()
buySetEndpoints uT = 
  selectForever [ endpoint @"buy" (NFTBSP.buy uT)
                , endpoint @"set-price"  (NFTBSP.setPrice uT)
                ]
