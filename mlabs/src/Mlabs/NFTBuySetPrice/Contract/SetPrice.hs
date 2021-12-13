{-# LANGUAGE UndecidableInstances #-}

module Mlabs.NFTBuySetPrice.Contract.SetPrice (
  setPrice,
) where

import PlutusTx.Prelude
-- import Prelude qualified as Hask

import Data.Text (Text)
import Plutus.Contract (Contract)

import Mlabs.NFT.Types (NftId(..), SetPriceParams(..), UniqueToken, UserWriter)
import Mlabs.NFTBuySetPrice.Types
import Mlabs.NFT.Contract.SetPrice qualified as NFT.SetPrice
import Mlabs.NFT.Contract.Aux (hashData)

{- |
  Attempts to set price of NFT, checks if price is being set by the owner
  and that NFT is not on an auction.
-}
setPrice :: UniqueToken -> BSPSetPriceParams -> Contract UserWriter s Text ()
setPrice uT BSPSetPriceParams {..} = 
  let nftId = NftId $ hashData bssp'nftContent
      setPriceParams = SetPriceParams nftId bssp'price
  in NFT.SetPrice.setPrice uT setPriceParams