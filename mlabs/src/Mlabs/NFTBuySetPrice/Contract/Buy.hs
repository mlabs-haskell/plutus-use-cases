{-# LANGUAGE UndecidableInstances #-}

module Mlabs.NFTBuySetPrice.Contract.Buy (
  buy,
) where

import PlutusTx.Prelude
-- import Prelude qualified as Hask

import Data.Text (Text)
import Plutus.Contract (Contract)

import Mlabs.NFT.Types (NftId(..), BuyRequestUser(..), UniqueToken, UserWriter)
import Mlabs.NFTBuySetPrice.Types
import Mlabs.NFT.Contract.Buy qualified as NFT.Buy
import Mlabs.NFT.Contract.Aux (hashContent)

{- | BUY.
 Attempts to buy a new NFT by changing the owner, pays the current owner and
 the author, and sets a new price for the NFT.
-}
buy :: forall s. UniqueToken -> BSPBuyRequestUser -> Contract UserWriter s Text ()
buy uT BSPBuyRequestUser {..} = 
  let nftId = NftId $ hashContent bsur'nftContent
      buyParams = BuyRequestUser nftId bsur'price bsur'newPrice
  in NFT.Buy.buy uT buyParams
