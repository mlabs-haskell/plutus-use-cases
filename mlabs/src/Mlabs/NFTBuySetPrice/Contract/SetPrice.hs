{-# LANGUAGE UndecidableInstances #-}

module Mlabs.NFTBuySetPrice.Contract.SetPrice (
  setPrice,
) where

import PlutusTx.Prelude hiding (mconcat, mempty, (<>))
import Prelude (mconcat)
import Prelude qualified as Hask

-- import Control.Lens ((^.))
-- import Control.Monad (void, when)

-- import Data.Map qualified as Map
-- import Data.Monoid (Last (..))
-- import Data.Text (Text)

-- --import Mlabs.Plutus.Contract ()
-- import Plutus.Contract (Contract)
-- import Plutus.Contract qualified as Contract
-- import PlutusTx qualified

-- import Ledger (
--   Redeemer (..),
--   ciTxOutValue,
--  )

-- import Ledger.Constraints qualified as Constraints
-- import Ledger.Typed.Scripts (validatorScript)

-- --import Ledger.Value as Value (AssetClass (..), TokenName (..), singleton)
-- --import Plutus.ChainIndex.Tx ()

import Mlabs.NFT.Contract.Aux
import Mlabs.NFT.Types
import Mlabs.NFT.Validation

{- |
  Attempts to set price of NFT, checks if price is being set by the owner
  and that NFT is not on an auction.
-}
-- Not sure about SetPriceParams - it has `NftId` - will calling side have access to it?
-- setPrice :: NftContent -> SetPriceParams -> Contract UserWriter s Text ()
setPrice = Hask.undefined