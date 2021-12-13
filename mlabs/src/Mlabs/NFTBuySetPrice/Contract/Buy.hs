{-# LANGUAGE UndecidableInstances #-}

module Mlabs.NFTBuySetPrice.Contract.Buy (
  buy,
) where

import Prelude (mconcat)
import Prelude qualified as Hask

-- import Control.Lens ((^.))
-- import Control.Monad (void, when)
-- import Data.Map qualified as Map
-- import Data.Monoid (Last (..), (<>))
-- import Data.Text (Text)

-- import Plutus.Contract (Contract)
-- import Plutus.Contract qualified as Contract
-- import Plutus.Contract.Constraints qualified as Constraints
-- import PlutusTx qualified
-- import PlutusTx.Prelude hiding (mconcat, mempty, (<>))

-- import Ledger (
--   Datum (..),
--   Redeemer (..),
--   ciTxOutValue,
--  )
-- import Ledger.Typed.Scripts (validatorScript)

-- import Mlabs.NFT.Contract.Aux
-- import Mlabs.NFT.Contract.Gov.Fees
-- import Mlabs.NFT.Contract.Gov.Query
import Mlabs.NFT.Types
-- import Mlabs.NFT.Validation

{- | BUY.
 Attempts to buy a new NFT by changing the owner, pays the current owner and
 the author, and sets a new price for the NFT.
-}
-- Not sure about BuyRequestUser - it has `NftId` - will calling side have access to it?
-- buy :: forall s. NftContent -> BuyRequestUser -> Contract UserWriter s Text ()
buy = Hask.undefined