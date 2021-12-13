module Mlabs.NFTBuySetPrice.PAB (
  runBuySetMarketplace,
) where

import Plutus.PAB.Effects.Contract.Builtin qualified as Builtin
import Plutus.PAB.Run (runWith)
import Prelude


-- There will be just one ADT to hook it to PAB from what I understand now, probably can just go here

-- | Start PAB for NFT contract
runBuySetMarketplace :: IO ()
runBuySetMarketplace = undefined
