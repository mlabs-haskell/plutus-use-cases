module Mlabs.Roundtrip.PAB.Run (
  DemoContracts
) where


import Plutus.PAB.Effects.Contract.Builtin qualified as Builtin
import Plutus.PAB.Run (runWith)
import Prelude

import Mlabs.Roundtrip.PAB.Contracts (DemoContracts)

-- | Start PAB for NFT contract
runRoundtripDemo :: IO ()
runNftMarketplace = do
  runWith (Builtin.handleBuiltin @DemoContracts)