module Mlabs.Roundtrip.PAB.Run (
  DemoContracts,
  runRoundtripDemo
) where


import Plutus.PAB.Effects.Contract.Builtin qualified as Builtin
import Plutus.PAB.Run (runWith)
import Prelude

import Mlabs.Roundtrip.PAB.Contracts (DemoContracts)

-- | Start PAB for roundtrip demo
runRoundtripDemo :: IO ()
runRoundtripDemo = do
  runWith (Builtin.handleBuiltin @DemoContracts)