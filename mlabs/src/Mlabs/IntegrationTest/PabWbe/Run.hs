module Mlabs.IntegrationTest.PabWbe.Run (
  runIntegrationTestPAB,
) where

import Mlabs.IntegrationTest.PabWbe.TestContracts (TestContracts)
import Plutus.PAB.Effects.Contract.Builtin qualified as Builtin
import Plutus.PAB.Run (runWith)
import Prelude qualified as Hask

runIntegrationTestPAB :: Hask.IO ()
runIntegrationTestPAB = do
  runWith (Builtin.handleBuiltin @TestContracts)
