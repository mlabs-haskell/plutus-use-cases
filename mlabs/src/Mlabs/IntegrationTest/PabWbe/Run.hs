module Mlabs.IntegrationTest.PabWbe.Run where

import           Mlabs.IntegrationTest.PabWbe.TestContracts (TestContracts)
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import           Plutus.PAB.Run                      (runWith)

runIntegrationTestPAB :: IO ()
runIntegrationTestPAB = do
    runWith (Builtin.handleBuiltin @TestContracts)