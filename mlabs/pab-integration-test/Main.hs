module Main where

import Mlabs.IntegrationTest.PabWbe.Pab
import Mlabs.IntegrationTest.PabWbe.Simulator (runSimulator)

import Plutus.PAB.Effects.Contract.Builtin qualified as Builtin
import Plutus.PAB.Run (runWith)

import Prelude

main :: IO ()
main = do
  -- runSimulator -- for debug
  runWith $ Builtin.handleBuiltin @TestContracts
