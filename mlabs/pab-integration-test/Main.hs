module Main where

import Mlabs.IntegrationTest.PabWbe.Pab

import Plutus.PAB.Effects.Contract.Builtin qualified as Builtin
import Plutus.PAB.Run (runWith)

import Prelude

main :: IO ()
main = runWith $ Builtin.handleBuiltin @TestContracts
