module Main (main) where

import Prelude

import Plutus.PAB.Effects.Contract.Builtin qualified as Builtin
import Plutus.PAB.Run (runWith)

import Mlabs.DemoPAB.PAB (DemoPABContracts)

main :: IO ()
main = do
    runWith (Builtin.handleBuiltin @DemoPABContracts)