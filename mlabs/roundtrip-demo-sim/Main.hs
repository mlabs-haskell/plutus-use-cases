module Main (main) where

import Prelude
import Mlabs.Roundtrip.PAB.Run (runRoundtripDemo)
import Mlabs.Roundtrip.PAB.Simulator

main :: IO ()
main = 
  -- runRoundtripDemo
  runSimulator
