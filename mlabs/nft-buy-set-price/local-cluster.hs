module Main where

import Mlabs.NFTBuySetPrice.PAB
import Mlabs.PAB.LocalCluster

import Prelude

main :: IO ()
main = runLocalCluster @BuySetContract
