module Main where

import Mlabs.NFTBuySetPrice.PAB
import Mlabs.PAB.LocalCluster

main :: IO ()
main = runLocalCluster @BuySetContract
