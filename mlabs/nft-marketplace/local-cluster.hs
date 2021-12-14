module Main where

import Mlabs.PAB.LocalCluster
import Mlabs.NFT.PAB.MarketplaceContract

import Prelude

main :: IO ()
main = runLocalCluster @MarketplaceContracts
