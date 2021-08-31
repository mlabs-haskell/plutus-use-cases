module Main where

import Prelude (String, IO, undefined, print, error)
import System.Environment (getArgs)
import System.Exit (die)
import PlutusTx.Prelude hiding (error)

import Mlabs.Nft.Contract.StateMachine as SM
import Mlabs.Nft.Logic.Types (Act(..), UserAct(..), Nft(..), NftId(..), toNftId, initNft)
import Mlabs.Nft.Contract.Forge as F
import Mlabs.Emulator.Types (UserId(..))

import           Cardano.Api
import           Cardano.Api.Shelley

import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import           Plutus.V1.Ledger.Api (Validator, MintingPolicy, TxOutRef)
import qualified Plutus.V1.Ledger.Api as Plutus
import           Codec.Serialise
import Ledger.Typed.Scripts.Validators as VS
import PlutusTx as PlutusTx


import qualified Data.ByteString.Lazy   as LB
import qualified Data.ByteString.Short  as SBS
import Data.Aeson as Json

import Data.ByteString as DB
import Mlabs.Deploy.Nft
import Mlabs.Deploy.Governance


main :: IO ()
main = do
  args <- getArgs
  case args of
    ["Nft"] -> 
      serializeNft 
        "8cf79ca29e5d62ad3c62c6f43c38a1eb1db75f0338e3c8ded5c7ca7be910eef3"
        0
        "4cebc6f2a3d0111ddeb09ac48e2053b83b33b15f29182f9b528c6491"
        "BFT Content"
        "./../.github/workflows/nft_delivery"
    -- ["gov"] -> serializeGovernance  
    _ -> 
      die "Unknown deployment tsk type"
