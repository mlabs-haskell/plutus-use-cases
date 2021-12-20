module Main (main) where

import Cardano.Api (NetworkId (Testnet), NetworkMagic (..))
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as JSON
import Data.Text qualified as T
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Proxy (Proxy (Proxy))
import Data.Default (def)
import GHC.Generics (Generic)
import MLabsPAB qualified
import MLabsPAB.Types (
  CLILocation (Local),
  LogLevel(Debug, Info),
  HasDefinitions (..),
  PABConfig (..),
  SomeBuiltin (..),
  endpointsToSchemas,
  
 )
import Mlabs.Plutus.Contracts.CurrencyMP qualified as Contract.Currency

import Prelude


import Cardano.Api.Shelley (ProtocolParameters)

data DummyParams

data NftDemoContracts
  = GenerateUniqueToken
  -- | MintListHead -- TODO
  -- | MintNft Content -- TODO
  -- | BuyNFT -- TODO
  -- | SetNftPice -- TODO
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance HasDefinitions NftDemoContracts where
  getDefinitions = [GenerateUniqueToken]

  getSchema = \case
    GenerateUniqueToken -> endpointsToSchemas @Contract.Currency.CurrencySchema

  getContract = \case
    GenerateUniqueToken -> SomeBuiltin Contract.Currency.mintCurrency



main :: IO ()
main = do
  let paramsFile =  "./nft-marketplace-dec-demo/pparams.json"
  protocolParams :: Maybe ProtocolParameters <- JSON.decode <$> LazyByteString.readFile paramsFile
  let pabConf = PABConfig
        { pcCliLocation = Local
        , pcNetwork = Testnet (NetworkMagic 1097911063)
        , pcProtocolParams = protocolParams
        , pcScriptFileDir  = "/home/mike/dev/mlabs/plutus-use-cases/mlabs/nft-marketplace-dec-demo/scripts_and_data"
        , pcSigningKeyFileDir = "/home/mike/dev/mlabs/plutus-use-cases/mlabs/nft-marketplace-dec-demo/signing_keys"
        , pcTxFileDir = "/home/mike/dev/mlabs/plutus-use-cases/mlabs/nft-marketplace-dec-demo/transactions"
        , pcProtocolParamsFile = T.pack paramsFile
        , pcDryRun = False
        , pcLogLevel = Debug
        , pcOwnPubKeyHash= "bcd6bceeb0d22a7ca6ba1cd00669f7eb60ca8938d853666d30d56a56"
        }
  putStrLn "Starting ML-PAB"
  MLabsPAB.runPAB @NftDemoContracts pabConf 