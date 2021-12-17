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
  LogLevel(Debug),
  HasDefinitions (..),
  PABConfig (..),
  SomeBuiltin (..),
  endpointsToSchemas,
  
 )
import Mlabs.Plutus.Contracts.Currency qualified as Contract.Currency

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
  getDefinitions = []

  getSchema = \case
    GenerateUniqueToken -> endpointsToSchemas @Contract.Currency.CurrencySchema

  getContract = \case
    GenerateUniqueToken -> SomeBuiltin Contract.Currency.mintCurrency



main :: IO ()
main = do
  let paramsFile =  "./nft-marketplace-dec-demo/pparams.json"
  protocolParams :: Maybe ProtocolParameters <- JSON.decode <$> LazyByteString.readFile paramsFile
  putStrLn $ show protocolParams
  let pabConf = PABConfig
        { pcCliLocation = Local
        , pcNetwork = Testnet (NetworkMagic 1097911063)
        , pcProtocolParams = protocolParams
        , pcScriptFileDir  = "./nft-marketplace-dec-demo/scripts_and_data"
        , pcSigningKeyFileDir = "./nft-marketplace-dec-demo/signing_keys"
        , pcTxFileDir = "./nft-marketplace-dec-demo/transactions"
        , pcProtocolParamsFile = T.pack paramsFile
        , pcDryRun = False
        , pcLogLevel = Debug
        }
  MLabsPAB.runPAB @NftDemoContracts pabConf 