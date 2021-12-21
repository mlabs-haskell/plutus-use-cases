module Main (main) where

import Cardano.Api (NetworkId (Testnet), NetworkMagic (..))
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as JSON
import Data.Text qualified as T
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Proxy (Proxy (Proxy))
import Data.Default (def)
import GHC.Generics (Generic)
import Data.ByteString.Lazy.Char8 qualified as LBS

import Plutus.Contract (Contract, EmptySchema)

import MLabsPAB qualified
import MLabsPAB.Types (
  CLILocation (Local),
  LogLevel(Debug, Info),
  HasDefinitions (..),
  PABConfig (..),
  SomeBuiltin (..),
  endpointsToSchemas,
  
 )
import Mlabs.NFT.Types (
  UniqueToken, 
  InitParams, 
  MintParams,
  Content(Content),
  Title(Title),
  MintParams(MintParams, mp'content),
  NftId(NftId),
  SetPriceParams(SetPriceParams)
  )
import Mlabs.Plutus.Contracts.CurrencyMP qualified as Contract.Currency
import Mlabs.NFT.Contract.InitMP qualified as Contract.Init
import Mlabs.NFT.Contract.MintMP qualified as Contract.Mint
import Mlabs.NFT.Contract.Aux (hashData)

import Prelude
import PlutusTx.Prelude qualified as PP

import PlutusTx.Builtins.Class (stringToBuiltinByteString)


import Cardano.Api.Shelley (ProtocolParameters)

data MintHeadParams = MintHeadParams
  { mhp'uniqueToken :: UniqueToken
  , mhp'initParams :: InitParams
  } deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data MintData = MintData
  { mdContent :: String
  , mdTitle :: String
  , mdShare :: PP.Rational
  , mdPrice :: Maybe Integer
  } deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

toMintParams :: MintData -> MintParams
toMintParams (MintData content title share price) = 
  MintParams 
    (Content $ stringToBuiltinByteString  content) 
    (Title $ stringToBuiltinByteString  title) 
    share 
    price


data MintNftParams = MintNftParams
  { mnp'uniqueToken :: UniqueToken,
    mnp'mintParams :: MintData 
    -- ^ `MintParams` not used straight to make request more readable for demo
  } deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data NftDemoContracts
  = GenerateUniqueToken
  | MintListHead MintHeadParams
  | MintNft MintNftParams
  -- | BuyNFT -- TODO
  -- | SetNftPice -- TODO
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance HasDefinitions NftDemoContracts where
  getDefinitions = [GenerateUniqueToken]

  getSchema = \case
    GenerateUniqueToken -> endpointsToSchemas @Contract.Currency.CurrencySchema
    MintListHead _ -> endpointsToSchemas @EmptySchema
    MintNft _ -> endpointsToSchemas @EmptySchema

  getContract = \case
    GenerateUniqueToken 
      -> SomeBuiltin $ Contract.Currency.mintCurrency Contract.Init.uniqueTokenName
    MintListHead (MintHeadParams ut initPs) 
      -> SomeBuiltin $ Contract.Init.initApp ut initPs
    MintNft (MintNftParams ut mintData) 
      -> SomeBuiltin $ Contract.Mint.mint ut (toMintParams mintData)



main :: IO ()
main = do
  let paramsFile =  "./nft-marketplace-dec-demo/pparams.json"
      someMintParams = MintParams (Content "Some Content") (Title "Some Title") (1 PP.%2) Nothing
  protocolParams :: Maybe ProtocolParameters <- JSON.decode <$> LazyByteString.readFile paramsFile
  let pabConf = PABConfig
        { pcCliLocation = Local
        , pcNetwork = Testnet (NetworkMagic 1097911063)
        , pcProtocolParams = protocolParams
        , pcScriptFileDir  = "nft-marketplace-dec-demo/scripts_and_data"
        , pcSigningKeyFileDir = "nft-marketplace-dec-demo/signing_keys"
        , pcTxFileDir = "nft-marketplace-dec-demo/transactions"
        , pcProtocolParamsFile = T.pack paramsFile
        , pcDryRun = False
        , pcLogLevel = Debug
        , pcOwnPubKeyHash= "bcd6bceeb0d22a7ca6ba1cd00669f7eb60ca8938d853666d30d56a56"
        }
  logExampleParams someMintParams
  MLabsPAB.runPAB @NftDemoContracts pabConf
  where
    logExampleParams mps = do
      let nftId = NftId . hashData . mp'content $ mps
          setPricePs = SetPriceParams nftId (Just 33)
      LBS.putStrLn ("Mint parameters: " <> JSON.encode mps)
      LBS.putStrLn ("Content hash: " <> JSON.encode nftId)
      LBS.putStrLn ("Set price: " <> JSON.encode setPricePs)