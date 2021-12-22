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
import Mlabs.NFT.Contract.SetPriceMP qualified as Contract.SetPrice
import Mlabs.NFT.Contract.Aux (hashData)

import Prelude
import PlutusTx.Prelude qualified as PP

import PlutusTx.Builtins.Class (stringToBuiltinByteString)


import Cardano.Api.Shelley (ProtocolParameters)

-- MINT HEAD
data MintHeadReq = MintHeadReq
  { mhr'uniqueToken :: UniqueToken
  , mhr'initParams :: InitParams
  } deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- MINT NFT

-- Data to substitute original contracts parameter, accepts `Content` as plain text
data MintNftData = MintNftData
  { mnd'content :: String
  , mnd'title :: String
  , mnd'share :: PP.Rational
  , mnd'price :: Maybe Integer
  } deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data MintNftReq = MintNftReq
  { mnr'uniqueToken :: UniqueToken
  , mnr'data :: MintNftData
  } deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

toMintParams :: MintNftData -> MintParams
toMintParams (MintNftData content title share price) = 
  MintParams 
    (toContent content) 
    (toTitle title) 
    share 
    price

toContent :: String -> Content
toContent = Content . stringToBuiltinByteString

toTitle :: String -> Title
toTitle = Title . stringToBuiltinByteString

-- SET NFT PRICE

-- Data to substitute original contracts parameter, accepts `Content` as plain text
data SetPriceData = SetPriceData
  { spd'content :: String
  , spd'price :: Maybe Integer
  } deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

nidFromContent :: Content -> NftId
nidFromContent = NftId . hashData

toSetPriceParams :: SetPriceData -> SetPriceParams
toSetPriceParams (SetPriceData content price) =
  SetPriceParams 
    (nidFromContent $ toContent content) 
    price

data SetPriceReq = SetPriceReq
  { spr'uniqueToken :: UniqueToken
  , spr'data :: SetPriceData
  } deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- PAB CONTRACTS
data NftDemoContracts
  = GenerateUniqueToken
  | MintListHead MintHeadReq
  | MintNft MintNftReq
  | SetNftPice SetPriceReq
  -- | BuyNFT -- TODO
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance HasDefinitions NftDemoContracts where
  getDefinitions = [GenerateUniqueToken]

  getSchema = \case
    GenerateUniqueToken -> endpointsToSchemas @Contract.Currency.CurrencySchema
    MintListHead _ -> endpointsToSchemas @EmptySchema
    MintNft _ -> endpointsToSchemas @EmptySchema
    SetNftPice _ -> endpointsToSchemas @EmptySchema

  getContract = \case
    GenerateUniqueToken 
      -> SomeBuiltin $ Contract.Currency.mintCurrency Contract.Init.uniqueTokenName
    MintListHead (MintHeadReq ut initPs) 
      -> SomeBuiltin $ Contract.Init.initApp ut initPs
    MintNft (MintNftReq ut mintData) 
      -> SomeBuiltin $ Contract.Mint.mint ut (toMintParams mintData)
    SetNftPice (SetPriceReq ut setPriceData)
      -> SomeBuiltin $ Contract.SetPrice.setPrice ut (toSetPriceParams setPriceData)



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
      let nftId = nidFromContent . mp'content $ mps
          setPricePs = SetPriceData "Content" (Just (-33))
      LBS.putStrLn ("Mint parameters: " <> JSON.encode mps)
      LBS.putStrLn ("Content hash: " <> JSON.encode nftId)
      LBS.putStrLn ("Set price: " <> JSON.encode setPricePs)