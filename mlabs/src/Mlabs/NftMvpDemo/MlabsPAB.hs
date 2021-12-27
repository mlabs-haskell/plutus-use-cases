module Mlabs.NftMvpDemo.MlabsPAB (
  runMlabsPab
) where

import System.Environment (getArgs)
import System.Exit (die)

import Cardano.Api (NetworkId (Testnet), NetworkMagic (..))
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as JSON
import Data.String (fromString)
import Data.Text qualified as T
import Data.ByteString.Lazy qualified as LazyByteString
import Control.Monad (when)
import GHC.Generics (Generic)

import Plutus.Contract (EmptySchema)

import MLabsPAB qualified
import MLabsPAB.Types (
  CLILocation (Local),
  LogLevel(Debug),
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
  MintParams(MintParams),
  NftId(NftId),
  SetPriceParams(SetPriceParams),
  BuyRequestUser(BuyRequestUser)
  )
import Mlabs.Plutus.Contracts.CurrencyMP qualified as Contract.Currency
import Mlabs.NftMvpDemo.Contract.Init qualified as Contract.Init
import Mlabs.NftMvpDemo.Contract.Mint qualified as Contract.Mint
import Mlabs.NftMvpDemo.Contract.SetPrice qualified as Contract.SetPrice
import Mlabs.NftMvpDemo.Contract.Buy qualified as Contract.Buy
import Mlabs.NFT.Contract.Aux (hashContent)

import Prelude
import PlutusTx.Prelude qualified as PP
import PlutusTx.Builtins.Class (stringToBuiltinByteString)
import Mlabs.NFT.Spooky (toSpooky)


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
toContent = Content . toSpooky . stringToBuiltinByteString

toTitle :: String -> Title
toTitle = Title . toSpooky . stringToBuiltinByteString

-- SET NFT PRICE
-- Data to substitute original contracts parameter, accepts `Content` as plain text
data SetPriceData = SetPriceData
  { spd'content :: String
  , spd'price :: Maybe Integer
  } deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

nidFromContent :: Content -> NftId
nidFromContent = NftId . toSpooky . hashContent

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

-- BUY NFT
data BuyNftData = BuyNftData
  { bnd'content :: String
  , bnd'price :: Integer
  , bnd'newPrice :: Maybe Integer
  } deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data BuyNftReq = BuyNftReq
  { bnr'uniqueToken :: UniqueToken
  , bnr'data :: BuyNftData
  } deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

toBuyParams :: BuyNftData -> BuyRequestUser
toBuyParams (BuyNftData content price newPrice) = 
  BuyRequestUser
    (nidFromContent $ toContent content)
    price
    newPrice

-- PAB CONTRACTS
data NftDemoContracts
  = GenerateUniqueToken
  | MintListHead MintHeadReq
  | MintNft MintNftReq
  | SetNftPice SetPriceReq
  | BuyNft BuyNftReq
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance HasDefinitions NftDemoContracts where
  getDefinitions = [GenerateUniqueToken]

  getSchema = \case
    GenerateUniqueToken -> endpointsToSchemas @Contract.Currency.CurrencySchema
    MintListHead _ -> endpointsToSchemas @EmptySchema
    MintNft _ -> endpointsToSchemas @EmptySchema
    SetNftPice _ -> endpointsToSchemas @EmptySchema
    BuyNft _ -> endpointsToSchemas @EmptySchema

  getContract = \case
    GenerateUniqueToken 
      -> SomeBuiltin $ Contract.Currency.mintCurrency Contract.Init.uniqueTokenName
    MintListHead (MintHeadReq ut initPs) 
      -> SomeBuiltin $ Contract.Init.initApp ut initPs
    MintNft (MintNftReq ut mintData) 
      -> SomeBuiltin $ Contract.Mint.mint ut (toMintParams mintData)
    SetNftPice (SetPriceReq ut setPriceData)
      -> SomeBuiltin $ Contract.SetPrice.setPrice ut (toSetPriceParams setPriceData)
    BuyNft (BuyNftReq ut buyData)
      -> SomeBuiltin $ Contract.Buy.buy ut (toBuyParams buyData)


runMlabsPab :: IO ()
runMlabsPab = do
  args <- getArgs
  when (length args /= 1) $ die "Exactly one parameter representing PubKeyHash required" 
  let paramsFile =  "./nft-marketplace-mvp/pparams.json"
      pkh = fromString $ head args
  protocolParams :: Maybe ProtocolParameters <- JSON.decode <$> LazyByteString.readFile paramsFile
  let pabConf = PABConfig
        { pcCliLocation = Local
        , pcNetwork = Testnet (NetworkMagic 1097911063)
        , pcProtocolParams = protocolParams
        , pcScriptFileDir  = "nft-marketplace-mvp/scripts_and_data"
        , pcSigningKeyFileDir = "nft-marketplace-mvp/signing_keys"
        , pcTxFileDir = "nft-marketplace-mvp/transactions"
        , pcProtocolParamsFile = T.pack paramsFile
        , pcDryRun = False
        , pcLogLevel = Debug
        , pcOwnPubKeyHash= pkh
        -- , pcOwnPubKeyHash= "bcd6bceeb0d22a7ca6ba1cd00669f7eb60ca8938d853666d30d56a56" -- Used as admin
        -- , pcOwnPubKeyHash= "25bd24abedaf5c68d898484d757f715c7b4413ad91a80d3cb0b3660d" -- Used as user
        }
  MLabsPAB.runPAB @NftDemoContracts pabConf