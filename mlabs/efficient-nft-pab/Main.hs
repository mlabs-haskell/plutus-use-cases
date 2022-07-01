{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Prelude qualified as Hask hiding (toEnum)
import PlutusTx.Prelude

import Data.Default (def)
import Data.String (fromString)
import Mlabs.EfficientNFT.Types (MintParams (..), SetPriceParams, NFTAppSchema, UserContract, MintCnftParams (..), NftCollection (..), NftData (..), nftData'nftCollection, NftId (..))
import Data.Monoid (Last (Last))
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import BotPlutusInterface.Types
import Mlabs.EfficientNFT.Contract.Mint (mintWithCollection)
import Mlabs.EfficientNFT.Contract.SetPrice (setPrice)
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Aeson as JSON
import Cardano.Api (NetworkId(Testnet), NetworkMagic (NetworkMagic))
import Data.Maybe (fromJust)
import BotPlutusInterface (runPAB)
import Servant.Client.Core (BaseUrl (BaseUrl), Scheme (Http))
import Plutus.Contract qualified as Contract
import Ledger.Constraints qualified as Constraints
import Data.Void (Void)
import Control.Monad (void)
import Ledger (scriptHash, mintingPolicyHash, PaymentPubKeyHash (PaymentPubKeyHash), StakePubKeyHash (StakePubKeyHash), MintingPolicy, TxOutRef, ScriptContext, TxInfo, pubKeyHashAddress, scriptContextTxInfo, txInInfoOutRef, txInfoInputs, mkMintingPolicyScript, scriptCurrencySymbol, minAdaTxOut)
import Ledger.Value (AssetClass, TokenName (TokenName), assetClass, singleton)
import Ledger.Typed.Scripts (wrapMintingPolicy, validatorHash)
import Plutus.V1.Ledger.Ada (lovelaceValueOf, toValue)
import Data.ByteString.Lazy.Char8 qualified as ByteString
import Cardano.Api (AsType (..), deserialiseAddress)
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Text (Text)
import qualified PlutusTx
import Ledger.Constraints.Metadata (NftMetadata (..), TxMetadata (..), NftMetadataToken (..))
import Mlabs.EfficientNFT.Dao (daoValidator)
import Mlabs.EfficientNFT.Lock (lockValidator)
import Mlabs.EfficientNFT.Contract.MarketplaceDeposit (marketplaceDeposit)
import Mlabs.EfficientNFT.Marketplace (marketplaceValidator)
import Mlabs.EfficientNFT.Token qualified as Token
import Plutus.V1.Ledger.Api (ToData(toBuiltinData), Datum (Datum))
import Options.Applicative (
  Parser,
  execParser,
  fullDesc,
  header,
  help,
  helper,
  info,
  long,
  strOption,
  value,
  (<**>),
 )

{-# INLINEABLE mkPolicy #-}
mkPolicy :: TxOutRef -> BuiltinData -> ScriptContext -> Bool
mkPolicy oref _ ctx =
  traceIfFalse "UTxO not consumed" hasUTxO
  where
    info_ :: TxInfo
    info_ = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info_

policy :: TxOutRef -> MintingPolicy
policy oref =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||\oref' -> wrapMintingPolicy $ mkPolicy oref' ||])
      `PlutusTx.applyCode` PlutusTx.liftCode oref

generateNft :: [MintCnftParams] -> Contract.Contract (Last Text) NFTAppSchema Text ()
generateNft tokens = do
  pkh <- Contract.ownPaymentPubKeyHash
  utxos <- Contract.utxosAt (pubKeyHashAddress pkh Nothing)
  case Map.keys utxos of
    [] -> Contract.logError @Hask.String "no utxo found"
    oref : _ -> do
      Contract.tell $ Last $ Just $ "Using oref:" Hask.<> Text.pack (Hask.show oref)
      let cs = scriptCurrencySymbol $ policy oref
          val = Hask.mconcat $ Hask.fmap ((\tn -> singleton cs (TokenName tn) 1) . mc'tokenName) tokens
          meta =
            NftMetadata $
              Map.singleton cs $
                Map.fromList $
                Hask.fmap (\MintCnftParams{..} -> (TokenName mc'tokenName,
                  NftMetadataToken
                    { nmtName = mc'name
                    , nmtImage = mc'image
                    , nmtMediaType = Hask.pure "image/png"
                    , nmtDescription = Just mc'description
                    , nmtFiles = Hask.mempty
                    , nmtOtherFields = Hask.mempty
                    })) tokens
          lookups =
            Hask.mconcat
              [ Constraints.mintingPolicy (policy oref)
              , Constraints.unspentOutputs utxos
              ]
          tx =
            Hask.mconcat
              [ Constraints.mustMintValue val
              , Constraints.mustSpendPubKeyOutput oref
              , Constraints.mustPayToPubKey pkh (val <> toValue minAdaTxOut)
              , Constraints.mustIncludeMetadata $ TxMetadata (Just meta) Hask.mempty
              , Constraints.mustPayWithDatumToPubKey pkh (Datum $ toBuiltinData ()) (lovelaceValueOf 5_000_000)
              ]
      void $ Contract.submitTxConstraintsWith @Void lookups tx
      Contract.tell $ Last $ Just "Finished"


data NftContracts
  = MkCollateral
  | MintCnft [MintCnftParams]
  | Mint (AssetClass, MintParams)
  | ChangePrice SetPriceParams
  | Deposit NftData
  deriving stock (Hask.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance HasDefinitions NftContracts where
  getDefinitions = []
  getSchema = const $ endpointsToSchemas @NFTAppSchema
  getContract = \case
    MkCollateral -> SomeBuiltin mkCollateral
    MintCnft params -> SomeBuiltin . generateNft $ params
    Mint params -> SomeBuiltin $ mintWithCollection params
    ChangePrice params -> SomeBuiltin . setPrice $ params
    Deposit params -> SomeBuiltin . marketplaceDeposit $ params

mkCollateral :: UserContract ()
mkCollateral = do
  pkh <- Contract.ownPaymentPubKeyHash
  void
    $ Contract.submitTxConstraintsWith @Void Hask.mempty
    $ Constraints.mustPayToPubKey pkh (lovelaceValueOf 5_000_000)

data CliOptions = CliOptions
  { phk :: Hask.String
  , authPhk :: Hask.String
  , currencySymbol :: Hask.String
  , tokenName :: Hask.String
  }

cliOptionsParser :: Parser CliOptions
cliOptionsParser =
  CliOptions
    Hask.<$> strOption (long "pkh" Hask.<> value "" Hask.<> help "own pub key hash")
    Hask.<*> strOption (long "auth-pkh" Hask.<> value "" Hask.<> help "author pub key hash")
    Hask.<*> strOption (long "currency" Hask.<> value "" Hask.<> help "currency symbol")
    Hask.<*> strOption (long "token" Hask.<> value "" Hask.<> help "tokenName as a string")

getCliOptions :: Hask.IO CliOptions
getCliOptions = execParser (info (cliOptionsParser <**> helper) (fullDesc Hask.<> header "Efficient NFT PAB"))

main :: Hask.IO ()
main = do
  -- Hask.print $ deserialiseAddress (AsAddress AsShelleyAddr) "addr_test"
  
  -- let foo1 = MintCnft
  --       [ MintCnftParams
  --           { mc'image = "ipfs://INSERT_HASH_HERE"
  --           , mc'tokenName = "cat-123"
  --           , mc'name = "Cat number 123"
  --           , mc'description = "Cat eating piece of cheese"
  --           }
  --       ]
  -- ByteString.putStrLn
  --   $ JSON.encode foo1

  CliOptions{phk, authPhk, currencySymbol, tokenName} <- getCliOptions
  let uCs = fromString currencySymbol -- CURRENCY_SYMBOL
      auth = PaymentPubKeyHash $ fromString authPhk -- YOUR_PKH
      mp = MintParams
             { mp'authorShare = toEnum 1000
             , mp'daoShare = toEnum 500
             , mp'price = toEnum 100_000_000
             , mp'lockLockup = 5
             , mp'lockLockupEnd = 5
             , mp'owner = Just (auth, Nothing)
             , mp'fakeAuthor = Just auth
             , mp'feeVaultKeys = []
             }

  Hask.putStrLn $ "currencySymbol: " <> Hask.show uCs

  Hask.putStr "seabug-mint-request: "
  ByteString.putStrLn
    $ JSON.encode
    $ Mint (assetClass uCs $ fromString tokenName, mp)

  let c = NftCollection
        { nftCollection'collectionNftCs = uCs
        , nftCollection'lockingScript = validatorHash $ lockValidator uCs 1 1
        , nftCollection'author = auth
        , nftCollection'authorShare = toEnum 1000
        , nftCollection'daoScript = validatorHash $ daoValidator []
        , nftCollection'daoShare = toEnum 500
        -- unused:
        , nftCollection'lockLockup = 1
        , nftCollection'lockLockupEnd = 1
        }

  Hask.putStr "unapplied-minting-policy: "
  ByteString.putStrLn $ JSON.encode Token.policyDataScript

  Hask.putStr "unapplied-minting-policy hash: "
  Hask.print $ scriptHash Token.unappliedPolicyScript

  Hask.putStr "NftCollection: "
  Hask.putStrLn $ Hask.show c

  Hask.putStr "minting-policy: "
  ByteString.putStrLn $ JSON.encode $ Token.policy c

  Hask.putStr "unapplied-minting-policy hash: "
  Hask.print $ mintingPolicyHash $ Token.policy c

  protocolParams <- fromJust . JSON.decode Hask.<$> LazyByteString.readFile "data/testnet-protocol-params.json"
  let pabConf =
        PABConfig
          { pcCliLocation = Local
          , pcNetwork = Testnet (NetworkMagic 1097911063)
          , pcProtocolParams = protocolParams
          , pcScriptFileDir = "pab/result-scripts"
          , pcTxFileDir = "pab/txs"
          , pcSigningKeyFileDir = "pab/signing-keys"
          , pcProtocolParamsFile  = "data/testnet-protocol-params.json"
          , pcChainIndexUrl = BaseUrl Http "localhost" 9083 ""
          , pcDryRun = False
          , pcPort = 3003
          , pcLogLevel = Debug
          , pcOwnPubKeyHash = fromString phk -- OWN_PUB_KEY
          , pcSlotConfig = def
          , pcEnableTxEndpoint = True
          , pcTipPollingInterval = 1_000_000
          , pcMetadataDir = "pab/metadata"
          , pcForceBudget = pure (10000000000, 16000000)
          }
  runPAB @NftContracts pabConf
