{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import PlutusTx.Prelude
import Prelude qualified as Hask hiding (toEnum)

import BotPlutusInterface (runPAB)
import BotPlutusInterface.Types
import Cardano.Api (NetworkId (Testnet), NetworkMagic (NetworkMagic))
import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as JSON
import Data.ByteString.Lazy qualified as LazyByteString
import Data.ByteString.Lazy.Char8 qualified as ByteString
import Data.Default (def)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Monoid (Last (Last))
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import GHC.Generics (Generic)
import Ledger (MintingPolicy, PaymentPubKeyHash (PaymentPubKeyHash), ScriptContext, TxInfo, TxOutRef, minAdaTxOut, mkMintingPolicyScript, pubKeyHashAddress, scriptContextTxInfo, scriptCurrencySymbol, txInInfoOutRef, txInfoInputs)
import Ledger.Constraints qualified as Constraints
import Ledger.Constraints.Metadata (NftMetadata (..), NftMetadataToken (..), TxMetadata (..))
import Ledger.Typed.Scripts (wrapMintingPolicy)
import Ledger.Value (AssetClass, TokenName (TokenName), assetClass, singleton)
import Mlabs.EfficientNFT.Contract.MarketplaceDeposit (marketplaceDeposit)
import Mlabs.EfficientNFT.Contract.Mint (mintWithCollection)
import Mlabs.EfficientNFT.Contract.SetPrice (setPrice)
import Mlabs.EfficientNFT.Token qualified as Token
import Mlabs.EfficientNFT.Types (MintCnftParams (..), MintParams (..), NFTAppSchema, NftData, SetPriceParams, UserContract)
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
import Plutus.Contract qualified as Contract
import Plutus.V1.Ledger.Ada (lovelaceValueOf, toValue)
import Plutus.V1.Ledger.Api (Datum (Datum), ToData (toBuiltinData))
import PlutusTx qualified
import Servant.Client.Core (BaseUrl (BaseUrl), Scheme (Http))

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
    $$(PlutusTx.compile [||wrapMintingPolicy . mkPolicy||])
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
                  Hask.fmap
                    ( \MintCnftParams {..} ->
                        ( TokenName mc'tokenName
                        , NftMetadataToken
                            { nmtName = mc'name
                            , nmtImage = mc'image
                            , nmtMediaType = Hask.pure "image/png"
                            , nmtDescription = Just mc'description
                            , nmtFiles = Hask.mempty
                            , nmtOtherFields = Hask.mempty
                            }
                        )
                    )
                    tokens
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
  void $
    Contract.submitTxConstraintsWith @Void Hask.mempty $
      Constraints.mustPayToPubKey pkh (lovelaceValueOf 5_000_000)

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
  CliOptions {phk, authPhk, currencySymbol, tokenName} <- getCliOptions
  let uCs = fromString currencySymbol -- CURRENCY_SYMBOL
      auth = PaymentPubKeyHash $ fromString authPhk -- YOUR_PKH
      mp =
        MintParams
          { mp'authorShare = toEnum 1000
          , mp'daoShare = toEnum 500
          , mp'price = toEnum 100_000_000
          , mp'lockLockup = 5
          , mp'lockLockupEnd = 5
          , mp'owner = Just (auth, Nothing)
          , mp'fakeAuthor = Just auth
          , mp'feeVaultKeys = []
          }

  Hask.putStr "seabug-mint-request: "
  ByteString.putStrLn $
    JSON.encode $
      Mint (assetClass uCs $ fromString tokenName, mp)

  Hask.putStr "unapplied-minting-policy: "
  ByteString.putStrLn $ JSON.encode Token.policyDataScript

  protocolParams <- fromJust . JSON.decode Hask.<$> LazyByteString.readFile "data/testnet-protocol-params.json"
  let pabConf =
        PABConfig
          { pcCliLocation = Local
          , pcNetwork = Testnet (NetworkMagic 1097911063)
          , pcProtocolParams = protocolParams
          , pcScriptFileDir = "pab/result-scripts"
          , pcTxFileDir = "pab/txs"
          , pcSigningKeyFileDir = "pab/signing-keys"
          , pcProtocolParamsFile = "data/testnet-protocol-params.json"
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
