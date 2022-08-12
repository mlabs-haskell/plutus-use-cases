{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import PlutusTx.Prelude
import Prelude qualified as Hask hiding (toEnum)

import BotPlutusInterface (runPAB)
import BotPlutusInterface.Types
import Cardano.Api (NetworkId (Testnet), NetworkMagic (NetworkMagic))
import Codec.Serialise
import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as JSON
import Data.ByteString.Base16.Lazy qualified as Base16
import Data.ByteString.Lazy qualified as LazyByteString
import Data.ByteString.Lazy.Char8 qualified as ByteString
import Data.Default (def)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Monoid (Last (Last))
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy.Encoding qualified as Text
import Data.Void (Void)
import GHC.Generics (Generic)
import Ledger (MintingPolicy, Script, ScriptContext, TxInfo, TxOutRef, fromCompiledCode, minAdaTxOut, mkMintingPolicyScript, pubKeyHashAddress, scriptContextTxInfo, scriptCurrencySymbol, txInInfoOutRef, txInfoInputs)
import Ledger.Constraints qualified as Constraints
import Ledger.Constraints.Metadata (NftMetadata (..), NftMetadataToken (..), TxMetadata (..))
import Ledger.Typed.Scripts (wrapMintingPolicy)
import Ledger.Value (AssetClass, TokenName (TokenName), singleton)
import Mlabs.EfficientNFT.Contract.MarketplaceDeposit (marketplaceDeposit)
import Mlabs.EfficientNFT.Contract.Mint (mintWithCollection)
import Mlabs.EfficientNFT.Contract.SetPrice (setPrice)
import Mlabs.EfficientNFT.Lock qualified as Lock
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
import Plutus.V1.Ledger.Api (Datum (Datum), ToData (toBuiltinData), unsafeFromBuiltinData)
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

{-# INLINEABLE mkPolicyUntyped #-}
mkPolicyUntyped :: BuiltinData -> BuiltinData -> ScriptContext -> Bool
mkPolicyUntyped oref = mkPolicy (unsafeFromBuiltinData oref)

policyUntyped :: TxOutRef -> MintingPolicy
policyUntyped oref =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||(wrapMintingPolicy . mkPolicyUntyped)||])
      `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData oref)

policyUntypedScript :: Script
policyUntypedScript =
  fromCompiledCode
    $$(PlutusTx.compile [||(wrapMintingPolicy . mkPolicyUntyped)||])

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

newtype CliOptions = CliOptions {phk :: Hask.String} -- Replace with data when more params added

cliOptionsParser :: Parser CliOptions
cliOptionsParser =
  CliOptions
    Hask.<$> strOption (long "pkh" Hask.<> value "" Hask.<> help "own pub key hash")

getCliOptions :: Hask.IO CliOptions
getCliOptions = execParser (info (cliOptionsParser <**> helper) (fullDesc Hask.<> header "Efficient NFT PAB"))

logScript :: Script -> Hask.IO ()
logScript = Hask.print . Text.decodeUtf8 . Base16.encode . serialise

main :: Hask.IO ()
main = do
  CliOptions {phk} <- getCliOptions

  Hask.putStr "Unapplied CNFT minting policy: "
  logScript policyUntypedScript

  Hask.putStr "Unapplied sgNFT minting policy: "
  logScript Token.policyDataScript

  Hask.putStr "Unapplied locking validator: "
  logScript Lock.lockScriptUntyped

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
