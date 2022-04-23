module Main (main) where

import Prelude qualified as Hask hiding (toEnum)
import PlutusTx.Prelude

import Data.Default (def)
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
import Ledger (PaymentPubKeyHash (PaymentPubKeyHash), StakePubKeyHash (StakePubKeyHash), MintingPolicy, TxOutRef, ScriptContext, TxInfo, pubKeyHashAddress, scriptContextTxInfo, txInInfoOutRef, txInfoInputs, mkMintingPolicyScript, scriptCurrencySymbol, minAdaTxOut)
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

{-# INLINEABLE mkPolicy #-}
mkPolicy :: TxOutRef -> BuiltinData -> ScriptContext -> Bool
mkPolicy oref _ ctx =
  traceIfFalse "UTxO not consumed" hasUTxO
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

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

  let uCs = "" -- CURRENCY_SYMBOL
      auth = PaymentPubKeyHash "" -- YOUR_PKH
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
  Hask.putStr "seabug-mint-request: "
  ByteString.putStrLn
    $ JSON.encode
    $ Mint (assetClass uCs "cat-123", mp)

  let c = NftCollection
        { nftCollection'collectionNftCs = uCs
        , nftCollection'lockLockup = 1
        , nftCollection'lockLockupEnd = 1
        , nftCollection'lockingScript = validatorHash $ lockValidator uCs 1 1
        , nftCollection'author = auth
        , nftCollection'authorShare = toEnum 1000
        , nftCollection'daoScript = validatorHash $ daoValidator []
        , nftCollection'daoShare = toEnum 500
        }

  Hask.putStr "minting-policy: "
  ByteString.putStrLn
    $ JSON.encode      
    $ Token.policy c

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
          , pcOwnPubKeyHash = "3f3464650beb5324d0e463ebe81fbe1fd519b6438521e96d0d35bd75" -- OWN_PUB_KEY
          , pcSlotConfig = def
          , pcEnableTxEndpoint = True
          , pcTipPollingInterval = 1_000_000
          , pcMetadataDir = "pab/metadata"
          , pcForceBudget = pure (10000000000, 16000000)
          }
  runPAB @NftContracts pabConf
