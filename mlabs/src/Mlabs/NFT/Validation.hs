{-# LANGUAGE UndecidableInstances #-}

module Mlabs.NFT.Validation where

import Control.Monad (forever, void)

import Data.Aeson (FromJSON, ToJSON, Value (Bool))
import Data.Map qualified as Map
import Data.Semigroup (Last (..), Semigroup, sconcat, (<>))
import Data.Text (Text)
import Data.Void (Void)

import GHC.Generics (Generic)

import Ledger qualified (Address, ScriptContext, Validator, ValidatorHash, validatorHash)
import Ledger.Address (pubKeyAddress)
import Ledger.Constraints qualified as Constraints
import Ledger.Contexts (scriptCurrencySymbol)
import Ledger.Crypto (pubKeyHash)
import Ledger.Crypto qualified as Crypto
import Ledger.Scripts qualified as Scripts
import Ledger.Tx qualified as Ledger
import Ledger.Typed.Scripts as TScripts
import Ledger.Value qualified as Value

import Mlabs.Plutus.Contract (Call, IsEndpoint (..), getEndpoint, selectForever)
import Mlabs.Utils.Wallet (walletFromNumber)

import Playground.Contract (ToSchema, TokenName (TokenName), TxOutRef, mkSchemaDefinitions)
import Plutus.ChainIndex.Tx qualified as ChainIx
import Plutus.Contract (Contract, Endpoint, endpoint, type (.\/))
import Plutus.Contract qualified as Contract

import Plutus.Trace.Emulator (EmulatorTrace, activateContractWallet, callEndpoint, runEmulatorTraceIO)
import Plutus.Trace.Emulator qualified as Trace

import Plutus.V1.Ledger.Address (Address)
import Plutus.V1.Ledger.Contexts (ScriptContext (..), TxInInfo (..), TxInfo (..))
import Plutus.V1.Ledger.Contexts qualified as Contexts
import Plutus.V1.Ledger.Crypto (PubKeyHash (..))
import Plutus.V1.Ledger.Scripts qualified as Scripts
import Plutus.V1.Ledger.Tx (Tx (..), TxOutRef (..))
import Plutus.V1.Ledger.Value (CurrencySymbol, TokenName (..), assetClass, assetClassValue, flattenValue, tokenName)

import PlutusTx qualified
import PlutusTx.Prelude (
  Applicative (pure),
  Bool (..),
  BuiltinByteString,
  Eq ((==)),
  Functor (fmap),
  Integer,
  Maybe (..),
  Rational,
  any,
  fst,
  maybe,
  traceIfFalse,
  ($),
  (%),
  (&&),
  (.),
  (<$>),
  (=<<),
  (>>),
 )

import Text.Printf (printf)

import Wallet.Emulator qualified as Emulator
import Wallet.Emulator.Wallet qualified as Wallet

import Prelude qualified as Hask

-- import PlutusTx.Builtins.Internal (BuiltinByteString(..))

newtype Content = Content {getContent :: BuiltinByteString}
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''Content
PlutusTx.makeLift ''Content

newtype Title = Title {getTitle :: BuiltinByteString}
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''Title
PlutusTx.makeLift ''Title

newtype UserId = UserId {getUserId :: PubKeyHash}
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''UserId
PlutusTx.makeLift ''UserId

{- | Unique identifier of NFT. we are not using utxo ref anymore to not to
 produce new address every time and guarantee that all utxos for
 content+author combination are at the same script and we can guarantee that
 author will mint some extra NFT for same content-
-}
data NftId = NftId
  { -- | Content Title.
    nftId'title :: Title
  , -- | token name is identified by content of the NFT (it's hash of it)
    nftId'token :: TokenName
  , -- | TxOutRef which was used to mint current NFT
    nftId'outRef :: TxOutRef
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

PlutusTx.unstableMakeIsData ''NftId
PlutusTx.makeLift ''NftId

-- | Data for NFTs
data NftContent = NftContent
  { -- | Content Title.
    ct'title :: Title
  , -- | data (NftContent, audio, photo, etc)
    ct'data :: Content
  , -- | author
    ct'author :: UserId
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

PlutusTx.unstableMakeIsData ''NftContent
PlutusTx.makeLift ''NftContent

-- | NFT Datum
data DatumNft = DatumNft
  { -- | NFT ID
    dNft'id :: NftId
  , -- | Share
    dNft'share :: Rational
  , -- | Author receives the shares of the price
    dNft'author :: UserId
  , -- | current owner
    dNft'owner :: UserId
  , -- | Price in ada, if it's nothing then nobody can buy
    dNft'price :: Maybe Integer
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

PlutusTx.unstableMakeIsData ''DatumNft
PlutusTx.makeLift ''DatumNft

data UserAct
  = -- | Buy NFT and set new price
    BuyAct
      { -- | price to buy
        act'price :: Integer
      , -- | new price for NFT (Nothing locks NFT)
        act'newPrice :: Maybe Integer
      }
  | -- | Set new price for NFT
    SetPriceAct
      { -- | new price for NFT (Nothing locks NFT)
        act'newPrice :: Maybe Integer
      }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON)
PlutusTx.makeLift ''UserAct
PlutusTx.unstableMakeIsData ''UserAct

{-# INLINEABLE mkMintPolicy #-}

-- | Minting policy for NFTs.
mkMintPolicy :: Address -> TxOutRef -> NftId -> () -> ScriptContext -> Bool
mkMintPolicy stateAddr oref (NftId title token author) _ ctx =
  -- ? maybe author could be checked also, their key should be in signatures.
  traceIfFalse "UTXO not consumed" hasUtxo
    && traceIfFalse "wrong amount minted" checkMintedAmount
    && traceIfFalse "Does not pay to state" paysToState
  where
    info = Contexts.scriptContextTxInfo ctx

    hasUtxo = any (\inp -> Contexts.txInInfoOutRef inp == oref) $ Contexts.txInfoInputs info

    checkMintedAmount = case Value.flattenValue (Contexts.txInfoMint info) of
      [(cur, tn, val)] ->
        Contexts.ownCurrencySymbol ctx == cur
          && token == tn
          && val == 1
      _ -> False

    paysToState = any hasNftToken $ Contexts.txInfoOutputs info

    hasNftToken Contexts.TxOut {..} =
      txOutAddress == stateAddr
        && txOutValue == Value.singleton (Contexts.ownCurrencySymbol ctx) token 1

mintPolicy :: Address -> TxOutRef -> NftId -> TScripts.MintingPolicy
mintPolicy stateAddr oref nid =
  Scripts.mkMintingPolicyScript $
    $$(PlutusTx.compile [||\x y z -> TScripts.wrapMintingPolicy (mkMintPolicy x y z)||])
      `PlutusTx.applyCode` PlutusTx.liftCode stateAddr
      `PlutusTx.applyCode` PlutusTx.liftCode oref
      `PlutusTx.applyCode` PlutusTx.liftCode nid

{-# INLINEABLE mKTxPolicy #-}

-- | A validator script for the user actions.
mKTxPolicy :: DatumNft -> UserAct -> ScriptContext -> Bool
mKTxPolicy dNft act ctx =
  -- ? maybe, some check that datum corresponds to NftId could be added
  traceIfFalse "Datum does not correspond to NFTId." True
    && traceIfFalse "Incorrect Datum attached to Tx." True
    && traceIfFalse "NFT doesn't exist at the address." True
    && case act of
      BuyAct {..} ->
        traceIfFalse "Not enough funds." True -- todo
          && traceIfFalse "User does not own NFT." True -- todo
      SetPriceAct {..} ->
        traceIfFalse "Price cannot be negative." True -- todo
          && traceIfFalse "User does not own NFT." True -- todo

data NftTrade
instance TScripts.ValidatorTypes NftTrade where
  type DatumType NftTrade = DatumNft
  type RedeemerType NftTrade = UserAct

{-# INLINEABLE txPolicy #-}
txPolicy :: NftId -> TScripts.TypedValidator NftTrade
txPolicy nftId =
  TScripts.mkTypedValidator @NftTrade
    $$(PlutusTx.compile [||mKTxPolicy||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = TScripts.wrapValidator @DatumNft @UserAct

{-# INLINEABLE txValHash #-}
txValHash :: NftId -> Ledger.ValidatorHash
txValHash = TScripts.validatorHash . txPolicy

{-# INLINEABLE txScrAddress #-}
txScrAddress :: NftId -> Ledger.Address
txScrAddress = TScripts.validatorAddress . txPolicy

type NFTAppSchema =
  NFTUserSchema
    .\/ NFTSAuthorSchema

type NFTUserSchema =
  Endpoint "buy" NftId

type NFTSAuthorSchema =
  Endpoint "mint" (Content, Title)

mkSchemaDefinitions ''NFTAppSchema
mkSchemaDefinitions ''NFTUserSchema
mkSchemaDefinitions ''NFTSAuthorSchema

{-# INLINEABLE curSymbol #-}

-- | Calculate the currency symbol of the NFT.
curSymbol :: Address -> TxOutRef -> NftId -> CurrencySymbol
curSymbol stateAddr oref nid = scriptCurrencySymbol $ mintPolicy stateAddr oref nid

-- | Mints an NFT and sends it to the App Address.
mint :: (Content, Title) -> Contract w NFTSAuthorSchema Text ()
mint nftContent = do
  addr <- pubKeyAddress <$> Contract.ownPubKey
  nft' <- nftInit nftContent
  utxos <- Contract.utxosAt addr
  case nft' of
    Nothing -> Contract.logError @Hask.String "Cannot create NFT."
    Just nft -> maybe err (continue utxos nft) =<< fstUtxo addr
  where
    err = Contract.logError @Hask.String "no utxo found at address."

    continue utxos nft oref = do
      let nftId = nft.dNft'id
          scrAddress = txScrAddress nftId
          nftPolicy = mintPolicy scrAddress oref nftId
          val = Value.singleton (scriptCurrencySymbol nftPolicy) nftId.nftId'token 1
          (lookups, tx) =
            ( Constraints.unspentOutputs utxos
                <> Constraints.mintingPolicy nftPolicy
                <> Constraints.typedValidatorLookups (txPolicy nftId)
            , Constraints.mustMintValue val
                <> Constraints.mustSpendPubKeyOutput oref
                <> Constraints.mustPayToTheScript nft val
            )
      void $ Contract.submitTxConstraintsWith @NftTrade lookups tx
      Contract.logInfo @Hask.String $ printf "forged %s" (Hask.show val)

endpoints :: Contract w NFTSAuthorSchema Text ()
endpoints = forever $ do
  Contract.awaitPromise $
    Hask.foldr1
      Contract.select
      [ endpoint @"mint" mint
      ]

-- | Get the user's ChainIndexTxOut
getUserUtxos :: Address -> Contract w NFTSAuthorSchema Text [Ledger.ChainIndexTxOut]
getUserUtxos adr = fmap fst . Map.elems <$> Contract.utxosTxOutTxAt adr

-- | Get first utxo at address.
fstUtxo :: Address -> Contract w s Text (Maybe TxOutRef)
fstUtxo address = do
  utxos <- Contract.utxosAt address
  case Map.keys utxos of
    [] -> pure Nothing
    x : _ -> pure $ Just x

-- | Initialise an NFT using the current wallet.
nftInit :: (Content, Title) -> Contract w s Text (Maybe DatumNft)
nftInit (nftContent, title) = do
  pk <- Contract.ownPubKey
  let pkh = pubKeyHash pk
      userAddress = pubKeyAddress pk
      user = UserId pkh
  oref' <- fstUtxo userAddress
  case oref' of
    Nothing ->
      Contract.logError @Hask.String "no utxo found" >> pure Nothing
    Just oref ->
      let hData = hashData nftContent
       in pure . Just $
            DatumNft
              { dNft'id =
                  NftId
                    { nftId'title = title
                    , nftId'token = TokenName hData
                    , nftId'outRef = oref
                    }
              , dNft'share = 1 % 10
              , dNft'author = user
              , dNft'owner = user
              , dNft'price = Just 10
              }

-- | todo: some hashing function here - at the moment getting the whole bytestring
hashData :: Content -> BuiltinByteString
hashData (Content b) = b

-- | Generic application Trace Handle.
type AppTraceHandle = Trace.ContractHandle () NFTSAuthorSchema Text

-- | Emulator Trace 1. Mints one NFT.
eTrace1 :: EmulatorTrace ()
eTrace1 = do
  let wallet1 = walletFromNumber 1 :: Emulator.Wallet
      wallet2 = walletFromNumber 2 :: Emulator.Wallet
  h1 :: AppTraceHandle <- activateContractWallet wallet1 endpoints
  h2 :: AppTraceHandle <- activateContractWallet wallet2 endpoints
  callEndpoint @"mint" h1 artwork
  callEndpoint @"mint" h1 artwork
  callEndpoint @"mint" h2 artwork
  void $ Trace.waitNSlots 1
  callEndpoint @"mint" h1 artwork
  callEndpoint @"mint" h2 artwork
  void $ Trace.waitNSlots 1
  where
    artwork = (Content "A painting.", Title "Fiona Lisa")

eTrace2 :: EmulatorTrace ()
eTrace2 = do
  let wallet1 = walletFromNumber 1 :: Emulator.Wallet
      wallet2 = walletFromNumber 2 :: Emulator.Wallet
      scrAddr = txScrAddress
  void $ Trace.waitNSlots 1

-- | Test for prototyping.
test :: Hask.IO ()
test = runEmulatorTraceIO eTrace1

test2 :: Hask.IO ()
test2 = runEmulatorTraceIO $ do
  eTrace1
  eTrace2
