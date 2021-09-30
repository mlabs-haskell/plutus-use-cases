{-# LANGUAGE UndecidableInstances #-}

module Mlabs.NFT.Validation where

import qualified Control.Monad.Freer.Extras.Log as Extras
import Control.Monad (forever, void)
import Control.Lens ((^..), _Right, traversed, to , _Just, filtered )

import Data.Aeson (FromJSON, ToJSON, Value (Bool))
import Data.Map qualified as Map
import Data.Semigroup (Semigroup, sconcat)
import Data.Monoid (Last (..), mconcat, (<>))
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

import Mlabs.Plutus.Contract (readChainIndexTxDatum, readDatum, readDatum', selectForever)

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
  return,
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
  deriving Eq
PlutusTx.unstableMakeIsData ''Title
PlutusTx.makeLift ''Title

newtype UserId = UserId {getUserId :: PubKeyHash}
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
  deriving Eq
PlutusTx.unstableMakeIsData ''UserId
PlutusTx.makeLift ''UserId

{- | Unique identifier of NFT.
 The NftId contains a human readable title, the hashed information of the
 content and the utxo ref included when minting the token.
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

{- | Type representing the data that gets hashed when the token is minted. The
 tile and author are included for proof of authenticity in the case the same
 artwork is hashed more than once.
-}
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
  deriving Eq

PlutusTx.unstableMakeIsData ''NftContent
PlutusTx.makeLift ''NftContent

-- | NFT Datum is checked communicates the ownership of the NFT.
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
  deriving Eq

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
  deriving anyclass (ToJSON, FromJSON )
  deriving Eq
PlutusTx.makeLift ''UserAct
PlutusTx.unstableMakeIsData ''UserAct

-- | Parameters that need to be submitted when minting a new NFT.
data MintParams = MintParams
  { -- | Content to be minted.
    mp'content :: Content
  , -- | Title of content.
    mp'title :: Title
  , -- | Shares retained by author.
    mp'share :: Rational
  , -- | Listing price of the NFT.
    mp'price :: Maybe Integer
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
  deriving Eq
PlutusTx.makeLift ''MintParams
PlutusTx.unstableMakeIsData ''MintParams

data BuyRequestUser = BuyRequestUser 
      { -- | nftId to Buy
       ur'nftId :: NftId
      , -- | price to buy
        ur'price :: Integer
      , -- | new price for NFT (Nothing locks NFT)
        ur'newPrice :: Maybe Integer
      }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
  deriving Eq
PlutusTx.makeLift ''BuyRequestUser 
PlutusTx.unstableMakeIsData ''BuyRequestUser 

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

    hasUtxo =
      any (\inp -> Contexts.txInInfoOutRef inp == oref) $
        Contexts.txInfoInputs info

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
  Endpoint "buy" BuyRequestUser
  .\/ Endpoint "mint" MintParams

mkSchemaDefinitions ''NFTAppSchema

{-# INLINEABLE curSymbol #-}

-- | Calculate the currency symbol of the NFT.
curSymbol :: Address -> TxOutRef -> NftId -> CurrencySymbol
curSymbol stateAddr oref nid = scriptCurrencySymbol $ mintPolicy stateAddr oref nid

getNftDatum :: NftId -> Contract w s Text (Maybe DatumNft)
getNftDatum nftId = do
  utxos :: [Ledger.ChainIndexTxOut] <- getAddrUtxos $ txScrAddress nftId
  let datums :: [DatumNft] = 
        utxos ^.. traversed . Ledger.ciTxOutDatum 
              .  _Right 
              . to ( PlutusTx.fromBuiltinData @DatumNft . Scripts.getDatum )
              . _Just 
              . filtered (\d -> d.dNft'id Hask.== nftId)
  case datums of 
    [x] -> pure $ Just x 
    [ ] -> Contract.logError @Hask.String "No Datum can be found." >> pure Nothing 
    _ : _  -> Contract.logError @Hask.String "More than one suitable datums can be found." >> pure Nothing 

getUserAddr = pubKeyAddress <$> Contract.ownPubKey

getUserUtxos :: Contract w s Text (Map.Map TxOutRef Ledger.ChainIndexTxOut)   
getUserUtxos = Contract.utxosAt =<< getUserAddr 

getUId :: Contract w s Text UserId
getUId = UserId . pubKeyHash <$> Contract.ownPubKey 

reactBuy :: BuyRequestUser -> Contract w NFTAppSchema Text ()
reactBuy (BuyRequestUser nftId bid newPrice) = buy bid newPrice nftId
  

buy :: Integer -> Maybe Integer -> NftId -> Contract w NFTAppSchema Text ()
buy bid newPrice nftId = do
  oldDatum' <- getNftDatum nftId
  user <- getUId
  utxos <- getUserUtxos
  case oldDatum' of
    Nothing -> Contract.logError @Hask.String "NFT Cannot be found."
    Just oldDatum ->
      case oldDatum.dNft'price of 
        Nothing -> Contract.logError @Hask.String "NFT not for sale."
        Just price ->
          if bid Hask.< price
            then Contract.logError @Hask.String "Bid Price is too low."
            else do
              let newDatum = Scripts.Datum . PlutusTx.toBuiltinData $
                    DatumNft
                      { dNft'id = oldDatum.dNft'id
                      , dNft'share = oldDatum.dNft'share
                      , dNft'author = oldDatum.dNft'author
                      , dNft'owner = user
                      , dNft'price = newPrice
                      }

                  (lookups, tx) =
                    ( Hask.foldr1 (<>)
                      [ Constraints.unspentOutputs utxos
                      , Constraints.typedValidatorLookups (txPolicy nftId)
                      ]
                    , Hask.foldr1 (<>) 
                      [ --Constraints.mustMintValue val
                      --, Constraints.mustSpendPubKeyOutput oref
                      -- Constraints.mustPayToTheScript nft val
                       Constraints.mustIncludeDatum newDatum
                      ]
                    )
              void $ Contract.submitTxConstraintsWith @NftTrade lookups tx

-- | Mints an NFT and sends it to the App Address.
mint :: MintParams -> Contract (Last NftId) s Text ()
mint nftContent = do
  addr <- pubKeyAddress <$> Contract.ownPubKey
  nft' <- nftInit nftContent
  utxos <- Contract.utxosAt addr
  case nft' of
    Nothing -> Contract.throwError @Text "Cannot create NFT."
    Just nft -> maybe err (continue utxos nft) =<< fstUtxo addr
  where
    err = Contract.throwError @Text "No utxo found at address."

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
      Contract.tell $ Last . Just $ nftId
      Contract.logInfo @Hask.String $ printf "forged %s" (Hask.show val)

endpoints :: Contract (Last NftId) NFTAppSchema Text ()
endpoints = forever $ do
  Contract.awaitPromise $
    Hask.foldr1
      Contract.select
      [ endpoint @"mint" mint
      , endpoint @"buy" reactBuy
      ]

-- | Get the user's ChainIndexTxOut
getAddrUtxos :: Address -> Contract w s Text [Ledger.ChainIndexTxOut]
getAddrUtxos adr = fmap fst . Map.elems <$> Contract.utxosTxOutTxAt adr

-- | Get first utxo at address.
fstUtxo :: Address -> Contract w s Text (Maybe TxOutRef)
fstUtxo address = do
  utxos <- Contract.utxosAt address
  case Map.keys utxos of
    [] -> pure Nothing
    x : _ -> pure $ Just x

-- | Initialise new NftId 
nftIdInit :: MintParams -> Contract w s Text NftId
nftIdInit mintP = do 
  userAddress <- getUserAddr
  oref' <- fstUtxo userAddress
  case oref' of
    Nothing ->
      Contract.throwError "no utxo found" 
    Just oref -> do
      let hData = hashData mintP.mp'content
      pure $ NftId
        { nftId'title = mintP.mp'title
        , nftId'token = TokenName hData
        , nftId'outRef = oref
        }

-- | Initialise an NFT using the current wallet.
nftInit :: MintParams -> Contract w s Text (Maybe DatumNft)
nftInit mintP = do
  user <- getUId
  nftId <- nftIdInit mintP
  pure . Just $
   DatumNft
     { dNft'id = nftId
     , dNft'share = mintP.mp'share
     , dNft'author = user
     , dNft'owner = user
     , dNft'price = mintP.mp'price
     }

-- | todo: some hashing function here - at the moment getting the whole
-- bytestring
hashData :: Content -> BuiltinByteString
hashData (Content b) = b

-- | Generic application Trace Handle.
type AppTraceHandle = Trace.ContractHandle (Last NftId) NFTAppSchema Text

-- | Emulator Trace 1. Mints Some NFT.
eTrace1 :: EmulatorTrace ()
eTrace1 = do
  let wallet1 = walletFromNumber 1 :: Emulator.Wallet
      wallet2 = walletFromNumber 2 :: Emulator.Wallet
  h1 :: AppTraceHandle <- activateContractWallet wallet1 endpoints
  h2 :: AppTraceHandle <- activateContractWallet wallet2 endpoints
  callEndpoint @"mint" h1 artwork
  callEndpoint @"mint" h2 artwork

  void $ Trace.waitNSlots 1
  oState <- Trace.observableState h2
  nftId <- case getLast oState of
             Nothing  -> Trace.throwError (Trace.GenericError "NftId not found")
             Just nid -> return nid
  void $ Trace.waitNSlots 1
  callEndpoint @"buy" h1 (buyParams nftId)
  callEndpoint @"buy" h2 (buyParams nftId)

  Extras.logInfo @Hask.String $ Hask.show  oState

  callEndpoint @"mint" h1 artwork
  callEndpoint @"mint" h2 artwork
  where
    artwork =
      MintParams
        { mp'content = Content "A painting."
        , mp'title = Title "Fiona Lisa"
        , mp'share = 1 % 10
        , mp'price = Just 100
        }
    buyParams nftId = BuyRequestUser nftId 100 (Just 200)

-- | Test for prototyping.
test :: Hask.IO ()
test = runEmulatorTraceIO eTrace1
