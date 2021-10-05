{-# LANGUAGE UndecidableInstances #-}

module Mlabs.NFT.Validation (
  DatumNft (..),
  NftTrade,
  calculateShares,
  UserAct (..),
  asRedeemer,
  txPolicy,
  txScrAddress,
  nftAsset,
  mintPolicy,
) where

import PlutusTx.Prelude
import Prelude qualified as Hask

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Plutus.V1.Ledger.Ada qualified as Ada

import Ledger (
  Address,
  AssetClass,
  CurrencySymbol,
  Datum (..),
  MintingPolicy,
  Redeemer (..),
  ScriptContext (..),
  TxOut (..),
  TxOutRef,
  ValidatorHash,
  Value,
  mkMintingPolicyScript,
  ownCurrencySymbol,
  pubKeyOutputsAt,
  scriptContextTxInfo,
  scriptCurrencySymbol,
  txInInfoOutRef,
  txInfoData,
  txInfoInputs,
  txInfoMint,
  txInfoOutputs,
 )
import Ledger.Typed.Scripts (
  DatumType,
  RedeemerType,
  TypedValidator,
  ValidatorTypes,
  mkTypedValidator,
  validatorAddress,
  validatorHash,
  wrapMintingPolicy,
  wrapValidator,
 )
import Ledger.Value qualified as Value
import Plutus.V1.Ledger.Value (AssetClass (AssetClass))
import PlutusTx qualified
import Schema (ToSchema)

import Mlabs.NFT.Types

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

PlutusTx.unstableMakeIsData ''DatumNft
PlutusTx.makeLift ''DatumNft

nftTokenName :: DatumNft -> Value.TokenName
nftTokenName = nftId'token . dNft'id

instance Eq DatumNft where
  {-# INLINEABLE (==) #-}
  (DatumNft id1 share1 author1 owner1 price1) == (DatumNft id2 share2 author2 owner2 price2) =
    id1 == id2 && share1 == share2 && author1 == author2 && owner1 == owner2 && price1 == price2

-- | NFT Redeemer
data UserAct
  = -- | Buy NFT and set new price
    BuyAct
      { -- | price to buy
        act'bid :: Integer
      , -- | new price for NFT (Nothing locks NFT)
        act'newPrice :: Maybe Integer
      , act'cs :: CurrencySymbol
      }
  | -- | Set new price for NFT
    SetPriceAct
      { -- | new price for NFT (Nothing locks NFT)
        act'newPrice :: Maybe Integer
      , act'cs :: CurrencySymbol
      }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''UserAct
PlutusTx.unstableMakeIsData ''UserAct

instance Eq UserAct where
  {-# INLINEABLE (==) #-}
  (BuyAct bid1 newPrice1 cs1) == (BuyAct bid2 newPrice2 cs2) =
    bid1 == bid2
      && newPrice1 == newPrice2
      && cs1 == cs2
  (SetPriceAct newPrice1 cs1) == (SetPriceAct newPrice2 cs2) =
    newPrice1 == newPrice2
      && cs1 == cs2
  _ == _ = False

asRedeemer :: UserAct -> Redeemer
asRedeemer = Redeemer . PlutusTx.toBuiltinData

{-# INLINEABLE mkMintPolicy #-}

-- | Minting policy for NFTs.
mkMintPolicy :: Address -> TxOutRef -> NftId -> () -> ScriptContext -> Bool
mkMintPolicy stateAddr oref (NftId _ token outRef) _ ctx =
  -- ? maybe author could be checked also, their key should be in signatures.
  traceIfFalse "UTXO not consumed" hasUtxo
    && traceIfFalse "Wrong amount minted" checkMintedAmount
    && traceIfFalse "Does not pay to state" paysToState
    && traceIfFalse "NFTid TxOutRef and minting TxOutRef are different" sameORef
  where
    info = scriptContextTxInfo ctx

    hasUtxo =
      any (\inp -> txInInfoOutRef inp == oref) $
        txInfoInputs info

    checkMintedAmount = case Value.flattenValue (txInfoMint info) of
      [(cur, tn, val)] ->
        ownCurrencySymbol ctx == cur
          && token == tn
          && val == 1
      _ -> False

    paysToState = any hasNftToken $ txInfoOutputs info

    -- Check to see if the NFT token is correctly minted.
    hasNftToken TxOut {..} =
      txOutAddress == stateAddr
        && txOutValue == Value.singleton (ownCurrencySymbol ctx) token 1

    -- Check to see if the received TxOutRef is the same as the  one the NFT is
    -- paramaterised by.
    sameORef = oref == outRef

mintPolicy :: Address -> TxOutRef -> NftId -> MintingPolicy
mintPolicy stateAddr oref nid =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||\x y z -> wrapMintingPolicy (mkMintPolicy x y z)||])
      `PlutusTx.applyCode` PlutusTx.liftCode stateAddr
      `PlutusTx.applyCode` PlutusTx.liftCode oref
      `PlutusTx.applyCode` PlutusTx.liftCode nid

{-# INLINEABLE mKTxPolicy #-}

-- | A validator script for the user actions.
mKTxPolicy :: DatumNft -> UserAct -> ScriptContext -> Bool
mKTxPolicy datum act ctx =
  traceIfFalse
    "Datum does not correspond to NFTId, no datum is present, or more than one suitable datums are present."
    correctDatum
    && traceIfFalse "Old transaction is not consumed or NFT not found." oldTxConsumed
    && traceIfFalse "Transaction should not mint anything." noMint
    && traceIfFalse "NFT is not sent to the correct address." tokenSentToCorrectAddress
    && case act of
      BuyAct {..} ->
        traceIfFalse "NFT not for sale." nftForSale
          && traceIfFalse "Bid is too low for the NFT price." (bidHighEnough act'bid)
          && traceIfFalse "New owner is not the payer." correctNewOwner
          && traceIfFalse "Author is not paid their share." (correctPaymentAuthor act'bid)
          && traceIfFalse "Current owner is not paid their share." (correctPaymentOwner act'bid)
          && traceIfFalse "Datum is not consistent, illegaly altered." consistentDatum
      SetPriceAct {} ->
        traceIfFalse "Price cannot be negative." True -- todo
          && traceIfFalse "User does not own NFT." True -- todo
  where
    ------------------------------------------------------------------------------
    -- Utility functions.
    getCtxDatum :: PlutusTx.FromData a => ScriptContext -> [a]
    getCtxDatum =
      id
        . fmap (\(Just x) -> x)
        . filter (maybe False (const True))
        . fmap PlutusTx.fromBuiltinData
        . fmap (\(Datum d) -> d)
        . fmap snd
        . txInfoData
        . scriptContextTxInfo

    ------------------------------------------------------------------------------
    -- Checks
    ------------------------------------------------------------------------------
    -- Check if the datum in the datum is also the same in the transaction.
    correctDatum =
      let datums :: [DatumNft] = getCtxDatum ctx
          suitableDatums = filter (== dNft'id datum) . fmap dNft'id $ datums
       in case suitableDatums of
            [_] -> True
            _ -> False

    ------------------------------------------------------------------------------
    -- Check if the NFT is for sale.
    nftForSale = maybe False (const True) $ dNft'price datum

    ------------------------------------------------------------------------------
    -- Check if the bid price is high enough.
    bidHighEnough bid =
      let price = dNft'price datum
       in fromMaybe False $ (bid >=) <$> price

    ------------------------------------------------------------------------------
    -- Check if the new owner is set correctly.
    correctNewOwner = True

    ------------------------------------------------------------------------------
    -- Check if the Author is being reimbursed accordingly.
    correctPaymentAuthor bid =
      let info = scriptContextTxInfo ctx
          authorPaid = pubKeyOutputsAt (getUserId . dNft'author $ datum) info
          authorDeserves = snd $ calculateShares bid (dNft'share datum)
       in case authorPaid of
            [x] -> authorDeserves == x
            _ -> False

    ------------------------------------------------------------------------------
    -- Check if the Current Owner is being reimbursed accordingly.
    correctPaymentOwner bid =
      let info = scriptContextTxInfo ctx
          ownerPaid = pubKeyOutputsAt (getUserId . dNft'owner $ datum) info
          ownerDeserves = fst $ calculateShares bid (dNft'share datum)
       in case ownerPaid of
            [x] -> ownerDeserves == x
            _ -> False

    ------------------------------------------------------------------------------
    -- Check if the new Datum is correctly.
    consistentDatum =
      let prevDatum :: DatumNft = head . getCtxDatum $ ctx
       in dNft'id prevDatum == dNft'id datum
            && dNft'share prevDatum == dNft'share datum
            && dNft'author prevDatum == dNft'author datum

    ------------------------------------------------------------------------------
    -- Check no new token is minted.
    noMint = Value.isZero . txInfoMint . scriptContextTxInfo $ ctx

    ------------------------------------------------------------------------------
    -- Check if the NFT is sent to the correct address.
    tokenSentToCorrectAddress = True

    ------------------------------------------------------------------------------
    -- Check if the old Tx containing the token is consumed.
    oldTxConsumed = True

data NftTrade
instance ValidatorTypes NftTrade where
  type DatumType NftTrade = DatumNft
  type RedeemerType NftTrade = UserAct

{-# INLINEABLE txPolicy #-}
txPolicy :: TypedValidator NftTrade
txPolicy =
  mkTypedValidator @NftTrade
    $$(PlutusTx.compile [||mKTxPolicy||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = wrapValidator @DatumNft @UserAct

{-# INLINEABLE txValHash #-}
txValHash :: ValidatorHash
txValHash = validatorHash txPolicy

{-# INLINEABLE txScrAddress #-}
txScrAddress :: Ledger.Address
txScrAddress = validatorAddress txPolicy

{-# INLINEABLE curSymbol #-}

-- | Calculate the currency symbol of the NFT.
curSymbol :: Address -> TxOutRef -> NftId -> CurrencySymbol
curSymbol stateAddr oref nid = scriptCurrencySymbol $ mintPolicy stateAddr oref nid

{-# INLINEABLE nftAsset #-}
nftAsset :: NftId -> AssetClass
nftAsset nid = AssetClass (cs, tn)
  where
    cs =
      scriptCurrencySymbol $
        mintPolicy txScrAddress (nftId'outRef nid) nid
    tn = nftId'token nid

{-# INLINEABLE calculateShares #-}

-- | Returns the calculated value of shares.
calculateShares :: Integer -> Rational -> (Value, Value)
calculateShares bid authorShare = (toOwner, toAuthor)
  where
    converAdaToLovelace = Ada.toValue . Ada.Lovelace . (* 1_000_000)
    toAuthor' = round $ fromInteger bid * authorShare
    toAuthor = converAdaToLovelace toAuthor'
    toOwner' = bid - toAuthor'
    toOwner = converAdaToLovelace toOwner'
