{-# LANGUAGE UndecidableInstances #-}

module Mlabs.NFT.Validation (
  DatumNft (..),
  NftTrade,
  calculateShares,
  UserAct (..),
  asRedeemer,
  txPolicy,
  mkTxPolicy,
  txScrAddress,
  txValHash,
  nftCurrency,
  nftAsset,
  mintPolicy,
  mkMintPolicy,
  priceNotNegative,
) where

import PlutusTx.Prelude
import Prelude qualified as Hask

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Plutus.V1.Ledger.Ada qualified as Ada (
  adaSymbol,
  adaToken,
  lovelaceValueOf,
 )

import Ledger (
  Address,
  AssetClass,
  CurrencySymbol,
  Datum (..),
  MintingPolicy,
  Redeemer (..),
  ScriptContext (..),
  TxInInfo (..),
  TxOut (..),
  TxOutRef,
  ValidatorHash,
  Value,
  contains,
  findDatumHash,
  findOwnInput,
  from,
  getContinuingOutputs,
  mkMintingPolicyScript,
  ownCurrencySymbol,
  scriptContextTxInfo,
  scriptCurrencySymbol,
  to,
  txInInfoOutRef,
  txInfoData,
  txInfoInputs,
  txInfoMint,
  txInfoOutputs,
  txInfoSignatories,
  txInfoValidRange,
  valuePaidTo,
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

import Ledger.Value (
  TokenName (..),
  assetClass,
  flattenValue,
  isZero,
  singleton,
  valueOf,
 )
import Plutus.V1.Ledger.Value (AssetClass (AssetClass), assetClassValueOf)
import PlutusTx qualified
import Schema (ToSchema)

import Mlabs.NFT.Types

-- | NFT Datum is checked communicates the ownership of the NFT.
data DatumNft = DatumNft
  { -- | NFT ID
    dNft'id :: NftId
  , -- | Author's share of the NFT.
    dNft'share :: Rational
  , -- | Author's wallet pubKey.
    dNft'author :: UserId
  , -- | Owner's wallet pubkey.
    dNft'owner :: UserId
  , -- | Price in Lovelace. If Nothing, NFT not for sale.
    dNft'price :: Maybe Integer
  , -- | Auction state
    dNft'auctionState :: Maybe AuctionState
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

PlutusTx.unstableMakeIsData ''DatumNft
PlutusTx.makeLift ''DatumNft

nftTokenName :: DatumNft -> TokenName
nftTokenName = nftId'token . dNft'id

instance Eq DatumNft where
  {-# INLINEABLE (==) #-}
  (DatumNft id1 share1 author1 owner1 price1 as1) == (DatumNft id2 share2 author2 owner2 price2 as2) =
    id1 == id2 && share1 == share2 && author1 == author2 && owner1 == owner2 && price1 == price2 && as1 == as2

-- | NFT Redeemer
data UserAct
  = -- | Buy NFT and set new price
    BuyAct
      { -- | price to buy. In Lovelace.
        act'bid :: Integer
      , -- | new price for NFT. In Lovelace.
        act'newPrice :: Maybe Integer
      , -- | CurencySymbol of the NFT the user is attempting to buy.
        act'cs :: CurrencySymbol
      }
  | -- | Set new price for NFT
    SetPriceAct
      { -- | new price for NFT. In Lovelace.
        act'newPrice :: Maybe Integer
      , -- | Currency Symbol of the NFT the user is attempting to set the price of.
        act'cs :: CurrencySymbol
      }
  | -- | Start NFT auction
    OpenAuctionAct
      { -- | TODO
        act'cs :: CurrencySymbol
      }
  | -- | Make a bid in an auction
    BidAuctionAct
      { -- | bid amount
        act'bid :: Integer
      , -- | TODO
        act'cs :: CurrencySymbol
      }
  | CloseAuctionAct
      { -- | TODO
        act'cs :: CurrencySymbol
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
  (OpenAuctionAct cs1) == (OpenAuctionAct cs2) =
    cs1 == cs2
  (BidAuctionAct bid1 cs1) == (BidAuctionAct bid2 cs2) =
    bid1 == bid2 && cs1 == cs2
  (CloseAuctionAct cs1) == (CloseAuctionAct cs2) =
    cs1 == cs2
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

    checkMintedAmount = case flattenValue (txInfoMint info) of
      [(cur, tn, val)] ->
        ownCurrencySymbol ctx == cur
          && token == tn
          && val == 1
      _ -> False

    paysToState = any hasNftToken $ txInfoOutputs info

    -- Check to see if the NFT token is correctly minted.
    hasNftToken TxOut {..} =
      txOutAddress == stateAddr
        && txOutValue == singleton (ownCurrencySymbol ctx) token 1

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

{-# INLINEABLE mkTxPolicy #-}

-- | A validator script for the user actions.
mkTxPolicy :: DatumNft -> UserAct -> ScriptContext -> Bool
mkTxPolicy datum act ctx =
  traceIfFalse "Datum does not correspond to NFTId, no datum is present, or more than one suitable datums are present." correctDatum
    && traceIfFalse "Datum is not present." correctDatum'
    && traceIfFalse "New Price cannot be negative." (setPositivePrice act)
    && traceIfFalse "Previous TX is not consumed." prevTxConsumed
    && traceIfFalse "NFT sent to wrong address." tokenSentToCorrectAddress
    && traceIfFalse "Transaction cannot mint." noMint
    && case act of
      BuyAct {..} ->
        traceIfFalse "Can't buy: Auction is in progress" noAuctionInProgress
          && traceIfFalse "NFT not for sale." nftForSale
          && traceIfFalse "Bid is too low for the NFT price." (bidHighEnough act'bid)
          && traceIfFalse "New owner is not the payer." correctNewOwner
          && traceIfFalse "Datum is not consistent, illegaly altered." consistentDatum
          && if ownerIsAuthor
            then traceIfFalse "Amount paid to author/owner does not match bid." (correctPaymentOnlyAuthor act'bid)
            else
              traceIfFalse "Current owner is not paid their share." (correctPaymentOwner act'bid)
                && traceIfFalse "Author is not paid their share." (correctPaymentAuthor act'bid)
      SetPriceAct {} ->
        traceIfFalse "Can't set price: Auction is in progress" noAuctionInProgress
          && traceIfFalse "Price can not be negative." priceNotNegative'
          && traceIfFalse "Only owner exclusively can set NFT price." signedByOwner
      OpenAuctionAct {} ->
        traceIfFalse "Can't open auction: already in progress" noAuctionInProgress
          && traceIfFalse "Only owner can open auction" signedByOwner
      BidAuctionAct {..} ->
        traceIfFalse "Can't bid: No auction is in progress" (not noAuctionInProgress)
          && traceIfFalse "Auction bid is too low" (auctionBidHighEnough act'bid)
          && traceIfFalse "Auction deadline reached" correctAuctionBidSlotInterval
          && traceIfFalse "Datum illegally altered" auctionConsistentDatum
          -- && traceIfFalse "Auction bid value not supplied" auctionBidValueCorrectInput
          && traceIfFalse "Redeemer and datum auction bids are inconsistent" (auctionConsistentDatumRedeemer act'bid)
      CloseAuctionAct {} ->
        traceIfFalse "Can't close auction: none in progress" (not noAuctionInProgress)
          && traceIfFalse "Auction deadline not yet reached" auctionDeadlineReached
          && traceIfFalse "Only owner can close auction" signedByOwner
          -- TODO: check amounts paid to owner & author
  where
    ------------------------------------------------------------------------------
    -- Utility functions.
    getCtxDatum :: PlutusTx.FromData a => ScriptContext -> [a]
    getCtxDatum =
      catMaybes'
        . fmap PlutusTx.fromBuiltinData
        . fmap (\(Datum d) -> d)
        . fmap snd
        . txInfoData
        . scriptContextTxInfo

    ownerIsAuthor :: Bool
    ownerIsAuthor = dNft'owner datum == dNft'author datum

    getAda = flip assetClassValueOf $ assetClass Ada.adaSymbol Ada.adaToken

    info = scriptContextTxInfo ctx

    ------------------------------------------------------------------------------
    -- Checks
    ------------------------------------------------------------------------------
    -- Check whether there's auction in progress and disallow buy/setprice actions.
    noAuctionInProgress :: Bool
    noAuctionInProgress = isNothing mauctionState

    mauctionState = dNft'auctionState datum

    withAuctionState f = maybe False f mauctionState

    auctionBidHighEnough :: Integer -> Bool
    auctionBidHighEnough amount =
      withAuctionState $ \auctionState ->
        case as'highestBid auctionState of
          Nothing -> amount >= as'minBid auctionState
          Just highestBid -> amount > ab'bid highestBid

    correctAuctionBidSlotInterval :: Bool
    correctAuctionBidSlotInterval =
      withAuctionState $ \auctionState ->
        (to $ as'deadline auctionState) `contains` txInfoValidRange info

    auctionDeadlineReached :: Bool
    auctionDeadlineReached =
      withAuctionState $ \auctionState ->
        (from $ as'deadline auctionState) `contains` txInfoValidRange info

    auctionConsistentDatumRedeemer :: Integer -> Bool
    auctionConsistentDatumRedeemer redeemerBid =
      withAuctionState $ \auctionState ->
        case as'highestBid auctionState of
          Nothing -> True
          Just highestBid -> False -- ab'bid highestBid == redeemerBid

    auctionConsistentDatum =
      let prevDatum :: DatumNft = head . getCtxDatum $ ctx
          -- TODO: remove?
          checkHighestBid =
            case (dNft'auctionState prevDatum, dNft'auctionState datum) of
              (Just (AuctionState (Just prevBid) _ _), Just (AuctionState (Just bid) _ _)) ->
                ab'bid prevBid < ab'bid bid
              (_, _) -> True

       in dNft'id prevDatum == dNft'id datum
            && dNft'share prevDatum == dNft'share datum
            && dNft'author prevDatum == dNft'author datum
            && dNft'owner prevDatum == dNft'owner datum
            && dNft'price prevDatum == dNft'price datum
            && checkHighestBid

    -- Check if the datum attached is also present in the is also in the transaction.
    correctDatum :: Bool
    correctDatum =
      let datums :: [DatumNft] = getCtxDatum ctx
          suitableDatums = filter (== dNft'id datum) . fmap dNft'id $ datums
       in case suitableDatums of
            _ : _ -> True
            _ -> False

    ------------------------------------------------------------------------------
    -- Check if the datum in the datum is also the same in the transaction, v2.
    correctDatum' :: Bool
    correctDatum' =
      let mDatums = findDatumHash (Datum . PlutusTx.toBuiltinData $ datum) info
       in maybe False (const True) mDatums

    ------------------------------------------------------------------------------
    -- Check if the NFT is for sale.
    nftForSale = maybe False (const True) $ dNft'price datum

    ------------------------------------------------------------------------------
    -- Check if the bid price is high enough.
    bidHighEnough bid =
      let price = dNft'price datum
       in fromMaybe False $ (bid >=) <$> price

    ------------------------------------------------------------------------------
    -- Check if the new owner is set correctly. todo
    correctNewOwner = True

    ------------------------------------------------------------------------------
    -- Check if the Person is being reimbursed accordingly, with the help of 2
    -- getter functions. Helper function.
    correctPayment f shareCalcFn bid = personGetsAda >= personWantsAda
      where
        personId = getUserId . f $ datum
        share = dNft'share datum
        personGetsAda = getAda $ valuePaidTo info personId
        personWantsAda = getAda $ shareCalcFn bid share

    ------------------------------------------------------------------------------
    -- Check if the Author is being reimbursed accordingly.
    correctPaymentAuthor = correctPayment dNft'author calculateAuthorShare

    ------------------------------------------------------------------------------
    -- Check if the Current Owner is being reimbursed accordingly.
    correctPaymentOwner = correctPayment dNft'owner calculateOwnerShare

    ------------------------------------------------------------------------------
    -- Check if the Author is being paid the full amount when they are both
    -- owner and author.
    correctPaymentOnlyAuthor bid = authorGetsAda >= bid
      where
        author = getUserId . dNft'author $ datum
        authorGetsAda = getAda $ valuePaidTo info author

    ------------------------------------------------------------------------------
    -- Check if the new Datum is correctly.
    consistentDatum =
      let prevDatum :: DatumNft = head . getCtxDatum $ ctx
       in dNft'id prevDatum == dNft'id datum
            && dNft'share prevDatum == dNft'share datum
            && dNft'author prevDatum == dNft'author datum

    ------------------------------------------------------------------------------
    -- Check no new token is minted.
    noMint = isZero . txInfoMint . scriptContextTxInfo $ ctx

    ------------------------------------------------------------------------------
    -- Check if the NFT is sent to the correct address.
    tokenSentToCorrectAddress =
      containsNft $ foldMap txOutValue (getContinuingOutputs ctx)

    containsNft v = valueOf v (act'cs act) (nftTokenName datum) == 1

    ------------------------------------------------------------------------------
    -- Check new price is positive or nothing.
    setPositivePrice = \case
      action@BuyAct {} ->
        case act'newPrice action of
          Nothing -> True
          Just x -> x > 0
      action@SetPriceAct {} ->
        case act'newPrice action of
          Nothing -> True
          Just x -> x > 0
      _ -> True

    ------------------------------------------------------------------------------
    -- Check if the previous Tx containing the token is consumed.
    prevTxConsumed =
      case findOwnInput ctx of
        Just (TxInInfo _ out) -> containsNft $ txOutValue out
        Nothing -> False

    ------------------------------------------------------------------------------
    -- Check if new price non-negative.
    priceNotNegative' = priceNotNegative (act'newPrice act)

    ------------------------------------------------------------------------------
    -- Check that price set by NFT owner.
    signedByOwner =
      case txInfoSignatories $ scriptContextTxInfo ctx of
        [pkh] -> pkh == getUserId (dNft'owner datum)
        _ -> False

{-# INLINEABLE catMaybes' #-}
catMaybes' :: [Maybe a] -> [a]
catMaybes' = mapMaybe id

{-# INLINEABLE priceNotNegative #-}
priceNotNegative :: Maybe Integer -> Bool
priceNotNegative = maybe True (>= 0)

data NftTrade
instance ValidatorTypes NftTrade where
  type DatumType NftTrade = DatumNft
  type RedeemerType NftTrade = UserAct

{-# INLINEABLE txPolicy #-}
txPolicy :: TypedValidator NftTrade
txPolicy =
  mkTypedValidator @NftTrade
    $$(PlutusTx.compile [||mkTxPolicy||])
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

{-# INLINEABLE nftCurrency #-}

-- | Calculate the NFT `CurrencySymbol` from NftId.
nftCurrency :: NftId -> CurrencySymbol
nftCurrency nid =
  scriptCurrencySymbol $
    mintPolicy txScrAddress (nftId'outRef nid) nid

{-# INLINEABLE nftAsset #-}

-- | Calculate the NFT `AssetClass` from NftId.
nftAsset :: NftId -> AssetClass
nftAsset nid = AssetClass (nftCurrency nid, nftId'token nid)

{-# INLINEABLE calculateShares #-}

{- | Returns the amount each party should be paid given the number of shares
 retained by author.
-}
calculateShares :: Integer -> Rational -> (Value, Value)
calculateShares bid authorShare = (toOwner, toAuthor)
  where
    authorPart = round $ fromInteger bid * authorShare
    toAuthor = Ada.lovelaceValueOf authorPart
    toOwner = Ada.lovelaceValueOf $ bid - authorPart

{-# INLINEABLE calculateOwnerShare #-}

-- | Returns the calculated value of shares.
calculateOwnerShare :: Integer -> Rational -> Value
calculateOwnerShare x y = fst $ calculateShares x y

{-# INLINEABLE calculateAuthorShare #-}

-- | Returns the calculated value of shares.
calculateAuthorShare :: Integer -> Rational -> Value
calculateAuthorShare x y = snd $ calculateShares x y
