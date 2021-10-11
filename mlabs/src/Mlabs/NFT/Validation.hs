{-# LANGUAGE UndecidableInstances #-}

module Mlabs.NFT.Validation (
  DatumNft (..),
  NftTrade,
  calculateShares,
  UserAct (..),
  asRedeemer,
  txPolicy,
  txScrAddress,
  nftCurrency,
  nftAsset,
  mintPolicy,
  priceNotNegative,
  nftTokenName,
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
  findDatumHash,
  findOwnInput,
  getContinuingOutputs,
  mkMintingPolicyScript,
  ownCurrencySymbol,
  scriptContextTxInfo,
  scriptCurrencySymbol,
  txInInfoOutRef,
  txInfoData,
  txInfoInputs,
  txInfoMint,
  txInfoOutputs,
  txInfoSignatories,
  valuePaidTo,
 )

import PlutusTx.Builtins.Class (stringToBuiltinByteString)

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

{- | A Deterministic Hashing function for the Datum. Todo:replace with something
 less obvious once testing is done.
-}
nftTokenName :: DatumNft -> TokenName
nftTokenName datum = TokenName . stringToBuiltinByteString $ repr
  where
    title = Hask.show . getTitle . nftId'title . dNft'id $ datum
    owner = Hask.show . getUserId . dNft'owner $ datum
    price = Hask.show . dNft'price $ datum
    repr = mconcat [title, " ", owner, " ", price]

instance Eq DatumNft where
  {-# INLINEABLE (==) #-}
  (DatumNft id1 share1 author1 owner1 price1) == (DatumNft id2 share2 author2 owner2 price2) =
    id1 == id2 && share1 == share2 && author1 == author2 && owner1 == owner2 && price1 == price2

-- | NFT Redeemer
data UserAct
  = -- | Buy NFT and set new price
    BuyAct
      { -- | price to buy.
        act'bid :: Integer
      , -- | new price for NFT (Nothing locks NFT).
        act'newPrice :: Maybe Integer
      , -- | CurencySymbol of the NFT the user is attempting to buy.
        act'cs :: CurrencySymbol
      }
  | -- | Set new price for NFT
    SetPriceAct
      { -- | new price for NFT (Nothing locks NFT)
        act'newPrice :: Maybe Integer
      , -- | Currency Symbol of the NFT the user is attempting to set the price of
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
  _ == _ = False

asRedeemer :: PlutusTx.ToData a => a -> Redeemer
asRedeemer = Redeemer . PlutusTx.toBuiltinData

{-# INLINEABLE mkMintPolicy #-}

-- | Minting policy for NFTs.
mkMintPolicy :: Address -> TxOutRef -> NftId -> MintAct -> ScriptContext -> Bool
mkMintPolicy stateAddr oref (NftId _ token outRef) mAct ctx =
  case mAct of
    TxAction from to -> queryFail "Not a valid Redeemer" True
    --      queryFail "No minting is allowed through querrying." noMint -- never remove this test
    --        && queryFail
    --          "Minted currency symbol does not match provided currency."
    --          (testMatch currSymbol)
    Mint tokenName ->
      mintFail "UTXO will not be consumed." hasUtxo
        && mintFail "Wrong amount minted" checkMintedAmount
        && mintFail "Does not pay to state" (paysToState tokenName)
        -- && mintFail "author could be checked also, their key should be in signatures." todo:
        && mintFail "NFTid TxOutRef and minting TxOutRef are different" sameORef
        && mintFail "Currency Name is not correct." True
  where
    -- Helper functions.
    mintFail :: BuiltinString -> Bool -> Bool
    mintFail xInfo test = traceIfFalse ("NFT Minting failed. " <> xInfo) test

    queryFail :: BuiltinString -> Bool -> Bool
    queryFail xInfo test = traceIfFalse ("NFT Query failed. " <> xInfo) test

    info = scriptContextTxInfo ctx
    ----------------------------------------------------------------------------
    -- Check Action - Tests
    --
    -- Extra attention should be given that the minting policy doesn't allow a
    -- token to be minted through this branch of logic -  as this would be a
    -- backdoor to incorrect minting.

    noMint :: Bool
    noMint = True

    --case flattenValue (txInfoMint info) of
    --    [(cur, tn, val)] -> True
    --    ownCurrencySymbol ctx == cur
    --      && token == tn
    --      && val == 1
    --    _ -> False

    -- Test that the currency symbol that would be minted by these specific
    -- configurations matches the currency symbol provided. If it matches, then
    -- the NFT is authentic - if not then it is a forgery. Also check that tx is
    -- minting a total of 0 tokens.

    ----------------------------------------------------------------------------
    -- Minting Action - Tests
    hasUtxo =
      any (\inp -> txInInfoOutRef inp == oref) $
        txInfoInputs info

    ----------------------------------------------------------------------------
    -- Check Minted
    checkMintedAmount = case flattenValue (txInfoMint info) of
      [(cur, tn, val)] ->
        ownCurrencySymbol ctx == cur
          -- && tokenName == tn
          && val == 1
      _ -> False

    paysToState token = any (hasNftToken token) $ txInfoOutputs info

    -- Check to see if the NFT token is correctly minted.
    hasNftToken token TxOut {..} =
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

{-# INLINEABLE mKTxPolicy #-}

-- | A validator script for the user actions.
mKTxPolicy :: DatumNft -> UserAct -> ScriptContext -> Bool
mKTxPolicy datum act ctx =
  traceIfFalse "Datum does not correspond to NFTId, no datum is present, or more than one suitable datums are present." correctDatum
    && traceIfFalse "Datum is not  present." correctDatum'
    && traceIfFalse "New Price cannot be negative." (setPositivePrice act)
    --  && traceIfFalse "Previous TX is not consumed." (prevTxConsumed ctx)
    --     && traceIfFalse "NFT sent to wrong address." tokenSentToCorrectAddress
    --    && traceIfFalse "Transaction cannot mint." noMint
    && case act of
      BuyAct {..} ->
        --         -- let
        --         --   mintZ = mintZeroTokens act'cs (nftId'token . dNft'id $ datum)
        --         -- in
        traceIfFalse "NFT not for sale." nftForSale
          --  && traceIfFalse ("NFT is not genuine.") mintZ
          && traceIfFalse "Bid is too low for the NFT price." (bidHighEnough act'bid)
          && traceIfFalse "New owner is not the payer." correctNewOwner
          && traceIfFalse "Author is not paid their share." (correctPaymentAuthor act'bid)
          && traceIfFalse "Current owner is not paid their share." (correctPaymentOwner act'bid)
          && traceIfFalse "Datum is not consistent, illegaly altered." consistentDatum
      SetPriceAct {} ->
        traceIfFalse "Price can not be negative." priceNotNegative'
          && traceIfFalse "Only owner exclusively can set NFT price." ownerSetsPrice
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
      let info = scriptContextTxInfo ctx
          mDatums = findDatumHash (Datum . PlutusTx.toBuiltinData $ datum) info
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
    -- Check if the new owner is set correctly.
    correctNewOwner = True
    ------------------------------------------------------------------------------
    -- Check if the Person is being reimbursed accordingly, with the help of 2
    -- getter functions. Helper function.
    correctPayment f g bid =
      let info = scriptContextTxInfo ctx
          personId = getUserId . f $ datum
          authorShare = dNft'share datum
          personGetsAda = getAda $ valuePaidTo info personId
          personWantsAda = getAda $ g bid authorShare
       in personGetsAda >= personWantsAda
      where
        getAda = flip assetClassValueOf $ assetClass Ada.adaSymbol Ada.adaToken
    ------------------------------------------------------------------------------
    -- Check if the Author is being reimbursed accordingly.
    correctPaymentAuthor = correctPayment dNft'author calculateAuthorShare
    ------------------------------------------------------------------------------
    -- Check if the Current Owner is being reimbursed accordingly.
    correctPaymentOwner = correctPayment dNft'author calculateOwnerShare
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
    -- Check no new token is minted.
    mintZeroTokens :: CurrencySymbol -> TokenName -> Bool
    mintZeroTokens symbol token = True
    -- case flattenValue . txInfoMint . scriptContextTxInfo $ ctx of
    --   [] -> True
    --   _ -> False
    ------------------------------------------------------------------------------
    -- Check if the NFT is still. ToDo:fix!
    tokenSentToCorrectAddress = True
    --  containsNft $ foldMap txOutValue (getContinuingOutputs ctx)

    ----------------------------------------------------------------------------
    -- Check if the NFT is sent to the correct address.
    --  containsNft v = valueOf v (act'cs act) nftTokenName == 1

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
    ------------------------------------------------------------------------------
    -- Check if the previous Tx containing the token is consumed.
    --  prevTxConsumed ctx =
    --    case findOwnInput ctx of
    --      Just (TxInInfo _ out) -> containsNft $ txOutValue out
    --      Nothing -> False
    ------------------------------------------------------------------------------
    -- Check if new price non-negative.
    priceNotNegative' = priceNotNegative (act'newPrice act)
    ------------------------------------------------------------------------------
    -- Check that price set by NFT owner.
    ownerSetsPrice =
      case txInfoSignatories $ scriptContextTxInfo ctx of
        [pkh] -> pkh == getUserId (dNft'owner datum)
        _ -> False

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

{-# INLINEABLE nftCurrency #-}

-- | Calculate the NFT `CurrencySymbol` from NftId.
nftCurrency :: DatumNft -> CurrencySymbol
nftCurrency datum =
  let nftId = dNft'id datum
      oRef = nftId'outRef nftId
   in curSymbol txScrAddress oRef nftId

{-# INLINEABLE nftAsset #-}

-- | Calculate the NFT `AssetClass` from Datum.
nftAsset :: DatumNft -> AssetClass
nftAsset datum =
  AssetClass
    ( nftCurrency datum
    , nftTokenName datum
    )

{-# INLINEABLE calculateShares #-}

-- | Returns the calculated value of shares.
calculateShares :: Integer -> Rational -> (Value, Value)
calculateShares bid authorShare = (toOwner, toAuthor)
  where
    adaToLovelaceVal = Ada.lovelaceValueOf . (* 1_000_000)

    toAuthor' = round $ fromInteger bid * authorShare
    toAuthor = adaToLovelaceVal toAuthor'
    toOwner' = bid - toAuthor'
    toOwner = adaToLovelaceVal toOwner'

{-# INLINEABLE calculateOwnerShare #-}

-- | Returns the calculated value of shares.
calculateOwnerShare :: Integer -> Rational -> Value
calculateOwnerShare x y = fst $ calculateShares x y

{-# INLINEABLE calculateAuthorShare #-}

-- | Returns the calculated value of shares.
calculateAuthorShare :: Integer -> Rational -> Value
calculateAuthorShare x y = snd $ calculateShares x y
