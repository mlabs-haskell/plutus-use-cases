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
  curSymbol,
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
  findDatum,
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

import Data.Function (on)
import Ledger.Value (
  TokenName (..),
  assetClass,
  flattenValue,
  singleton,
  valueOf,
 )
import Plutus.V1.Ledger.Value (AssetClass (..), assetClassValueOf, isZero)
import PlutusTx qualified
import Schema (ToSchema)

import Mlabs.NFT.Types

asRedeemer :: PlutusTx.ToData a => a -> Redeemer
asRedeemer = Redeemer . PlutusTx.toBuiltinData

{-# INLINEABLE mkMintPolicy #-}

-- | Minting policy for NFTs.
mkMintPolicy :: NftAppInstance -> MintAct -> ScriptContext -> Bool
mkMintPolicy appInstance act ctx =
  True
    --     && traceIfFalse "Only One Token Can be Minted" checkMintedAmount
    && case act of
      Mint nftid ->
        traceIfFalse "Only pointer of first node can change." True
      --          && traceIfFalse "Old first node must point to second node." (first `pointsTo'` second)
      --          && traceIfFalse "New first node must point to new node." (newFirst `pointsTo` newInserted)
      --          && traceIfFalse "New node must point to second node." (newInserted `pointsTo'` second)
      --          && traceIfFalse "New price cannot be negative." priceNotNegative'
      --          && traceIfFalse "Currency symbol must match app instance" checkCurrencySymbol
      --          && traceIfFalse "Minted tokens are sent to script address" checkSentAddress
      Initialise ->
        traceIfFalse "The token is not present." True -- todo
          && traceIfFalse "The token is not sent to the right address" True -- todo
  where
    info = scriptContextTxInfo ctx
    scriptAddress = appInstance'Address appInstance

    priceNotNegative' = case newInserted of
      NodeDatum node -> priceNotNegative (info'price . node'information $ node)
      _ -> False

    checkSentAddress = all sentToScript $ txInfoOutputs info

    sentToScript TxOut {..} =
      txOutAddress == scriptAddress

    checkCurrencySymbol =
      getAppInstance first == appInstance
        && getAppInstance newInserted == appInstance

    (newFirst, newInserted) =
      let outs = getContinuingOutputs ctx
          datums :: [DatumNft] =
            mapMaybe (PlutusTx.fromBuiltinData . getDatum)
              . mapMaybe (\hash -> findDatum hash info)
              . mapMaybe txOutDatumHash
              $ outs
       in case datums of
            [x, y] -> (x, y)
            _ -> traceError "Expected exactly two continuing outputs with datums."

    (first, inserted) = case getCtxDatum ctx of
      [x, y] -> if x < y then (x, y) else (y, x)
      [_] -> traceError "Expected exactly two inputs with datums. Receiving less."
      [] -> traceError "Expected exactly two inputs with datums. Receiving none."
      _ -> traceError "Expected exactly two inputs with datums. Receiving more."

    second = getDatumPointer first

    checkMintedAmount = case flattenValue (txInfoMint info) of
      [(cur, tn, val)] ->
        ownCurrencySymbol ctx == cur
          && nftTokenName newInserted == tn
          && val == 1
      _ -> False

    checkFirstNodeAltered = case (first, newFirst) of
      (NodeDatum node1, NodeDatum node2) ->
        node'appInstance node1 == node'appInstance node2
          && node'information node1 == node'information node2
      (HeadDatum node1, HeadDatum node2) ->
        head'appInstance node1 == head'appInstance node2
      _ -> False

    pointsTo d1 d2 = case (d1, d2) of
      (_, NodeDatum _) -> case getDatumPointer d1 of
        Just ptr -> (== nftTokenName d2) . snd . unAssetClass . pointer'assetClass $ ptr
        Nothing -> False
      _ -> False

    pointsTo' :: DatumNft -> Maybe Pointer -> Bool
    pointsTo' datum pointer = getDatumPointer datum == pointer

mintPolicy :: NftAppInstance -> MintingPolicy
mintPolicy appInstance =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||wrapMintingPolicy . mkMintPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode appInstance

{-# INLINEABLE mKTxPolicy #-}

-- | A validator script for the user actions.
mKTxPolicy :: DatumNft -> UserAct -> ScriptContext -> Bool
mKTxPolicy datum' act ctx =
  case datum' of
    HeadDatum _ ->
      traceIfFalse "Proof Token must be paid back" True -- todo
      -- must always pay back the proof Token. This happens when the Head datum is
      -- updated as the utxo needs to be consumed
    NodeDatum node ->
      traceIfFalse "New Price cannot be negative." priceNotNegative'
        && traceIfFalse "Previous TX is not consumed." prevTxConsumed
        && traceIfFalse "NFT sent to wrong address." tokenSentToCorrectAddress
        && traceIfFalse "Transaction cannot mint." noMint
        && case act of
          MintAct {} ->
            traceIfFalse "Only one token can be minted" True -- todo
              && traceIfFalse "Proof token must be paid back when using Head" True -- todo
          BuyAct {} ->
            let bid = act'bid act
             in traceIfFalse "NFT not for sale." nftForSale
                  && traceIfFalse "Bid is too low for the NFT price." (bidHighEnough bid)
                  && traceIfFalse "Datum is not consistent, illegaly altered." consistentDatumBuy
                  && if ownerIsAuthor
                    then traceIfFalse "Amount paid to author/owner does not match bid." (correctPaymentOnlyAuthor bid)
                    else
                      traceIfFalse "Current owner is not paid their share." (correctPaymentOwner bid)
                        && traceIfFalse "Author is not paid their share." (correctPaymentAuthor bid)
          SetPriceAct {} ->
            traceIfFalse "Datum does not correspond to NFTId, no datum is present, or more than one suitable datums are present." correctDatumSetPrice
              && traceIfFalse "Only owner exclusively can set NFT price." ownerSetsPrice
              && traceIfFalse "Datum is not consistent, illegaly altered." consistentDatumSetPrice
      where
        nInfo = node'information node
        nNext = node'next node
        nAppInstance = node'appInstance node
        prevDatum :: DatumNft = head . getCtxDatum $ ctx
        prevNode' :: Maybe NftListNode = case prevDatum of
          NodeDatum prevNode -> Just prevNode
          _ -> Nothing
        ------------------------------------------------------------------------------
        -- Utility functions.

        containsNft v = valueOf v (instanceCurrency $ node'appInstance node) (nftTokenName datum') == 1

        getAda = flip assetClassValueOf $ assetClass Ada.adaSymbol Ada.adaToken

        -- Check if the Person is being reimbursed accordingly, with the help of 2
        -- getter functions. Helper function.
        correctPayment userIdGetter shareCalcFn bid = personGetsAda >= personWantsAda
          where
            info = scriptContextTxInfo ctx
            personId = getUserId . userIdGetter $ node
            share = info'share . node'information $ node
            personGetsAda = getAda $ valuePaidTo info personId
            personWantsAda = getAda $ shareCalcFn bid share

        ------------------------------------------------------------------------------
        -- Checks

        consistentDatumBuy = case prevDatum of
          NodeDatum prevNode ->
            on (==) node'next prevNode node
              && on (==) node'appInstance prevNode node
              && on (==) (info'author . node'information) prevNode node
              && on (==) (info'share . node'information) prevNode node
              && on (==) (info'id . node'information) prevNode node
          _ -> False

        -- Check if nft is for sale (price is not Nothing)
        nftForSale = maybe False (isJust . info'price . node'information) prevNode'

        ownerIsAuthor = case prevDatum of
          NodeDatum oldNode -> (info'owner . node'information $ oldNode) == (info'author . node'information $ oldNode)
          _ -> False

        -- Check if author of NFT receives share
        correctPaymentAuthor = correctPayment (info'author . node'information) calculateAuthorShare

        -- Check if owner of NFT receives share
        correctPaymentOwner = correctPayment (info'owner . node'information) calculateOwnerShare

        -- Check if author of NFT receives share when is also owner
        correctPaymentOnlyAuthor bid = authorGetsAda >= bid
          where
            info = scriptContextTxInfo ctx
            author = getUserId . info'author . node'information $ node
            authorGetsAda = getAda $ valuePaidTo info author

        -- Check if buy bid is higher or equal than price
        bidHighEnough bid = maybe False (maybe False (>= bid) . info'price . node'information) prevNode'

        -- Check if the datum attached is also present in the set price transaction.
        correctDatumSetPrice :: Bool
        correctDatumSetPrice =
          let datums :: [DatumNft] = getCtxDatum ctx
              nodes :: [NftListNode] =
                mapMaybe
                  ( \case
                      NodeDatum n -> Just n
                      _ -> Nothing
                  )
                  datums
              suitableDatums = filter (== info'id nInfo) . fmap (info'id . node'information) $ nodes
           in case suitableDatums of
                [_] -> True
                _ -> False

        consistentDatumSetPrice = case prevDatum of
          NodeDatum prevNode ->
            on (==) node'next prevNode node
              && on (==) node'appInstance prevNode node
              && on (==) (info'author . node'information) prevNode node
              && on (==) (info'owner . node'information) prevNode node
              && on (==) (info'share . node'information) prevNode node
              && on (==) (info'id . node'information) prevNode node
          _ -> False

        -- Check if the price of NFT is changed by the owner of NFT
        ownerSetsPrice =
          case txInfoSignatories $ scriptContextTxInfo ctx of
            [pkh] -> pkh == getUserId (info'owner $ node'information node)
            _ -> False

        -- Check if new price non-negative.
        priceNotNegative' = priceNotNegative (act'newPrice act)

        -- Check if no new token is minted.
        noMint = isZero . txInfoMint . scriptContextTxInfo $ ctx

        -- Check if the NFT is sent to the correct address.
        tokenSentToCorrectAddress = containsNft $ foldMap txOutValue (getContinuingOutputs ctx)

        -- Check if the previous Tx containing the token is consumed.
        prevTxConsumed =
          case findOwnInput ctx of
            Just (TxInInfo _ out) -> containsNft $ txOutValue out
            Nothing -> False

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
curSymbol :: NftAppInstance -> CurrencySymbol
curSymbol appInstance = scriptCurrencySymbol $ mintPolicy appInstance

{-# INLINEABLE nftCurrency #-}

-- | Calculate the NFT `CurrencySymbol` from NftId.
nftCurrency :: DatumNft -> CurrencySymbol
nftCurrency = \case
  HeadDatum x -> curSymbol $ head'appInstance x
  NodeDatum x -> curSymbol $ node'appInstance x

{-# INLINEABLE nftAsset #-}

-- | Calculate the NFT `AssetClass` from Datum.
nftAsset :: DatumNft -> AssetClass
nftAsset datum =
  AssetClass
    ( nftCurrency datum
    , nftTokenName datum
    )

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

{-# INLINEABLE getCtxDatum #-}

-- | Get datum form script context
getCtxDatum :: PlutusTx.FromData a => ScriptContext -> [a]
getCtxDatum =
  catMaybes'
    . fmap PlutusTx.fromBuiltinData
    . fmap (\(Datum d) -> d)
    . fmap snd
    . txInfoData
    . scriptContextTxInfo
