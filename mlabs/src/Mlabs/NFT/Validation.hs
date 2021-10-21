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

import Ledger.Value (
  TokenName (..),
  assetClass,
  flattenValue,
  singleton,
  valueOf,
 )
import Plutus.V1.Ledger.Value (AssetClass (AssetClass), assetClassValueOf, isZero)
import PlutusTx qualified
import Schema (ToSchema)

import Mlabs.NFT.Types

asRedeemer :: PlutusTx.ToData a => a -> Redeemer
asRedeemer = Redeemer . PlutusTx.toBuiltinData

{-# INLINEABLE mkMintPolicy #-}

-- | Minting policy for NFTs.
mkMintPolicy :: NftAppInstance -> MintAct -> ScriptContext -> Bool
mkMintPolicy _ act _ =
  traceIfFalse "Only One Token Can be Minted" True
    && case act of
      Mint nftid ->
        traceIfFalse "NFT Minting failed." True
          && traceIfFalse "Neighbouring transactions are not present." True
          && traceIfFalse "The token is not sent to the right address" True
      Initialise ->
        traceIfFalse "The token is not present." True
          && traceIfFalse "The initial token is not being consumed." True
          && traceIfFalse "The token is not sent to the right address" True

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
    HeadDatum _ -> traceError "Head Datum should not be provided"
    NodeDatum node ->
      traceIfFalse "New Price cannot be negative." priceNotNegative'
        && traceIfFalse "Previous TX is not consumed." prevTxConsumed
        && traceIfFalse "NFT sent to wrong address." tokenSentToCorrectAddress
        && traceIfFalse "Transaction cannot mint." noMint
        && case act of
          BuyAct {..} -> True -- TODO implement it
          SetPriceAct {} ->
            traceIfFalse "Datum does not correspond to NFTId, no datum is present, or more than one suitable datums are present." correctDatumSetPrice
              && traceIfFalse "Only owner exclusively can set NFT price." ownerSetsPrice
              && traceIfFalse "Datum is not consistent, illegaly altered." consistentDatumSetPrice
      where
        nInfo = node'information node
        nNext = node'next node
        nAppInstance = node'appInstance node
        ------------------------------------------------------------------------------
        -- Utility functions.

        -- Get datum form script context
        getCtxDatum :: PlutusTx.FromData a => ScriptContext -> [a]
        getCtxDatum =
          catMaybes'
            . fmap PlutusTx.fromBuiltinData
            . fmap (\(Datum d) -> d)
            . fmap snd
            . txInfoData
            . scriptContextTxInfo

        containsNft v = valueOf v (instanceCurrency $ node'appInstance node) (nftTokenName datum') == 1

        ------------------------------------------------------------------------------
        -- Checks

        -- Check if the datum attached is also present in the set price transaction.
        correctDatumSetPrice :: Bool
        correctDatumSetPrice =
          let datums :: [DatumNft] = getCtxDatum ctx
              nodes :: [NftListNode] = mapMaybe (\d -> case d of NodeDatum n -> Just n; _ -> Nothing) datums
              suitableDatums = filter (== info'id nInfo) . fmap (info'id . node'information) $ nodes
           in case suitableDatums of
                [_] -> True
                _ -> False

        consistentDatumSetPrice =
          let prevDatum :: DatumNft = head . getCtxDatum $ ctx
           in case prevDatum of
                NodeDatum prevNode ->
                  node'next prevNode == node'next node
                    && node'appInstance prevNode == node'appInstance node
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
