{-# LANGUAGE BangPatterns #-}
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
  curSymbol,
) where

import PlutusTx.Prelude

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
  ValidatorHash,
  contains,
  findDatum,
  findOwnInput,
  from,
  getContinuingOutputs,
  mkMintingPolicyScript,
  ownCurrencySymbol,
  scriptContextTxInfo,
  scriptCurrencySymbol,
  to,
  txInInfoResolved,
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

import Data.Function (on)
import Ledger.Value (
  TokenName (..),
  assetClass,
  flattenValue,
  singleton,
  valueOf,
 )
import Plutus.V1.Ledger.Value (AssetClass (..), Value (..), assetClassValueOf, isZero)
import PlutusTx qualified

import Data.Maybe (catMaybes)
import Data.Tuple.Extra (uncurry3)
import Mlabs.NFT.Types (
  AuctionBid (..),
  AuctionState (..),
  DatumNft (..),
  InformationNft (
    info'auctionState,
    info'author,
    info'id,
    info'owner,
    info'price,
    info'share
  ),
  MintAct (Initialise, Mint),
  NftAppInstance (appInstance'Address, appInstance'AppAssetClass),
  NftAppSymbol (app'symbol),
  NftId (..),
  NftListHead (head'appInstance),
  NftListNode (node'appInstance, node'information, node'next),
  Pointer (pointer'assetClass),
  UserAct (..),
  UserId (getUserId),
  getAppInstance,
  getDatumPointer,
  nftTokenName,
 )

asRedeemer :: PlutusTx.ToData a => a -> Redeemer
asRedeemer = Redeemer . PlutusTx.toBuiltinData

{-# INLINEABLE mkMintPolicy #-}

-- | Minting policy for NFTs.
mkMintPolicy :: NftAppInstance -> MintAct -> ScriptContext -> Bool
mkMintPolicy !appInstance !act !ctx =
  case act of
    Mint nftid ->
      traceIfFalse "Only pointer of first node can change." firstChangedOnlyPtr
        && traceIfFalse "Exactly one NFT must be minted" (checkMintedAmount nftid)
        && traceIfFalse "Old first node must point to second node." (first `pointsTo'` second)
        && traceIfFalse "New first node must point to new node." (newFirst `pointsTo` newInserted)
        && traceIfFalse "New node must point to second node." (newInserted `pointsTo'` second)
        && traceIfFalse "New node must be smaller than second node." newIsSmallerThanSecond
        && traceIfFalse "New price cannot be negative." priceNotNegative'
        && traceIfFalse "Currency symbol must match app instance" checkCurrencySymbol
        && traceIfFalse "Minted token must be sent to script address" (checkSentAddress nftid)
        && traceIfFalse "Nodes must be sent to script address" checkNodesAddresses
        && traceIfFalse "Datum is not atttached to UTXo with correct Token" (checkAttachedDatum nftid)
    Initialise ->
      traceIfFalse "The token is not present." True -- todo
        && traceIfFalse "Only One Unique Token Can be Minted" True -- todo
        && traceIfFalse "Only an Admin can initialise App." True -- todo
        && traceIfFalse "The token is not sent to the right address" True -- todo
  where
    ------------------------------------------------------------------------------
    -- Helpers

    !info = scriptContextTxInfo ctx
    !scriptAddress = appInstance'Address appInstance

    sentToScript TxOut {..} = txOutAddress == scriptAddress

    sort2 (x, y) = if x < y then (x, y) else (y, x)

    (newFirst, newInserted) = case getOutputDatums ctx of
      [x, y] -> sort2 (x, y)
      [_] -> traceError "Expected exactly two outputs with datums. Receiving one."
      [] -> traceError "Expected exactly two outputs with datums. Receiving none."
      _ -> traceError "Expected exactly two outputs with datums. Receiving more."

    first = case getInputDatums ctx of
      [x] -> x
      [] -> traceError "Expected exactly one input with datums. Receiving none."
      _ -> traceError "Expected exactly one input with datums. Receiving more."

    second = getDatumPointer first

    pointsTo d1 d2 = case (d1, d2) of
      (_, NodeDatum _) -> case getDatumPointer d1 of
        Just ptr -> (== nftTokenName d2) . snd . unAssetClass . pointer'assetClass $ ptr
        Nothing -> False
      _ -> False

    pointsTo' :: DatumNft -> Maybe Pointer -> Bool
    pointsTo' !datum !pointer = getDatumPointer datum == pointer

    ------------------------------------------------------------------------------
    -- Checks

    -- Check if nodes are sent back to script address
    checkNodesAddresses =
      let txs :: [TxOut] =
            fmap snd
              . getOutputDatumsWithTx @DatumNft
              $ ctx
       in all sentToScript txs

    -- Check if price is positive
    priceNotNegative' = case newInserted of
      NodeDatum node -> priceNotNegative (info'price . node'information $ node)
      _ -> False

    -- Check if minted NFT is sent to script address
    checkSentAddress nftId =
      let currency = ownCurrencySymbol ctx
          tokenName = TokenName . nftId'contentHash $ nftId
       in maybe
            False
            sentToScript
            ( find (\TxOut {..} -> valueOf txOutValue currency tokenName == 1) $
                txInfoOutputs info
            )

    newIsSmallerThanSecond = case second of
      Nothing -> True
      Just ptr -> (> nftTokenName newInserted) . snd . unAssetClass . pointer'assetClass $ ptr

    -- Check if currency symbol is consistent
    checkCurrencySymbol =
      getAppInstance first == appInstance
        && getAppInstance newInserted == appInstance

    -- Check if minting only one token
    checkMintedAmount nftid =
      let currency = ownCurrencySymbol ctx
          tokenName = TokenName . nftId'contentHash $ nftid
       in txInfoMint info == singleton currency tokenName 1

    -- Check if only thing changed in first node is `next` pointer
    firstChangedOnlyPtr = case (first, newFirst) of
      (NodeDatum node1, NodeDatum node2) ->
        node'appInstance node1 == node'appInstance node2
          && node'information node1 == node'information node2
      (HeadDatum node1, HeadDatum node2) ->
        head'appInstance node1 == head'appInstance node2
      _ -> False

    -- Check if Datum and Token id matches
    checkAttachedDatum nftId =
      let snd3 (_, y, _) = y
          mintedId =
            NftId
              . unTokenName
              . snd3
              . head
              . flattenValue
              . txInfoMint
              . scriptContextTxInfo
              $ ctx
       in case newInserted of
            HeadDatum _ -> False
            NodeDatum node ->
              let datumId = info'id . node'information $ node
               in mintedId == datumId && datumId == nftId

mintPolicy :: NftAppInstance -> MintingPolicy
mintPolicy appInstance =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||wrapMintingPolicy . mkMintPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode appInstance

{-# INLINEABLE mkTxPolicy #-}

-- | A validator script for the user actions.
mkTxPolicy :: DatumNft -> UserAct -> ScriptContext -> Bool
mkTxPolicy datum' act ctx =
  -- Gov Management
  traceIfFalse "Incorrect amount of Gov/xGov Minted" checkGovMinted -- toDo
    && traceIfFalse "Gov sent to wrong address" checkGovSentCorrectly -- toDo
    -- Fee Management
    && traceIfFalse "Correct Fee must be paid to the correct address." checkFeePaid -- toDo
    -- NFT Logic
    && case act of
      --
      -- Mint
      MintAct {} -> case datum' of
        HeadDatum headDat ->
          traceIfFalse "Proof Token must be paid back when using Head" (proofPaidBack headDat)
            && traceIfFalse "Transaction that uses Head as list proof must return it unchanged." (headUnchanged headDat)
        NodeDatum _ ->
          traceIfFalse "NFT sent to wrong address." tokenSentToCorrectAddress
            && traceIfFalse "Transaction can only use one NftListNode element as uniqueness proof." onlyOneNodeAttached
            && traceIfFalse "Not all used tokens are returned." checkTokenReturned
            && traceIfFalse "Returned Token UTXOs have mismatching datums." checkMissMatchDatumMint
      --
      -- Buy
      BuyAct {..} -> case datum' of
        NodeDatum node ->
          traceIfFalse "Transaction cannot mint." noMint
            && traceIfFalse "NFT not for sale." nftForSale
            && traceIfFalse "New Price cannot be negative." (priceNotNegative act'newPrice)
            && traceIfFalse "Act'Bid is too low for the NFT price." (bidHighEnough act'bid)
            && traceIfFalse "Datum is not consistent, illegaly altered." (consistentDatumBuy node)
            && traceIfFalse "Only one Node must be used in a Buy Action." onlyOneNodeAttached
            && traceIfFalse "Not all used Tokens are returned." checkTokenReturned
            && traceIfFalse "Returned Token UTXO has mismatching datum." checkMissMatchDatum
            && if ownerIsAuthor
              then traceIfFalse "Amount paid to author/owner does not match act'bid." (correctPaymentOnlyAuthor node act'bid)
              else
                traceIfFalse "Current owner is not paid their share." (correctPaymentOwner node act'bid)
                  && traceIfFalse "Author is not paid their share." (correctPaymentAuthor node act'bid)
        HeadDatum _ ->
          traceError "Cannot buy or set price of Head."
      --
      -- SetPrice
      --
      SetPriceAct {..} -> case datum' of
        NodeDatum node ->
          traceIfFalse "Transaction cannot mint." noMint
            && traceIfFalse "Datum does not correspond to NFTId, no datum is present, or more than one suitable datums are present." (correctDatumSetPrice node)
            && traceIfFalse "New Price cannot be negative." (priceNotNegative act'newPrice)
            && traceIfFalse "Only owner exclusively can set NFT price." (signedByOwner node)
            && traceIfFalse "Datum is not consistent, illegaly altered." (consistentDatumSetPrice node)
            && traceIfFalse "Only one Node must be used in a SetPrice Action." onlyOneNodeAttached
            && traceIfFalse "Not all used Tokens are returned." checkTokenReturned
            && traceIfFalse "Returned Token UTXO has mismatching datum." checkMissMatchDatum
        HeadDatum _ ->
          traceError "Cannot buy or set price of Head."
      --
      -- Open Auction
      OpenAuctionAct {} -> case datum' of
        NodeDatum node ->
          traceIfFalse "Can't open auction: already in progress" (noAuctionInProgress node)
            && traceIfFalse "Only owner can open auction" (signedByOwner node)
            && traceIfFalse "Auction: datum illegally altered" (auctionConsistentOpenDatum node)
        HeadDatum _ ->
          traceError "Cannot open Auction for Head."
      --
      -- Bid
      BidAuctionAct {..} -> case datum' of
        NodeDatum node ->
          traceIfFalse "Can't bid: No auction is in progress" (not . noAuctionInProgress $ node)
            && traceIfFalse "Auction bid is too low" (auctionBidHighEnough node act'bid)
            && traceIfFalse "Auction deadline reached" (correctAuctionBidSlotInterval node)
            && traceIfFalse "Auction: wrong input value" (correctInputValue node)
            && traceIfFalse "Auction: datum illegally altered" (auctionConsistentDatum node act'bid)
            && traceIfFalse "Auction bid value not supplied" (auctionBidValueSupplied act'bid)
            && traceIfFalse "Incorrect bid refund" (correctBidRefund node)
        HeadDatum _ ->
          traceError "Cannot Bid for List Head."
      --
      -- Close Auction
      CloseAuctionAct {} -> case datum' of
        HeadDatum _ -> traceError "Cannot Close Bid for  Head."
        NodeDatum node ->
          traceIfFalse "Can't close auction: none in progress" (not . noAuctionInProgress $ node)
            && traceIfFalse "Auction deadline not yet reached" (auctionDeadlineReached node)
            && traceIfFalse "Only owner can close auction" (signedByOwner node)
            && traceIfFalse "Auction: new owner set incorrectly" (auctionCorrectNewOwner node)
            && traceIfFalse "Auction: datum illegally altered" (auctionConsistentCloseDatum node)
            && if ownerIsAuthor
              then traceIfFalse "Auction: amount paid to author/owner does not match bid" (auctionCorrectPaymentOnlyAuthor node)
              else
                traceIfFalse "Auction: owner not paid their share" (auctionCorrectPaymentOwner node)
                  && traceIfFalse "Auction: author not paid their share" (auctionCorrectPaymentAuthor node)
  where
    ------------------------------------------------------------------------------
    -- Utils
    info = scriptContextTxInfo ctx

    !nInfo = node'information

    oldNode :: NftListNode = case getNode oldDatum of
      Just n -> n
      Nothing -> traceError "Input datum is Head."

    !mauctionState = info'auctionState . nInfo
    ------------------------------------------------------------------------------
    -- Gov And Fee Management
    checkGovMinted = True

    checkGovSentCorrectly = True

    checkFeePaid = True

    ------------------------------------------------------------------------------
    tokenValue :: Value
    tokenValue = singleton (app'symbol . act'symbol $ act) (nftTokenName datum') 1

    oldDatum :: DatumNft = head . getInputDatums $ ctx

    oldHead :: NftListHead = case oldDatum of
      HeadDatum h -> h
      _ -> traceError "Input datum is Node."

    proofPaidBack headDat =
      let (currency, tokenName) = unAssetClass . appInstance'AppAssetClass $ head'appInstance headDat
          paysBack tx = valueOf (txOutValue tx) currency tokenName == 1
       in any paysBack . txInfoOutputs . scriptContextTxInfo $ ctx

    headUnchanged = (oldHead ==)
    ------------------------------------------------------------------------------
    -- Utility functions.

    sort2On f (x, y) = if f x < f y then (x, y) else (y, x)

    containsNft !v = valueOf v (app'symbol . act'symbol $ act) (nftTokenName datum') == 1

    !getAda = flip assetClassValueOf $ assetClass Ada.adaSymbol Ada.adaToken

    -- Check if the Person is being reimbursed accordingly, with the help of 2
    -- getter functions. Helper function.
    correctPayment node !userIdGetter !shareCalcFn !bid = personGetsAda >= personWantsAda
      where
        personId = getUserId . userIdGetter $ node
        share = info'share . node'information $ node
        personGetsAda = getAda $ valuePaidTo info personId
        personWantsAda = getAda $ shareCalcFn bid share

    !ownerIsAuthor =
      (info'owner . node'information $ oldNode) == (info'author . node'information $ oldNode)

    getNode = \case
      NodeDatum n -> Just n
      _ -> Nothing

    withAuctionState node f = maybe (traceError "Auction state expected") f (mauctionState node)

    convDatum :: Datum -> Maybe DatumNft
    convDatum (Datum d) = PlutusTx.fromBuiltinData d

    newDatum :: DatumNft
    newDatum =
      case getContinuingOutputs ctx of
        [out] ->
          case txOutDatumHash out of
            Nothing -> traceError "getNextDatum: expected datum hash"
            Just dhash ->
              case findDatum dhash info >>= convDatum of
                Nothing -> traceError "getNextDatum: expected datum"
                Just dt -> dt
        [] -> traceError "nextDatum: expected exactly one continuing output, got none"
        _ -> traceError "nextDatum: expected exactly one continuing output, got several instead"

    newNodeInfo :: InformationNft
    newNodeInfo =
      case newDatum of
        HeadDatum _ -> traceError "nextNodeInfo: expected NodeDatum, got HeadDatum instead"
        NodeDatum listNode -> node'information listNode

    -- Check if Datum id matches NFT id in UTXO
    checkTxDatumMatch nodeDatum tx =
      let cur = app'symbol . act'symbol $ act
          tn = TokenName . nftId'contentHash . info'id . node'information $ nodeDatum
       in valueOf (txOutValue tx) cur tn == 1

    ------------------------------------------------------------------------------
    -- Checks

    -- Check whether there's auction in progress and disallow buy/setprice actions.
    noAuctionInProgress :: NftListNode -> Bool
    noAuctionInProgress = isNothing . mauctionState

    auctionBidHighEnough :: NftListNode -> Integer -> Bool
    auctionBidHighEnough node amount =
      withAuctionState node $ \auctionState ->
        case as'highestBid auctionState of
          Nothing -> amount >= as'minBid auctionState
          Just highestBid -> amount > ab'bid highestBid

    correctAuctionBidSlotInterval :: NftListNode -> Bool
    correctAuctionBidSlotInterval node =
      withAuctionState node $ \auctionState ->
        to (as'deadline auctionState) `contains` txInfoValidRange info

    auctionDeadlineReached :: NftListNode -> Bool
    auctionDeadlineReached node =
      withAuctionState node $ \auctionState ->
        from (as'deadline auctionState) `contains` txInfoValidRange info

    auctionCorrectPayment :: NftListNode -> (Integer -> Bool) -> Bool
    auctionCorrectPayment node correctPaymentCheck =
      withAuctionState node $ \auctionState ->
        case as'highestBid auctionState of
          Nothing -> True
          Just (AuctionBid bid _bidder) ->
            correctPaymentCheck bid

    auctionCorrectPaymentOwner node = auctionCorrectPayment node $ correctPaymentOwner node

    auctionCorrectPaymentAuthor node = auctionCorrectPayment node $ correctPaymentAuthor node

    auctionCorrectPaymentOnlyAuthor :: NftListNode -> Bool
    auctionCorrectPaymentOnlyAuthor node =
      withAuctionState node $ \auctionState ->
        case as'highestBid auctionState of
          Nothing -> True
          Just (AuctionBid bid _) ->
            correctPaymentOnlyAuthor node bid

    correctBidRefund :: NftListNode -> Bool
    correctBidRefund node =
      withAuctionState node $ \auctionState ->
        case as'highestBid auctionState of
          Nothing -> True
          Just (AuctionBid bid bidder) ->
            valuePaidTo info (getUserId bidder) == Ada.lovelaceValueOf bid

    correctInputValue :: NftListNode -> Bool
    correctInputValue node =
      case findOwnInput ctx of
        Nothing -> traceError "findOwnInput: Nothing"
        Just (TxInInfo _ out) ->
          case mauctionState node of
            Nothing -> traceError "mauctionState: Nothing"
            Just as -> case as'highestBid as of
              Nothing -> tokenValue == txOutValue out
              Just hb -> txOutValue out == (tokenValue <> Ada.lovelaceValueOf (ab'bid hb))

    auctionBidValueSupplied :: Integer -> Bool
    auctionBidValueSupplied redeemerBid =
      case getContinuingOutputs ctx of
        [out] -> txOutValue out == tokenValue <> Ada.lovelaceValueOf redeemerBid
        [] -> traceError "auctionBidValueSupplied: expected exactly one continuing output, got none"
        _ -> traceError "auctionBidValueSupplied: expected exactly one continuing output, got several instead"

    auctionCorrectNewOwner :: NftListNode -> Bool
    auctionCorrectNewOwner node =
      withAuctionState node $ \auctionState ->
        case as'highestBid auctionState of
          Nothing -> True
          Just (AuctionBid _ bidder) ->
            bidder == newOwner
      where
        newOwner = info'owner newNodeInfo

    auctionConsistentCloseDatum node =
      -- Checking that all fields remain the same except owner
      info'id newNodeInfo == info'id nInfo'
        && info'share newNodeInfo == info'share nInfo'
        && info'author newNodeInfo == info'author nInfo'
        && info'price newNodeInfo == info'price nInfo'
        && checkOwner
      where
        nInfo' = nInfo node

        checkOwner = withAuctionState node $ \auctionState ->
          case as'highestBid auctionState of
            Nothing -> info'owner newNodeInfo == info'owner nInfo'
            _ -> True

    auctionConsistentOpenDatum :: NftListNode -> Bool
    auctionConsistentOpenDatum node =
      -- Checking that all fields remain the same except auctionState
      info'id newNodeInfo == info'id nInfo'
        && info'share newNodeInfo == info'share nInfo'
        && info'author newNodeInfo == info'author nInfo'
        && info'owner newNodeInfo == info'owner nInfo'
        && info'price newNodeInfo == info'price nInfo'
      where
        nInfo' = nInfo node

    auctionConsistentDatum :: NftListNode -> Integer -> Bool
    auctionConsistentDatum node redeemerBid =
      let nInfo' = nInfo node

          checkAuctionState =
            case (info'auctionState newNodeInfo, info'auctionState nInfo') of
              ( Just (AuctionState _ nextDeadline nextMinBid)
                , Just (AuctionState _ deadline minBid)
                ) ->
                  nextDeadline == deadline && nextMinBid == minBid
              _ -> traceError "auctionConsistentDatum (checkAauctionState): expected auction state"

          checkHighestBid =
            case (info'auctionState newNodeInfo, info'auctionState nInfo') of
              ( Just (AuctionState (Just (AuctionBid nextBid _)) _ _)
                , Just (AuctionState (Just (AuctionBid bid _)) _ _)
                ) ->
                  nextBid > bid && nextBid == redeemerBid
              ( Just (AuctionState (Just (AuctionBid nextBid _)) _ _)
                , Just (AuctionState Nothing _ minBid)
                ) ->
                  nextBid >= minBid && nextBid == redeemerBid
              _ -> traceError "auctionConsistentDatum (checkHighestBid): expected auction state"
       in info'id newNodeInfo == info'id nInfo'
            && info'share newNodeInfo == info'share nInfo'
            && info'author newNodeInfo == info'author nInfo'
            && info'owner newNodeInfo == info'owner nInfo'
            && info'price newNodeInfo == info'price nInfo'
            && checkAuctionState
            && checkHighestBid

    -- Check if changed only owner and price
    consistentDatumBuy node =
      on (==) node'next oldNode node
        && on (==) node'appInstance oldNode node
        && on (==) (info'author . node'information) oldNode node
        && on (==) (info'share . node'information) oldNode node
        && on (==) (info'id . node'information) oldNode node

    -- Check if nft is for sale (price is not Nothing)
    !nftForSale = isJust . info'price . node'information $ oldNode

    -- Check if author of NFT receives share
    correctPaymentAuthor :: NftListNode -> Integer -> Bool
    correctPaymentAuthor node = correctPayment node (info'author . node'information) calculateAuthorShare

    -- Check if owner of NFT receives share
    correctPaymentOwner :: NftListNode -> Integer -> Bool
    correctPaymentOwner node = correctPayment node (info'owner . node'information) calculateOwnerShare

    -- Check if author of NFT receives share when is also owner
    correctPaymentOnlyAuthor !node !bid = authorGetsAda >= bid
      where
        author = getUserId . info'author . node'information $ node
        authorGetsAda = getAda $ valuePaidTo info author

    -- Check if buy bid is higher or equal than price
    bidHighEnough !bid = case info'price . node'information $ oldNode of
      Nothing -> False -- NFT not for sale.
      Just price -> price <= bid

    -- Check if the datum attached is also present in the set price transaction.
    correctDatumSetPrice !node = (== (info'id $ nInfo node)) . info'id . node'information $ oldNode

    -- Check if only thing changed in nodes is price
    consistentDatumSetPrice !node =
      on (==) node'next oldNode node
        && on (==) node'appInstance oldNode node
        && on (==) (info'author . node'information) oldNode node
        && on (==) (info'owner . node'information) oldNode node
        && on (==) (info'share . node'information) oldNode node
        && on (==) (info'id . node'information) oldNode node

    -- Check if the price of NFT is changed by the owner of NFT
    signedByOwner !node =
      case txInfoSignatories $ scriptContextTxInfo ctx of
        [pkh] -> pkh == getUserId (info'owner $ node'information node)
        _ -> False

    -- Check if no new token is minted.
    !noMint = isZero . txInfoMint . scriptContextTxInfo $ ctx

    -- Check if the NFT is sent to the correct address.
    !tokenSentToCorrectAddress =
      let addr = appInstance'Address . node'appInstance $ oldNode
          sentBack tx = txOutAddress tx == addr
       in all sentBack $ filter (containsNft . txOutValue) (txInfoOutputs . scriptContextTxInfo $ ctx)

    -- Check if exactly two Datums are attached to Mint output, and ids matches
    !checkMissMatchDatumMint = case getOutputDatumsWithTx @DatumNft ctx of
      [x, y] -> case sort2On fst (x, y) of
        ((HeadDatum _, _), (NodeDatum datum2, tx2)) -> checkTxDatumMatch datum2 tx2
        ((NodeDatum datum1, tx1), (NodeDatum datum2, tx2)) ->
          checkTxDatumMatch datum1 tx1 && checkTxDatumMatch datum2 tx2
        _ -> False
      _ -> False

    -- Check if exactly one Node is attached to outputs, and ids matches
    !checkMissMatchDatum = case getOutputDatumsWithTx @DatumNft ctx of
      [(NodeDatum datum, tx)] -> checkTxDatumMatch datum tx
      _ -> False

    -- Check if exactly one Node is attached to inputs, and ids matches
    !onlyOneNodeAttached = case getInputDatumsWithTx @DatumNft ctx of
      [(NodeDatum datum, tx)] -> checkTxDatumMatch datum tx
      _ -> False

    -- Check if all tokens from input and mint are returnded
    !checkTokenReturned =
      let cur = app'symbol . act'symbol $ act
          fst3 (x, _, _) = x
          getNfts =
            mconcat
              . fmap (uncurry3 singleton)
              . filter ((== cur) . fst3)
              . flattenValue
              . mconcat
              . fmap txOutValue
          inNfts =
            getNfts
              . fmap txInInfoResolved
              . txInfoInputs
              . scriptContextTxInfo
              $ ctx
          outNfts =
            getNfts
              . txInfoOutputs
              . scriptContextTxInfo
              $ ctx
          mintedNfts =
            txInfoMint
              . scriptContextTxInfo
              $ ctx
       in (inNfts <> mintedNfts) == outNfts

{-# INLINEABLE catMaybes' #-}
catMaybes' :: [Maybe a] -> [a]
catMaybes' = catMaybes

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

{-# INLINEABLE getInputDatums #-}

-- | Retuns datums attached to inputs of transaction
getInputDatums :: PlutusTx.FromData a => ScriptContext -> [a]
getInputDatums = fmap fst . getInputDatumsWithTx

{-# INLINEABLE getInputDatumsWithTx #-}

-- | Retuns datums aand corresponding UTXOs attached to inputs of transaction
getInputDatumsWithTx :: PlutusTx.FromData a => ScriptContext -> [(a, TxOut)]
getInputDatumsWithTx ctx =
  mapMaybe (\(datum, tx) -> (,) <$> (PlutusTx.fromBuiltinData . getDatum $ datum) <*> pure tx)
    . mapMaybe (\(hash, tx) -> (,) <$> findDatum hash (scriptContextTxInfo ctx) <*> pure tx)
    . mapMaybe ((\tx -> (,) <$> txOutDatumHash tx <*> pure tx) . txInInfoResolved)
    . txInfoInputs
    . scriptContextTxInfo
    $ ctx

{-# INLINEABLE getOutputDatums #-}

-- | Retuns datums attached to outputs of transaction
getOutputDatums :: PlutusTx.FromData a => ScriptContext -> [a]
getOutputDatums = fmap fst . getOutputDatumsWithTx

{-# INLINEABLE getOutputDatumsWithTx #-}

-- | Returns datums and coresponding UTXOs attached to outputs of transaction
getOutputDatumsWithTx :: PlutusTx.FromData a => ScriptContext -> [(a, TxOut)]
getOutputDatumsWithTx ctx =
  mapMaybe (\(datum, tx) -> (,) <$> (PlutusTx.fromBuiltinData . getDatum $ datum) <*> pure tx)
    . mapMaybe (\(hash, tx) -> (,) <$> findDatum hash (scriptContextTxInfo ctx) <*> pure tx)
    . mapMaybe (\tx -> (,) <$> txOutDatumHash tx <*> pure tx)
    . txInfoOutputs
    . scriptContextTxInfo
    $ ctx
