{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-local-binds -Wno-unused-matches #-}

-- TODO remove after implementig fees
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
  -- findOwnInput,
  from,
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
  NftAppInstance (..),
  NftAppSymbol (app'symbol),
  NftId (..),
  NftListHead (head'appInstance),
  NftListNode (node'appInstance, node'information, node'next),
  Pointer (pointer'assetClass),
  UserAct (..),
  UserId (..),
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
      traceIfFalse "The token is not present." headTokenIsPresent
        && traceIfFalse "Only one Unique Token can be minted" headTokenIsUnique
        && traceIfFalse "The token is not sent to the right address" headTokenToRightAddress
        && traceIfFalse "Only an admin can initialise app." checkAdminSig
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

    !outputsWithHeadDatum =
      filter
        ( \(datum, _) ->
            case datum of
              HeadDatum _ -> True
              _ -> False
        )
        $ getOutputDatumsWithTx ctx

    -- Check if the head token is present
    headTokenIsPresent =
      let validValue (sym, _, _) = sym == ownCurrencySymbol ctx
          validHeadToken tx = any validValue $ flattenValue . txOutValue $ tx
       in any (validHeadToken . snd) outputsWithHeadDatum

    -- Check if the head token is spent to the right address
    headTokenToRightAddress =
      let validValue (sym, _, _) = sym == ownCurrencySymbol ctx
          validHeadToken tx =
            sentToScript tx
              && any validValue (flattenValue . txOutValue $ tx)
       in any (validHeadToken . snd) outputsWithHeadDatum

    -- Check the uniqueness of minted head token
    headTokenIsUnique =
      let validValue (sym, _, v) = (sym == ownCurrencySymbol ctx) && (v == 1)
          validHeadToken tx =
            sentToScript tx
              && any validValue (flattenValue . txOutValue $ tx)
       in any (validHeadToken . snd) outputsWithHeadDatum

    -- Check an admin signed the transaction
    checkAdminSig =
      let admins = appInstance'Admins appInstance
       in any (`elem` admins) $ UserId <$> txInfoSignatories info

mintPolicy :: NftAppInstance -> MintingPolicy
mintPolicy appInstance =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||wrapMintingPolicy . mkMintPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode appInstance

{-# INLINEABLE mkTxPolicy #-}

-- | A validator script for the user actions.
mkTxPolicy :: DatumNft -> UserAct -> ScriptContext -> Bool
mkTxPolicy !datum' !act !ctx =
  --  traceIfFalse "Fees must be paid" proofPaidBack &&
  case datum' of
    HeadDatum headDat -> case act of
      MintAct {} ->
        traceIfFalse "Proof Token must be paid back when using Head" proofPaidBack
          && traceIfFalse "Transaction that uses Head as list proof must return it unchanged." headUnchanged
      -- must always pay back the proof Token. This happens when the Head datum is
      -- updated as the utxo needs to be consumed
      _ -> traceError "Cannot buy or set price of Head."
      where
        oldDatum :: DatumNft = head . getInputDatums $ ctx

        oldHead :: NftListHead = case oldDatum of
          HeadDatum h -> h
          _ -> traceError "Input datum is Node."

        !proofPaidBack =
          let (currency, tokenName) = unAssetClass . appInstance'AppAssetClass . head'appInstance $ headDat
              paysBack tx = valueOf (txOutValue tx) currency tokenName == 1
           in any paysBack . txInfoOutputs . scriptContextTxInfo $ ctx

        !headUnchanged = oldHead == headDat
    NodeDatum node ->
      traceIfFalse "NFT sent to wrong address." tokenSentToCorrectAddress
        && case act of
          MintAct {} ->
            traceIfFalse "Transaction can only use one NftListNode element as uniqueness proof." onlyOneNodeAttached
              && traceIfFalse "Not all used tokens are returned." checkTokenReturned
              && traceIfFalse "Returned Token UTXOs have mismatching datums." checkMissMatchDatumMint
          BuyAct {..} ->
            traceIfFalse "Transaction cannot mint." True -- noMint TODO: allow minting Gov
              && traceIfFalse "NFT not for sale." nftForSale
              && traceIfFalse "New Price cannot be negative." (priceNotNegative act'newPrice)
              && traceIfFalse "Act'Bid is too low for the NFT price." (bidHighEnough act'bid)
              && traceIfFalse "Datum is not consistent, illegaly altered." consistentDatumBuy
              && traceIfFalse "Only one Node must be used in a Buy Action." onlyOneNodeAttached
              && traceIfFalse "Not all used Tokens are returned." True -- checkTokenReturned TODO: Fix for Gov mint
              && traceIfFalse "Returned Token UTXO has mismatching datum." checkMissMatchDatum
              && if ownerIsAuthor
                then traceIfFalse "Amount paid to author/owner does not match act'bid." True -- TODO (correctPaymentOnlyAuthor act'bid)
                else
                  traceIfFalse "Current owner is not paid their share." True -- TODO (correctPaymentOwner act'bid)
                    && traceIfFalse "Author is not paid their share." True -- TODO (correctPaymentAuthor act'bid)
          SetPriceAct {..} ->
            traceIfFalse "Transaction cannot mint." noMint
              && traceIfFalse "Datum does not correspond to NFTId, no datum is present, or more than one suitable datums are present." correctDatumSetPrice
              && traceIfFalse "New Price cannot be negative." (priceNotNegative act'newPrice)
              && traceIfFalse "Only owner exclusively can set NFT price." signedByOwner
              && traceIfFalse "Datum is not consistent, illegaly altered." consistentDatumSetPrice
              && traceIfFalse "Only one Node must be used in a SetPrice Action." onlyOneNodeAttached
              && traceIfFalse "Not all used Tokens are returned." checkTokenReturned
              && traceIfFalse "Returned Token UTXO has mismatching datum." checkMissMatchDatum
              && traceIfFalse "NFT is on auction" checkIsNotOnAuction
          OpenAuctionAct {} ->
            traceIfFalse "Can't open auction: already in progress" noAuctionInProgress
              && traceIfFalse "Only owner can open auction" signedByOwner
              && traceIfFalse "Open Auction: datum illegally altered" auctionConsistentOpenDatum
              && traceIfFalse "NFT price must be set to Nothing" checkPriceIsNothing
          BidAuctionAct {..} ->
            traceIfFalse "Can't bid: No auction is in progress" (not noAuctionInProgress)
              && traceIfFalse "Auction bid is too low" (auctionBidHighEnough act'bid)
              && traceIfFalse "Auction deadline reached" correctAuctionBidSlotInterval
              && traceIfFalse "Auction: wrong input value" correctInputValue
              && traceIfFalse "Bid Auction: datum illegally altered" (auctionConsistentDatum act'bid)
              && traceIfFalse "Auction bid value not supplied" True -- TODO (auctionBidValueSupplied act'bid)
              && traceIfFalse "Incorrect bid refund" correctBidRefund
          CloseAuctionAct {} ->
            traceIfFalse "Can't close auction: none in progress" (not noAuctionInProgress)
              && traceIfFalse "Auction deadline not yet reached" auctionDeadlineReached
              && traceIfFalse "Auction: new owner set incorrectly" auctionCorrectNewOwner
              && traceIfFalse "Close Auction: datum illegally altered" auctionConsistentCloseDatum
              && if ownerIsAuthor
                then traceIfFalse "Auction: amount paid to author/owner does not match bid" True -- TODO auctionCorrectPaymentOnlyAuthor
                else
                  traceIfFalse "Auction: owner not paid their share" auctionCorrectPaymentOwner
                    && traceIfFalse "Auction: author not paid their share" auctionCorrectPaymentAuthor
      where
        info = scriptContextTxInfo ctx

        !nInfo = node'information node
        oldDatum :: DatumNft = head . getInputDatums $ ctx

        oldNode :: NftListNode = case getNode oldDatum of
          Just n -> n
          Nothing -> traceError "Input datum is Head."

        !mauctionState = info'auctionState nInfo

        tokenValue :: Value
        tokenValue = singleton (app'symbol . act'symbol $ act) (nftTokenName datum') 1

        ------------------------------------------------------------------------------
        -- Utility functions.

        sort2On f (x, y) = if f x < f y then (x, y) else (y, x)

        containsNft !v = valueOf v (app'symbol . act'symbol $ act) (nftTokenName datum') == 1

        !getAda = flip assetClassValueOf $ assetClass Ada.adaSymbol Ada.adaToken

        -- Check if the Person is being reimbursed accordingly, with the help of 2
        -- getter functions. Helper function.
        correctPayment !userIdGetter !shareCalcFn !bid = personGetsAda >= personWantsAda
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

        withAuctionState f = maybe (traceError "Auction state expected") f mauctionState

        newDatum = case getOutputDatums ctx of
          [x] -> x
          [] -> traceError "Expected exactly one input with datums. Receiving none."
          _ -> traceError "Expected exactly one input with datums. Receiving more."

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
        noAuctionInProgress :: Bool
        noAuctionInProgress = isNothing mauctionState

        auctionBidHighEnough :: Integer -> Bool
        auctionBidHighEnough amount =
          withAuctionState $ \auctionState ->
            case as'highestBid auctionState of
              Nothing -> amount >= as'minBid auctionState
              Just highestBid -> amount > ab'bid highestBid

        correctAuctionBidSlotInterval :: Bool
        correctAuctionBidSlotInterval =
          withAuctionState $ \auctionState ->
            to (as'deadline auctionState) `contains` txInfoValidRange info

        auctionDeadlineReached :: Bool
        auctionDeadlineReached =
          withAuctionState $ \auctionState ->
            from (as'deadline auctionState) `contains` txInfoValidRange info

        auctionCorrectPayment :: (Integer -> Bool) -> Bool
        auctionCorrectPayment correctPaymentCheck =
          withAuctionState $ \auctionState ->
            case as'highestBid auctionState of
              Nothing -> True
              Just (AuctionBid bid _bidder) ->
                correctPaymentCheck bid

        auctionCorrectPaymentOwner :: Bool
        auctionCorrectPaymentOwner = True -- FIXME auctionCorrectPayment correctPaymentOwner
        auctionCorrectPaymentAuthor :: Bool
        auctionCorrectPaymentAuthor = True -- FIXME auctionCorrectPayment correctPaymentAuthor

        -- auctionCorrectPaymentOnlyAuthor :: Bool
        -- auctionCorrectPaymentOnlyAuthor =
        --   withAuctionState $ \auctionState ->
        --     case as'highestBid auctionState of
        --       Nothing -> True
        --       Just (AuctionBid bid _) ->
        --         correctPaymentOnlyAuthor bid

        correctBidRefund :: Bool
        correctBidRefund = True
        -- FIXME: Check is broken ?
        -- withAuctionState $ \auctionState ->
        --   case as'highestBid auctionState of
        --     Nothing -> True
        --     Just (AuctionBid bid bidder) ->
        --       valuePaidTo info (getUserId bidder) == Ada.lovelaceValueOf bid

        correctInputValue :: Bool
        correctInputValue = True
        -- FIXME
        -- case findOwnInput ctx of
        --   Nothing -> traceError "findOwnInput: Nothing"
        --   Just (TxInInfo _ out) ->
        --     case mauctionState of
        --       Nothing -> traceError "mauctionState: Nothing"
        --       Just as -> case as'highestBid as of
        --         Nothing -> tokenValue == txOutValue out
        --         Just hb -> txOutValue out == (tokenValue <> Ada.lovelaceValueOf (ab'bid hb))

        auctionBidValueSupplied :: Integer -> Bool
        auctionBidValueSupplied redeemerBid =
          case fmap snd . getOutputDatumsWithTx @DatumNft $ ctx of
            [out] -> txOutValue out == tokenValue <> Ada.lovelaceValueOf redeemerBid
            [] -> traceError "auctionBidValueSupplied: expected exactly one continuing output, got none"
            _ -> traceError "auctionBidValueSupplied: expected exactly one continuing output, got several instead"

        auctionCorrectNewOwner :: Bool
        auctionCorrectNewOwner =
          withAuctionState $ \auctionState ->
            case as'highestBid auctionState of
              Nothing -> True
              Just (AuctionBid _ bidder) ->
                bidder == newOwner
          where
            newOwner = info'owner newNodeInfo

        auctionConsistentCloseDatum :: Bool
        auctionConsistentCloseDatum =
          -- Checking that all fields remain the same except owner
          info'id newNodeInfo == info'id nInfo
            && info'share newNodeInfo == info'share nInfo
            && info'author newNodeInfo == info'author nInfo
            && info'price newNodeInfo == info'price nInfo
            && checkOwner
          where
            checkOwner = withAuctionState $ \auctionState ->
              case as'highestBid auctionState of
                Nothing -> info'owner newNodeInfo == info'owner nInfo
                _ -> True

        auctionConsistentOpenDatum :: Bool
        auctionConsistentOpenDatum =
          -- Checking that all fields remain the same except auctionState
          info'id newNodeInfo == info'id nInfo
            && info'share newNodeInfo == info'share nInfo
            && info'author newNodeInfo == info'author nInfo
            && info'owner newNodeInfo == info'owner nInfo

        checkPriceIsNothing = isNothing . info'price $ newNodeInfo

        auctionConsistentDatum :: Integer -> Bool
        auctionConsistentDatum redeemerBid =
          let checkAuctionState =
                case (info'auctionState newNodeInfo, info'auctionState nInfo) of
                  ( Just (AuctionState _ nextDeadline nextMinBid)
                    , Just (AuctionState _ deadline minBid)
                    ) ->
                      nextDeadline == deadline && nextMinBid == minBid
                  _ -> traceError "auctionConsistentDatum (checkAauctionState): expected auction state"

              checkHighestBid =
                case (info'auctionState newNodeInfo, info'auctionState nInfo) of
                  ( Just (AuctionState (Just (AuctionBid nextBid _)) _ _)
                    , Just (AuctionState (Just (AuctionBid bid _)) _ _)
                    ) ->
                      nextBid > bid && nextBid == redeemerBid
                  ( Just (AuctionState (Just (AuctionBid nextBid _)) _ _)
                    , Just (AuctionState Nothing _ minBid)
                    ) ->
                      nextBid >= minBid && nextBid == redeemerBid
                  _ -> traceError "auctionConsistentDatum (checkHighestBid): expected auction state"
           in info'id newNodeInfo == info'id nInfo
                && info'share newNodeInfo == info'share nInfo
                && info'author newNodeInfo == info'author nInfo
                && info'owner newNodeInfo == info'owner nInfo
                && info'price newNodeInfo == info'price nInfo
                && checkAuctionState
                && checkHighestBid

        -- Check if changed only owner and price
        !consistentDatumBuy =
          on (==) node'next oldNode node
            && on (==) node'appInstance oldNode node
            && on (==) (info'author . node'information) oldNode node
            && on (==) (info'share . node'information) oldNode node
            && on (==) (info'id . node'information) oldNode node

        -- Check if nft is for sale (price is not Nothing)
        !nftForSale = isJust . info'price . node'information $ oldNode

        -- Check if author of NFT receives share
        !correctPaymentAuthor = correctPayment (info'author . node'information) calculateAuthorShare

        -- Check if owner of NFT receives share
        !correctPaymentOwner = correctPayment (info'owner . node'information) calculateOwnerShare

        -- Check if author of NFT receives share when is also owner
        -- correctPaymentOnlyAuthor !bid = authorGetsAda >= bid
        --   where
        --     author = getUserId . info'author . node'information $ node
        --     authorGetsAda = getAda $ valuePaidTo info author

        -- Check if buy bid is higher or equal than price
        bidHighEnough !bid = case info'price . node'information $ oldNode of
          Nothing -> False -- NFT not for sale.
          Just price -> price <= bid

        -- Check if the datum attached is also present in the set price transaction.
        !correctDatumSetPrice = (== info'id nInfo) . info'id . node'information $ oldNode

        -- Check if only thing changed in nodes is price
        !consistentDatumSetPrice =
          on (==) node'next oldNode node
            && on (==) node'appInstance oldNode node
            && on (==) (info'author . node'information) oldNode node
            && on (==) (info'owner . node'information) oldNode node
            && on (==) (info'share . node'information) oldNode node
            && on (==) (info'id . node'information) oldNode node

        !checkIsNotOnAuction = isNothing . info'auctionState . node'information $ node

        -- Check if the price of NFT is changed by the owner of NFT
        !signedByOwner =
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

        -- Check if all tokens from input and mint are returned
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

-- | Returns datums attached to inputs of transaction
getInputDatums :: PlutusTx.FromData a => ScriptContext -> [a]
getInputDatums = fmap fst . getInputDatumsWithTx

{-# INLINEABLE getInputDatumsWithTx #-}

-- | Returns datums and corresponding UTXOs attached to inputs of transaction
getInputDatumsWithTx :: PlutusTx.FromData a => ScriptContext -> [(a, TxOut)]
getInputDatumsWithTx ctx =
  mapMaybe (\(datum, tx) -> (,) <$> (PlutusTx.fromBuiltinData . getDatum $ datum) <*> pure tx)
    . mapMaybe (\(hash, tx) -> (,) <$> findDatum hash (scriptContextTxInfo ctx) <*> pure tx)
    . mapMaybe ((\tx -> (,) <$> txOutDatumHash tx <*> pure tx) . txInInfoResolved)
    . txInfoInputs
    . scriptContextTxInfo
    $ ctx

{-# INLINEABLE getOutputDatums #-}

-- | Returns datums attached to outputs of transaction
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
