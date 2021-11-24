module Mlabs.NFT.Governance.Validation (
  govScript,
  govMintPolicy,
  govScrAddress,
  GovManage,
) where

--import Prelude qualified as Hask

import Ledger (
  Address,
  CurrencySymbol,
  MintingPolicy,
  ScriptContext,
  TxOut,
  findDatum,
  findOwnInput,
  getContinuingOutputs,
  getDatum,
  mkMintingPolicyScript,
  ownCurrencySymbol,
  scriptContextTxInfo,
  txInInfoResolved,
  txInfoInputs,
  txInfoOutputs,
  txInfoSignatories,
  txOutAddress,
  txOutDatumHash,
  txOutValue,
 )
import Ledger.Typed.Scripts (
  TypedValidator,
  ValidatorTypes (..),
  mkTypedValidator,
  validatorAddress,
  wrapMintingPolicy,
  wrapValidator,
 )

import Plutus.V1.Ledger.Crypto (
  getPubKeyHash,
 )

import Mlabs.NFT.Governance.Types (
  GovAct (..),
  GovDatum (..),
  GovLHead (..),
  LList (..),
  gov'list,
 )

import Mlabs.NFT.Types (
  NftAppInstance,
  UniqueToken,
  UserId (..),
  appInstance'Address,
 )

import Plutus.V1.Ledger.Ada (adaSymbol, adaToken, lovelaceValueOf)
import Plutus.V1.Ledger.Value (
  AssetClass (..),
  TokenName (..),
  assetClassValue,
  assetClassValueOf,
  flattenValue,
  geq,
 )
import PlutusTx qualified
import PlutusTx.Prelude
import PlutusTx.Ratio (Ratio)

data GovManage
instance ValidatorTypes GovManage where
  type DatumType GovManage = GovDatum
  type RedeemerType GovManage = GovAct

{-# INLINEABLE mkGovMintPolicy #-}

-- | Minting policy for GOV and xGOV tokens.
mkGovMintPolicy :: NftAppInstance -> GovAct -> ScriptContext -> Bool
mkGovMintPolicy appInstance act ctx =
  case act of
    InitialiseGov -> traceIfFalse "Only allowd to mint proof token" checkOnlyProofMinted
    MintGov ->
      traceIfFalse "List head must be spent" checkListHeadIsSpent
        && traceIfFalse "Fee must be paid to the list head." checkFeeToTheListHead
        && traceIfFalse
          "List node incorrectly minted or updated"
          (nodeCanBeUpdated && nodeCorrectlyUpdated)
          || (nodeCanBeInserted && nodeCorrectlyInserted)
        && traceIfFalse "Equal amounts of listGov and freeGov tokens must be minted/burned." checkListGovFreeGovEquality
        && traceIfFalse "Only allowd to mint/burn xGov and Gov" checkOnlyListGovFreeGovMinted
        && traceIfFalse "The minted/burned amount must be equal to the fee." checkGovEqualToFee
        && traceIfFalse "listGov tokens must be sent to the new node" checkAllListGovToAddress
    Proof -> traceError "Not allowed to mint using Proof as the redeemer."
    ProofAndBurn ->
      traceIfFalse "List head must be spent" checkListHeadIsSpent
        && traceIfFalse "Equal amounts of freeGov and listGov tokens must be burned." checkListGovFreeGovEquality
        && traceIfFalse "Maximum fee number of lovelace is allowed to remove from the list head." checkFeeFromTheListHead
        && traceIfFalse "Only allowd to mint/burn freeGov and listGov" checkOnlyListGovFreeGovMinted
        && traceIfFalse "The minted/burned amount must be equal to the fee." checkListGovEqualToNegFee
  where
    outputsWithHeadDatum =
      filter
        ( \(datum', txO) ->
            datumIsHead (gov'list datum') && tokenPresent txO
        )
        $ getOutputDatumsWithTx @GovDatum ctx

    outputsWithNodeDatum =
      filter
        ( \(datum', txO) ->
            datumIsNode (gov'list datum') && tokenPresent txO
        )
        $ getOutputDatumsWithTx @GovDatum ctx

    outputsToScriptAddress =
      filter
        ( \(_, txO) -> toScriptAddress txO
        )
        $ getOutputDatumsWithTx @GovDatum ctx

    inputsWithHeadDatum =
      filter
        ( \(datum', txO) ->
            datumIsHead (gov'list datum') && tokenPresent txO
        )
        $ getInputDatumsWithTx @GovDatum ctx

    inputsWithNodeDatum =
      filter
        ( \(datum', txO) ->
            datumIsNode (gov'list datum') && toScriptAddress txO
        )
        $ getInputDatumsWithTx @GovDatum ctx

    nodeCanBeUpdated = length inputsWithNodeDatum == 1

    sort2 :: ((GovDatum, TxOut), (GovDatum, TxOut)) -> ((GovDatum, TxOut), (GovDatum, TxOut))
    sort2 (x'@(x, _), y'@(y, _)) =
      case (gov'list x, gov'list y) of
        (HeadLList {}, HeadLList {}) -> (x', y')
        (NodeLList {}, HeadLList {}) -> (x', y')
        (HeadLList {}, NodeLList {}) -> (y', x')
        (NodeLList k1 _ _, NodeLList k2 _ _) -> if k1 < k2 then (x', y') else (y', x')

    nodeCanBeInserted =
      wrapMaybe $ do
        (prevHead, _) <- getMaybeOne inputsWithHeadDatum
        return $
          (isNothing (getNext prevHead) && (length outputsWithNodeDatum == 1) && null inputsWithNodeDatum)
            || (null inputsWithNodeDatum && (length outputsWithNodeDatum == 1))
            || ((length inputsWithNodeDatum == 1) && (length outputsWithNodeDatum == 2))
    nodeCorrectlyInserted
      | isNothing (getNext prevHead) && null inputsWithNodeDatum && (length outputsWithNodeDatum == 1) =
        wrapMaybe $ do
          (node, _) <- getMaybeOne outputsWithNodeDatum
          next <- getNext prevHead
          key <- getKey node
          return $ key == next
      | isJust (getNext prevHead) && null inputsWithNodeDatum && (length outputsWithNodeDatum == 1) =
        wrapMaybe $ do
          (newHead, _) <- getMaybeOne outputsWithHeadDatum
          (newNode, _) <- getMaybeOne outputsWithNodeDatum
          secondKey <- getNext prevHead
          newNodeKey <- getKey newNode
          newHeadNext <- getNext newHead
          newNodeNext <- getNext newNode
          return $ (newNodeKey < secondKey) && (newHeadNext == newNodeKey) && (newNodeNext == secondKey)
      | (length inputsWithNodeDatum == 1) && (length outputsWithNodeDatum == 2) =
        wrapMaybe $ do
          let ((newFirst, _), (newSecond, _)) = sort2 (head outputsWithNodeDatum, outputsWithNodeDatum !! 1)
          (oldFirst, _) <- getMaybeOne inputsWithNodeDatum
          oldFirstKey <- getKey oldFirst
          newFirstNext <- getNext newFirst
          newFirstKey <- getKey newFirst
          newSecondKey <- getKey newSecond
          return $
            if isNothing . getNext $ oldFirst
              then (newSecondKey == newFirstNext) && (oldFirstKey == newFirstKey)
              else wrapMaybe $ do
                oldFirstNext <- getNext oldFirst
                newSecondNext <- getNext newSecond
                return $ (newFirstKey == oldFirstKey) && (newSecondNext == oldFirstNext) && (newFirstNext == newSecondKey)
      | otherwise = False
      where
        prevHead = case getMaybeOne inputsWithHeadDatum of
          Just (h, _) -> h
          Nothing -> traceError "No head found"

    getNext :: GovDatum -> Maybe UserId
    getNext (GovDatum (HeadLList _ next)) = next
    getNext (GovDatum (NodeLList _ _ next)) = next

    checkListHeadIsSpent =
      case inputsWithHeadDatum of
        [] -> False
        [_] -> True
        _ -> traceError "Multiple unique tokens found" -- This should never happen
    checkFeeToTheListHead =
      wrapMaybe $ do
        (h, txOut) <- getMaybeOne outputsWithHeadDatum
        (_, txIn) <- getMaybeOne inputsWithHeadDatum
        feeRate <- getFeeRate h
        let valOut = txOutValue txOut
            valIn = txOutValue txIn
        return $ valOut `geq` (valIn <> lovelaceValueOf (getFee feeRate))

    checkFeeFromTheListHead =
      wrapMaybe $ do
        (h, txOut) <- getMaybeOne outputsWithHeadDatum
        (_, txIn) <- getMaybeOne inputsWithHeadDatum
        feeRate <- getFeeRate h
        let valOut = txOutValue txOut
            valIn = txOutValue txIn
        return $ valOut `geq` (valIn <> lovelaceValueOf (getFee feeRate))

    checkListGovFreeGovEquality = freeGovMinted == listGovMinted

    checkOnlyListGovFreeGovMinted =
      valueMinted
        == assetClassValue freeGov freeGovMinted <> assetClassValue listGov listGovMinted

    checkOnlyProofMinted =
      valueMinted == assetClassValue proofToken 1

    checkGovEqualToFee =
      wrapMaybe $ do
        (h, _) <- getMaybeOne outputsWithHeadDatum
        feeRate <- getFeeRate h
        return $ freeGovMinted == getFee feeRate

    checkListGovEqualToNegFee =
      wrapMaybe $ do
        (h, _) <- getMaybeOne outputsWithHeadDatum
        feeRate <- getFeeRate h
        return $ freeGovMinted == (-1) * getFee feeRate

    checkAllListGovToAddress =
      listGovMinted == assetClassValueOf (foldMap (txOutValue . snd) outputsToScriptAddress) listGov

    nodeCorrectlyUpdated =
      wrapMaybe $ do
        (input, txOIn) <- getMaybeOne inputsWithNodeDatum
        (output, txOOut) <- getMaybeOne outputsWithNodeDatum
        (headD, _) <- getMaybeOne inputsWithHeadDatum
        feeRate <- getFeeRate headD
        let inListGov = assetClassValueOf (txOutValue txOIn) listGov
            outListGov = assetClassValueOf (txOutValue txOOut) listGov
        return $
          (outListGov < inListGov + getFee feeRate)
            && (input == output)

    wrapMaybe v = Just True == v

    datumIsNode = not . datumIsHead

    datumIsHead (HeadLList _ _) = True
    datumIsHead _ = False

    getFeeRate :: GovDatum -> Maybe Rational
    getFeeRate (GovDatum datum) =
      case datum of
        HeadLList (GovLHead fee) _ -> Just fee
        _ -> Nothing

    symbol = ownCurrencySymbol ctx
    asset tn = AssetClass (symbol, TokenName tn)
    userId = getPubKeyHash $ head (txInfoSignatories . scriptContextTxInfo $ ctx) -- FIXME more sophisticated way is needed
    freeGov = asset ("freeGov" <> userId)
    listGov = asset ("listGov" <> userId)
    proofToken = asset emptyByteString

    tokenPresent txO = 1 == assetClassValueOf (txOutValue txO) proofToken

    toScriptAddress txO = txOutAddress txO == appInstance'Address appInstance

    valueIn = foldMap (txOutValue . txInInfoResolved) $ txInfoInputs . scriptContextTxInfo $ ctx

    valueOut = foldMap txOutValue $ txInfoOutputs . scriptContextTxInfo $ ctx

    valueMinted =
      let allMinted = flattenValue $ valueOut <> negate valueIn
       in mconcat
            . map (\(s, n, v) -> assetClassValue (AssetClass (s, n)) v)
            . filter (\(s, _, _) -> s == ownCurrencySymbol ctx)
            $ allMinted

    freeGovMinted = assetClassValueOf valueMinted freeGov
    listGovMinted = assetClassValueOf valueMinted listGov

    getMaybeOne :: [a] -> Maybe a
    getMaybeOne [x] = Just x
    getMaybeOne _ = Nothing

    price = 10 -- FIXME
    getFee :: Ratio Integer -> Integer
    getFee feeRate = round $ fromInteger price * feeRate

    getKey (GovDatum (HeadLList _ _)) = Nothing
    getKey (GovDatum (NodeLList _ _ k)) = k

{-# INLINEABLE govMintPolicy #-}

-- | Gov Minting Policy
govMintPolicy :: NftAppInstance -> MintingPolicy
govMintPolicy x =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||wrapMintingPolicy . mkGovMintPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode x

{-# INLINEABLE mkGovScript #-}

-- | Minting policy for GOV and xGOV tokens.
mkGovScript :: UniqueToken -> GovDatum -> GovAct -> ScriptContext -> Bool
mkGovScript ut datum act ctx =
  case act of
    InitialiseGov ->
      False -- FIXME Why should anybody for good reason use it as the redeemer?!
    MintGov ->
      -- Inserting new node in the governance linked list is a bit more complicated than the NFT linked list.
      -- The reason is that here we need the list head to always be consumed, and it hardens
      -- the detection of 'first', 'second', 'newFirst', 'newInserted' in the terminology
      -- of NFT/Validation.hs validator script. So we can't use the same pattern here.
      traceIfFalse "New listGov tokens must be minted" checkListGovIsMinted -- We need to ensure that our minting policy verifications are called.
        && traceIfFalse "New head's fee must be the same as old head's fee" checkHeadsFeeEquality
        && traceIfFalse "New first must point to the new node" (newFirst `pointsTo` (newSecond >>= getKey . fst))
        && traceIfFalse "New node must point to the old first's next if there was any" checkNewNodePointsCorrectly
        && traceIfFalse "New node must contain listGov equal to fee" checkGovIsPaid
        && traceIfFalse "New head must have everything plus fee" checkHeadHasEverythingPlusFee
        && traceIfFalse "Old first must remain consistent" checkFirstIsConsistent
    Proof ->
      traceIfFalse "Free Gov tokens are required to unlock." freeGovPresent
        && traceIfFalse "ListGov must be sent bach unchanged." listGovSentBackUnchanged
        && traceIfFalse "FreeGov must be sent bach unchanged." freeGovSentBackUnchanged
    ProofAndBurn ->
      traceIfFalse "Free Gov tokens are required to unlock." freeGovPresent
        && traceIfFalse "New head's fee must be the same as old head's fee" checkHeadsFeeEquality
        && traceIfFalse "New head's next must be the same as old head's next" checkHeadsNextEquality
        && traceIfFalse "Free Gov tokens must be burnt correctly." govBurntCorrectly
        && traceIfFalse "There is no list head or stakes are withdrawn incorrectly." checkWithdrawStakes
  where
    pointsTo first key =
      wrapMaybe $ do
        (first', _) <- first
        key' <- key
        next <- getNext first'
        return $ key' == next

    sort2 :: ((GovDatum, TxOut), (GovDatum, TxOut)) -> ((GovDatum, TxOut), (GovDatum, TxOut))
    sort2 (x'@(x, _), y'@(y, _)) =
      case (gov'list x, gov'list y) of
        (HeadLList {}, HeadLList {}) -> (x', y')
        (NodeLList {}, HeadLList {}) -> (x', y')
        (HeadLList {}, NodeLList {}) -> (y', x')
        (NodeLList k1 _ _, NodeLList k2 _ _) -> if k1 < k2 then (x', y') else (y', x')

    oldFirst :: Maybe (GovDatum, TxOut)
    oldFirst =
      case act of
        MintGov ->
          case gov'list datum of
            HeadLList _ Nothing -> getMaybeOne inputsFromScriptAddress -- To ensure that there is no other input
            HeadLList {}
              | length inputsFromScriptAddress == 1 -> getMaybeOne inputsFromScriptAddress -- Inserting after head
              | length inputsFromScriptAddress == 2 -> getMaybeOne inputsWithNodeDatum -- Inserting somewhere else
              | otherwise -> traceError "Illegal script inptuts"
            NodeLList {} -> getMaybeOne inputsWithNodeDatum
        _ -> traceError "Pointers only are allowed for minting."

    newFirst :: Maybe (GovDatum, TxOut)
    newFirst =
      case act of
        MintGov ->
          case gov'list datum of
            HeadLList _ Nothing -> getMaybeOne outputsWithHeadDatum
            HeadLList _ _
              | length inputsFromScriptAddress == 1 -> getMaybeOne outputsWithNodeDatum -- Inserting after head
              | length inputsFromScriptAddress == 2 ->
                do
                  [d1, d2] <- getMaybeTwo outputsWithNodeDatum
                  return $ fst . sort2 $ (d1, d2)
              | otherwise -> traceError "Illegal script inptuts"
            NodeLList {} -> getMaybeOne inputsWithNodeDatum
        _ -> traceError "Pointers only are allowed for minting."

    oldSecond :: Maybe UserId
    oldSecond = oldFirst >>= getNext . fst

    newSecond :: Maybe (GovDatum, TxOut)
    newSecond =
      case act of
        MintGov ->
          case gov'list datum of
            HeadLList _ Nothing -> getMaybeOne outputsWithNodeDatum
            HeadLList {}
              | length inputsFromScriptAddress == 1 -> getMaybeOne outputsWithNodeDatum -- Inserting after head
              | length inputsFromScriptAddress == 2 ->
                do
                  [d1, d2] <- getMaybeTwo outputsWithNodeDatum
                  return $ snd . sort2 $ (d1, d2)
              | otherwise -> traceError "Illegal script inptuts"
            NodeLList {} -> getMaybeOne inputsWithNodeDatum
        _ -> traceError "Pointers only are allowed for minting."

    outputsWithHeadDatum =
      filter
        ( \(datum', txO) ->
            datumIsHead (gov'list datum') && tokenPresent txO
        )
        $ getDatumsWithTx @GovDatum (getContinuingOutputs ctx) ctx

    outputsWithNodeDatum =
      filter
        ( \(datum', _) ->
            datumIsNode (gov'list datum')
        )
        $ getDatumsWithTx @GovDatum (getContinuingOutputs ctx) ctx

    inputsFromScriptAddress :: [(GovDatum, TxOut)]
    inputsFromScriptAddress =
      filter
        ( \(_, txO) ->
            txOutAddress txO == ownAddress
        )
        $ getInputDatumsWithTx @GovDatum ctx

    inputsWithHeadDatum = filter (\(d, _) -> datumIsHead . gov'list $ d) inputsFromScriptAddress
    inputsWithNodeDatum = filter (\(d, _) -> datumIsNode . gov'list $ d) inputsFromScriptAddress

    checkNewNodePointsCorrectly =
      case oldSecond of
        Nothing ->
          wrapMaybe $ do
            (d, _) <- newSecond
            return . isNothing . getNext $ d
        _ -> newSecond `pointsTo` oldSecond

    checkFirstIsConsistent =
      wrapMaybe $ do
        (dOld, oldTxO) <- oldFirst
        (dNew, newTxO) <- newFirst
        oldKey <- getKey dOld
        newKey <- getKey dNew
        let valIn = txOutValue oldTxO
            valOut = txOutValue newTxO
        return $ (oldKey == newKey) && (valOut `geq` valIn)

    checkListGovIsMinted = listGovMinted > 0

    checkHeadHasEverythingPlusFee =
      wrapMaybe $ do
        (d, oldTxO) <- getMaybeOne inputsWithHeadDatum
        (_, newTxO) <- getMaybeOne outputsWithHeadDatum
        feeRate <- getFeeRate d
        return $ txOutValue newTxO `geq` (txOutValue oldTxO <> lovelaceValueOf (getFee feeRate))

    checkGovIsPaid =
      wrapMaybe $ do
        (headD, _) <- getMaybeOne inputsWithHeadDatum
        feeRate <- getFeeRate headD
        (_, txO) <- newSecond
        return $ assetClassValueOf (txOutValue txO) listGov == getFee feeRate -- geq won't work since we can't handle multiple minting at in this version
    checkHeadsNextEquality =
      case getMaybeOne inputsWithHeadDatum of
        Nothing ->
          wrapMaybe $ do
            (new, _) <- getMaybeOne outputsWithHeadDatum
            return . isNothing . getNext $ new
        Just (old, _) ->
          wrapMaybe $ do
            (new, _) <- getMaybeOne outputsWithHeadDatum
            newNext <- getNext new
            oldNext <- getNext old
            return $ oldNext == newNext

    checkHeadsFeeEquality =
      wrapMaybe $ do
        (oldDatum, _) <- getMaybeOne inputsWithHeadDatum
        (newDatum, _) <- getMaybeOne outputsWithHeadDatum
        oldFeeRate <- getFeeRate oldDatum
        newFeeRate <- getFeeRate newDatum
        return $ newFeeRate == oldFeeRate

    ownAddress =
      case txOutAddress . txInInfoResolved <$> findOwnInput ctx of
        Just addr -> addr
        Nothing -> traceError "Can't find own address!"

    nodeToBurn :: Maybe (GovDatum, TxOut)
    nodeToBurn =
      case act of
        ProofAndBurn -> getMaybeOne inputsWithNodeDatum
        _ -> traceError "Act has to be ProofAndBurn"

    freeGovPresent = assetClassValueOf valueIn freeGov > 0

    govBurntCorrectly =
      wrapMaybe $ do
        (_, txOutIn) <- nodeToBurn
        (_, txOutOut) <- getMaybeOne outputsWithNodeDatum
        let toBurnFreeGov = assetClassValueOf valueIn freeGov
            remainedListGov = assetClassValueOf (txOutValue txOutOut) listGov
            burntListGov = assetClassValueOf (txOutValue txOutIn) listGov - remainedListGov
        return $
          (toBurnFreeGov == burntListGov) && (listGovMinted == (-1) * burntListGov) && (listGovMinted == freeGovMinted)

    checkWithdrawStakes =
      wrapMaybe $ do
        (_, txOutIn) <- getMaybeOne inputsWithHeadDatum
        (_, txOutOut) <- getMaybeOne outputsWithHeadDatum
        let ada = AssetClass (adaSymbol, adaToken)
            valueDif = txOutValue txOutIn <> negate (txOutValue txOutOut)
        return $ valueDif == assetClassValue ada freeGovMinted

    listGovSentBackUnchanged = True -- TODO
    freeGovSentBackUnchanged = True -- TODO
    ownCurrencySymbol' :: CurrencySymbol
    ownCurrencySymbol' =
      case otherSymbols of
        [s] -> s
        [] -> traceError "Gov Proof Token must be in the list head."
        _ -> traceError "Illegal currency symbols found in the input."
      where
        fst3 (a, _, _) = a
        value = txOutValue . txInInfoResolved <$> findOwnInput ctx
        allCurrencySymbols =
          case value of
            Nothing -> traceError "Own input wasn't find!"
            Just v -> fst3 <$> flattenValue v
        validSymbols = [fst . unAssetClass $ ut, adaSymbol]
        otherSymbols = filter (`notElem` validSymbols) allCurrencySymbols

    asset tn = AssetClass (ownCurrencySymbol', TokenName tn)

    userId =
      case act of
        MintGov ->
          case gov'list datum of
            HeadLList {} ->
              do
                (d, _) <- getMaybeOne outputsWithNodeDatum
                key <- getKey d
                return . getPubKeyHash . getUserId $ key
            NodeLList {} ->
              do
                [d1, d2] <- getMaybeTwo outputsWithNodeDatum
                key <- getKey $ fst . snd . sort2 $ (d1, d2)
                return . getPubKeyHash . getUserId $ key
        _ ->
          do
            (d, _) <- getMaybeOne inputsWithNodeDatum
            key <- getKey d
            return . getPubKeyHash . getUserId $ key

    (listGov, freeGov) =
      case userId of
        Just uId -> (asset ("listGov" <> uId), asset ("freeGov" <> uId))
        Nothing -> traceError "Didn't find UserId"

    valueIn = foldMap (txOutValue . txInInfoResolved) $ txInfoInputs . scriptContextTxInfo $ ctx

    valueOut = foldMap txOutValue $ txInfoOutputs . scriptContextTxInfo $ ctx

    valueMinted =
      let allMinted = flattenValue $ valueOut <> negate valueIn
       in mconcat
            . map (\(s, n, v) -> assetClassValue (AssetClass (s, n)) v)
            . filter (\(s, _, _) -> s == ownCurrencySymbol')
            $ allMinted

    listGovMinted = assetClassValueOf valueMinted listGov
    freeGovMinted = assetClassValueOf valueMinted freeGov
    tokenPresent txO = 1 == assetClassValueOf (txOutValue txO) ut

    wrapMaybe v = Just True == v

    getMaybeOne :: [a] -> Maybe a
    getMaybeOne [x] = Just x
    getMaybeOne _ = Nothing

    getMaybeTwo :: [a] -> Maybe [a]
    getMaybeTwo [x, y] = Just [x, y]
    getMaybeTwo _ = Nothing

    getNext :: GovDatum -> Maybe UserId
    getNext (GovDatum (HeadLList _ next)) = next
    getNext (GovDatum (NodeLList _ _ next)) = next

    getFeeRate :: GovDatum -> Maybe Rational
    getFeeRate (GovDatum d) =
      case d of
        HeadLList (GovLHead feeRate) _ -> Just feeRate
        _ -> Nothing

    price = 10 -- FIXME
    getFee :: Ratio Integer -> Integer
    getFee feeRate = round $ fromInteger price * feeRate

    getKey :: GovDatum -> Maybe UserId
    getKey (GovDatum (HeadLList _ _)) = Nothing
    getKey (GovDatum (NodeLList _ _ k)) = k

    datumIsHead (HeadLList _ _) = True
    datumIsHead _ = False

    datumIsNode = not . datumIsHead

{-# INLINEABLE govScript #-}
govScript :: UniqueToken -> TypedValidator GovManage
govScript ut =
  mkTypedValidator @GovManage
    ($$(PlutusTx.compile [||mkGovScript||]) `PlutusTx.applyCode` PlutusTx.liftCode ut)
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = wrapValidator @GovDatum @GovAct

{-# INLINEABLE govScrAddress #-}

-- | Address of Gov Script Logic.
govScrAddress :: UniqueToken -> Ledger.Address
govScrAddress = validatorAddress . govScript

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

-- | Returns datums and corresponding UTXOs attached to the list of UTxOs
getDatumsWithTx :: PlutusTx.FromData a => [TxOut] -> ScriptContext -> [(a, TxOut)]
getDatumsWithTx utxos ctx =
  mapMaybe (\(datum, tx) -> (,) <$> (PlutusTx.fromBuiltinData . getDatum $ datum) <*> pure tx)
    . mapMaybe (\(hash, tx) -> (,) <$> findDatum hash (scriptContextTxInfo ctx) <*> pure tx)
    . mapMaybe (\tx -> (,) <$> txOutDatumHash tx <*> pure tx)
    $ utxos
