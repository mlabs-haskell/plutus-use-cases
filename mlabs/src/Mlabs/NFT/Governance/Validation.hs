module Mlabs.NFT.Governance.Validation (
  mkGovScript,
  govScript,
  govMintPolicy,
  govScrAddress,
  GovManage,
) where

import Ledger (
  Address,
  CurrencySymbol,
  MintingPolicy,
  PubKeyHash,
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
  appInstance'Governance,
  appInstance'UniqueToken,
 )

import Plutus.V1.Ledger.Ada (adaSymbol, adaToken, lovelaceValueOf)
import Plutus.V1.Ledger.Address (pubKeyHashAddress)
import Plutus.V1.Ledger.Value (
  AssetClass (..),
  TokenName (..),
  Value (..),
  assetClass,
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
    InitialiseGov ->
      traceIfFalse "NFT-Gov-Mint: Only allowed to mint a proof token" checkOnlyProofMinted
        && traceIfFalse "NFT-Gov-Mint: uniquetoken and proofToken are not sent to the head" checkProofTokenToHead
        && traceIfFalse "NFT-Gov-Mint: List head with the unique token is not present" checkListHeadInit
    MintGov ->
      traceIfFalse "NFT-Gov-Mint: Fee wasn't paid to the head pubkey hash." checkFeeToHeadPkh
        --      traceIfFalse "NFT-Gov-Mint: Fee must be paid to the list head." checkFeeToTheListHead -- It automatically checks that the list head is present.
        && traceIfFalse
          "NFT-Gov-Mint: List node incorrectly minted or updated"
          (nodeCanBeUpdated && nodeUpdatedWithCorrectValues)
          || (nodeCanBeInserted && nodeInsertedWithCorrectValues)
        && traceIfFalse "NFT-Gov-Mint: Equal amounts of listGov and freeGov tokens must be minted/burned." checkListGovFreeGovEquality
        && traceIfFalse "NFT-Gov-Mint: Only allowd to mint/burn listGov and freeGov" checkOnlyListGovFreeGovMinted
        && traceIfFalse "NFT-Gov-Mint: The minted amount of listGov must be positive." checkListGovPositive
        && traceIfFalse "NFT-Gov-Mint: The minted amount of listGov must be equal to the fee." checkListGovEqualToFee
    Proof -> traceError "NFT-Gov-Mint: Not allowed to mint using Proof as the redeemer."
    ProofAndBurn ->
      traceIfFalse "NFT-Gov-Mint: Only allowd to mint/burn freeGov and listGov" checkOnlyListGovFreeGovMinted
        && traceIfFalse "NFT-Gov-Mint: Equal amounts of listGov and freeGov tokens must be minted/burned." checkListGovFreeGovEquality
        && traceIfFalse "NFT-Gov-Mint: The burnt amount of listGov must be positive." checkListGovNegative
        && traceIfFalse
          "NFT-Gov-Mint: Values for burning are updated incorrectly."
          (allTokensCanBeBurnt && allTokensBurntCorrectly)
          || someTokensBurntCorrectly
        && traceIfFalse "NFT-Gov-Mint: Stakes must be withdrawn correctly." checkStakeWithdrawnCorrectly
  where
    ------------------------------------------------------------------------------
    -- Checks

    allTokensCanBeBurnt
      | (length inputsWithNodeDatum == 2) && (length outputsWithNodeDatum == 1) = True
      | length inputsWithNodeDatum == 1 && null outputsWithNodeDatum = True
      | otherwise = False

    allTokensBurntCorrectly
      | length inputsWithNodeDatum == 2 && length outputsWithNodeDatum == 1 =
        wrapMaybe $ do
          ((_, txNode1), (_, txNode2)) <- sort2 <$> getMaybeTwo inputsWithNodeDatum
          (_, txNode1New) <- getMaybeOne outputsWithNodeDatum
          return $ checkAllBurnt txNode2 && (txOutValue txNode1 == txOutValue txNode1New)
      | length inputsWithNodeDatum == 1 && null outputsWithNodeDatum =
        wrapMaybe $ do
          (_, txNode) <- getMaybeOne inputsWithNodeDatum
          return $ checkAllBurnt txNode -- Head value is handled in checkStakeWithdrawnCorrectly
      | otherwise = False
      where
        checkAllBurnt tx = assetClassValueOf (txOutValue tx) listGov == listGovMinted * (-1)

    -- ProofAndBurn is used as the redeemer. There must be exactly one input and one output list node.
    someTokensBurntCorrectly =
      wrapMaybe $ do
        (_, nodeTxIn) <- getMaybeOne inputsWithNodeDatum
        (_, nodeTxOut) <- getMaybeOne outputsWithNodeDatum
        let listGovBurnt = (-1) * listGovMinted
            nodeInValue = txOutValue nodeTxIn
            nodeOutValue = txOutValue nodeTxOut
            nodeRemovedValue = nodeInValue <> negate nodeOutValue
        return $
          -- Every listGov that burnt must be from the node
          (nodeRemovedValue == assetClassValue listGov listGovBurnt)
            && assetClassValueOf nodeInValue listGov > listGovBurnt

    checkListHeadInit =
      any
        ( \(datum', txO) ->
            datumIsHead (gov'list datum') && uTokenPresent txO
        )
        $ getInputDatumsWithTx @GovDatum ctx
      where
        uTokenPresent txO = assetClassValueOf (txOutValue txO) uniqueToken == 1

    -- Checks whether fees are paid to head correctly or not
    --    checkFeeToTheListHead =
    --      wrapMaybe $ do
    --        (h, txOut) <- getMaybeOne outputsWithHeadDatum
    --        (_, txIn) <- getMaybeOne inputsWithHeadDatum
    --        feeRate <- getFeeRate h
    --        let valOut = txOutValue txOut
    --            valIn = txOutValue txIn
    --        return $
    --          -- Fees are added to the head
    --          valOut `geq` (valIn <> lovelaceValueOf (getFee feeRate))

    -- For ProofAndBurn, checks whether staked lovelace are removed from the head correctly or not.
    checkStakeWithdrawnCorrectly =
      wrapMaybe $ do
        (_, headTxIn) <- getMaybeOne inputsWithHeadDatum
        (_, headTxOut) <- getMaybeOne outputsWithHeadDatum
        let listGovBurnt = (-1) * listGovMinted
            headOutVal = txOutValue headTxOut
            headInVal = txOutValue headTxIn
        return $
          -- Staked lovelace from the head are removed
          headOutVal `geq` (headInVal <> (negate . lovelaceValueOf $ listGovBurnt))

    -- MingGov is used as the redeemer and finding a single and identical list node in inputs and outputs
    -- means that we want to update it, .e.g add listGov tokens to it
    nodeCanBeUpdated =
      wrapMaybe $ do
        (inputDatum, _) <- getMaybeOne inputsWithNodeDatum
        (outputDatum, _) <- getMaybeOne outputsWithNodeDatum
        return $ inputDatum == outputDatum

    -- Makes sure the value of list node being updated is changed correctly.
    nodeUpdatedWithCorrectValues =
      wrapMaybe $ do
        (headDatum, _) <- getMaybeOne inputsWithHeadDatum
        (_, txIn) <- getMaybeOne inputsWithNodeDatum
        (_, txOut) <- getMaybeOne outputsWithNodeDatum
        feeRate <- getFeeRate headDatum
        let nodeAddedVal = txOutValue txOut <> negate (txOutValue txIn)
            fee = getFee feeRate
        return $
          -- Every listGov that minted must send to the node
          nodeAddedVal == assetClassValue listGov listGovMinted
            -- The number of minted listGov is correct
            && assetClassValueOf nodeAddedVal listGov == fee

    -- MintGov is used as the redeemer and number of output list nodes is more than the number of input list nodes by one
    -- means that we want to insert a new node to list.
    nodeCanBeInserted =
      wrapMaybe $ do
        (oldHead, _) <- getMaybeOne inputsWithHeadDatum
        return $
          -- Inserting the first node
          (isNothing (getNext oldHead) && (length outputsWithNodeDatum == 1) && null inputsWithNodeDatum)
            -- Inserting a new node next to the head, while the head already has one
            || (isJust (getNext oldHead) && null inputsWithNodeDatum && length outputsWithNodeDatum == 1)
            -- Inserting a new node to other places in the list
            || (length inputsWithNodeDatum == 1 && length outputsWithNodeDatum == 2)

    -- Makes sure the value of new list node is adjusted correctly.
    nodeInsertedWithCorrectValues
      -- Adding a new node to head's next. It doesn't matter that head primarily had a next or not.
      -- New node's value in both cases are the same
      | null inputsWithNodeDatum && length outputsWithNodeDatum == 1 =
        wrapMaybe $ do
          (_, txNode) <- getMaybeOne outputsWithNodeDatum
          let newNodeVal = txOutValue txNode
          return $
            -- New node has newly generated listGov tokens equal to the fee.
            newNodeVal == assetClassValue listGov fee
      -- Adding a new node to other places.
      | length inputsWithNodeDatum == 1 && length outputsWithNodeDatum == 2 =
        wrapMaybe $ do
          outputs <- getMaybeTwo outputsWithNodeDatum
          (_, txOldFirst) <- getMaybeOne inputsWithNodeDatum
          let ((_, txNewFirst), (_, txNewNode)) = sort2 outputs
              oldFirstVal = txOutValue txOldFirst
              newFirstVal = txOutValue txNewFirst
              newNodeVal = txOutValue txNewNode
          return $
            -- New node has newly generated listGov tokens equal to the fee.
            (newNodeVal == assetClassValue listGov fee)
              -- The first node's value must remain unchanged after insertion.
              && (oldFirstVal == newFirstVal)
      | otherwise = False
      where
        (oldHead, _) =
          case getMaybeOne inputsWithHeadDatum of
            Just h -> h
            Nothing -> traceError "No head found in inputs"
        feeRate =
          case getFeeRate oldHead of
            Just r -> r
            Nothing -> traceError "Fee rate not found." -- This should never happen
        fee = getFee feeRate

    -- Minted freeGov must be the same as minted listGov
    checkListGovFreeGovEquality = freeGovMinted == listGovMinted

    -- Checks nothing else is minted
    checkOnlyListGovFreeGovMinted =
      valueMinted
        == assetClassValue freeGov freeGovMinted <> assetClassValue listGov listGovMinted

    checkOnlyProofMinted =
      valueMinted == assetClassValue proofToken 1

    checkProofTokenToHead = length outputsWithHeadDatum == 1

    checkListGovEqualToFee =
      wrapMaybe $ do
        (h, _) <- getMaybeOne outputsWithHeadDatum
        feeRate <- getFeeRate h
        return $ freeGovMinted == getFee feeRate

    checkFeeToHeadPkh =
      wrapMaybe $ do
        (h, _) <- getMaybeOne inputsWithHeadDatum
        addr <- pubKeyHashAddress <$> getHeadPkh h
        feeRate <- getFeeRate h
        let outputs = txInfoOutputs . scriptContextTxInfo $ ctx
            fee = getFee feeRate
        return $
          any (\tx -> (txOutAddress tx == addr) && (txOutValue tx `geq` lovelaceValueOf fee)) outputs

    checkListGovPositive = listGovMinted > 0

    checkListGovNegative = listGovMinted < 0

    ------------------------------------------------------------------------------
    -- Helpers

    outputsWithHeadDatum :: [(GovDatum, TxOut)]
    outputsWithHeadDatum =
      filter
        ( \(datum', txO) ->
            datumIsHead (gov'list datum') && tokensPresent txO
        )
        $ getOutputDatumsWithTx @GovDatum ctx

    outputsWithNodeDatum :: [(GovDatum, TxOut)]
    outputsWithNodeDatum =
      filter
        ( \(datum', txO) ->
            datumIsNode (gov'list datum') && toScriptAddress txO
        )
        $ getOutputDatumsWithTx @GovDatum ctx

    inputsWithHeadDatum :: [(GovDatum, TxOut)]
    inputsWithHeadDatum =
      filter
        ( \(datum', txO) ->
            datumIsHead (gov'list datum') && tokensPresent txO
        )
        $ getInputDatumsWithTx @GovDatum ctx

    inputsWithNodeDatum :: [(GovDatum, TxOut)]
    inputsWithNodeDatum =
      filter
        ( \(datum', txO) ->
            datumIsNode (gov'list datum') && toScriptAddress txO
        )
        $ getInputDatumsWithTx @GovDatum ctx

    tokensPresent txO =
      assetClassValueOf (txOutValue txO) proofToken == 1
        && assetClassValueOf (txOutValue txO) uniqueToken >= 1

    toScriptAddress txO = txOutAddress txO == appInstance'Governance appInstance

    maybeUserId =
      case act of
        MintGov
          | length outputsWithNodeDatum == 1 ->
            do
              (datum, _) <- getMaybeOne outputsWithNodeDatum
              getId datum
          | otherwise ->
            do
              (datum, _) <- snd . sort2 <$> getMaybeTwo outputsWithNodeDatum
              getId datum
        ProofAndBurn
          | (length inputsWithNodeDatum == 2) && (length outputsWithNodeDatum == 1) ->
            do
              (datum, _) <- snd . sort2 <$> getMaybeTwo inputsWithNodeDatum
              getId datum
          | (length inputsWithNodeDatum == 1) && null outputsWithNodeDatum ->
            do
              (datum, _) <- getMaybeOne inputsWithNodeDatum
              getId datum
          | (length inputsWithNodeDatum == 1) && (length outputsWithNodeDatum == 1) ->
            do
              (datum, _) <- getMaybeOne inputsWithNodeDatum
              getId datum
        _ -> Nothing
      where
        getId datum =
          do
            key <- getKey datum
            return . getPubKeyHash . getUserId $ key

    userId :: BuiltinByteString
    userId = case maybeUserId of
      Just uid -> uid
      Nothing -> traceError "NFT-Gov-Mint: Didn't find UserId."

    symbol = ownCurrencySymbol ctx
    asset tn = AssetClass (symbol, TokenName tn)
    freeGov = asset ("freeGov" <> userId)
    listGov = asset ("listGov" <> userId)
    proofToken = asset emptyByteString
    uniqueToken = appInstance'UniqueToken appInstance

    valueIn, valueOut, valueMinted :: Value
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

    wrapMaybe :: Maybe Bool -> Bool
    wrapMaybe v = Just True == v

    getMaybeOne :: [a] -> Maybe a
    getMaybeOne [x] = Just x
    getMaybeOne _ = Nothing

    getMaybeTwo :: [a] -> Maybe (a, a)
    getMaybeTwo [x, y] = Just (x, y)
    getMaybeTwo _ = Nothing

    --    price = 6_000_000 -- FIXME

    --    getFee :: Ratio Integer -> Integer
    --    getFee feeRate = round $ fromInteger price * feeRate

    getFee :: Ratio Integer -> Integer
    getFee _ =
      case sillyCalculateFee of
        Just fee -> fee
        Nothing -> traceError "NFT-Gov-Mint: Silly calculate fee error."
      where
        sillyCalculateFee =
          do
            (head', _) <- getMaybeOne inputsWithHeadDatum
            pkhAddr <- pubKeyHashAddress <$> getHeadPkh head'
            let outputsToPkh = filter (\txO -> txOutAddress txO == pkhAddr) $ txInfoOutputs . scriptContextTxInfo $ ctx
                inputsFromPkh = filter (\txO -> txOutAddress (txInInfoResolved txO) == pkhAddr) $ txInfoInputs . scriptContextTxInfo $ ctx
                valueIn' = foldMap (txOutValue . txInInfoResolved) inputsFromPkh
                valueOut' = foldMap txOutValue outputsToPkh
                diff = valueOut' <> negate valueIn'
            return $
              assetClassValueOf diff $ assetClass adaSymbol adaToken
    --            (_, headIn) <- getMaybeOne inputsWithHeadDatum
    --            (_, headOut) <- getMaybeOne outputsWithHeadDatum
    --            let diff = txOutValue headOut <> (negate . txOutValue $ headIn)
    --            return $
    --              assetClassValueOf diff $ assetClass adaSymbol adaToken

    getFeeRate :: GovDatum -> Maybe Rational
    getFeeRate (GovDatum datum) =
      case datum of
        HeadLList (GovLHead feeRate _) _ -> Just feeRate
        _ -> Nothing

    getHeadPkh :: GovDatum -> Maybe PubKeyHash
    getHeadPkh (GovDatum datum) =
      case datum of
        HeadLList (GovLHead _ pkh) _ -> Just pkh
        _ -> Nothing

    getKey :: GovDatum -> Maybe UserId
    getKey (GovDatum (HeadLList _ _)) = Nothing
    getKey (GovDatum (NodeLList k _ _)) = Just k

    getNext :: GovDatum -> Maybe UserId
    getNext (GovDatum (HeadLList _ next)) = next
    getNext (GovDatum (NodeLList _ _ next)) = next

    datumIsHead (HeadLList _ _) = True
    datumIsHead _ = False
    datumIsNode = not . datumIsHead

    sort2 :: ((GovDatum, TxOut), (GovDatum, TxOut)) -> ((GovDatum, TxOut), (GovDatum, TxOut))
    sort2 (x'@(x, _), y'@(y, _)) = if x < y then (x', y') else (y', x')

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
mkGovScript ut _ act ctx =
  case act of
    InitialiseGov ->
      traceIfFalse "NFT-Gov-Script-Validator: Unique token is not present." checkUniqueToken
        && traceIfFalse "NFT-Gov-Script-Validator: Proof token must be minted." checkProofTokenMinted
        && traceIfFalse "NFT-Gov-Script-Validator: A new head with both unique and proof tokens in outputs is missing." checkHeadInOutputs
    MintGov ->
      traceIfFalse "NFT-Gov-Script-Validator: New listGov tokens must be minted" checkListGovIsMinted -- We need to ensure that our minting policy verifications are called.
        && traceIfFalse
          "NFT-Gov-Script-Validator: List node incorrectly minted or updated."
          (nodeCanBeUpdated && nodeUpdatedWithCorrectPointers)
          || (nodeCanBeInserted && nodeInsertedWithCorrectPointers)
    Proof ->
      traceIfFalse "NFT-Gov-Script-Validator: Free Gov tokens are required to unlock." checkFreeGovPresent
        && traceIfFalse "NFT-Gov-Script-Validator: ListGov must be sent back unchanged." listGovSentBackUnchanged
        && traceIfFalse "NFT-Gov-Script-Validator: FreeGov must be sent back unchanged." freeGovSentBackUnchanged
    ProofAndBurn ->
      traceIfFalse "NFT-Gov-Script-Validator: Free Gov tokens must be burnt." checkGovBurnt -- We need to ensure that our minting policy verifications are called.
        && traceIfFalse "NFT-Gov-Script-Validator: Free Gov tokens are required to unlock." checkFreeGovPresent
        && traceIfFalse
          "NFT-Gov-Script-Validator: Pointers for burning are updated incorrectly."
          (allTokensCanBeBurnt && allTokensBurntCorrectly)
          || (someTokensCanBeBurnt && someTokensBurntCorrectly)
  where
    ------------------------------------------------------------------------------
    -- Checks

    allTokensCanBeBurnt =
      (length inputsWithNodeDatum == 2 && length outputsWithNodeDatum == 1)
        || (length inputsWithNodeDatum == 1 && null outputsWithNodeDatum)

    someTokensCanBeBurnt = (length inputsWithNodeDatum == length outputsWithNodeDatum) && (length inputsWithNodeDatum == 1)

    someTokensBurntCorrectly = checkBurntNodeDatumUnchanged && checkHeadDatumUnchanged

    -- The node's datum whose listGov is being burnt must remain unchanged.
    checkBurntNodeDatumUnchanged =
      wrapMaybe $ do
        (oldDatum, _) <- getMaybeOne inputsWithNodeDatum
        (newDatum, _) <- getMaybeOne outputsWithNodeDatum
        return $
          -- Can't alter node's datum
          oldDatum == newDatum

    checkHeadDatumUnchanged =
      wrapMaybe $ do
        (oldHead, _) <- getMaybeOne inputsWithHeadDatum
        (newHead, _) <- getMaybeOne outputsWithHeadDatum
        return $ oldHead == newHead

    allTokensBurntCorrectly
      | length outputsWithNodeDatum == 1 =
        wrapMaybe $ do
          ((node1, _), (node2, _)) <- sort2 <$> getMaybeTwo inputsWithNodeDatum
          node1Next <- getNext node1
          node1Key <- getKey node1
          node2Key <- getKey node2
          (node1New, _) <- getMaybeOne outputsWithNodeDatum
          node1NewKey <- getKey node1New
          return $
            if isNothing . getNext $ node2
              then (isNothing . getNext $ node1New) && (node1NewKey == node1Key) && (node1Next == node2Key)
              else wrapMaybe $ do
                node2Next <- getNext node2
                node1NewNext <- getNext node1New
                return $
                  (node1NewKey == node1Key)
                    && (node1NewNext == node2Next)
                    && (node1Next == node2Key)
      | length inputsWithNodeDatum == 1 && null outputsWithNodeDatum = True
      | otherwise = False

    -- MingGov is used as the redeemer and finding a single and identical list node in inputs and outputs
    -- means that we want to update it, .e.g add listGov tokens to it
    nodeCanBeUpdated =
      wrapMaybe $ do
        (inputDatum, _) <- getMaybeOne inputsWithNodeDatum
        (outputDatum, _) <- getMaybeOne outputsWithNodeDatum
        return $ inputDatum == outputDatum

    -- Makes sure the pointers of list node being updated and head remain unchanged.
    nodeUpdatedWithCorrectPointers =
      wrapMaybe $ do
        (nodeIn, _) <- getMaybeOne inputsWithNodeDatum
        (nodeOut, _) <- getMaybeOne outputsWithNodeDatum
        (oldHead, _) <- getMaybeOne inputsWithHeadDatum
        (newHead, _) <- getMaybeOne outputsWithHeadDatum
        return $
          -- Can't alter node's state
          nodeIn == nodeOut
            -- Can't alter head's state
            && oldHead == newHead

    -- MingGov is used as the redeemer and number of output list nodes is more than the number of input list nodes by one
    -- means that we want to insert a new node to list.
    nodeCanBeInserted =
      wrapMaybe $ do
        (oldHead, _) <- getMaybeOne inputsWithHeadDatum
        return $
          -- Inserting the first node
          (isNothing (getNext oldHead) && (length outputsWithNodeDatum == 1) && null inputsWithNodeDatum)
            -- Inserting a new node next to the head, while the head already has one
            || (isJust (getNext oldHead) && null inputsWithNodeDatum && length outputsWithNodeDatum == 1)
            -- Inserting a new node to other places in the list
            || (length inputsWithNodeDatum == 1 && length outputsWithNodeDatum == 2)

    -- Makes sure the pointers of the new list node and its parent are adjusted correctly.
    nodeInsertedWithCorrectPointers
      -- Adding the first node to the list.
      | isNothing (getNext oldHead) && null inputsWithNodeDatum && (length outputsWithNodeDatum == 1) =
        wrapMaybe $ do
          (node, _) <- getMaybeOne outputsWithNodeDatum
          next <- getNext newHead
          key <- getKey node
          oldFeeRate <- getFeeRate oldHead
          newFeeRate <- getFeeRate newHead
          return $
            -- New head must point to the new node
            key == next
              -- New node must point to Nothing
              && (isNothing . getNext $ node)
              -- Head's data other than next (feeRate) must remain unchanged
              && oldFeeRate == newFeeRate
      -- Inserting a new node after head while it already has one next
      | isJust (getNext oldHead) && null inputsWithNodeDatum && (length outputsWithNodeDatum == 1) =
        wrapMaybe $ do
          (newNode, _) <- getMaybeOne outputsWithNodeDatum
          oldHeadNext <- getNext oldHead
          newNodeKey <- getKey newNode
          newHeadNext <- getNext newHead
          newNodeNext <- getNext newNode
          oldFeeRate <- getFeeRate oldHead
          newFeeRate <- getFeeRate newHead
          oldHeadPkh <- getHeadPkh oldHead
          newHeadPkh <- getHeadPkh newHead
          return $
            -- New node's position in the list must be correct
            (newNodeKey < oldHeadNext)
              -- New head must point to the new node
              && (newHeadNext == newNodeKey)
              -- New node must point to the previous head's next
              && (newNodeNext == oldHeadNext)
              -- Head's data other than next (feeRate) must remain unchanged
              && oldFeeRate == newFeeRate
              && oldHeadPkh == newHeadPkh
      -- Inserting a new node in other places
      | (length inputsWithNodeDatum == 1) && (length outputsWithNodeDatum == 2) =
        wrapMaybe $ do
          outputs <- getMaybeTwo outputsWithNodeDatum
          let ((newFirst, _), (newNode, _)) = sort2 outputs
          (oldFirst, _) <- getMaybeOne inputsWithNodeDatum
          oldFirstKey <- getKey oldFirst
          newFirstNext <- getNext newFirst
          newFirstKey <- getKey newFirst
          newNodeKey <- getKey newNode
          return $
            if isNothing . getNext $ oldFirst
              then -- The new node as added to the end of the list

                (newNodeKey == newFirstNext)
                  && (oldFirstKey == newFirstKey)
                  && (isNothing . getNext $ newNode)
                  && oldHead == newHead
              else wrapMaybe $ do
                -- The new node is added to another slot.
                oldFirstNext <- getNext oldFirst
                newNodeNext <- getNext newNode
                return $
                  (newFirstKey == oldFirstKey)
                    && (newNodeNext == oldFirstNext)
                    && (newFirstNext == newNodeKey)
                    && oldHead == newHead
      | otherwise = False
      where
        (oldHead, _) =
          case getMaybeOne inputsWithHeadDatum of
            Just h -> h
            Nothing -> traceError "No head found in inputs"
        (newHead, _) =
          case getMaybeOne outputsWithHeadDatum of
            Just h -> h
            Nothing -> traceError "No head found in outputs"

    checkUniqueToken =
      any
        ( \(datum', txO) ->
            datumIsHead (gov'list datum') && uTokenPresent txO
        )
        $ getInputDatumsWithTx @GovDatum ctx

    checkProofTokenMinted =
      assetClassValueOf valueMinted proofToken == 1

    checkHeadInOutputs = length outputsWithHeadDatum == 1

    checkFreeGovPresent = assetClassValueOf valueIn freeGov > 0

    checkListGovIsMinted = listGovMinted > 0

    checkGovBurnt = freeGovMinted < 0

    listGovSentBackUnchanged = True -- TODO
    freeGovSentBackUnchanged = True -- TODO

    ------------------------------------------------------------------------------
    -- Helpers

    outputsWithHeadDatum :: [(GovDatum, TxOut)]
    outputsWithHeadDatum =
      filter
        ( \(datum', txO) ->
            datumIsHead (gov'list datum') && uTokenPresent txO && pTokenPresent txO
        )
        $ getDatumsWithTx @GovDatum (getContinuingOutputs ctx) ctx

    outputsWithNodeDatum :: [(GovDatum, TxOut)]
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

    inputsWithHeadDatum, inputsWithNodeDatum :: [(GovDatum, TxOut)]
    inputsWithHeadDatum = filter (\(d, txO) -> pTokenPresent txO && uTokenPresent txO && (datumIsHead . gov'list $ d)) inputsFromScriptAddress
    inputsWithNodeDatum = filter (\(d, _) -> datumIsNode . gov'list $ d) inputsFromScriptAddress

    ownAddress :: Address
    ownAddress =
      case txOutAddress . txInInfoResolved <$> findOwnInput ctx of
        Just addr -> addr
        Nothing -> traceError "Can't find own address!"

    ownCurrencySymbol' :: CurrencySymbol
    ownCurrencySymbol' =
      case otherSymbols of
        [s] -> s
        [] -> traceError "Gov Proof Token must be in the list head."
        _ -> traceError "Illegal currency symbols found in the input."
      where
        (_, outputHeadTx) =
          head $
            filter
              ( \(datum', txO) ->
                  datumIsHead (gov'list datum') && uTokenPresent txO
              )
              $ getDatumsWithTx @GovDatum (getContinuingOutputs ctx) ctx
        fst3 (a, _, _) = a
        value = txOutValue outputHeadTx
        allCurrencySymbols = fst3 <$> flattenValue value
        validSymbols = [fst . unAssetClass $ ut, adaSymbol]
        otherSymbols = filter (`notElem` validSymbols) allCurrencySymbols

    asset tn = AssetClass (ownCurrencySymbol', TokenName tn)

    maybeUserId =
      case act of
        MintGov
          | length outputsWithNodeDatum == 1 ->
            do
              (datum, _) <- getMaybeOne outputsWithNodeDatum
              getId datum
          | otherwise ->
            do
              (datum, _) <- snd . sort2 <$> getMaybeTwo outputsWithNodeDatum
              getId datum
        ProofAndBurn
          | (length inputsWithNodeDatum == 2) && (length outputsWithNodeDatum == 1) ->
            do
              (datum, _) <- snd .sort2 <$> getMaybeTwo inputsWithNodeDatum
              getId datum
          | (length inputsWithNodeDatum == 1) && null outputsWithNodeDatum ->
            do
              (datum, _) <- getMaybeOne inputsWithNodeDatum
              getId datum
          | (length inputsWithNodeDatum == 1) && (length outputsWithNodeDatum == 1) ->
            do
              (datum, _) <- getMaybeOne inputsWithNodeDatum
              getId datum
        _ -> Nothing
      where
        getId datum =
          do
            key <- getKey datum
            return . getPubKeyHash . getUserId $ key

    userId :: BuiltinByteString
    userId = case maybeUserId of
      Just uid -> uid
      Nothing -> traceError "NFT-Gov-Script-Validator: Didn't find UserId."

    listGov = asset $ "listGov" <> userId
    freeGov = asset $ "freeGov" <> userId
    proofToken = asset emptyByteString

    valueIn, valueOut, valueMinted :: Value
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
    uTokenPresent txO = 1 == assetClassValueOf (txOutValue txO) ut
    pTokenPresent txO = 1 == assetClassValueOf (txOutValue txO) proofToken

    wrapMaybe :: Maybe Bool -> Bool
    wrapMaybe v = Just True == v

    getMaybeOne :: [a] -> Maybe a
    getMaybeOne [x] = Just x
    getMaybeOne _ = Nothing

    getMaybeTwo :: [a] -> Maybe (a, a)
    getMaybeTwo [x, y] = Just (x, y)
    getMaybeTwo _ = Nothing

    getNext :: GovDatum -> Maybe UserId
    getNext (GovDatum (HeadLList _ next)) = next
    getNext (GovDatum (NodeLList _ _ next)) = next

    getFeeRate :: GovDatum -> Maybe Rational
    getFeeRate (GovDatum d) =
      case d of
        HeadLList (GovLHead feeRate _) _ -> Just feeRate
        _ -> Nothing

    getHeadPkh :: GovDatum -> Maybe PubKeyHash
    getHeadPkh (GovDatum datum) =
      case datum of
        HeadLList (GovLHead _ pkh) _ -> Just pkh
        _ -> Nothing

    getKey :: GovDatum -> Maybe UserId
    getKey (GovDatum (HeadLList _ _)) = Nothing
    getKey (GovDatum (NodeLList k _ _)) = Just k

    datumIsHead (HeadLList _ _) = True
    datumIsHead _ = False

    datumIsNode = not . datumIsHead

    sort2 :: ((GovDatum, TxOut), (GovDatum, TxOut)) -> ((GovDatum, TxOut), (GovDatum, TxOut))
    sort2 (x'@(x, _), y'@(y, _)) = if x < y then (x', y') else (y', x')

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
