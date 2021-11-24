module Mlabs.NFT.Governance.Validation (
  govScript,
  govMintPolicy,
  govScrAddress,
  GovManage,
) where

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

import Plutus.V1.Ledger.Ada (adaSymbol, lovelaceValueOf)
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
      traceIfFalse "Fee must be paid to the list head." checkFeeToTheListHead -- It automatically checks that the list head is present.
        && traceIfFalse
          "List node incorrectly minted or updated"
          (nodeCanBeUpdated && nodeUpdatedWithCorrectValues)
          || (nodeCanBeInserted && nodeInsertedWithCorrectValues)
        && traceIfFalse "Equal amounts of listGov and freeGov tokens must be minted/burned." checkListGovFreeGovEquality
        && traceIfFalse "Only allowd to mint/burn xGov and Gov" checkOnlyListGovFreeGovMinted
        && traceIfFalse "The minted amount must be equal to the fee." checkGovEqualToFee
    Proof -> traceError "Not allowed to mint using Proof as the redeemer."
    ProofAndBurn ->
      traceIfFalse "Only allowd to mint/burn freeGov and listGov" checkOnlyListGovFreeGovMinted
        && traceIfFalse "Node values updated incorrectly" burntWithCorrectValues
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

    nodeCanBeUpdated =
      wrapMaybe $ do
        (inputDatum, _) <- getMaybeOne inputsWithNodeDatum
        (outputDatum, _) <- getMaybeOne outputsWithNodeDatum
        return $ inputDatum == outputDatum

    nodeUpdatedWithCorrectValues =
      wrapMaybe $ do
        (headDatum, txIn) <- getMaybeOne inputsWithNodeDatum
        (_, txOut) <- getMaybeOne outputsWithNodeDatum
        feeRate <- getFeeRate headDatum
        let inListGov = assetClassValueOf (txOutValue txIn) listGov
            outListGov = assetClassValueOf (txOutValue txOut) listGov
            fee = getFee feeRate
        return $
          outListGov == inListGov + fee

    nodeCanBeInserted =
      wrapMaybe $ do
        (oldHead, _) <- getMaybeOne inputsWithHeadDatum
        return $
          (isNothing (getNext oldHead) && (length outputsWithNodeDatum == 1) && null inputsWithNodeDatum)
            || (null inputsWithNodeDatum && (length outputsWithNodeDatum == 1))
            || ((length inputsWithNodeDatum == 1) && (length outputsWithNodeDatum == 2))

    burntWithCorrectValues =
      wrapMaybe $ do
        (headDatum, headTxIn) <- getMaybeOne inputsWithHeadDatum
        feeRate <- getFeeRate headDatum
        (_, headTxOut) <- getMaybeOne outputsWithHeadDatum
        (_, nodeTxIn) <- getMaybeOne inputsWithNodeDatum
        (_, nodeTxOut) <- getMaybeOne outputsWithNodeDatum
        let freeGovBurnt = (-1) * freeGovMinted
            listGovBurnt = (-1) * listGovMinted
            nodeInVal = txOutValue nodeTxIn
            nodeOutVal = txOutValue nodeTxOut
            headOutVal = txOutValue headTxIn
            headInVal = txOutValue headTxOut
            nodeRemovedVal = nodeInVal <> negate nodeOutVal
            fee = getFee feeRate
        return $
          nodeRemovedVal == assetClassValue listGov listGovBurnt
            && freeGovBurnt == listGovBurnt
            && headOutVal `geq` (headInVal <> (negate . lovelaceValueOf $ fee))

    nodeInsertedWithCorrectValues
      | null inputsWithNodeDatum && (length outputsWithNodeDatum == 1) =
        wrapMaybe $ do
          (_, txNode) <- getMaybeOne outputsWithNodeDatum
          let newNodeVal = txOutValue txNode
          return $
            newNodeVal == assetClassValue listGov fee
      | (length inputsWithNodeDatum == 1) && (length outputsWithNodeDatum == 2) =
        wrapMaybe $ do
          outputs <- getMaybeTwo outputsWithNodeDatum
          (_, txOldFirst) <- getMaybeOne inputsWithNodeDatum
          let ((_, txNewFirst), (_, txNewNode)) = sort2 outputs
              oldFirstVal = txOutValue txOldFirst
              newFirstVal = txOutValue txNewFirst
              newNodeVal = txOutValue txNewNode
          return $
            (newNodeVal == assetClassValue listGov fee) && (oldFirstVal == newFirstVal)
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

    checkFeeToTheListHead =
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

    wrapMaybe v = Just True == v

    datumIsNode = not . datumIsHead

    tokenPresent txO = 1 == assetClassValueOf (txOutValue txO) proofToken
    toScriptAddress txO = txOutAddress txO == appInstance'Address appInstance

    datumIsHead (HeadLList _ _) = True
    datumIsHead _ = False

    userId = getPubKeyHash $ head (txInfoSignatories . scriptContextTxInfo $ ctx) -- FIXME more sophisticated way is needed
    symbol = ownCurrencySymbol ctx
    asset tn = AssetClass (symbol, TokenName tn)
    freeGov = asset ("freeGov" <> userId)
    listGov = asset ("listGov" <> userId)
    proofToken = asset emptyByteString

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

    getMaybeTwo :: [a] -> Maybe (a, a)
    getMaybeTwo [x, y] = Just (x, y)
    getMaybeTwo _ = Nothing

    price = 10 -- FIXME
    getFee :: Ratio Integer -> Integer
    getFee feeRate = round $ fromInteger price * feeRate

    getFeeRate :: GovDatum -> Maybe Rational
    getFeeRate (GovDatum datum) =
      case datum of
        HeadLList (GovLHead fee) _ -> Just fee
        _ -> Nothing

    getNext :: GovDatum -> Maybe UserId
    getNext (GovDatum (HeadLList _ next)) = next
    getNext (GovDatum (NodeLList _ _ next)) = next

    sort2 :: ((GovDatum, TxOut), (GovDatum, TxOut)) -> ((GovDatum, TxOut), (GovDatum, TxOut))
    sort2 (x'@(x, _), y'@(y, _)) = if x > y then (x', y') else (y', x')

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
      traceIfFalse "New listGov tokens must be minted" checkListGovIsMinted -- We need to ensure that our minting policy verifications are called.
        && traceIfFalse
          "List node incorrectly minted or updated"
          (nodeCanBeUpdated && nodeUpdatedWithCorrectPointers)
          || (nodeCanBeInserted && nodeInsertedWithCorrectPointers)
    Proof ->
      traceIfFalse "Free Gov tokens are required to unlock." freeGovPresent
        && traceIfFalse "ListGov must be sent bach unchanged." listGovSentBackUnchanged
        && traceIfFalse "FreeGov must be sent bach unchanged." freeGovSentBackUnchanged
    ProofAndBurn ->
      traceIfFalse "Free Gov tokens are required to unlock." freeGovPresent
        && traceIfFalse "New head's datum be the same as old head's datum" checkHeadsEquality
        && traceIfFalse "New node's datum be the same as old node's datum" checkBurntNodeEquality
        && traceIfFalse "Free Gov tokens must be burnt." checkGovBurnt -- We need to ensure that our minting policy verifications are called.
  where
    sort2 :: ((GovDatum, TxOut), (GovDatum, TxOut)) -> ((GovDatum, TxOut), (GovDatum, TxOut))
    sort2 (x'@(x, _), y'@(y, _)) = if x > y then (x', y') else (y', x')

    nodeCanBeUpdated =
      wrapMaybe $ do
        (inputDatum, _) <- getMaybeOne inputsWithNodeDatum
        (outputDatum, _) <- getMaybeOne outputsWithNodeDatum
        return $ inputDatum == outputDatum

    nodeUpdatedWithCorrectPointers =
      wrapMaybe $ do
        (inputDatum, _) <- getMaybeOne inputsWithNodeDatum
        (outputDatum, _) <- getMaybeOne outputsWithNodeDatum
        (oldHead, _) <- getMaybeOne inputsWithHeadDatum
        (newHead, _) <- getMaybeOne outputsWithHeadDatum
        return $
          inputDatum == outputDatum
            && oldHead == newHead

    checkBurntNodeEquality =
      wrapMaybe $ do
        (oldDatum, _) <- getMaybeOne inputsWithNodeDatum
        (newDatum, _) <- getMaybeOne outputsWithNodeDatum
        return $
          oldDatum == newDatum

    nodeCanBeInserted =
      wrapMaybe $ do
        (oldHead, _) <- getMaybeOne inputsWithHeadDatum
        return $
          (isNothing (getNext oldHead) && (length outputsWithNodeDatum == 1) && null inputsWithNodeDatum)
            || (null inputsWithNodeDatum && (length outputsWithNodeDatum == 1))
            || ((length inputsWithNodeDatum == 1) && (length outputsWithNodeDatum == 2))

    nodeInsertedWithCorrectPointers
      | isNothing (getNext oldHead) && null inputsWithNodeDatum && (length outputsWithNodeDatum == 1) =
        wrapMaybe $ do
          (node, _) <- getMaybeOne outputsWithNodeDatum
          next <- getNext newHead
          key <- getKey node
          oldFeeRate <- getFeeRate oldHead
          newFeeRate <- getFeeRate newHead
          return $
            key == next
              && (isNothing . getNext $ node)
              && oldFeeRate == newFeeRate
      | isJust (getNext oldHead) && null inputsWithNodeDatum && (length outputsWithNodeDatum == 1) =
        wrapMaybe $ do
          (newNode, _) <- getMaybeOne outputsWithNodeDatum
          secondKey <- getNext oldHead
          newNodeKey <- getKey newNode
          newHeadNext <- getNext newHead
          newNodeNext <- getNext newNode
          oldFeeRate <- getFeeRate oldHead
          newFeeRate <- getFeeRate newHead
          return $
            (newNodeKey < secondKey)
              && (newHeadNext == newNodeKey)
              && (newNodeNext == secondKey)
              && oldFeeRate == newFeeRate
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
              then
                (newNodeKey == newFirstNext)
                  && (oldFirstKey == newFirstKey)
                  && (isNothing . getNext $ newNode)
                  && oldHead == newHead
              else wrapMaybe $ do
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

    checkListGovIsMinted = listGovMinted > 0

    checkHeadsEquality =
      wrapMaybe $ do
        (oldHead, _) <- getMaybeOne inputsWithHeadDatum
        (newHead, _) <- getMaybeOne outputsWithHeadDatum
        return $ oldHead == newHead

    ownAddress =
      case txOutAddress . txInInfoResolved <$> findOwnInput ctx of
        Just addr -> addr
        Nothing -> traceError "Can't find own address!"

    freeGovPresent = assetClassValueOf valueIn freeGov > 0

    checkGovBurnt = freeGovMinted < 0

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
                outputs <- getMaybeTwo outputsWithNodeDatum
                key <- getKey . fst . snd . sort2 $ outputs
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

    getMaybeTwo :: [a] -> Maybe (a, a)
    getMaybeTwo [x, y] = Just (x, y)
    getMaybeTwo _ = Nothing

    getNext :: GovDatum -> Maybe UserId
    getNext (GovDatum (HeadLList _ next)) = next
    getNext (GovDatum (NodeLList _ _ next)) = next

    getFeeRate :: GovDatum -> Maybe Rational
    getFeeRate (GovDatum d) =
      case d of
        HeadLList (GovLHead feeRate) _ -> Just feeRate
        _ -> Nothing

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
