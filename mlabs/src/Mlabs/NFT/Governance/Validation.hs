module Mlabs.NFT.Governance.Validation (
  govScript,
  govMintPolicy,
  govScrAddress,
  GovManage,
) where

--import Prelude qualified as Hask

import Ledger (
  Address,
  MintingPolicy,
  ScriptContext,
  CurrencySymbol,
  TxOut,
  mkMintingPolicyScript,
  ownCurrencySymbol,
  txOutValue,
  scriptContextTxInfo,
  getDatum,
  findDatum,
  txOutDatumHash,
  txInInfoResolved,
  txInfoInputs,
  txInfoOutputs,
  txOutAddress,
  findOwnInput,
  getContinuingOutputs,
  txInfoMint,
  txInfoSignatories,
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
  GovDatum,
  LList (..),
  gov'list,
  GovLHead (..),
  GovLNode (..),
  GovDatum (..),
  GovLList,
  )

import Mlabs.NFT.Types (
  NftAppInstance,
  appInstance'Address,
  UserId (..),
  UniqueToken (..),
  )

import PlutusTx qualified
import PlutusTx.Prelude
import PlutusTx.Ratio (Ratio)
import Plutus.V1.Ledger.Value (
  AssetClass (..),
  TokenName (..),
  assetClassValueOf,
  geq,
  flattenValue,
  assetClassValue,
 )

import Plutus.V1.Ledger.Ada (lovelaceValueOf, adaSymbol)
import PlutusTx.Builtins (divideInteger)

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

      && traceIfFalse "List node incorrectly minted or updated"
        (nodeCanBeUpdated && nodeCorrectlyUpdated) || (nodeCanBeInserted && nodeCorrectlyInserted)


      && traceIfFalse "Equal amounts of listGov and freeGov tokens must be minted/burned." checkListGovFreeGovEquality
      && traceIfFalse "Only allowd to mint/burn xGov and Gov" checkOnlyListGovFreeGovMinted
      && traceIfFalse "The minted/burned amount must be equal to the fee." checkGovEqualToFee
      && traceIfFalse "listGov tokens must be sent to the new node" checkAllListGovToAddress


      -- makes sure that 1:1 Gov/xGov tokens are minted with the amount of
      -- lovelace paid at the Treasury address. Makes sure that the Gov is
      -- inserted to the linked list (correctly).

      -- So after initiation, if a user wants to perform any action, he does these also:
      --  pays the fee in lovelace to the list head
      --  mints equally amounts of Gov and xGov tokens (equal to the amount of lovelace spent as the fee)
      --  adds a new node to the GovLList with GovLNode datum and sends the minted Gov tokens to the new node
      --  (also sets the key of the new node equal to his PubKeyHash)

    Proof -> traceError "Not allowed to mint using Proof as the redeemer."

    ProofAndBurn ->
      traceIfFalse "List head must be spent" checkListHeadIsSpent
      && traceIfFalse "Equal amounts of xGov and Gov tokens must be burned." checkListGovFreeGovEquality
      && traceIfFalse "Maximum fee number of lovelace is allowed to remove from the list head." checkFeeFromTheListHead
      && traceIfFalse "Only allowd to mint/burn freeGov and listGov" checkOnlyListGovFreeGovMinted
      && traceIfFalse "The minted/burned amount must be equal to the fee." checkListGovEqualToNegFee
--      && traceIfFalse "List updated incorrectly" burnUpdate

      -- makes sure that Gov/xGov is removed and the Gov linked list is
      -- updated accordingly.

      -- Later, any body can get back his locked lovelace by the following steps:
      --  Spending the head, his corresponding GovLNode and the previous node in the list.
      --  Burning the Gov and xGov tokens and sending equal amount of lovelace to himself.
      --  Updating the next pointer of the previous node in the GovLList .
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
        ((HeadLList _ _), (HeadLList _ _)) -> (x', y')
        ((NodeLList _ _ _), (HeadLList _ _)) -> (x', y')
        ((HeadLList _ _), (NodeLList _ _ _)) -> (y', x')
        ((NodeLList k1 _ _), (NodeLList k2 _ _)) -> if k1 < k2 then (x', y') else (y', x')

    nodeCanBeInserted =
      wrapMaybe $ do
        (head, _) <- getMaybeOne inputsWithHeadDatum
        return $
          ((isNothing $ getNext head) && (length outputsWithNodeDatum == 1) && (length inputsWithNodeDatum == 0)) ||
          ((length inputsWithNodeDatum == 0) && (length outputsWithNodeDatum == 1) ) ||
          ((length inputsWithNodeDatum == 1) && (length outputsWithNodeDatum == 2) )

    nodeCorrectlyInserted
      | ((isNothing $ getNext head) && (length outputsWithNodeDatum == 1) && (length inputsWithNodeDatum == 0)) =
        wrapMaybe $ do
          (head, _) <- getMaybeOne outputsWithHeadDatum
          (node, _) <- getMaybeOne outputsWithNodeDatum
          next <- getNext head
          key <- getKey node
          return $ key == next

      | ((isJust $ getNext head) && (length inputsWithNodeDatum == 0) && (length outputsWithNodeDatum == 1) ) =
        wrapMaybe $ do
          (prevHead, _) <- getMaybeOne inputsWithHeadDatum
          (newHead, _) <- getMaybeOne outputsWithHeadDatum
          (newNode, _) <- getMaybeOne outputsWithNodeDatum
          secondKey <- getNext prevHead
          newNodeKey <- getKey newNode
          newHeadNext <- getNext newHead
          newNodeNext <- getNext newNode
          return $ (newNodeKey < secondKey) && (newHeadNext == newNodeKey) && (newNodeNext == secondKey)

      | ((length inputsWithNodeDatum == 1) && (length outputsWithNodeDatum == 2) ) =
        wrapMaybe $ do
          let ((newFirst, _), (newSecond, _)) = sort2 (outputsWithNodeDatum!!0, outputsWithNodeDatum!!1)
          (oldFirst, _) <- getMaybeOne inputsWithNodeDatum
          oldFirstKey <- getKey oldFirst
          newFirstNext <- getNext newFirst
          newFirstKey <- getKey newFirst
          newSecondKey <- getKey newSecond
          return $ if isNothing . getNext $ oldFirst
          then
            (newSecondKey == newFirstNext) && (oldFirstKey == newFirstKey)
          else
            wrapMaybe $ do
              oldFirstNext <- getNext oldFirst
              newSecondNext <- getNext newSecond
              return $ (newFirstKey == oldFirstKey) && (newSecondNext == oldFirstNext) && (newFirstNext == newSecondKey)
      where
        head = case getMaybeOne inputsWithHeadDatum of
          Just (h, _) -> h
          Nothing -> traceError "No head found"



    getNext :: GovDatum -> Maybe UserId
    getNext (GovDatum (HeadLList _ next)) = next
    getNext (GovDatum (NodeLList _ _ next)) = next

    checkListHeadIsSpent =
      case inputsWithHeadDatum of
        [] -> False
        [_] -> True
        _   -> traceError "Multiple unique tokens found" -- This should never happen

    checkFeeToTheListHead =
      wrapMaybe $ do
        (h, txOut) <- getMaybeOne outputsWithHeadDatum
        (_, txIn) <- getMaybeOne inputsWithHeadDatum
        feeRate <- getFeeRate h
        let valOut = txOutValue txOut
            valIn = txOutValue txIn
        return $ valOut `geq` (valIn <> lovelaceValueOf (getFee feeRate))

--     burnUpdate =
--      wrapMaybe $ do
--        (headIn, headInTxOut) <- getMaybeOne inputsWithHeadDatum
--        (nodeIn, nodeInTxOut) <- getMaybeOne inputsWithNodeDatum
--
--        (headOut, headOutTxOut) <- getMaybeOne outputsWithHeadDatum
--        (nodeOut, nodeOutTxOut) <- getMaybeOne outputsWithNodeDatum
--
--        feeRate <- getFeeRate headIn
--
--        return $
--          (lovelaceValueOf (txOutValue headOut) >= lovelaceValueOf (txOutValue headIn) - (getFee feeRate) )
--          &&

    checkFeeFromTheListHead =
      wrapMaybe $ do
        (h, txOut) <- getMaybeOne outputsWithHeadDatum
        (_, txIn) <- getMaybeOne inputsWithHeadDatum
        feeRate <- getFeeRate h
        let valOut = txOutValue txOut
            valIn = txOutValue txIn
        return $ valOut `geq` (valIn <> (lovelaceValueOf (getFee feeRate)))

    checkListGovFreeGovEquality = freeGovMinted == listGovMinted

    checkOnlyListGovFreeGovMinted =
      valueMinted  ==
      (assetClassValue freeGov freeGovMinted) <> (assetClassValue listGov $ listGovMinted)

    checkOnlyProofMinted =
      valueMinted  == (assetClassValue proofToken 1)

    checkGovEqualToFee =
      wrapMaybe $ do
        (h, _) <- getMaybeOne outputsWithHeadDatum
        feeRate <- getFeeRate h
        return $ freeGovMinted == getFee feeRate


    checkListGovEqualToNegFee =
      wrapMaybe $ do
        (h, _) <- getMaybeOne outputsWithHeadDatum
        feeRate <- getFeeRate h
        return $ freeGovMinted == (-1) * (getFee feeRate)

    checkAllListGovToAddress =
      listGovMinted == (assetClassValueOf (foldMap (txOutValue . snd) outputsToScriptAddress) listGov)


    nodeCorrectlyUpdated =
      wrapMaybe $ do
        (input, txOIn) <- getMaybeOne inputsWithNodeDatum
        (output, txOOut) <- getMaybeOne outputsWithNodeDatum
        (head, _) <- getMaybeOne inputsWithHeadDatum
        feeRate <- getFeeRate head
        let inListGov = (assetClassValueOf (txOutValue txOIn) listGov)
            outListGov = (assetClassValueOf (txOutValue txOOut) listGov)
        return $ (outListGov < inListGov + (getFee feeRate))
          && (input == output)

    wrapMaybe v = maybe False id v

    datumIsNode = not . datumIsHead

    datumIsHead (HeadLList _ _) = True
    datumIsHead _             = False

    getFeeRate :: GovDatum -> Maybe Rational
    getFeeRate (GovDatum datum) =
      case datum of
        HeadLList (GovLHead fee) _ -> Just fee
        _ -> Nothing


    symbol = ownCurrencySymbol ctx
    asset tn = AssetClass (symbol, TokenName tn)
    userId = getPubKeyHash $ (txInfoSignatories . scriptContextTxInfo $ ctx)!!0
    freeGov = asset ("freeGov" <> userId) -- FIXME more sophisticated way is needed
    listGov = asset ("listGov" <> userId)
    proofToken = asset emptyByteString

    tokenPresent txO = 1 == assetClassValueOf (txOutValue txO) proofToken

    toScriptAddress txO = txOutAddress txO == appInstance'Address appInstance


    valueIn = foldMap (txOutValue . txInInfoResolved) $ txInfoInputs . scriptContextTxInfo $ ctx
    --    ownValueIn = valueOf valueIn symbol gov <> valueOf valueIn symbol xGov
    --    More complicated of finding value in with our own currency symbol.
    --    Useful for then we have token names other than Gov and xGov
    --    ownValueIn = Value <$>
    --      (AssocMap.fromList <$> (sequence [(,) <$> pure symbol  <*> (AssocMap.lookup symbol $ getValue valueIn)]))

    valueOut = foldMap txOutValue $ txInfoOutputs . scriptContextTxInfo $ ctx

    valueMinted =
      let allMinted = flattenValue $ valueOut <> negate valueIn
      in mconcat
        . map (\(s, n, v) -> assetClassValue (AssetClass (s, n)) v)
        . filter (\(s, _, _) -> s == ownCurrencySymbol ctx)
        $ allMinted

    freeGovMinted = assetClassValueOf valueMinted freeGov
    listGovMinted = assetClassValueOf valueMinted listGov

--    validInput txO = (assetClassValueOf (txOutValue txO) gov) >= 0

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
    MintGov -> -- Inserting new node in the governance linked list is a bit more complicated than the NFT linked list.
               -- The reason is that here we need the list head to always be consumed, and it hardens
               -- the detection of 'first', 'second', 'newFirst', 'newInserted' in the terminology
               -- of NFT/Validation.hs validator script. So we can't use the same pattern here.
      case gov'list datum of
        HeadLList (GovLHead fee) Nothing ->
          -- We are head and there must be only one node input. (There is no update, just adding new node)
          traceIfFalse "New listGov tokens must be minted" checkListGovIsMinted -- We need to ensure that our minting policy verifications are called.
          && traceIfFalse "New head's fee must be the same as old head's fee" (checkHeadsFeeEquality fee)
          && traceIfFalse "New first must point to the new node" (newFirst `pointsTo` (newSecond >>= getKey . fst))
          && traceIfFalse "New node must contain listGov equal to fee" checkGovIsPaid
--          && traceIfFalse "New head must have unique token and proof token" checkNewHeadHasTokens
          && traceIfFalse "New head must have value everything plus fee" checkHeadHasEverythingPlusFee

        HeadLList (GovLHead fee) (Just key) ->
          traceIfFalse "New listGov tokens must be minted" checkListGovIsMinted -- We need to ensure that our minting policy verifications are called.
          && traceIfFalse "New head's fee must be the same as old head's fee" (checkHeadsFeeEquality fee)
          && traceIfFalse "New first must point to the new node" (newFirst `pointsTo` (newSecond >>= getKey . fst))
          && traceIfFalse "New node must point to the old first's next if there was any" checkNewNodePointsCorrectly
          && traceIfFalse "New node must contain listGov equal to fee" checkGovIsPaid
          && traceIfFalse "New head must have everything plus fee" checkHeadHasEverythingPlusFee

        NodeLList _ _ _ ->
          traceIfFalse "New Gov tokens must be minted" checkListGovIsMinted
          && traceIfFalse "Old first must remain consistent" checkFirstIsConsistent -- This line actually always executes. Since there has to be at least one node input
  --            && traceIfFalse "New first must point to the new node" (newFirst `pointsTo` (newSecond >>= getKey . fst))
  --            && traceIfFalse "New node must point to the old first's next" (newSecond `pointsTo` oldSecond)
  --            && traceIfFalse "New node must contain Gov equal to fee" checkGovIsPaid
  --            && traceIfFalse "New first/head must have value equal to old first/head" checkFirstValueIsConsistent


    Proof ->
      traceIfFalse "Free Gov tokens are required to unlock." freeGovPresent
      && traceIfFalse "ListGov must be sent bach unchanged." listGovSentBackUnchanged
      && traceIfFalse "FreeGov must be sent bach unchanged." freeGovSentBackUnchanged
--      case gov'list datum of
--        HeadLList (GovLHead fee) (Just _) ->
--          traceIfFalse "New head's fee must be the same as old head's fee" (checkHeadsFeeEquality fee)
--          && traceIfFalse "New head must have value at least equal to old head" checkFirstValueIsConsistent
--          && traceIfFalse "New head's next must be equal to old head's next" checkFirstNextIsConsistent
--          && traceIfFalse "New head must have the unique token and the proof token" checkNewHeadHasTokens
--
--        HeadLList (GovLHead fee) Nothing ->
--          traceError "Not allowed to consume a head as a Proof when it has no next. Use MintGov instead."
--
--        NodeLList key _ next ->
--          traceError "Not implemented. Spec needed" -- FIXME

       -- makes sure that the token is used as proof and returned to the Gov
      -- Address, with nothing being altered.
    ProofAndBurn ->
      -- There has to be only one input node, and that's us. Only updated is allowed to happen
      traceIfFalse "FILL IN LATER" True -- FIXME

      -- makes sure, that the corresponding Gov to the xGov is removed from
      -- the list. The user can also claim their locked lovelace back (take
      -- their stake out of the app).

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
          ((HeadLList _ _), (HeadLList _ _)) -> (x', y')
          ((NodeLList _ _ _), (HeadLList _ _)) -> (x', y')
          ((HeadLList _ _), (NodeLList _ _ _)) -> (y', x')
          ((NodeLList k1 _ _), (NodeLList k2 _ _)) -> if k1 < k2 then (x', y') else (y', x')

      oldFirst :: Maybe (GovDatum, TxOut)
      oldFirst =
        case act of
          MintGov ->
            case gov'list datum of
              HeadLList _ Nothing -> getMaybeOne inputsWithHeadDatum
              HeadLList _ _
                | length inputsFromScriptAddress == 1 -> getMaybeOne inputsFromScriptAddress -- Inserting after head
                | length inputsFromScriptAddress == 2 -> getMaybeOne inputsWithNodeDatum -- Inserting somewhere else
                | otherwise -> traceError "Illegal script inptuts"
              NodeLList _ _ _ -> getMaybeOne inputsWithNodeDatum
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
              NodeLList _ _ _ -> getMaybeOne inputsWithNodeDatum
          _ -> traceError "Pointers only are allowed for minting."

      oldSecond :: Maybe UserId
      oldSecond = oldFirst >>= getNext . fst

      newSecond :: Maybe (GovDatum, TxOut)
      newSecond =
        case act of
          MintGov ->
            case gov'list datum of
              HeadLList _ Nothing -> getMaybeOne outputsWithNodeDatum
              HeadLList _ _
                | length inputsFromScriptAddress == 1 -> getMaybeOne outputsWithNodeDatum -- Inserting after head
                | length inputsFromScriptAddress == 2 ->
                  do
                    [d1, d2] <- getMaybeTwo outputsWithNodeDatum
                    return $ snd . sort2 $ (d1, d2)
                | otherwise -> traceError "Illegal script inptuts"
              NodeLList _ _ _ -> getMaybeOne inputsWithNodeDatum
          _ -> traceError "Pointers only are allowed for minting."


      nodeToBurn :: Maybe (GovDatum, TxOut)
      nodeToBurn =
        case act of
          ProofAndBurn -> getMaybeOne inputsWithNodeDatum
          _ -> traceError "Act has to be ProofAndBurn"

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
          _ -> (newSecond `pointsTo` oldSecond)

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

      checkNewHeadPointsToNewNode =
        wrapMaybe $ do
          (newHeadDatum, _) <- getMaybeOne outputsWithHeadDatum
          newHeadNext <- getNext newHeadDatum
          (newNodeDatum, _) <- getMaybeOne outputsWithNodeDatum
          newNodeKey <- getKey newNodeDatum
          return $ newNodeKey == newHeadNext

      checkHeadHasEverythingPlusFee =
        wrapMaybe $ do
          (d, oldTxO) <- getMaybeOne inputsWithHeadDatum
          (_, newTxO) <- getMaybeOne outputsWithHeadDatum
          feeRate <- getFeeRate d
          return $ (txOutValue newTxO) `geq` ((txOutValue oldTxO) <> (lovelaceValueOf $ getFee feeRate))

      checkListHeadIsSpent = length inputsWithHeadDatum == 1

      checkGovIsPaid =
        wrapMaybe $ do
          (head, _) <- getMaybeOne inputsWithHeadDatum
          feeRate <- getFeeRate head
          (_, txO) <- newSecond
          return $ assetClassValueOf (txOutValue txO) listGov == getFee feeRate -- geq won't work since we can't handle multiple minting at in this version

      checkHeadsFeeEquality oldFee =
        wrapMaybe $ do
          (newDatum, _) <- getMaybeOne outputsWithHeadDatum
          newFee <- getFeeRate newDatum
          return $ newFee == oldFee

      ownAddress =
        case txOutAddress . txInInfoResolved <$> findOwnInput ctx of
          Just addr -> addr
          Nothing -> traceError "Can't find own address!"

      freeGovPresent = True -- TODO
      listGovSentBackUnchanged = True -- TODO
      freeGovSentBackUnchanged = True -- TODO

      ownCurrencySymbol' :: CurrencySymbol
      ownCurrencySymbol' =
        case otherSymbols of
          [s] -> s
          [] -> traceError "Gov Proof Token must be in the list head."
          _ -> traceError "Illegal currency symbols found in the input."
        where fst3 (a, _, _) = a
              value = txOutValue . txInInfoResolved <$> findOwnInput ctx
              allCurrencySymbols =
                case value of
                  Nothing -> traceError "Own input wasn't find!"
                  Just v -> fmap fst3 $ flattenValue v
              validSymbols = [fst . unAssetClass $ ut, adaSymbol]
              otherSymbols = filter (\s -> not . elem s $ validSymbols) allCurrencySymbols

      asset tn = AssetClass (ownCurrencySymbol', TokenName tn)

      userId =
        case act of
          MintGov ->
            case gov'list datum of
              HeadLList _ _ ->
                do
                  (d, _) <- getMaybeOne outputsWithNodeDatum
                  key <- getKey d
                  return . getPubKeyHash . getUserId $ key
              NodeLList _ _ _ ->
                do
                  [d1, d2] <- getMaybeTwo outputsWithNodeDatum
                  key <- getKey $ fst . snd . sort2 $ (d1, d2)
                  return . getPubKeyHash . getUserId $ key

          _ ->
            do
              (d, _) <- getMaybeOne inputsWithNodeDatum
              key <- getKey d
              return . getPubKeyHash . getUserId $ key


      listGov =
        case userId of
          Just id -> asset $ "listGov" <> id
          Nothing -> traceError "UserId didn't find"
  --        xGov = asset "xGov"

      valueIn = foldMap (txOutValue . txInInfoResolved) $ txInfoInputs . scriptContextTxInfo $ ctx

      valueOut = foldMap txOutValue $ txInfoOutputs . scriptContextTxInfo $ ctx

      valueMinted =
        let allMinted = flattenValue $ valueOut <> negate valueIn
        in mconcat
          . map (\(s, n, v) -> assetClassValue (AssetClass (s, n)) v)
          . filter (\(s, _, _) -> s == ownCurrencySymbol')
          $ allMinted

      listGovMinted = assetClassValueOf valueMinted listGov
      tokenPresent txO = 1 == (assetClassValueOf (txOutValue txO) ut)


      -- makes sure that the correct fees are paid, and the correct amount
            -- of Gov/xGov is minted. Also makes sure that the Gov/xGov are sent
            -- to the correct addresses, and that the Gov list is not altered
            -- maliciously.

      wrapMaybe v = maybe False id v

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
      datumIsHead _             = False

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