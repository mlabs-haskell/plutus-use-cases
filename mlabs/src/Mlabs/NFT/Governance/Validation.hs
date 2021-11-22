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
 )
import Ledger.Typed.Scripts (
  TypedValidator,
  ValidatorTypes (..),
  mkTypedValidator,
  validatorAddress,
  wrapMintingPolicy,
  wrapValidator,
 )

import Mlabs.NFT.Governance.Types (
  GovAct (..),
  GovDatum,
  LList (..),
  gov'list,
  GovLHead (..),
  GovLNode (..),
  GovDatum (..),
  UniqueToken (..),
  GovLList,
  )
import Mlabs.NFT.Types (NftAppInstance, appInstance'Address, UserId (..))
import PlutusTx qualified
import PlutusTx.Prelude
import Plutus.V1.Ledger.Value (
  AssetClass (..),
  TokenName (..),
  assetClassValueOf,
  geq,
  flattenValue,
  assetClassValue,
 )

import Plutus.V1.Ledger.Ada (lovelaceValueOf, adaSymbol)
--import PlutusTx.Builtins.Internal (BuiltinString (..))
--import Prelude (show)
--import Data.String (fromString)
--import Data.Text (pack, unpack)
--import Data.Text.Encoding (encodeUtf8)
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
--      && traceIfFalse "New head must be in the outputs" checkListHeadInOutputs
      && traceIfFalse "Fee must be paid to the list head." checkFeeToTheListHead
      && traceIfFalse "Equal amounts of xGov and Gov tokens must be minted/burned." checkGovxGovEquality
      && traceIfFalse "Only allowd to mint/burn xGov and Gov" checkOnlyGovxGovMinted
      && traceIfFalse "The minted amount must be equal to the fee." checkGovEqualToFee
--        && traceIfFalse "A new node must be inserted to the linked list correctly" checkNewNodeInsertedCorrectly
      && traceIfFalse "Gov tokens must be sent to the new node" checkAllGovToAddress

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
      && traceIfFalse "Equal amounts of xGov and Gov tokens must be burned." checkGovxGovEquality
      && traceIfFalse "Maximum fee number of lovelace is allowed to remove from the list head." checkFeeFromTheListHead
      && traceIfFalse "Only allowd to mint/burn xGov and Gov" checkOnlyGovxGovMinted
      && traceIfFalse "The minted amount must be equal to the fee." checkGovEqualToNegFee

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

    checkListHeadIsSpent =
      case inputsWithHeadDatum of
        [] -> False
        [_] -> True
        _   -> traceError "Multiple unique tokens found" -- This should never happen

    checkFeeToTheListHead =
      wrapMaybe $ do
        (h, txOut) <- getMaybeOne outputsWithHeadDatum
        (_, txIn) <- getMaybeOne inputsWithHeadDatum
        let valOut = txOutValue txOut
            valIn = txOutValue txIn
        fee <- getFee h
        return $ valOut `geq` (valIn <> lovelaceValueOf fee)

    checkFeeFromTheListHead =
      wrapMaybe $ do
        (h, txOut) <- getMaybeOne outputsWithHeadDatum
        (_, txIn) <- getMaybeOne inputsWithHeadDatum
        let valOut = txOutValue txOut
            valIn = txOutValue txIn
        fee <- getFee h
        return $ valOut `geq` (valIn <> (lovelaceValueOf $ (-1) * fee))

    checkGovxGovEquality = xGovMinted == govMinted

    checkOnlyGovxGovMinted =
      valueMinted  ==
      (assetClassValue xGov xGovMinted) <> (assetClassValue gov $ govMinted)

    checkOnlyProofMinted =
      valueMinted  == (assetClassValue proofToken 1)

    checkGovEqualToFee =
      wrapMaybe $ do
        (h, _) <- getMaybeOne outputsWithHeadDatum
        fee <- getFee h
        return $ xGovMinted == fee

    checkGovEqualToNegFee =
      wrapMaybe $ do
        (h, _) <- getMaybeOne outputsWithHeadDatum
        fee <- getFee h
        return $ xGovMinted == (-1) * fee

    checkAllGovToAddress =
      govMinted == (assetClassValueOf (foldMap (txOutValue . snd) outputsToScriptAddress) gov)

    wrapMaybe v = maybe False id v

    datumIsHead (HeadLList _ _) = True
    datumIsHead _             = False

--    datumIsNode = not . datumIsHead

    getFee :: GovDatum -> Maybe Integer
    getFee (GovDatum datum) =
      case datum of
        HeadLList (GovLHead fee) _ -> Just fee
        _ -> Nothing

--    getNext :: GovDatum -> Maybe UserId
--    getNext (GovDatum (HeadLList _ next)) = next
--    getNext (GovDatum (NodeLList _ _ next)) = next

--    getKey (GovDatum (HeadLList _ _)) = Nothing
--    getKey (GovDatum (NodeLList _ _ k)) = k
    symbol = ownCurrencySymbol ctx
    asset tn = AssetClass (symbol, TokenName tn)
    gov = asset "Gov"
    xGov = asset "xGov"
    proofToken = asset emptyByteString

    tokenPresent txO = 1 == assetClassValueOf (txOutValue txO) proofToken

    toScriptAddress txO = txOutAddress txO == appInstance'Address appInstance

--    validInput txO = (assetClassValueOf (txOutValue txO) gov) >= 0

    getMaybeOne :: [a] -> Maybe a
    getMaybeOne [x] = Just x
    getMaybeOne _ = Nothing

    valueIn = foldMap (txOutValue . txInInfoResolved) $ txInfoInputs . scriptContextTxInfo $ ctx
--    ownValueIn = valueOf valueIn symbol gov <> valueOf valueIn symbol xGov
--    More complicated of finding value in with our own currency symbol.
--    Useful for then we have token names other than Gov and xGov
--    ownValueIn = Value <$>
--      (AssocMap.fromList <$> (sequence [(,) <$> pure symbol  <*> (AssocMap.lookup symbol $ getValue valueIn)]))

    valueOut = foldMap txOutValue $ txInfoOutputs . scriptContextTxInfo $ ctx
--    ownValueOut = valueOf valueOut symbol gov <> valueOf valueIn symbol xGov

    valueMinted =
      let allMinted = flattenValue $ valueOut <> negate valueIn
      in mconcat
        . map (\(s, n, v) -> assetClassValue (AssetClass (s, n)) v)
        . filter (\(s, _, _) -> s == ownCurrencySymbol ctx)
        $ allMinted

    xGovMinted = assetClassValueOf valueMinted xGov
    govMinted = assetClassValueOf valueMinted gov


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
      False -- FIXME It was True. Why should anybody for good reason use it as the redeemer?!
    MintGov -> -- Inserting new node in the governance linked list is a bit more complicated than the NFT linked list.
               -- The reason is that here we need the list head to always be consumed, and it hardens 
               -- the detection of 'first', 'second', 'newFirst', 'newInserted' in the terminology 
               -- of NFT/Validation.hs validator script. So we can't use the same pattern here.
      case gov'list datum of
        HeadLList (GovLHead fee) Nothing ->
          traceIfFalse "New Gov tokens must be minted" checkGovIsMinted -- We need to ensure that our minting policy verifications are called.
          && traceIfFalse "New head's fee must be the same as old head's fee" (checkHeadsFeeEquality fee)
          && traceIfFalse "New first must point to the new node" (newFirst `pointsTo` (newSecond >>= getKey . fst)) -- It automatically ensures that there is only one output with node datum
          && traceIfFalse "New node must contain Gov equal to fee" checkGovIsPaid
--          && traceIfFalse "New head must have unique token and proof token" checkNewHeadHasTokens
          && traceIfFalse "New first/head must have value at least equal to old head" checkFirstValueIsConsistent



        HeadLList (GovLHead fee) (Just key)
          -> -- new node has to be inserted after the head
            traceIfFalse "New Gov tokens must be minted" checkGovIsMinted
            && traceIfFalse "New head's fee must be the same as old head's fee" (checkHeadsFeeEquality fee)
            && traceIfFalse "New first/head must have value at least equal to old first/head" checkFirstValueIsConsistent
  --              && traceIfFalse "New head key must be same as old head key" checkFirstValueIsConsistent
            && traceIfFalse "New first must point to the new node" (newFirst `pointsTo` (newSecond >>= getKey . fst))
            && traceIfFalse "New node must point to the old first's next" (newSecond `pointsTo` oldSecond)
            && traceIfFalse "New node must contain Gov equal to fee" checkGovIsPaid
--            && traceIfFalse "New head must have the unique token and the proof token" checkNewHeadHasTokens
--            && traceIfFalse "List head must be spent" checkListHeadIsSpent


--          | (length inputsFromScriptAddress) == 2
--            ->
--              traceIfFalse "New Gov tokens must be minted" checkGovIsMinted
--              && traceIfFalse "Old first node must remain consistent" checkFirstIsConsistent
--              && traceIfFalse "New nodes must contain Gov equal to fee" (checkGovIsPaidToTwo fee)
--              && traceIfFalse "New head's fee must be the same as old head's fee" (checkHeadsFeeEquality fee)
--              && traceIfFalse "New first must point to the new second" (newFirst `pointsTo` (newSecond >>= getKey))
--              && traceIfFalse "New second must point to the old second" (newSecond `pointsTo` oldSecond)
--              && traceIfFalse "New head must have the unique token and the proof token" checkNewHeadHasTokens

--          | otherwise -> False

        NodeLList _ _ _ ->
            traceIfFalse "New Gov tokens must be minted" checkGovIsMinted
            && traceIfFalse "New first must point to the new node" (newFirst `pointsTo` (newSecond >>= getKey . fst))
            && traceIfFalse "New node must point to the old first's next" (newSecond `pointsTo` oldSecond)
            && traceIfFalse "New node must contain Gov equal to fee" checkGovIsPaid
            && traceIfFalse "New first/head must have value equal to old first/head" checkFirstValueIsConsistent

--            && traceIfFalse "New head must have the unique token and the proof token" checkNewHeadHasTokens

    Proof ->
      case gov'list datum of
        HeadLList (GovLHead fee) (Just _) ->
          traceIfFalse "New head's fee must be the same as old head's fee" (checkHeadsFeeEquality fee)
          && traceIfFalse "New head must have value at least equal to old head" checkFirstValueIsConsistent
          && traceIfFalse "New head's next must be equal to old head's next" checkFirstNextIsConsistent
          && traceIfFalse "New head must have the unique token and the proof token" checkNewHeadHasTokens

        HeadLList (GovLHead fee) Nothing ->
          traceError "Not allowed to consume a head as a Proof when it has no next. Use MintGov instead."

        NodeLList key _ next ->
          traceError "Not implemented. Spec needed" -- FIXME

       -- makes sure that the token is used as proof and returned to the Gov
      -- Address, with nothing being altered.
    ProofAndBurn ->
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
              HeadLList _ _ -> getMaybeOne inputsFromScriptAddress
              NodeLList _ _ _ -> getMaybeOne inputsWithHeadDatum
          Proof ->
            case gov'list datum of
              HeadLList _ Nothing -> traceError "Not allowed to consume a head as a Proof when it has no next. Use MintGov instead."
              HeadLList _ (Just _) -> getMaybeOne inputsWithNodeDatum
              NodeLList _ _ _ -> traceError "Not allowed to consume a node as a Proof."
          ProofAndBurn ->
            case gov'list datum of
              HeadLList _ Nothing -> traceError "There is nothing to burn in the list."
              HeadLList _ (Just _) -> getMaybeOne inputsWithNodeDatum
              NodeLList _ _ _ -> traceError "Not allowed to consume a node as a Proof."


      newFirst :: Maybe (GovDatum, TxOut)
      newFirst =
        case act of
          MintGov ->
            case gov'list datum of
              HeadLList _ _ -> getMaybeOne outputsWithHeadDatum
              NodeLList _ _ _ -> do
                [d1, d2] <- getMaybeTwo outputsWithNodeDatum
                return $ fst . sort2 $ (d1, d2)
          Proof -> getMaybeOne outputsWithNodeDatum
          ProofAndBurn -> traceError "Not implemented yet." -- FIXME


      oldSecond :: Maybe UserId
      oldSecond = oldFirst >>= getNext . fst


      newSecond :: Maybe (GovDatum, TxOut)
      newSecond =
        case act of
          MintGov ->
            case gov'list datum of
              HeadLList _ _ -> getMaybeOne outputsWithNodeDatum
              NodeLList _ _ _ -> do
                [d1, d2] <- getMaybeTwo outputsWithNodeDatum
                return $ snd . sort2 $ (d1, d2)
          Proof -> traceError "New second pointer isn't needed for the Proof act"
          ProofAndBurn -> traceError "Not implemented yet." -- FIXME


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

      checkGovIsMinted = govMinted > 0

      checkNewHeadPointsToNewNode =
        wrapMaybe $ do
          (newHeadDatum, _) <- getMaybeOne outputsWithHeadDatum
          newHeadNext <- getNext newHeadDatum
          (newNodeDatum, _) <- getMaybeOne outputsWithNodeDatum
          newNodeKey <- getKey newNodeDatum
          return $ newNodeKey == newHeadNext

      checkFirstValueIsConsistent =
        wrapMaybe $ do
          (_, oldTxO) <- oldFirst
          (_, newTxO) <- newFirst
          return $ (txOutValue newTxO) `geq` (txOutValue oldTxO)

      checkListHeadIsSpent = length inputsWithHeadDatum == 1

      checkFirstNextIsConsistent =
        wrapMaybe $ do
          (oldFirst', _) <- oldFirst
          oldNext <- getNext oldFirst'
          (newFirst', _) <- newFirst
          newNext <- getNext newFirst'
          return $ oldNext == newNext

      checkNewHeadHasTokens =
        wrapMaybe $ do
          (_, txO) <- getMaybeOne outputsWithHeadDatum
          return $ txOutValue txO `geq` (uniqueToken <> govProofToken)
        where
          uniqueToken = assetClassValue (uniqueToken'assetClass ut) 1
          govProofToken = assetClassValue (asset emptyByteString) 1

      checkGovIsPaid =
        wrapMaybe $ do
          (head, _) <- getMaybeOne inputsWithHeadDatum
          fee <- getFee head
          (_, txO) <- newSecond
          return $ assetClassValueOf (txOutValue txO) gov == fee -- geq won't work since we can't handle multiple minting at in this version

      checkHeadsFeeEquality oldFee =
        wrapMaybe $ do
          (newDatum, _) <- getMaybeOne outputsWithHeadDatum
          newFee <- getFee newDatum
          return $ newFee == oldFee

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
        where fst3 (a, _, _) = a
              value = txOutValue . txInInfoResolved <$> findOwnInput ctx
              allCurrencySymbols =
                case value of
                  Nothing -> traceError "Own input wasn't find!"
                  Just v -> fmap fst3 $ flattenValue v
              validSymbols = [fst . unAssetClass . uniqueToken'assetClass $ ut, adaSymbol]
              otherSymbols = filter (\s -> not . elem s $ validSymbols) allCurrencySymbols

      asset tn = AssetClass (ownCurrencySymbol', TokenName tn)
      gov = asset "Gov"
  --        xGov = asset "xGov"

      valueIn = foldMap (txOutValue . txInInfoResolved) $ txInfoInputs . scriptContextTxInfo $ ctx

      valueOut = foldMap txOutValue $ txInfoOutputs . scriptContextTxInfo $ ctx

      valueMinted =
        let allMinted = flattenValue $ valueOut <> negate valueIn
        in mconcat
          . map (\(s, n, v) -> assetClassValue (AssetClass (s, n)) v)
          . filter (\(s, _, _) -> s == ownCurrencySymbol')
          $ allMinted

      govMinted = assetClassValueOf valueMinted gov
      tokenPresent txO = 1 == (assetClassValueOf (txOutValue txO) $ uniqueToken'assetClass ut)


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

      getFee :: GovDatum -> Maybe Integer
      getFee (GovDatum d) =
        case d of
          HeadLList (GovLHead fee) _ -> Just fee
          _ -> Nothing


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