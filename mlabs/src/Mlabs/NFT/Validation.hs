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
  InformationNft (..),
  NftAppInstance (..),
  NftListNode (..),
  NftListHead (..),
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

-- | NFT Information.
data InformationNft = InformationNft
  { -- | NFT ID. Represents the key of the Datum. ?could even be taken out of the information?
    info'id :: NftId
  , -- | Author's share of the NFT.
    info'share :: Rational
  , -- | Author's wallet pubKey.
    info'author :: UserId
  , -- | Owner's wallet pubkey.
    info'owner :: UserId
  , -- | Price in Lovelace. If Nothing, NFT not for sale.
    info'price :: Maybe Integer
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''InformationNft
PlutusTx.makeLift ''InformationNft
instance Eq InformationNft where
  {-# INLINEABLE (==) #-}
  (InformationNft a b c d e) == (InformationNft a' b' c' d' e') =
    a == a' && b == b' && c == c' && d == d' && e == e'

{- | App Instace is parametrised by the one time nft consumed at the creation of
 the HEAD and the script address.
-}
data NftAppInstance = NftAppInstance
  { -- | Script Address where all the NFTs can be found
    appInstance'Address :: Address
  , -- | AssetClass with which all the NFTs are parametrised - guarantees the proof of uniqueness.
    appInstance'AppAssetClass :: AssetClass
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''NftAppInstance
PlutusTx.makeLift ''NftAppInstance
instance Eq NftAppInstance where
  (NftAppInstance a b) == (NftAppInstance a' b') = a == a' && b == b'

{-
Diagram (sort of):
|   HEAD                                   |               |    NODE artwork1
|CurrSymbol: n                             |               | CurrSymbol: n
|TokenName: HEAD                           |               | ...
|...                                       |               |
|NextNode : Pointer { CurrSymbol: n        |               | InformationNft : { ...
|                     TokenName : artwork1 | --------------|--> info'id {}
|                   }                      |               |

Properties:
- artwork1 < artwork2 <=> artwork1:next = artwork2
-
- If artwork1 == artwork2                                 => duplication of artwork (not allowed to mint the same artwork twice)
- If artwork2 > artwork1 and artwork1:nextNode -> Nothing => artwork2 hasn't been minted

-}

{- | The AssetClass is the pointer itself. Each NFT has the same CurrencySymbol,
 and their TokenName is the Hash of their Content.
-}
newtype Pointer = Pointer
  { pointer'assetClass :: AssetClass
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

PlutusTx.unstableMakeIsData ''Pointer
PlutusTx.makeLift ''Pointer
instance Eq Pointer where
  (Pointer a) == (Pointer a') = a == a'

{- | The head datum is unique for each list. Its token is minted when the unique
 NFT is consumed.
-}
data NftListHead = NftListHead
  { -- | Pointer to the next node.
    head'next :: Maybe Pointer
  , -- | Node App Instance
    head'appInstance :: NftAppInstance
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''NftListHead
PlutusTx.makeLift ''NftListHead
instance Eq NftListHead where
  (NftListHead a b) == (NftListHead a' b') = a == a' && b == b'

-- | The nft list node is based on the above described properties.
data NftListNode = NftListNode
  { -- | The value held at the node
    node'information :: InformationNft
  , -- | The next Node.
    node'next :: Maybe Pointer
  , -- | Node App Instance
    node'appInstance :: NftAppInstance
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''NftListNode
PlutusTx.makeLift ''NftListNode
instance Eq NftListNode where
  (NftListNode a b c) == (NftListNode a' b' c') = a == a' && b == b' && c == c'

-- | The datum of an Nft is either head or node.
data DatumNft
  = -- | Head of a List
    HeadDatum NftListHead
  | -- | A node of the list.
    NodeDatum NftListNode
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''DatumNft
PlutusTx.makeLift ''DatumNft

instance Eq DatumNft where
  {-# INLINEABLE (==) #-}
  (HeadDatum x1) == (HeadDatum x2) = x1 == x2
  (NodeDatum x1) == (NodeDatum x2) = x1 == x2
  _ == _ = False

{- | Token Name is represented by the HASH of the artwork. The Head TokenName is
the empty ByteString, smaller than any other ByteString, and is minted at the
intialisation of the app.
-}
nftTokenName :: DatumNft -> TokenName
nftTokenName = \case
  HeadDatum _ -> TokenName PlutusTx.Prelude.emptyByteString
  NodeDatum n -> TokenName . nftId'contentHash . info'id . node'information $ n

-- | NFT Redeemer
data UserAct
  = -- | Buy NFT and set new price
    BuyAct
      { -- | price to buy. In Lovelace.
        act'bid :: Integer
      , -- | new price for NFT. In Lovelace.
        act'newPrice :: Maybe Integer
      }
  | -- | Set new price for NFT
    SetPriceAct
      { -- | new price for NFT. In Lovelace.
        act'newPrice :: Maybe Integer
      }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''UserAct
PlutusTx.unstableMakeIsData ''UserAct

instance Eq UserAct where
  {-# INLINEABLE (==) #-}
  (BuyAct bid1 newPrice1) == (BuyAct bid2 newPrice2) =
    bid1 == bid2 && newPrice1 == newPrice2
  (SetPriceAct newPrice1) == (SetPriceAct newPrice2) =
    newPrice1 == newPrice2
  _ == _ = False

asRedeemer :: PlutusTx.ToData a => a -> Redeemer
asRedeemer = Redeemer . PlutusTx.toBuiltinData

--{-# INLINEABLE mkHeadMintPolicy #-}

---- | Any NFT Policy that makes sure that the HEAD is Unique, and that it can
---- burn the token exactly once - when the HEAD is created. Out of scope at this
---- point, but any will do. To be replacede with the Plutus defaul mint action Contract
--mkHeadMintPolicy :: Address -> () -> ScriptContext -> Bool
--mkHeadMintPolicy _ _ _ = True

--headMintPolicy :: Address -> MintingPolicy
--headMintPolicy stateAddr =
--  mkMintingPolicyScript $
--    $$(PlutusTx.compile [||\x -> wrapMintingPolicy (mkHeadMintPolicy x)||])
--      `PlutusTx.applyCode` PlutusTx.liftCode stateAddr

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
      traceIfFalse "Datum does not correspond to NFTId, no datum is present, or more than one suitable datums are present." correctDatum
      where
        -- && traceIfFalse "Datum is not present." correctDatum'
        -- && traceIfFalse "New Price cannot be negative." (setPositivePrice act)
        -- && traceIfFalse "Previous TX is not consumed." prevTxConsumed
        -- && traceIfFalse "NFT sent to wrong address." tokenSentToCorrectAddress
        -- && traceIfFalse "Transaction cannot mint." noMint
        -- && case act of
        --   BuyAct {..} ->
        --     traceIfFalse "NFT not for sale." nftForSale
        --       && traceIfFalse "Bid is too low for the NFT price." (bidHighEnough act'bid)
        --       && traceIfFalse "New owner is not the payer." correctNewOwner
        --       && traceIfFalse "Datum is not consistent, illegaly altered." consistentDatum
        --       && if ownerIsAuthor
        --         then traceIfFalse "Amount paid to author/owner does not match bid." (correctPaymentOnlyAuthor act'bid)
        --         else
        --           traceIfFalse "Current owner is not paid their share." (correctPaymentOwner act'bid)
        --             && traceIfFalse "Author is not paid their share." (correctPaymentAuthor act'bid)
        --   SetPriceAct {} ->
        --     traceIfFalse "Price can not be negative." priceNotNegative'
        --       && traceIfFalse "Only owner exclusively can set NFT price." ownerSetsPrice

        nInfo = node'information node
        nNext = node'next node
        nAppInstance = node'appInstance node

        ------------------------------------------------------------------------------
        -- Utility functions.
        getCtxDatum :: PlutusTx.FromData a => ScriptContext -> [a]
        getCtxDatum =
          catMaybes'
            . fmap PlutusTx.fromBuiltinData
            . fmap (\(Datum d) -> d)
            . fmap snd
            . txInfoData
            . scriptContextTxInfo

        ownerIsAuthor :: Bool
        ownerIsAuthor = info'owner nInfo == info'author nInfo

        getLovelace = flip assetClassValueOf $ assetClass Ada.adaSymbol Ada.adaToken

        ------------------------------------------------------------------------------
        -- Checks
        ------------------------------------------------------------------------------
        -- Check if the datum attached is also present in the is also in the transaction.
        correctDatum :: Bool
        correctDatum = True

--  let datums :: [DatumNft] = getCtxDatum ctx
--      suitableDatums = filter (== info'id datum) . fmap info'id $ datums
--   in case suitableDatums of
--        _ : _ -> True
--        _ -> False
--
------------------------------------------------------------------------------
-- Check if the datum in the datum is also the same in the transaction, v2.
--correctDatum' :: Bool
--correctDatum' =
--  let info = scriptContextTxInfo ctx
--      mDatums = findDatumHash (Datum . PlutusTx.toBuiltinData $ datum) info
--   in maybe False (const True) mDatums
--
--------------------------------------------------------------------------------
---- Check if the NFT is for sale.
--nftForSale = maybe False (const True) $ dNft'price datum
--
--------------------------------------------------------------------------------
---- Check if the bid price is high enough.
--bidHighEnough bid =
--  let price = dNft'price datum
--   in fromMaybe False $ (bid >=) <$> price
--
--------------------------------------------------------------------------------
---- Check if the new owner is set correctly. todo
--correctNewOwner = True
--
--------------------------------------------------------------------------------
---- Check if the Person is being reimbursed accordingly, with the help of 2
---- getter functions. Helper function.
--correctPayment f shareCalcFn bid = personGetsAda >= personWantsAda
--  where
--    info = scriptContextTxInfo ctx
--    personId = getUserId . f $ datum
--    share = dNft'share datum
--    personGetsAda = getAda $ valuePaidTo info personId
--    personWantsAda = getAda $ shareCalcFn bid share
--
--------------------------------------------------------------------------------
---- Check if the Author is being reimbursed accordingly.
--correctPaymentAuthor = correctPayment dNft'author calculateAuthorShare
--
--------------------------------------------------------------------------------
---- Check if the Current Owner is being reimbursed accordingly.
--correctPaymentOwner = correctPayment dNft'owner calculateOwnerShare
--
--------------------------------------------------------------------------------
---- Check if the Author is being paid the full amount when they are both
---- owner and author.
--correctPaymentOnlyAuthor bid = authorGetsAda >= bid
--  where
--    info = scriptContextTxInfo ctx
--    author = getUserId . dNft'author $ datum
--    authorGetsAda = getAda $ valuePaidTo info author
--
--------------------------------------------------------------------------------
---- Check if the new Datum is correctly.
--consistentDatum =
--  let prevDatum :: DatumNft = head . getCtxDatum $ ctx
--   in dNft'id prevDatum == dNft'id datum
--        && dNft'share prevDatum == dNft'share datum
--        && dNft'author prevDatum == dNft'author datum
--
--------------------------------------------------------------------------------
---- Check no new token is minted.
--noMint = isZero . txInfoMint . scriptContextTxInfo $ ctx
--
--------------------------------------------------------------------------------
---- Check if the NFT is sent to the correct address.
--tokenSentToCorrectAddress =
--  containsNft $ foldMap txOutValue (getContinuingOutputs ctx)
--
--containsNft v = valueOf v (act'cs act) (nftTokenName datum) == 1
--
--------------------------------------------------------------------------------
---- Check new price is positive or nothing.
--setPositivePrice = \case
--  action@BuyAct {} ->
--    case act'newPrice action of
--      Nothing -> True
--      Just x -> x > 0
--  action@SetPriceAct {} ->
--    case act'newPrice action of
--      Nothing -> True
--      Just x -> x > 0
--
--------------------------------------------------------------------------------
---- Check if the previous Tx containing the token is consumed.
--prevTxConsumed =
--  case findOwnInput ctx of
--    Just (TxInInfo _ out) -> containsNft $ txOutValue out
--    Nothing -> False
--
--------------------------------------------------------------------------------
---- Check if new price non-negative.
--priceNotNegative' = priceNotNegative (act'newPrice act)
--
--------------------------------------------------------------------------------
---- Check that price set by NFT owner.
--ownerSetsPrice =
--  case txInfoSignatories $ scriptContextTxInfo ctx of
--    [pkh] -> pkh == getUserId (dNft'owner datum)
--    _ -> False

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
