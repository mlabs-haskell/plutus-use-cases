{-# LANGUAGE UndecidableInstances #-}

module Mlabs.NFT.Validation where

import PlutusTx.Prelude 
import Prelude qualified as Hask



import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

import Ledger (
    Address
  , AssetClass  
  , Datum(..)  
  , Redeemer(..)
  , TxOutRef
  , ScriptContext(..)
  , TxOut(..)
  , ValidatorHash
  , MintingPolicy
  , CurrencySymbol
  , scriptContextTxInfo
  , txInfoData
  , txInfoInputs
  , txInInfoOutRef
  , txInfoMint
  , txInfoOutputs
  , ownCurrencySymbol
  , mkMintingPolicyScript
  , scriptCurrencySymbol
  )
import Ledger.Typed.Scripts (
    TypedValidator
  , ValidatorTypes
  , DatumType
  , RedeemerType
  , mkTypedValidator
  , wrapMintingPolicy
  , wrapValidator
  , validatorAddress
  , validatorHash
  )
import Ledger.Value qualified as Value
import Plutus.V1.Ledger.Value (AssetClass(AssetClass))
import PlutusTx qualified
import Schema (ToSchema)

import Mlabs.NFT.Types

-- | NFT Datum is checked communicates the ownership of the NFT.
data DatumNft = DatumNft
  { -- | NFT ID
    dNft'id :: NftId
  , -- | Share
    dNft'share :: Rational
  , -- | Author receives the shares of the price
    dNft'author :: UserId
  , -- | current owner
    dNft'owner :: UserId
  , -- | Price in ada, if it's nothing then nobody can buy
    dNft'price :: Maybe Integer
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
PlutusTx.unstableMakeIsData ''DatumNft
PlutusTx.makeLift ''DatumNft

instance Eq DatumNft  where
  {-# INLINEABLE (==) #-}
  (DatumNft id1 share1 author1 owner1 price1) == (DatumNft id2 share2 author2 owner2 price2) = 
    id1 == id2 && share1 == share2 && author1 == author2 && owner1 == owner2 && price1 == price2

-- | NFT Redeemer
data UserAct
  = -- | Buy NFT and set new price
    BuyAct
      { -- | price to buy
        act'price :: Integer
      , -- | new price for NFT (Nothing locks NFT)
        act'newPrice :: Maybe Integer
      }
  | -- | Set new price for NFT
    SetPriceAct
      { -- | new price for NFT (Nothing locks NFT)
        act'newPrice :: Maybe Integer
      }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (ToJSON, FromJSON)
PlutusTx.makeLift ''UserAct
PlutusTx.unstableMakeIsData ''UserAct

instance Eq UserAct where
  {-# INLINEABLE (==) #-}
  (BuyAct bid1 newPrice1) == (BuyAct bid2 newPrice2) = bid1 == bid2 && newPrice1 == newPrice2
  (SetPriceAct newPrice1) == (SetPriceAct newPrice2) = newPrice1 == newPrice2

asRedeemer :: UserAct -> Redeemer
asRedeemer = Redeemer . PlutusTx.toBuiltinData

{-# INLINEABLE mkMintPolicy #-}

-- | Minting policy for NFTs.
mkMintPolicy :: Address -> TxOutRef -> NftId -> () -> ScriptContext -> Bool
mkMintPolicy stateAddr oref (NftId title token author) _ ctx =
  -- ? maybe author could be checked also, their key should be in signatures.
  traceIfFalse "UTXO not consumed" hasUtxo
    && traceIfFalse "wrong amount minted" checkMintedAmount
    && traceIfFalse "Does not pay to state" paysToState
  where
    info = scriptContextTxInfo ctx

    hasUtxo =
      any (\inp -> txInInfoOutRef inp == oref) $
        txInfoInputs info

    checkMintedAmount = case Value.flattenValue (txInfoMint info) of
      [(cur, tn, val)] ->
        ownCurrencySymbol ctx == cur
          && token == tn
          && val == 1
      _ -> False

    paysToState = any hasNftToken $ txInfoOutputs info

    hasNftToken TxOut {..} =
      txOutAddress == stateAddr
        && txOutValue == Value.singleton (ownCurrencySymbol ctx) token 1

mintPolicy :: Address -> TxOutRef -> NftId -> MintingPolicy
mintPolicy stateAddr oref nid =
 mkMintingPolicyScript $
    $$(PlutusTx.compile [||\x y z -> wrapMintingPolicy (mkMintPolicy x y z)||])
      `PlutusTx.applyCode` PlutusTx.liftCode stateAddr
      `PlutusTx.applyCode` PlutusTx.liftCode oref
      `PlutusTx.applyCode` PlutusTx.liftCode nid


{-# INLINEABLE mKTxPolicy #-}

-- | A validator script for the user actions.
mKTxPolicy :: DatumNft -> UserAct -> ScriptContext -> Bool
mKTxPolicy dNft act ctx =
  -- ? maybe, some check that datum corresponds to NftId could be added
  traceIfFalse "Datum does not correspond to NFTId." True
    && traceIfFalse "Incorrect Datum attached to Tx." True
    && traceIfFalse "NFT doesn't exist at the address." True
    && case act of
      BuyAct {..} ->
        traceIfFalse "Not enough funds." True -- todo
          && traceIfFalse "User does not own NFT." True -- todo
      SetPriceAct {..} ->
        traceIfFalse "Price cannot be negative." True -- todo
          && traceIfFalse "User does not own NFT." True -- todo
  where
  ------------------------------------------------------------------------------
  -- Utility functions.
    getCtxDatum :: PlutusTx.FromData a => ScriptContext -> [a]
    getCtxDatum = 
         id
         . fmap (\(Just x) -> x)
         . filter (maybe False (const True))
         . fmap PlutusTx.fromBuiltinData 
         . fmap (\(Datum d) -> d)
         . fmap Hask.snd 
         . txInfoData 
         . scriptContextTxInfo
  
  ------------------------------------------------------------------------------
  -- Check if the datum is correct.
    correctDatum = error ()
--      let
--       datums :: [DatumNft] = getCtxDatum ctx  
--      in
--        case fmap dNft'id  datums of 
--          [x] ->  x == dId 
--          _  -> False
  ------------------------------------------------------------------------------

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
curSymbol :: Address -> TxOutRef -> NftId -> CurrencySymbol
curSymbol stateAddr oref nid = scriptCurrencySymbol $ mintPolicy stateAddr oref nid

nftAsset :: NftId -> AssetClass
nftAsset nid = AssetClass (cs, tn) where
    cs = scriptCurrencySymbol $
          mintPolicy txScrAddress (nftId'outRef nid) nid
    tn = nftId'token nid

