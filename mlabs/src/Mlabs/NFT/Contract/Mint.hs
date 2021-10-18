{-# LANGUAGE UndecidableInstances #-}

module Mlabs.NFT.Contract.Mint (
  mint, getDatumsTxsOrdered, hashData
) where

import PlutusTx.Prelude hiding (mconcat, (<>), mempty)
import Prelude (mconcat, mempty)
import Prelude qualified as Hask

import Control.Monad (void)
import Data.Map qualified as Map
import Data.Monoid (Last (..))
import Data.Text (Text)
import Text.Printf (printf)
import Data.List qualified as L
import Data.Function (on)
import Control.Lens ((^.))

import Plutus.Contract (Contract)
import Plutus.Contract qualified as Contract
import Mlabs.Plutus.Contract (readDatum')

import Ledger (
  Redeemer,
  ChainIndexTxOut,
  TxOutRef,
  PubKeyHash(..),
  pubKeyHash,
  scriptCurrencySymbol,
  ciTxOutValue,
 )

import Ledger.Constraints qualified as Constraints
import Ledger.Typed.Scripts (validatorScript)
import Ledger.Value as Value (AssetClass(..), TokenName(..), singleton)
import Plutus.ChainIndex.Tx (ChainIndexTx)

import Mlabs.NFT.Types {-(
  BuyRequestUser (..),
  Content (..),
  MintAct (..),
  MintParams (..),
  NftId (..),
  QueryResponse (..),
  SetPriceParams (..),
  UserId (..),
 ) -}

import Mlabs.NFT.Validation {-(
  DatumNft (..),
  NftTrade,
  UserAct (..),
  asRedeemer,
  calculateShares,
  mintPolicy,
  nftAsset,
  nftCurrency,
  nftTokenName,
  priceNotNegative,
  txPolicy,
  txScrAddress,
 )-}
import Mlabs.NFT.Contract.Aux
--------------------------------------------------------------------------------
-- MINT --

type PointInfo = (DatumNft, (TxOutRef, ChainIndexTxOut))
-- | Two positions in on-chain list between which new NFT will be "inserted"
data InsertPoint = InsertPoint
  { prev :: PointInfo
  , next  :: Maybe PointInfo
  }


---- | Mints an NFT and sends it to the App Address.
mint :: NftAppSymbol -> MintParams -> Contract (Last NftId) s Text ()
mint symbol params = do
  ownOrefTxOut <- getUserAddr >>= fstUtxoAt
  nftId        <- nftIdInit params
  ownPkh       <- pubKeyHash <$> Contract.ownPubKey
  insertPoint  <- findInsertPoint symbol nftId
  let precedingDatum = fst $ prev insertPoint
      newNftNode                 = mkNode (getAppInstance precedingDatum) nftId params ownPkh
      (newPreceding, linkedNode) = precedingDatum `linkWith` newNftNode
      (lookups, tx, mintedVal)   = buildLookupsTx newPreceding linkedNode ownOrefTxOut insertPoint
  void $ Contract.submitTxConstraintsWith @NftTrade lookups tx
  Contract.tell . Last . Just $ nftId
  Contract.logInfo @Hask.String $ printf "forged %s" (Hask.show mintedVal)
  where
    nftIdInit = return . NftId . hashData . mp'content

    findInsertPoint 
      :: NftAppSymbol 
      -> NftId 
      -> Contract (Last NftId) s Text InsertPoint
    findInsertPoint appCS (NftId contentHash) = do
      res <- getDatumsTxsOrdered appCS
      let newNftPointer = Pointer $ AssetClass (app'symbol appCS, TokenName contentHash)
          spanP (datum,_) = case datumPointer datum of 
            Nothing -> True
            Just ac -> ac < newNftPointer 
      case L.span spanP res of
        ([], _)    -> Contract.throwError "Head not found"
        (prvs, []) -> pure $ InsertPoint (L.last prvs) Nothing 
        (prvs, nxt : _) -> pure $ InsertPoint (L.last prvs) (Just nxt)
        -- todo is some additional checks needed? E.g:
        -- - both prev and next found, but prev node has None Pointer

    mkNode 
      :: NftAppInstance 
      -> NftId 
      -> MintParams 
      -> PubKeyHash 
      -> NftListNode
    mkNode inst nid MintParams{..} pkh = 
      let author = UserId pkh
          info = InformationNft nid mp'share author author mp'price
      in NftListNode info Nothing inst

    linkWith :: DatumNft -> NftListNode -> (DatumNft, NftListNode)
    linkWith precedingDatum node = 
      let nftTN = TokenName . nftId'contentHash . info'id . node'information $ node
          toNftPointer = Just $ Pointer $ AssetClass (app'symbol symbol, nftTN)
      in case precedingDatum of
        HeadDatum (NftListHead Nothing inst)
          -> (HeadDatum (NftListHead toNftPointer inst), node)
        HeadDatum (NftListHead p inst)
          -> (HeadDatum (NftListHead toNftPointer inst), node{node'next = p})
        NodeDatum (NftListNode info Nothing inst)
          -> (NodeDatum (NftListNode info toNftPointer inst), node)
        NodeDatum (NftListNode info p inst)
          -> (NodeDatum (NftListNode info toNftPointer inst), node{node'next = p})

    buildLookupsTx newPrecedingDatum newNode authorOrefTxOut insertPoint = 
      let 
          nftDatum     = NodeDatum newNode
          precedingNft = let (_, (_, txOut)) = prev insertPoint in txOut ^. ciTxOutValue
          nftPolicy = mintPolicy (getAppInstance nftDatum)
          nftTokName = nftTokenName nftDatum
          nftVal = Value.singleton (scriptCurrencySymbol nftPolicy) nftTokName 1
          mintRedeemer = asRedeemer $ Mint (info'id $ node'information newNode)
          spendRedeemer = error ()
          lookups =
            mconcat
              [ Constraints.unspentOutputs $ Map.fromList [authorOrefTxOut]
              , Constraints.unspentOutputs $ Map.fromList [snd $ prev insertPoint]
              , lookupNext (next insertPoint)
              , Constraints.mintingPolicy nftPolicy
              , Constraints.typedValidatorLookups txPolicy
              , Constraints.otherScript (validatorScript txPolicy)
              ]
          tx =
            mconcat
              [ Constraints.mustMintValueWithRedeemer mintRedeemer nftVal 
                -- ^ minting new NFT
              , Constraints.mustSpendScriptOutput (fst $ snd $ prev insertPoint) spendRedeemer 
                -- ^ spending UTXO of preceding node
              , mustSpendNext (next insertPoint) spendRedeemer 
                -- ^ spending UTXO of next node if exists
              , Constraints.mustPayToTheScript newPrecedingDatum precedingNft 
                -- ^ putting back to script NFT from preceding node with updated Datum
              , mustPayNext $ next insertPoint
                -- ^ putting back to script NFT from next node with unchanged Datum
              , Constraints.mustPayToTheScript nftDatum nftVal
                -- ^ paying new NFT to script
              , Constraints.mustSpendPubKeyOutput (fst authorOrefTxOut)
              -- ? do we check that UTXO was consumed during minting
              -- as minting policy have no access to authorOref
              ]
       in (lookups, tx, nftVal)

-- | A hashing function to minimise the data to be attached to the NTFid.
hashData :: Content -> BuiltinByteString
hashData (Content b) = sha2_256 b


-- | Get `DatumNft` together with`TxOutRef` and `ChainIndexTxOut` 
-- for particular `NftAppSymbol` and return them sorted by `DatumNft`'s `Pointer`:
-- head node first, list nodes ordered by pointer 
getDatumsTxsOrdered :: NftAppSymbol -> Contract w s Text [PointInfo]
getDatumsTxsOrdered nftAS = do
  utxos <- Map.toList <$> getAddrValidUtxos nftAS
  datums <- mapM withDatum utxos
            -- todo should it be `catMaybes . fmap readDatum'` instead? 
            -- as with current approach somebody can break contract by submitting UTXO w/o Datum
  return 
    -- $ checkConsistent 
    -- todo do any checks needed to be here: e.g. only one head, no duplicate pointers, etc.?
    $ L.sortBy nftDatumOrd datums
  where
    withDatum 
      :: (TxOutRef, (ChainIndexTxOut, ChainIndexTx))
      -> Contract w s Text PointInfo
    withDatum (oref,(out,_)) = case readDatum' out of
      Nothing -> Contract.throwError "Datum not found"
      Just d -> pure (d, (oref, out))

    nftDatumOrd (l,_) (r,_) = case (l, r) of
      (HeadDatum _, HeadDatum _)   -> EQ
      (HeadDatum _, NodeDatum _)   -> LT
      (NodeDatum _, HeadDatum _)   -> GT
      (NodeDatum ln, NodeDatum rn) -> (pointerOrd `on` node'next) ln rn
    
    pointerOrd :: Maybe Pointer -> Maybe Pointer -> Ordering
    pointerOrd pl pr = case (pl, pr) of
      (Nothing, Nothing) -> EQ
      (Nothing, Just _)  -> GT
      (Just _, Nothing)  -> LT
      (Just l, Just r)   -> compare l r


lookupNext 
  :: Maybe PointInfo
  -> Constraints.ScriptLookups a2
lookupNext = \case 
  Just (_, (oref, toOut)) -> Constraints.unspentOutputs $ Map.fromList [(oref, toOut)]
  Nothing -> mempty

mustSpendNext 
  :: Maybe PointInfo
  -> Redeemer 
  -> Constraints.TxConstraints i o
mustSpendNext maybeNext redeemer = case maybeNext of 
  Just (_, (oref, _)) -> Constraints.mustSpendScriptOutput oref redeemer
  Nothing -> mempty

mustPayNext 
  :: Maybe PointInfo
  -> Constraints.TxConstraints i DatumNft
mustPayNext = \case
  Just (datum, (_, txOut)) -> Constraints.mustPayToTheScript datum (txOut ^. ciTxOutValue)
  Nothing -> mempty
