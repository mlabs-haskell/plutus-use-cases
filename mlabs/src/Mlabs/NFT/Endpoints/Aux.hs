module Mlabs.NFT.Endpoints.Aux where

import PlutusTx.Prelude hiding (mconcat, (<>))
import Prelude (mconcat, (<>))
import Prelude qualified as Hask

import Control.Lens (filtered, to, traversed, (^.), (^..), _Just, _Right)
import Control.Monad (join, void)
import Data.List qualified as L
import Data.Map qualified as Map
import Data.Monoid (Last (..))
import Data.Text (Text, pack)
import Text.Printf (printf)

import Plutus.ChainIndex.Tx (ChainIndexTx)
import Plutus.Contract (Contract, mapError, ownPubKey, utxosTxOutTxAt)
import Plutus.Contract qualified as Contract
import PlutusTx qualified

import Plutus.Contracts.Currency (CurrencyError, mintContract, mintedValue)
import Plutus.Contracts.Currency qualified as MC
import Plutus.V1.Ledger.Value (TokenName (..), assetClass, currencySymbol, flattenValue, symbols)

import Ledger (
  Address,
  AssetClass,
  ChainIndexTxOut,
  Datum (..),
  Redeemer (..),
  TxOutRef,
  Value,
  ciTxOutDatum,
  ciTxOutValue,
  getDatum,
  pubKeyAddress,
  pubKeyHash,
  scriptCurrencySymbol,
  txId,
 )

import Ledger.Constraints qualified as Constraints
import Ledger.Typed.Scripts (validatorScript)
import Ledger.Value as Value (singleton, unAssetClass, valueOf)
import Mlabs.Plutus.Contract (readDatum')

import Mlabs.NFT.Types
import Mlabs.NFT.Validation

getScriptAddrUtxos :: Contract w s Text (Map.Map TxOutRef (ChainIndexTxOut, ChainIndexTx))
getScriptAddrUtxos = utxosTxOutTxAt txScrAddress


-- HELPER FUNCTIONS AND CONTRACTS --

-- | Get the current Wallet's publick key.
getUserAddr :: Contract w s Text Address
getUserAddr = pubKeyAddress <$> Contract.ownPubKey

-- | Get the current wallet's utxos.
getUserUtxos :: Contract w s Text (Map.Map TxOutRef Ledger.ChainIndexTxOut)
getUserUtxos = getAddrUtxos =<< getUserAddr

-- | Get the current wallet's userId.
getUId :: Contract w s Text UserId
getUId = UserId . pubKeyHash <$> Contract.ownPubKey

-- | Get the ChainIndexTxOut at an address.
getAddrUtxos :: Address -> Contract w s Text (Map.Map TxOutRef ChainIndexTxOut)
getAddrUtxos adr = Map.map fst <$> utxosTxOutTxAt adr

-- | Get the ChainIndexTxOut at an address.
getAddrValidUtxos :: NftAppSymbol -> Contract w s Text (Map.Map TxOutRef (ChainIndexTxOut, ChainIndexTx))
getAddrValidUtxos appSymbol = Map.filter validTx <$> utxosTxOutTxAt txScrAddress
  where
    -- | Check if the given transactions contain the correct Currency Symbol.
    validTx (cIxTxOut, _) = elem (app'symbol appSymbol) $ symbols (cIxTxOut ^. ciTxOutValue)

-- | Serialise Datum
serialiseDatum :: PlutusTx.ToData a => a -> Datum
serialiseDatum = Datum . PlutusTx.toBuiltinData

-- | Returns the Datum of a specific nftId from the Script address.
getNftDatum ::  NftId -> NftAppSymbol -> Contract w s Text (Maybe DatumNft)
getNftDatum nftId appSymbol = do
   utxos :: [Ledger.ChainIndexTxOut] <- fmap fst . Map.elems <$> getAddrValidUtxos appSymbol
   let datums :: [DatumNft] =
         utxos
           ^.. traversed . Ledger.ciTxOutDatum
             . _Right
             . to (PlutusTx.fromBuiltinData @DatumNft . getDatum)
             . _Just
             . filtered (
                \d -> case d of
                  HeadDatum _ -> False
                  NodeDatum node ->
                    let nftId' = info'id . node'information $ node
                    in nftId' == nftId
                )
   Contract.logInfo @Hask.String $ Hask.show $ "Datum Found:" <> Hask.show datums
   Contract.logInfo @Hask.String $ Hask.show $ "Datum length:" <> Hask.show (Hask.length datums)
   case datums of
     [x] ->
       pure $ Just x
     [] -> do
       Contract.logError @Hask.String "No suitable Datum can be found."
       pure Nothing
     _ : _ -> do
       Contract.logError @Hask.String "More than one suitable Datum can be found. This should never happen."
       pure Nothing

{- | Gets the Datum of a specific nftId from the Script address, and applies an
  extraction function to it.
 -}
getsNftDatum :: (DatumNft -> b) -> NftId -> NftAppSymbol -> Contract a s Text (Maybe b)
getsNftDatum f nftId  = fmap (fmap f) . getNftDatum nftId 

-- | A hashing function to minimise the data to be attached to the NTFid.
hashData :: Content -> BuiltinByteString
hashData (Content b) = sha2_256 b

-- | Find NFTs at a specific Address. Will throw an error if none or many are found.
findNft :: NftId -> NftAppSymbol -> Contract w s Text (TxOutRef, ChainIndexTxOut, DatumNft)
findNft nftId cSymbol = do
   utxos <-  getAddrValidUtxos cSymbol
   case findData utxos of
     [v] -> do
       Contract.logInfo @Hask.String $ Hask.show $ "NFT Found:" <> Hask.show v
       pure v
     [] -> Contract.throwError $ "DatumNft not found for " <> (pack . Hask.show) nftId
     _ ->
       Contract.throwError $
         "Should not happen! More than one DatumNft found for "
           <> (pack . Hask.show) nftId
   where
     findData =
       L.filter hasCorrectNft -- filter only datums with desired NftId
         . mapMaybe readTxData -- map to Maybe (TxOutRef, ChainIndexTxOut, DatumNft)
         . Map.toList

     readTxData (oref, (ciTxOut, _)) = (oref,ciTxOut,) <$> readDatum' ciTxOut

     hasCorrectNft (_, ciTxOut, datum) =
       let (cs, tn) = unAssetClass $ nftAsset datum
        in tn == nftTokenName datum -- sanity check
           && case datum of
                NodeDatum datum' ->
                  (info'id . node'information $ datum') == nftId -- check that Datum has correct NftId
                  && valueOf (ciTxOut ^. ciTxOutValue) cs tn == 1 -- check that UTXO has single NFT in Value
                HeadDatum _ -> False

-- | Get first utxo at address. Will throw an error if no utxo can be found.
fstUtxoAt :: Address -> Contract w s Text (TxOutRef, ChainIndexTxOut)
fstUtxoAt address = do
  utxos <- Contract.utxosAt address
  case Map.toList utxos of
    [] -> Contract.throwError @Text "No utxo found at address."
    x : _ -> pure x
    