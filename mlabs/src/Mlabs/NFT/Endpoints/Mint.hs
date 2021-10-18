module Mlabs.NFT.Endpoints.Mint (
  mint
) where

import PlutusTx.Prelude hiding (mconcat, (<>))
import Prelude (mconcat)
import Prelude qualified as Hask

import Control.Monad (void)
import Data.Map qualified as Map
import Data.Monoid (Last (..))
import Data.Text (Text)
import Text.Printf (printf)

import Plutus.Contract (Contract)
import Plutus.Contract qualified as Contract

import Ledger (
  ChainIndexTxOut,
  TxOutRef,
  PubKeyHash(..),
  pubKeyHash,
  scriptCurrencySymbol,
 )

import Ledger.Constraints qualified as Constraints
import Ledger.Typed.Scripts (validatorScript)
import Ledger.Value as Value (AssetClass(..), TokenName(..), singleton)

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
import Mlabs.NFT.Endpoints.Aux
--------------------------------------------------------------------------------
-- MINT --

---- | Mints an NFT and sends it to the App Address.
mint :: NftAppSymbol -> MintParams -> Contract (Last NftId) s Text ()
mint symbol params = do
  ownOrefTxOut <- getUserAddr >>= fstUtxoAt
  nftId        <- nftIdInit params
  ownPkh       <- pubKeyHash <$> Contract.ownPubKey
  (precedingDatum, orefTxOut) 
               <- findNode symbol nftId
  let newNftNode                 = mkNode (getAppInstance precedingDatum) nftId params ownPkh
      (newPreceding, linkedNode) = precedingDatum `linkWith` newNftNode
      (lookups, tx, mintedVal)   = buildLookupsTx newPreceding linkedNode ownOrefTxOut orefTxOut
  void $ Contract.submitTxConstraintsWith @NftTrade lookups tx
  Contract.tell . Last . Just $ nftId
  Contract.logInfo @Hask.String $ printf "forged %s" (Hask.show mintedVal)
  where
    nftIdInit = error ()

    findNode 
      :: NftAppSymbol 
      -> NftId 
      -> Contract (Last NftId) s Text (DatumNft, (TxOutRef, ChainIndexTxOut))
    findNode appCS contentHash = Contract.throwError "TODO"

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

    buildLookupsTx newPrecedingDatum newNode authorOrefTxOut precedingOrefTxOut = 
      let 
          nftDatum     = NodeDatum newNode
          precedingNft = error ()
          nftPolicy = mintPolicy (getAppInstance nftDatum) -- ? do we check that UTXO was consumed during minting
          nftTokName = nftTokenName nftDatum
          nftVal = Value.singleton (scriptCurrencySymbol nftPolicy) nftTokName 1
          mintRedeemer = asRedeemer $ Mint (info'id $ node'information newNode)
          spendRedeemer = error ()
          lookups =
            mconcat
              [ Constraints.unspentOutputs $ Map.fromList [authorOrefTxOut]
              , Constraints.unspentOutputs $ Map.fromList [precedingOrefTxOut]
              , Constraints.mintingPolicy nftPolicy
              , Constraints.typedValidatorLookups txPolicy
              , Constraints.otherScript (validatorScript txPolicy)
              ]
          tx =
            mconcat
              [ Constraints.mustMintValueWithRedeemer mintRedeemer nftVal 
                -- ^ minting new NFT
              , Constraints.mustSpendScriptOutput (fst precedingOrefTxOut) spendRedeemer 
                -- ^ spending UTXO of preceding node
              , Constraints.mustPayToTheScript newPrecedingDatum precedingNft 
                -- ^ putting back to script NFT from preceding node with updated Datum
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