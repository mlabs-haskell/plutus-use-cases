{-# LANGUAGE UndecidableInstances #-}

module Mlabs.NFT.Contract.SetPrice (
  setPrice,
) where

import PlutusTx.Prelude hiding (mconcat, mempty, (<>))
import Prelude (mconcat, mempty)
import Prelude qualified as Hask

import Control.Lens ((^.))
import Control.Monad (void, when)
import Data.Function (on)
import Data.List qualified as L
import Data.Map qualified as Map
import Data.Monoid (Last (..))
import Data.Text (Text)
import Text.Printf (printf)

import Mlabs.Plutus.Contract (readDatum')
import Plutus.Contract (Contract)
import Plutus.Contract qualified as Contract

import Ledger (
  ChainIndexTxOut,
  PubKeyHash (..),
  Redeemer,
  TxOutRef,
  ciTxOutValue,
  pubKeyHash,
  scriptCurrencySymbol,
 )

import Ledger.Constraints qualified as Constraints
import Ledger.Typed.Scripts (validatorScript)
import Ledger.Value as Value (AssetClass (..), TokenName (..), singleton)
import Plutus.ChainIndex.Tx (ChainIndexTx)

import Mlabs.NFT.Contract.Aux (fstUtxoAt, getUserAddr)
import Mlabs.NFT.Types
import Mlabs.NFT.Validation

-- TODO: Move shared code to some Utils module
import Mlabs.NFT.Contract.Mint ()

getDatumsTxsOrdered = error () -- to fix to the new datatype

type PointInfo = (DatumNft, (TxOutRef, ChainIndexTxOut))

--------------------------------------------------------------------------------
-- Set Price

setPrice :: NftAppSymbol -> SetPriceParams -> Contract (Last NftId) s Text ()
setPrice symbol params = do
  when negativePrice $ Contract.throwError "New price can not be negative"
  ownOrefTxOut <- getUserAddr >>= fstUtxoAt
  ownPkh <- pubKeyHash <$> Contract.ownPubKey
  (oldNode, (oref, toOut)) <- findNode symbol params.sp'nftId
  when (getUserId oldNode.node'information.info'owner /= ownPkh) $
    Contract.throwError "Only owner can set price"
  let nftDatum = NodeDatum $ updateDatum oldNode
      nftVal = toOut ^. ciTxOutValue
      lookups =
        mconcat
          [ Constraints.unspentOutputs $ Map.fromList [ownOrefTxOut]
          , Constraints.unspentOutputs $ Map.fromList [(oref, toOut)]
          , Constraints.typedValidatorLookups txPolicy
          , Constraints.otherScript (validatorScript txPolicy)
          ]
      tx =
        mconcat
          [ Constraints.mustPayToTheScript nftDatum nftVal
          , Constraints.mustSpendPubKeyOutput (fst ownOrefTxOut)
          ]

  void $ Contract.submitTxConstraintsWith @NftTrade lookups tx
  Contract.tell . Last . Just $ params.sp'nftId
  where
    updateDatum node = node {node'information = node.node'information {info'price = params.sp'price}}

    getNode (NodeDatum datum, x) = Just (datum, x)
    getNode _ = Nothing

    nftEq nftId (datum, _) = nftId == info'id (node'information datum)

    findNode ::
      NftAppSymbol ->
      NftId ->
      Contract (Last NftId) s Text (NftListNode, (TxOutRef, ChainIndexTxOut))
    findNode appCS nftId = do
      lst <- getDatumsTxsOrdered appCS
      let res = find (nftEq nftId) . mapMaybe getNode $ lst
      case res of
        Nothing -> Contract.throwError "NFT not found"
        Just res' -> Hask.pure res'

    negativePrice = case params.sp'price of
      Nothing -> False
      Just v -> v < 0