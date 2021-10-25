{-# LANGUAGE UndecidableInstances #-}

module Mlabs.NFT.Contract.Buy (
  buy,
) where

import PlutusTx.Prelude hiding (mconcat, mempty, (<>))
import Prelude (mconcat, mempty)
import Prelude qualified as Hask

import Control.Lens ((^.))
import Control.Monad (void)
import Data.Function (on)
import Data.List qualified as L
import Data.Map qualified as Map
import Data.Monoid (Last (..))
import Data.Text (Text)
import Text.Printf (printf)

import Mlabs.Plutus.Contract (readDatum')
import Plutus.Contract (Contract)
import Plutus.Contract qualified as Contract
import PlutusTx qualified

import Ledger (
  ChainIndexTxOut,
  Datum (..),
  PubKeyHash (..),
  Redeemer (..),
  TxOutRef,
  ciTxOutValue,
  pubKeyHash,
  scriptCurrencySymbol,
 )

import Ledger.Constraints qualified as Constraints
import Ledger.Typed.Scripts (validatorScript)
import Ledger.Value as Value (AssetClass (..), TokenName (..), singleton)
import Plutus.ChainIndex.Tx (ChainIndexTx)

import Mlabs.NFT.Contract.Aux
import Mlabs.NFT.Types
import Mlabs.NFT.Validation

-- TODO: Move shared code to some Utils module
import Mlabs.NFT.Contract.Mint (getDatumsTxsOrdered)

{- | BUY.
 Attempts to buy a new NFT by changing the owner, pays the current owner and
 the author, and sets a new price for the NFT.
-}
buy :: NftAppSymbol -> BuyRequestUser -> Contract (Last NftId) s Text ()
buy symbol params = do
  ownOrefTxOut <- getUserAddr >>= fstUtxoAt
  ownPkh <- pubKeyHash <$> Contract.ownPubKey
  (node, (oref, toOut)) <- findNode symbol params.ur'nftId
  case node.node'information.info'price of
    Nothing -> Contract.logError @Hask.String "NFT not for sale."
    Just price ->
      if params.ur'price < price
        then Contract.logError @Hask.String "Bid price is too low."
        else do
          userUtxos <- getUserUtxos
          let (paidToOwner, paidToAuthor) = calculateShares params.ur'price node.node'information.info'share
              nftDatum = NodeDatum $ updateDatum ownPkh node
              nftVal = toOut ^. ciTxOutValue
              action =
                BuyAct
                  { act'bid = params.ur'price
                  , act'newPrice = params.ur'newPrice
                  }
              lookups =
                mconcat
                  [ Constraints.unspentOutputs userUtxos
                  , Constraints.unspentOutputs $ Map.fromList [ownOrefTxOut]
                  , Constraints.unspentOutputs $ Map.fromList [(oref, toOut)]
                  , Constraints.typedValidatorLookups txPolicy
                  , Constraints.otherScript (validatorScript txPolicy)
                  ]
              tx =
                mconcat
                  [ Constraints.mustPayToTheScript nftDatum nftVal
                  , Constraints.mustIncludeDatum (Datum . PlutusTx.toBuiltinData $ nftDatum)
                  , Constraints.mustPayToPubKey (getUserId node.node'information.info'author) paidToAuthor
                  , Constraints.mustPayToPubKey (getUserId node.node'information.info'owner) paidToAuthor
                  , Constraints.mustSpendPubKeyOutput (fst ownOrefTxOut)
                  , Constraints.mustSpendScriptOutput
                      oref
                      (Redeemer . PlutusTx.toBuiltinData $ action)
                  ]
          void $ Contract.submitTxConstraintsWith @NftTrade lookups tx
  Contract.tell . Last . Just $ params.ur'nftId
  where
    updateDatum newOwner node =
      node
        { node'information =
            node.node'information
              { info'price = params.ur'newPrice
              , info'owner = UserId newOwner
              }
        }

    getNode (NodeDatum datum, x) = Just (datum, x)
    getNode _ = Nothing

    nftEq nftId (datum, _) = nftId == info'id (node'information datum)

    findNode ::
      NftAppSymbol ->
      NftId ->
      Contract w s Text (NftListNode, (TxOutRef, ChainIndexTxOut)) -- to change to new PointInfo
    findNode appCS nftId = error ()

--      lst <- getDatumsTxsOrdered appCS
--      let res = find (nftEq nftId) . mapMaybe getNode $ lst
--      case res of
--        Nothing -> Contract.throwError "NFT not found"
--        Just res' -> Hask.pure res'
