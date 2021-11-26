{-# LANGUAGE UndecidableInstances #-}

module Mlabs.NFT.Contract.SetPrice (
  setPrice,
) where

import PlutusTx.Prelude hiding (mconcat, mempty, (<>))
import Prelude (mconcat)
import Prelude qualified as Hask

import Control.Lens ((^.))
import Control.Monad (void, when)

import Data.Map qualified as Map
import Data.Monoid (Last (..))
import Data.Text (Text)

--import Mlabs.Plutus.Contract ()
import Plutus.Contract (Contract)
import Plutus.Contract qualified as Contract
import PlutusTx qualified

import Ledger (
  Redeemer (..),
  ciTxOutValue,
 )

import Ledger.Constraints qualified as Constraints
import Ledger.Typed.Scripts (validatorScript)

--import Ledger.Value as Value (AssetClass (..), TokenName (..), singleton)
--import Plutus.ChainIndex.Tx ()

import Mlabs.NFT.Contract.Aux
import Mlabs.NFT.Types
import Mlabs.NFT.Validation

{- |
  Attempts to set price of NFT, checks if price is being set by the owner
  and that NFT is not on an auction.
-}
setPrice :: UniqueToken -> SetPriceParams -> Contract UserWriter s Text ()
setPrice ut SetPriceParams {..} = do
  aSymbol <- getNftAppSymbol ut
  when negativePrice $ Contract.throwError "New price can not be negative"
  ownOrefTxOut <- getUserAddr >>= fstUtxoAt
  ownPkh <- Contract.ownPubKeyHash
  PointInfo {..} <- findNft sp'nftId ut
  oldNode <- case pi'data of
    NodeDatum n -> Hask.pure n
    _ -> Contract.throwError "NFT not found"
  when (getUserId ((info'owner . node'information) oldNode) /= ownPkh) $
    Contract.throwError "Only owner can set price"
  when (isJust . info'auctionState . node'information $ oldNode) $
    Contract.throwError "Can't set price auction is already in progress"

  let nftDatum = NodeDatum $ updateDatum oldNode
      nftVal = pi'CITxO ^. ciTxOutValue
      action = SetPriceAct sp'price aSymbol
      lookups =
        mconcat
          [ Constraints.unspentOutputs $ Map.fromList [ownOrefTxOut]
          , Constraints.unspentOutputs $ Map.fromList [(pi'TOR, pi'CITxO)]
          , Constraints.typedValidatorLookups (txPolicy ut)
          , Constraints.otherScript (validatorScript . txPolicy $ ut)
          ]
      tx =
        mconcat
          [ Constraints.mustPayToTheScript nftDatum nftVal
          , Constraints.mustSpendPubKeyOutput (fst ownOrefTxOut)
          , Constraints.mustSpendScriptOutput
              pi'TOR
              (Redeemer . PlutusTx.toBuiltinData $ action)
          ]

  void $ Contract.submitTxConstraintsWith @NftTrade lookups tx
  Contract.tell . Last . Just . Left $ sp'nftId
  Contract.logInfo @Hask.String "set-price successful!"
  where
    updateDatum node = node {node'information = (node'information node) {info'price = sp'price}}

    negativePrice = case sp'price of
      Nothing -> False
      Just v -> v < 0
