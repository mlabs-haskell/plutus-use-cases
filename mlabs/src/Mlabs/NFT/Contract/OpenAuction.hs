{-# LANGUAGE UndecidableInstances #-}

module Mlabs.NFT.Contract.OpenAuction (
  openAuction,
) where

import PlutusTx.Prelude hiding (mconcat, mempty, unless, (<>))
import Prelude (mconcat)
import Prelude qualified as Hask

import Control.Monad (unless, void, when)
import Data.Map qualified as Map
import Data.Monoid (Last (..))
import Data.Text (Text)
import Text.Printf (printf)

import Plutus.Contract (Contract)
import Plutus.Contract qualified as Contract
import PlutusTx qualified

import Ledger (
  Datum (..),
  Redeemer (..),
  getCardanoTxId,
 )

import Ledger.Constraints qualified as Constraints
import Ledger.Typed.Scripts (validatorScript)
import Ledger.Value qualified as Value

import Mlabs.NFT.Contract.Aux
import Mlabs.NFT.Types
import Mlabs.NFT.Validation

openAuction :: NftAppSymbol -> AuctionOpenParams -> Contract UserWriter s Text ()
openAuction symbol (AuctionOpenParams nftId deadline minBid) = do
  ownOrefTxOut <- getUserAddr >>= fstUtxoAt
  ownPkh <- Contract.ownPubKeyHash
  PointInfo {..} <- findNft nftId symbol
  node <- case pi'datum of
    NodeDatum n -> Hask.pure n
    _ -> Contract.throwError "NFT not found"

  let auctionState = info'auctionState . node'information $ node
      isOwner = ownPkh == (getUserId . info'owner . node'information) node

  when (isJust auctionState) $ Contract.throwError "Can't open: auction is already in progress"
  unless isOwner $ Contract.throwError "Only owner can start auction"

  userUtxos <- getUserUtxos
  let nftDatum = NodeDatum $ updateDatum node
      nftVal = Value.singleton (app'symbol symbol) (Value.TokenName . nftId'contentHash $ nftId) 1
      action =
        OpenAuctionAct
          { act'symbol = symbol
          }
      lookups =
        mconcat
          [ Constraints.unspentOutputs userUtxos
          , Constraints.unspentOutputs $ Map.fromList [ownOrefTxOut]
          , Constraints.unspentOutputs $ Map.fromList [(pi'TOR, pi'CITxO)]
          , Constraints.typedValidatorLookups txPolicy
          , Constraints.otherScript (validatorScript txPolicy)
          ]
      tx =
        mconcat
          [ Constraints.mustPayToTheScript nftDatum nftVal
          , Constraints.mustIncludeDatum (Datum . PlutusTx.toBuiltinData $ nftDatum)
          , Constraints.mustSpendPubKeyOutput (fst ownOrefTxOut)
          , Constraints.mustSpendScriptOutput
              pi'TOR
              (Redeemer . PlutusTx.toBuiltinData $ action)
          ]
  ledgerTx <- Contract.submitTxConstraintsWith @NftTrade lookups tx
  Contract.tell . Last . Just . Left $ nftId
  void $ Contract.logInfo @Hask.String $ printf "Started auction for %s" $ Hask.show nftVal
  void $ Contract.awaitTxConfirmed $ getCardanoTxId ledgerTx
  void $ Contract.logInfo @Hask.String $ printf "Confirmed start auction for %s" $ Hask.show nftVal
  where
    newAuctionState =
      AuctionState
        { as'highestBid = Nothing
        , as'deadline = deadline
        , as'minBid = minBid
        }

    updateDatum node =
      node
        { node'information =
            (node'information node)
              { info'auctionState = Just newAuctionState
              }
        }
