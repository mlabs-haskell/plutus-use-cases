module Mlabs.NFT.Contract.Gov.Burn (burnGov) where

import Prelude qualified as Hask

import Control.Monad (void, when)
import Data.Map qualified as Map
import Data.Monoid ((<>))
import Data.Text (Text)

import Plutus.Contract (Contract)
import Plutus.Contract qualified as Contract
import Plutus.Contract.Constraints qualified as Constraints
import Plutus.V1.Ledger.Ada qualified as Ada (
  lovelaceValueOf,
 )
import PlutusTx.Prelude hiding (mconcat, mempty, (<>))

import Ledger (
  Address,
  getPubKeyHash,
  scriptCurrencySymbol,
 )
import Ledger.Typed.Scripts (validatorHash, validatorScript)
import Ledger.Value as Value (TokenName (..), singleton)

import Mlabs.Data.LinkedList
import Mlabs.NFT.Contract.Aux
import Mlabs.NFT.Contract.Gov.Aux
import Mlabs.NFT.Contract.Gov.Query
import Mlabs.NFT.Governance.Types
import Mlabs.NFT.Governance.Validation (govMintPolicy, govScript)
import Mlabs.NFT.Types
import Mlabs.NFT.Validation

burnGov ::
  forall s.
  UniqueToken ->
  Integer ->
  Contract UserWriter s Text ()
burnGov uT amountToBurn = do
  user <- getUId
  let ownPkh = getUserId user
  nftHead' <- getNftHead uT
  nftHead <- case nftHead' of
    Just (PointInfo (HeadDatum x) _ _ _) -> Hask.pure x
    _ -> Contract.throwError "burnGov: NFT HEAD not found"
  let newGovDatum = GovDatum $ NodeLList user GovLNode Nothing
      govAddr = appInstance'Governance . head'appInstance $ nftHead
      govValidator = govScript . appInstance'UniqueToken . head'appInstance $ nftHead
      govScriptHash = validatorHash govValidator

  govHead' <- getGovHead govAddr
  govHead <- case govHead' of
    Just x -> Hask.pure x
    Nothing -> Contract.throwError "burnGov: GOV HEAD not found"

  g@(prevGov, InsertPoint currGov nextGov) <- findGovBurnInsertPoint govAddr newGovDatum
  Contract.logInfo @Hask.String $ Hask.show $ "Gov found: " <> Hask.show g

  currStake <- querryCurrentStake uT ()

  when (currStake < amountToBurn || amountToBurn <= 0) $
    Contract.throwError "Cannot burn more GOV than current stake"

  let nodeValue = piValue govAddr currGov
      mkGov name =
        Value.singleton
          govCurr
          (TokenName . ((name <>) . getPubKeyHash) $ ownPkh)
          (negate amountToBurn)
      burntFreeGov = mkGov "freeGov"
      burntListGov = mkGov "listGov"
      govCurr = scriptCurrencySymbol govPolicy
      appInstance = head'appInstance nftHead
      govPolicy = govMintPolicy appInstance
      govRedeemer = asRedeemer ProofAndBurn
      posFee = Ada.lovelaceValueOf amountToBurn
      negFee = negate posFee
      headPrevValue = piValue govAddr govHead
      payToHead =
        Constraints.mustPayToOtherScript
          govScriptHash
          (toDatum $ pi'data govHead)
          (negFee <> headPrevValue)
      sharedLookups =
        Hask.mconcat
          [ Constraints.mintingPolicy govPolicy
          , Constraints.otherScript (validatorScript govValidator)
          , Constraints.unspentOutputs $ Map.singleton (pi'TOR govHead) (pi'CITxO govHead)
          , Constraints.unspentOutputs $ Map.singleton (pi'TOR currGov) (pi'CITxO currGov)
          ]
      sharedTx =
        Hask.mconcat
          [ Constraints.mustSpendScriptOutput (pi'TOR currGov) govRedeemer
          , Constraints.mustSpendScriptOutput (pi'TOR govHead) govRedeemer
          , Constraints.mustPayToPubKey ownPkh posFee
          , Constraints.mustMintValueWithRedeemer govRedeemer (burntFreeGov <> burntListGov)
          ]
      (lookups, tx) =
        if currStake == amountToBurn
          then
            let updatedPrevDatum = pointNodeToMaybe' (pi'data prevGov) (pi'data <$> nextGov)
                prevNodeValue = piValue govAddr prevGov
                lookups' =
                  Hask.mconcat
                    [ Constraints.unspentOutputs $ Map.singleton (pi'TOR prevGov) (pi'CITxO prevGov)
                    ]
                tx' =
                  Hask.mconcat
                    [ Constraints.mustSpendScriptOutput (pi'TOR prevGov) govRedeemer
                    , case gov'list updatedPrevDatum of
                        HeadLList {} ->
                          Constraints.mustPayToOtherScript
                            govScriptHash
                            (toDatum updatedPrevDatum)
                            (negFee <> prevNodeValue)
                        NodeLList {} ->
                          Hask.mconcat
                            [ payToHead
                            , Constraints.mustPayToOtherScript
                                govScriptHash
                                (toDatum updatedPrevDatum)
                                prevNodeValue
                            ]
                    ]
             in (lookups', tx')
          else
            let lookups' = Hask.mempty
                tx' =
                  Hask.mconcat
                    [ Constraints.mustPayToOtherScript
                        govScriptHash
                        (toDatum . pi'data $ currGov)
                        (burntListGov <> nodeValue)
                    , payToHead
                    ]
             in (lookups', tx')
  void $ Contract.submitTxConstraintsWith @NftTrade (lookups <> sharedLookups) (tx <> sharedTx)
  Contract.logInfo @Hask.String "burn successful!"

findGovBurnInsertPoint :: Address -> GovDatum -> GenericContract (PointInfo GovDatum, InsertPoint GovDatum)
findGovBurnInsertPoint addr node = do
  list <- getDatumsTxsOrderedFromAddr @GovDatum addr
  findPoint list
  where
    findPoint = \case
      (x1 : x2 : x3 : xs) ->
        -- x1 -> x2 -> x3 -> ...
        if pi'data x2 == node
          then pure (x1, InsertPoint x2 (Just x3))
          else findPoint (x2 : x3 : xs)
      (x1 : x2 : _) ->
        -- x1 -> x2 -> Nothing
        if pi'data x2 == node
          then pure (x1, InsertPoint x2 Nothing)
          else Contract.throwError "GOV node not found"
      _ -> Contract.throwError "GOV node not found"
