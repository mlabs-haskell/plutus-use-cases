module Mlabs.Roundtrip.PreBalance(
  PrebalancedTx(..),
  preBalanceTxFrom
) where

import PlutusTx.Prelude
import Prelude qualified as Hask

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Control.Lens

import Ledger
import Ledger.Constraints.OffChain (UnbalancedTx(..), adjustUnbalancedTx, tx)
import Plutus.Contract
import Plutus.V1.Ledger.Value as Value
import Plutus.V1.Ledger.Ada qualified as Ada


newtype PrebalancedTx = PrebTx UnbalancedTx

type InsValue = Value
type OutsValue = Value
type FeeValue = Value

preBalanceTxFrom 
  :: Address
  -> TxOutRef
  -> UnbalancedTx
  -> Contract w s ContractError PrebalancedTx
preBalanceTxFrom addr collateralRef utx = do
  logInfo @Hask.String $ "Getting UTxOs"
  txFee <- getFee
  addrUtxos <- utxosAt addr
  let outsValue :: Value = mconcat $ fmap txOutValue (utx ^. tx . outputs)
  (insValue, insForBalancing) <- getBalanceInputs addrUtxos collateralRef outsValue
  collateralSet <- mkCollateralByRef collateralRef addrUtxos
  logInfo @Hask.String $ "All UTXOs from address:"
  mapM_ (logInfo @Hask.String . Hask.show) (Map.toList addrUtxos)
  logInfo @Hask.String $ "Inputs Value:" Hask.++ (Hask.show insValue)
  logInfo @Hask.String $ "Ins:" Hask.++ (Hask.show insForBalancing)
  logInfo @Hask.String $ "Unbalanced:"
  logInfo @Hask.String $ (Hask.show utx)
  let 
      changeOut = mkChangeOut addr insValue outsValue txFee
      newUtx = 
        set (tx . fee)  txFee
        $ over (tx . inputs) (Set.union insForBalancing)
        $ set (tx . collateralInputs) collateralSet
        $ over (tx . outputs) (changeOut : )
        $ utx
  logInfo @Hask.String $ "PRE Unbalanced:"
  logInfo @Hask.String $ (Hask.show newUtx)
  pure $ PrebTx $ adjustUnbalancedTx newUtx

getFee :: Contract w s e Value
getFee = pure $ Ada.adaValueOf 2

mkCollateralByRef :: TxOutRef -> Map TxOutRef ChainIndexTxOut -> Contract w s ContractError (Set TxIn)
mkCollateralByRef cOref addrUtxos = do
  case cOref `Map.member` addrUtxos of
    True -> pure . Set.singleton $ TxIn cOref (Just ConsumePublicKeyAddress)
    _ -> throwError $ OtherError "Collateral not found in inputs"


getBalanceInputs 
  :: Map TxOutRef ChainIndexTxOut
  -> TxOutRef
  -> OutsValue 
  -> Contract w s ContractError (Value, Set TxIn)
getBalanceInputs addrUtxos collRef outsValue = do
  let (insValue, insRefs) = getEnoughInputs (Map.toList addrUtxos)
  if insValue `Value.geq` outsValue
    then pure $ (insValue, insRefs)
    else do
      logInfo @Hask.String $ "Inputs from address dont have enough value: " 
      logInfo @Hask.String $ "Total UTXOs at address: " Hask.++ (Hask.show $ Map.size addrUtxos) 
      logInfo @Hask.String $ "Outs Value: " Hask.++ (Hask.show outsValue) 
      logInfo @Hask.String $ "Ins Value: " Hask.++ (Hask.show insValue) 
      logInfo @Hask.String $ "Ins refs used: " Hask.++ (Hask.show $ Set.size insRefs) 
      logInfo @Hask.String $ "Ins refs: " Hask.++ (Hask.show insRefs) 
      throwError $ OtherError "Not enough inputs to balance transaction"
  where
    getEnoughInputs :: [(TxOutRef, ChainIndexTxOut)] -> (Value, Set TxIn)
    getEnoughInputs utxoList = 
      fmap (Set.fromList . fmap (`TxIn` (Just ConsumePublicKeyAddress)) ) 
      $ go (mempty, []) filteredIns
      where
        -- Nami wallet won't let use collateral as input,
        -- need to filter collateral out before picking inputs for balancing
        filteredIns = filter ((/= collRef) . fst) utxoList
        -- filteredIns = filter ((/= collRef) . fst) [utxoList !! 1] -- fixme: it's hardcoded to make it work, 
                                                                  -- coz chain-index returns some utxos
                                                                  -- that are not actually at that address (?)
                                                                  -- probably for new run correct index need to be found
                                                                  -- by examining address with cardano-xli
        go (v,l) [] = (v,l) 
        go (v,l) _ | v `Value.geq` outsValue = (v,l)
        go (v,l) ((oref, ci):ocs) = go (v + (ci ^. ciTxOutValue), oref:l) ocs

mkChangeOut :: Address -> InsValue -> OutsValue -> FeeValue -> TxOut
mkChangeOut addr insValue outsValue feeValue = 
   TxOut addr (insValue - outsValue - feeValue) Nothing
