module Mlabs.Roundtrip.PreBalance(
  PrebalancedTx(..),
  preBalanceTxFrom
) where

import PlutusTx.Prelude
import Prelude qualified as Hask

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
  -> UnbalancedTx
  -> Contract w s ContractError PrebalancedTx
preBalanceTxFrom addr utx = do
  logInfo @Hask.String $ "Getting UTxOs"
  txFee <- getFee
  let outsValue :: Value = mconcat $ fmap txOutValue (utx ^. tx . outputs)
  (insValue, insForBalancing) <- getBalanceInputs addr outsValue
  logInfo @Hask.String $ "Inputs Value:" Hask.++ (Hask.show insValue)
  logInfo @Hask.String $ "Ins:" Hask.++ (Hask.show insForBalancing)
  logInfo @Hask.String $ "Unbalanced:"
  logInfo @Hask.String $ (Hask.show utx)
  let 
      changeOut = mkChangeOut addr insValue outsValue txFee
      newUtx = 
        set (tx . fee)  txFee
        $ over (tx . inputs) (Set.union insForBalancing)
        $ over (tx . outputs) (changeOut : )
        $ utx
  logInfo @Hask.String $ "PRE Unbalanced:"
  logInfo @Hask.String $ (Hask.show newUtx)
  pure $ PrebTx $ adjustUnbalancedTx newUtx

getFee :: Contract w s e Value
getFee = pure $ Ada.adaValueOf 2

getBalanceInputs 
  :: Address 
  -> OutsValue 
  -> Contract w s ContractError (Value, Set TxIn)
getBalanceInputs addr outsValue = do
  utxos <- Map.toList <$> utxosAt addr
  let (insValue, insRefs) = getEnoughInputs utxos
  if insValue `Value.geq` outsValue
    then pure $ (insValue, insRefs)
    else do
      logInfo @Hask.String $ "Inputs from address dont have enough value: " 
      logInfo @Hask.String $ "Total UTXOs at address: " Hask.++ (Hask.show $ length utxos) 
      logInfo @Hask.String $ "Outs Value: " Hask.++ (Hask.show outsValue) 
      logInfo @Hask.String $ "Ins Value: " Hask.++ (Hask.show insValue) 
      logInfo @Hask.String $ "Ins refs used: " Hask.++ (Hask.show $ Set.size insRefs) 
      logInfo @Hask.String $ "Ins refs: " Hask.++ (Hask.show insRefs) 
      throwError $ OtherError "Not enough inputs to balance transaction"
  where
    getEnoughInputs :: [(TxOutRef, ChainIndexTxOut)] -> (Value, Set TxIn)
    getEnoughInputs utxoList = 
      fmap (Set.fromList . fmap (`TxIn` (Just ConsumePublicKeyAddress)) ) 
      $ go (mempty, []) utxoList
      where
        -- (insValue, insORefs) = go (mempty, []) utxoList
        go (v,l) [] = (v,l) 
        go (v,l) _ | v `Value.geq` outsValue = (v,l)
        go (v,l) ((oref, ci):ocs) = go (v + (ci ^. ciTxOutValue), oref:l) ocs

mkChangeOut :: Address -> InsValue -> OutsValue -> FeeValue -> TxOut
mkChangeOut addr insValue outsValue feeValue = 
   TxOut addr (insValue - outsValue - feeValue) Nothing