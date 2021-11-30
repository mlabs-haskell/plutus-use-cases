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
  :: PubKeyHash
  -> UnbalancedTx
  -> Contract w s ContractError PrebalancedTx
preBalanceTxFrom pkh utx = do
  logInfo @Hask.String $ "Getting UTxOs"
  txFee <- getFee
  let outsValue :: Value = mconcat $ fmap txOutValue (utx ^. tx . outputs)
  (insValue, insForBalancing) <- getBalanceInputs pkh outsValue
  logInfo @Hask.String $ "Inputs Value:" Hask.++ (Hask.show insValue)
  logInfo @Hask.String $ "Ins:" Hask.++ (Hask.show insForBalancing)
  logInfo @Hask.String $ "Unbalanced:"
  logInfo @Hask.String $ (Hask.show utx)
  let 
      changeOut = mkChangeOut pkh insValue outsValue txFee
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
  :: PubKeyHash 
  -> OutsValue 
  -> Contract w s ContractError (Value, Set TxIn)
getBalanceInputs pkh outsValue = do
  utxos <- Map.toList <$> utxosAt (pubKeyHashAddress pkh)
  maybe
    (throwError $ OtherError "Not enough inputs to balance transaction")
    pure
    (enoughInputs utxos)
  where
    enoughInputs :: [(TxOutRef, ChainIndexTxOut)] -> Maybe (Value, Set TxIn)
    enoughInputs utxoList =
      if insValue `Value.geq` outsValue
        then Just (insValue,  Set.fromList $ (`TxIn` (Just ConsumePublicKeyAddress)) <$> insORefs)
        else Nothing
      where
        (insValue, insORefs) = go (mempty, []) utxoList
        go (v,l) [] = (v,l) 
        go (v,l) _ | v `Value.geq` outsValue = (v,l)
        go (v,l) ((oref, ci):ocs) = go (v + (ci ^. ciTxOutValue), oref:l) ocs

mkChangeOut :: PubKeyHash -> InsValue -> OutsValue -> FeeValue -> TxOut
mkChangeOut pkh insValue outsValue feeValue = 
   TxOut (pubKeyHashAddress pkh) (insValue - outsValue - feeValue) Nothing