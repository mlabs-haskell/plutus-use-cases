module Mlabs.WbeTest.Checks (
  Reportable,
  report,
  mustBeBalanced,
  feeMustBeAdded,
  cNot
  -- checkSigned
) where

import Prelude (print)
import Prelude qualified as Hask
import PlutusTx.Prelude

import Ledger (Value)
import Cardano.Ledger.Coin (Coin(..))

import Mlabs.WbeTest.TxInfo

-- debug imports
import Plutus.V1.Ledger.Ada (lovelaceValueOf)

data CheckContext = Success | Fail

-- todo maybe Tx id should be included here
data Check a = Check CheckContext a

data Balanced 
  = Balanced 
  | Unbalanced {insValue :: Value, outsValue :: Value}
  deriving stock Hask.Show

mustBeBalanced :: BalanceInfo -> Check Balanced
mustBeBalanced BalanceInfo {..} = 
  if valueBalanced 
    then  Check Success Balanced
    else  Check Fail (Unbalanced totalInputsValue totalOutsValue)
  where
    valueBalanced = totalInputsValue == totalOutsValue + feeLovelaces
    totalInputsValue = lookupsTotalValue + utxosTotalValue
    feeLovelaces = maybe mempty (lovelaceValueOf . unCoin) fee

newtype Fee = 
  Fee {feeCoin :: Maybe Coin}
  deriving newtype Hask.Show

feeMustBeAdded :: BalanceInfo -> Check Fee
feeMustBeAdded BalanceInfo {..} = 
  if feeAdded
    then Check Success (Fee fee)
    else Check Fail (Fee fee)
  where
    feeAdded = isJust fee

cNot :: Check a -> Check a
cNot = \case
  Check Success a -> Check Fail a
  Check Fail a -> Check Success a

class Reportable a where
  report :: a -> Hask.String --todo color coding?
  say :: a -> Hask.String
  sayOpposite :: a -> Hask.String

instance Reportable (Check Balanced) where
  report c@(Check ctx res) = case ctx of
    Success -> 
      Hask.mconcat [
        "Balance check is Ok\n"
      , "+ result expected to be ", say c
      , " and it is: " ++ Hask.show res
      ]
    Fail -> Hask.mconcat [
        "Balance check FAILED\n"
      , "- result expected to be ", sayOpposite c
      , " BUT it is: " ++ Hask.show res
      ]

  say = \case
    (Check _ Balanced) -> "balanced"
    (Check _ (Unbalanced _ _)) -> "unbalanced"
  
  
  sayOpposite = \case
    (Check _ Balanced) -> "unbalanced"
    (Check _ (Unbalanced _ _)) -> "balanced"

instance Reportable (Check Fee) where
  report c@(Check ctx res) = case ctx of
    Success -> 
      Hask.mconcat [
        "Fee check is Ok\n"
      , "+ result expected to ", say c
      , " and fee is: " ++ Hask.show res
      ]
    Fail -> Hask.mconcat [
        "Balance check FAILED\n"
      , "- result expected to be ", sayOpposite c
      , " BUT fee is: " ++ Hask.show res
      ]

  say (Check _ (Fee v)) = 
    if isJust v
      then "has fee"
      else "has no fee"
  
  
  sayOpposite (Check _ (Fee v)) = 
    if isJust v
      then "has no fee"
      else "has fee fee"
