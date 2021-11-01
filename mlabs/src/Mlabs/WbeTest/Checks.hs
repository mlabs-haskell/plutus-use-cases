module Mlabs.WbeTest.Checks (
  Reportable(..),
  Check(..),
  Balanced(..),
  mustBeBalanced,
  feeMustBeAdded,
  cNot
  -- checkSigned
) where

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
mustBeBalanced BalanceInfo {..}
  | valueBalanced = Check Success Balanced
  | otherwise = Check Fail (Unbalanced totalInputsValue totalOutsValue)
  where
    valueBalanced = totalInputsValue == totalOutsValue + feeLovelaces
    totalInputsValue = lookupsTotalValue + utxosTotalValue
    feeLovelaces = maybe mempty (lovelaceValueOf . unCoin) fee

newtype Fee =
  Fee {feeCoin :: Maybe Coin}
  deriving newtype Hask.Show

feeMustBeAdded :: BalanceInfo -> Check Fee
feeMustBeAdded BalanceInfo {..}
  | isJust fee = Check Success (Fee fee)
  | otherwise = Check Fail (Fee fee)
  where

cNot :: Check a -> Check a
cNot = \case
  Check Success a -> Check Fail a
  Check Fail a -> Check Success a

class Reportable a where
  report :: a -> Hask.String --todo color coding?
  say :: Bool -> a -> Hask.String

instance Reportable (Check Balanced) where
  report c@(Check ctx res) = case ctx of
    Success ->
      Hask.mconcat [
        "Balance check is Ok\n"
      , "+ result expected to be ", say False c
      , " and it is: " ++ Hask.show res
      ]
    Fail -> Hask.mconcat [
        "Balance check FAILED\n"
      , "- result expected to be ", say True c
      , " BUT it is: " ++ Hask.show res
      ]

  say opposite ch 
    | not opposite && isBalanced 
    = "balanced"
    | otherwise 
    = "unbalanced"
    where
      isBalanced = case ch of
        (Check _ Balanced) -> True
        (Check _ (Unbalanced _ _)) -> False

instance Reportable (Check Fee) where
  report c@(Check ctx res) = case ctx of
    Success ->
      Hask.mconcat [
        "Fee check is Ok\n"
      , "+ result expected to ", say False c
      , " and fee is: " ++ Hask.show res
      ]
    Fail -> Hask.mconcat [
        "Balance check FAILED\n"
      , "- result expected to be ", say True c
      , " BUT fee is: " ++ Hask.show res
      ]

  say opposite (Check _ (Fee v)) 
    | not opposite && isJust v 
    = "has fee"
    | otherwise 
    = "has no fee"
