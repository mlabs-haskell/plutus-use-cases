module Mlabs.WbeTest.Checks (
  Reportable(..),
  Check(..),
  Balanced(..),
  mustBeBalanced,
  feeMustBeAdded,
  inputsMustBeAdded,
  unbalancedInsOutsShouldNotChange,
  mustBeMintBalanced,
  cNot
  -- checkSigned
) where

import Prelude

import PlutusTx.Prelude qualified as PP

import Data.Maybe (isJust)
import Data.Set (Set) 
import Data.Set qualified as Set 
import Data.Map qualified as Map
import Data.List qualified as L
import Data.Function (on)

import Ledger (Value, TxIn)
import Plutus.V1.Ledger.Value (symbols)
import Cardano.Ledger.Coin (Coin(..))

import Mlabs.WbeTest.TxInfo

import Plutus.ChainIndex (ChainIndexTxOutputs(..))
import Plutus.V1.Ledger.Ada (lovelaceValueOf, fromValue, getAda)

data CheckContext = Success | Fail

-- todo maybe Tx id should be included here
data Check a = Check CheckContext a

data Balanced
  = Balanced
  | Unbalanced {insValue :: Value, outsValue :: Value}
  deriving stock Show

mustBeBalanced :: BalanceInfo -> Check Balanced
mustBeBalanced BalanceInfo {..}
  | valueIsBalanced = Check Success Balanced
  | otherwise = Check Fail (Unbalanced totalInputsValue totalOutsValue)
  where
    valueIsBalanced = totalInputsValue PP.== totalOutsValue PP.+ feeLovelaces
    totalInputsValue = lookupsTotalValue PP.+ fromWalletTotalValue
    feeLovelaces = maybe mempty (lovelaceValueOf . unCoin) fee

mustBeMintBalanced :: BalanceInfo -> Check Balanced
mustBeMintBalanced BalanceInfo {..}
  | valueMinted && balancedOnAda = Check Success Balanced
  | otherwise = Check Fail (Unbalanced totalInputsValue totalOutsValue)
  where
    insAda = fromValue totalInputsValue
    outsAda = fromValue $ totalBalancedValue
    balancedOnAda = insAda PP.== outsAda
    totalBalancedValue = totalOutsValue PP.+ feeLovelaces
    totalInputsValue = lookupsTotalValue PP.+ fromWalletTotalValue
    feeLovelaces = maybe mempty (lovelaceValueOf . unCoin) fee
    valueMinted = any (\s -> not $ L.elem s (symbols totalInputsValue)) (symbols totalBalancedValue)

newtype Fee =
  Fee {feeCoin :: Maybe Coin}
  deriving newtype Show

feeMustBeAdded :: BalanceInfo -> Check Fee
feeMustBeAdded BalanceInfo {..}
  | isJust fee = Check Success (Fee fee)
  | otherwise = Check Fail (Fee fee)
  where

data Inputs 
  = NotAdded
  | Added (Set TxIn)
  deriving Show

inputsMustBeAdded :: BalanceInfo -> Check Inputs
inputsMustBeAdded BalanceInfo {..}
  | Set.null txInFromWallet = Check Fail NotAdded
  | otherwise = Check Success (Added txInFromWallet)


data InsOutsChanged
  = Changed
  | NotChanged
  deriving Show

unbalancedInsOutsShouldNotChange :: BalanceInfo -> Check InsOutsChanged
unbalancedInsOutsShouldNotChange BalanceInfo {..}
  | insNotChanged && outsNotChanged 
  = Check Success NotChanged
  | otherwise
  = Check Fail Changed
  where 
    (unbIns, unbOuts) = unbalancedIsOuts 
    (bcdIns, bcdOuts) = balancedIsOuts
    insNotChanged = all (`Set.member` bcdIns) unbIns
    outsNotChanged = case  (unbOuts, bcdOuts) of
      (InvalidTx, InvalidTx) -> True
      (ValidTx unbOuts', ValidTx bcdOuts') -> all (`L.elem` bcdOuts') unbOuts'
      _ -> False

cNot :: Check a -> Check a
cNot = \case
  Check Success a -> Check Fail a
  Check Fail a -> Check Success a

class Reportable a where
  report :: a -> String --todo color coding?
  say :: (Bool -> Bool) -> a -> String

asIs :: a -> a
asIs = id

instance Reportable (Check Balanced) where
  report c@(Check ctx res) = case ctx of
    Success ->
      mconcat [
        "Balance check is Ok\n"
      , "+ result expected to be ", say asIs c
      , " and it is: " ++ show res
      ]
    Fail -> mconcat [
        "Balance check FAILED\n"
      , "- result expected to be ", say not c
      , " BUT it is: " ++ show res
      ]

  say f ch 
    | f isBalanced
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
      mconcat [
        "Fee check is Ok\n"
      , "+ result expected to ", say asIs c
      , " and fee is: " ++ show res
      ]
    Fail -> mconcat [
        "Balance check FAILED\n"
      , "- result expected to be ", say not c
      , " BUT fee is: " ++ show res
      ]

  say f (Check _ (Fee v)) 
    | f (isJust v)
    = "has fee"
    | otherwise 
    = "has no fee"

instance Reportable (Check Inputs) where
  report c@(Check ctx res) = case ctx of
    Success ->
      mconcat [
        "Wallet inputs check is Ok\n"
      , "+ result expected to ", say asIs c
      , " and those inputs are: " ++ show res
      ]
    Fail -> mconcat [
        "Wallet inputs check FAILED\n"
      , "- result expected to ", say not c
      , " BUT inputs are: " ++ show res
      ]

  say f (Check _ inputs) 
    | f insAdded 
    = "have inputs added by Wallet"
    | otherwise 
    = "have NO inputs added by Wallet"
    where
      insAdded = case inputs of
        NotAdded -> False
        Added _ -> True


instance Reportable (Check InsOutsChanged) where
  report c@(Check ctx res) = case ctx of
    Success ->
      mconcat [
        "Ins and Outs change check is Ok\n"
      , "+ ins and outs added before balancing supposed to ", say asIs c
      , " and they are: " ++ show res
      ]
    Fail -> mconcat [
        "Ins and Outs change check FAILED\n"
      , "- ins and outs of added before balancing supposed to ", say not c
      , " BUT inputs are: " ++ show res
      ]

  say f (Check _ changed) 
    | f inChanged 
    = "change"
    | otherwise 
    = "stay unchanged"
    where
      inChanged = case changed of
        Changed -> True
        NotChanged  -> False
