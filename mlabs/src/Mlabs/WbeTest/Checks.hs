module Mlabs.WbeTest.Checks (
  Reportable (..),
  Check (..),
  Balanced (..),
  WitnessesAdded (..),
  mustBeBalanced,
  feeMustBeAdded,
  inputsMustBeAdded,
  unbalancedInsOutsShouldNotChange,
  mustBeMintBalanced,
  cNot,
  witnessesMustBeAdded,
) where

import Prelude

import Cardano.Ledger.Coin (Coin (..))

import Data.List qualified as L
import Data.Maybe (isJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text

import Ledger (TxIn, Value)

import Mlabs.WbeTest.TxInfo

import Plutus.ChainIndex (ChainIndexTxOutputs (..))
import Plutus.V1.Ledger.Ada (fromValue, lovelaceValueOf)
import Plutus.V1.Ledger.Value (symbols)

import PlutusTx.Prelude qualified as PP

class Reportable a where
  report :: a -> Text --todo color coding?
  say :: (Bool -> Bool) -> a -> Text

data CheckContext = Success | Fail

-- TODO maybe Tx id should be included here
data Check a = Check CheckContext a

data Balanced
  = Balanced
  | Unbalanced
      Value
      -- ^ In value
      Value
      -- ^ Out value
  deriving stock (Show)

instance Reportable (Check Balanced) where
  report c@(Check ctx res) = case ctx of
    Success ->
      mconcat
        [ "Balance check is Ok\n"
        , "+ result expected to be "
        , say asIs c
        , " and it is: " <> tshow res
        ]
    Fail ->
      mconcat
        [ "Balance check FAILED\n"
        , "- result expected to be "
        , say not c
        , " BUT it is: " <> tshow res
        ]

  say f ch
    | f isBalanced =
      "balanced"
    | otherwise =
      "unbalanced"
    where
      isBalanced = case ch of
        Check _ Balanced -> True
        Check _ (Unbalanced _ _) -> False

mustBeBalanced :: BalanceInfo -> Check Balanced
mustBeBalanced BalanceInfo {..}
  | valueIsBalanced = Check Success Balanced
  | otherwise = Check Fail (Unbalanced totalInputsValue totalOutsValue)
  where
    valueIsBalanced = totalInputsValue PP.== totalOutsValue PP.+ feeLovelaces
    totalInputsValue = fromWalletTotalValue
    feeLovelaces = maybe mempty (lovelaceValueOf . unCoin) fee

mustBeMintBalanced :: BalanceInfo -> Check Balanced
mustBeMintBalanced BalanceInfo {..}
  | valueMinted && balancedOnAda = Check Success Balanced
  | otherwise = Check Fail (Unbalanced totalInputsValue totalOutsValue)
  where
    insAda = fromValue totalInputsValue
    outsAda = fromValue totalBalancedValue
    balancedOnAda = insAda PP.== outsAda
    totalBalancedValue = totalOutsValue PP.+ feeLovelaces
    totalInputsValue = fromWalletTotalValue
    feeLovelaces = maybe mempty (lovelaceValueOf . unCoin) fee
    valueMinted = any (`L.notElem` symbols totalInputsValue) (symbols totalBalancedValue)

newtype Fee = Fee (Maybe Coin)
  deriving newtype (Show)

instance Reportable (Check Fee) where
  report c@(Check ctx res) = case ctx of
    Success ->
      mconcat
        [ "Fee check is Ok\n"
        , "+ result expected to "
        , say asIs c
        , " and fee is: " <> tshow res
        ]
    Fail ->
      mconcat
        [ "Balance check FAILED\n"
        , "- result expected to be "
        , say not c
        , " BUT fee is: " <> tshow res
        ]

  say f (Check _ (Fee v))
    | f (isJust v) =
      "has fee"
    | otherwise =
      "has no fee"

feeMustBeAdded :: BalanceInfo -> Check Fee
feeMustBeAdded BalanceInfo {..}
  | isJust fee = Check Success (Fee fee)
  | otherwise = Check Fail (Fee fee)

data Inputs
  = NotAdded
  | Added (Set TxIn)
  deriving stock (Show)

instance Reportable (Check Inputs) where
  report c@(Check ctx res) = case ctx of
    Success ->
      mconcat
        [ "Wallet inputs check is Ok\n"
        , "+ result expected to "
        , say asIs c
        , " and those inputs are: " <> tshow res
        ]
    Fail ->
      mconcat
        [ "Wallet inputs check FAILED\n"
        , "- result expected to "
        , say not c
        , " BUT inputs are: " <> tshow res
        ]

  say f (Check _ inputs)
    | f insAdded =
      "have inputs added by Wallet"
    | otherwise =
      "have NO inputs added by Wallet"
    where
      insAdded = case inputs of
        NotAdded -> False
        Added _ -> True

inputsMustBeAdded :: BalanceInfo -> Check Inputs
inputsMustBeAdded BalanceInfo {..}
  | Set.null txInFromWallet = Check Fail NotAdded
  | otherwise = Check Success (Added txInFromWallet)

data InsOutsChanged
  = Changed
  | NotChanged
  deriving stock (Show)

instance Reportable (Check InsOutsChanged) where
  report c@(Check ctx res) = case ctx of
    Success ->
      mconcat
        [ "Ins and Outs change check is Ok\n"
        , "+ ins and outs added before balancing supposed to "
        , say asIs c
        , " and they are: " <> tshow res
        ]
    Fail ->
      mconcat
        [ "Ins and Outs change check FAILED\n"
        , "- ins and outs of added before balancing supposed to "
        , say not c
        , " BUT inputs are: " <> tshow res
        ]

  say f (Check _ changed)
    | f inChanged =
      "change"
    | otherwise =
      "stay unchanged"
    where
      inChanged = case changed of
        Changed -> True
        NotChanged -> False

unbalancedInsOutsShouldNotChange :: BalanceInfo -> Check InsOutsChanged
unbalancedInsOutsShouldNotChange BalanceInfo {..}
  | insNotChanged && outsNotChanged =
    Check Success NotChanged
  | otherwise =
    Check Fail Changed
  where
    (unbIns, unbOuts) = unbalancedIsOuts
    (bcdIns, bcdOuts) = balancedIsOuts
    insNotChanged = all (`Set.member` bcdIns) unbIns
    outsNotChanged = case (unbOuts, bcdOuts) of
      (InvalidTx, InvalidTx) -> True
      (ValidTx unbOuts', ValidTx bcdOuts') -> all (`L.elem` bcdOuts') unbOuts'
      _ -> False

data WitnessesAdded
  = WitnessesAdded
  | NoWitnessesAdded
  deriving stock (Show)

instance Reportable (Check WitnessesAdded) where
  report c@(Check ctx res) = case ctx of
    Success ->
      mconcat
        [ "Signing check is Ok\n"
        , "+ witness set of result was expected to "
        , say asIs c
        , " and has had: " <> tshow res
        ]
    Fail ->
      mconcat
        [ "Signing check FAILED\n"
        , "- witness set of result was expected to "
        , say not c
        , " BUT has had: " <> tshow res
        ]

  say f (Check _ signed)
    | f hasBeenSigned = "have witnesses added"
    | otherwise = "remain unchanged"
    where
      hasBeenSigned = case signed of
        WitnessesAdded -> True
        NoWitnessesAdded -> False

witnessesMustBeAdded :: SignInfo -> Check WitnessesAdded
witnessesMustBeAdded SignInfo {..}
  | diffNotNull && moreWitnessesAfterSigning = Check Success WitnessesAdded
  | otherwise = Check Fail NoWitnessesAdded
  where
    diffNotNull = not $ null witnessDiff
    moreWitnessesAfterSigning =
      length signedWitnesses > length balancedWitnesses

cNot :: Check a -> Check a
cNot = \case
  Check Success a -> Check Fail a
  Check Fail a -> Check Success a

asIs :: a -> a
asIs = id

tshow :: Show a => a -> Text
tshow = Text.pack . show
