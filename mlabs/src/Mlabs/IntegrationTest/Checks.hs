{-# LANGUAGE GADTs #-}

module Mlabs.IntegrationTest.Checks (
  Reportable (..),
  AnyCheck (..),
  Check (..),
  CheckContext (..),
  Balanced (..),
  Fee (..),
  Inputs (..),
  InsOutsChanged (..),
  WitnessesAdded (..),
  cNot,
  asIs,
  witnessesMustBeAdded,
  mustBeBalanced,
  mustBeMintBalanced,
  feeMustBeAdded,
  inputsMustBeAdded,
  unbalancedInsOutsShouldNotChange,
) where

import Prelude

import Data.List qualified as L
import Data.Maybe (fromMaybe, isJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)

import Ledger (TxIn, Value)
import Ledger.Ada qualified as Ada
import Ledger.Value qualified as Value

import Mlabs.IntegrationTest.Types
import Mlabs.IntegrationTest.Utils

import Plutus.ChainIndex (ChainIndexTxOutputs (..))

import PlutusTx.Prelude qualified as PP

class Reportable a where
  report :: a -> Text --todo color coding?
  say :: (Bool -> Bool) -> a -> Text

data AnyCheck where
  AnyCheck :: forall a. Reportable (Check a) => Check a -> AnyCheck

-- TODO maybe Tx id should be included here
data Check a = Check CheckContext a

data CheckContext = Success | Fail

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

newtype Fee = Fee (Maybe Value)
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

cNot :: Check a -> Check a
cNot = \case
  Check Success a -> Check Fail a
  Check Fail a -> Check Success a

asIs :: a -> a
asIs = id

--Individual checks------------------------------------------------------------
witnessesMustBeAdded :: SignInfo -> Check WitnessesAdded
witnessesMustBeAdded SignInfo {..}
  | diffNotNull && moreWitnessesAfterSigning = Check Success WitnessesAdded
  | otherwise = Check Fail NoWitnessesAdded
  where
    diffNotNull = not $ null witnessDiff
    moreWitnessesAfterSigning =
      length signedWitnesses > length balancedWitnesses

mustBeBalanced :: BalanceInfo -> Check Balanced
mustBeBalanced BalanceInfo {..}
  | valueIsBalanced = Check Success Balanced
  | otherwise = Check Fail (Unbalanced totalInsValue totalOutsValue)
  where
    valueIsBalanced =
      totalInsValue PP.== totalOutsValue
        PP.+ fromMaybe mempty fee

mustBeMintBalanced :: BalanceInfo -> Check Balanced
mustBeMintBalanced BalanceInfo {..}
  | valueMinted && balancedOnAda = Check Success Balanced
  | otherwise = Check Fail (Unbalanced totalInsValue totalOutsValue)
  where
    insAda = Ada.fromValue totalInsValue
    outsAda = Ada.fromValue totalBalancedValue
    balancedOnAda = insAda PP.== outsAda
    totalBalancedValue = totalOutsValue PP.+ fromMaybe mempty fee
    valueMinted =
      any
        (`L.notElem` Value.symbols totalInsValue)
        (Value.symbols totalBalancedValue)

feeMustBeAdded :: BalanceInfo -> Check Fee
feeMustBeAdded BalanceInfo {..}
  | isJust fee = Check Success (Fee fee)
  | otherwise = Check Fail (Fee fee)

inputsMustBeAdded :: BalanceInfo -> Check Inputs
inputsMustBeAdded BalanceInfo {..}
  | Set.null txInFromWallet = Check Fail NotAdded
  | otherwise = Check Success (Added txInFromWallet)

unbalancedInsOutsShouldNotChange :: BalanceInfo -> Check InsOutsChanged
unbalancedInsOutsShouldNotChange BalanceInfo {..}
  | insNotChanged && outsNotChanged =
    Check Success NotChanged
  | otherwise =
    Check Fail Changed
  where
    (unbIns, unbOuts) = unbalancedInsOuts
    (bcdIns, bcdOuts) = balancedInsOuts
    insNotChanged = all (`Set.member` bcdIns) unbIns
    outsNotChanged = case (unbOuts, bcdOuts) of
      (InvalidTx, InvalidTx) -> True
      (ValidTx unbOuts', ValidTx bcdOuts') -> all (`L.elem` bcdOuts') unbOuts'
      _ -> False
