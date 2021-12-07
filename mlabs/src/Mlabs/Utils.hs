module Mlabs.Utils (
  submitTxConstraintsWithUnbalanced,
) where

import Data.Kind (Type)
import Data.Row (Row)

import Ledger (CardanoTx)
import Ledger.Constraints (ScriptLookups, TxConstraints)
import Ledger.Constraints qualified as Constraints
import Ledger.Typed.Scripts (RedeemerType, ValidatorTypes (DatumType))

import Plutus.Contract (Contract)
import Plutus.Contract qualified as Contract
import PlutusTx qualified

import PlutusTx.Prelude

submitTxConstraintsWithUnbalanced ::
  forall a w (s :: Row Type) e.
  ( PlutusTx.ToData (RedeemerType a)
  , PlutusTx.FromData (DatumType a)
  , PlutusTx.ToData (DatumType a)
  , Contract.AsContractError e
  ) =>
  ScriptLookups a ->
  TxConstraints (RedeemerType a) (DatumType a) ->
  Contract w s e CardanoTx
submitTxConstraintsWithUnbalanced lookups tx =
  Contract.mkTxConstraints @a lookups tx
    >>= Contract.submitUnbalancedTx . Constraints.adjustUnbalancedTx
