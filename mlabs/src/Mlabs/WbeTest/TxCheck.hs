module Mlabs.WbeTest.TxCheck (BalanceResult(..), checkBalanced, _checkBalanced) where

import Prelude qualified as Hask
import PlutusTx.Prelude

import Data.Set (Set)
import Data.Set qualified as Set
import Data.Function (on)
import Control.Lens ((^.))

import Ledger (Tx, TxIn)
import Plutus.ChainIndex (ChainIndexTx, citxInputs)
import Plutus.Contract.Wallet (ExportTx (..))

import Mlabs.WbeTest.TxRead
import Mlabs.WbeTest.Types

data BalanceResult = BalanceCheck
  { addedInputs :: Set TxIn
  }
  deriving stock (Hask.Show)

checkBalanced :: WbeExportTx -> WbeTx Balanced -> Either Hask.String BalanceResult
checkBalanced (WbeExportTx (ExportTx apiTx _ _)) wtx = do
  initialTx <- toChainIndexTx apiTx
  balancedTx <- parseTx wtx
  return $ _checkBalanced initialTx balancedTx

_checkBalanced :: ChainIndexTx -> ChainIndexTx -> BalanceResult
_checkBalanced initial balanced = 
  BalanceCheck $ (Set.difference `on` (^. citxInputs)) balanced initial 