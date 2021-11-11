module Mlabs.IntegrationTest.Types (
  TestError (..),
  BalanceInfo (..),
  SignInfo (..),
  TxBodyContent,
) where

import Cardano.Api qualified as C

import Control.Exception (Exception)

import Data.Set (Set)
import Data.Yaml (ParseException)

import GHC.Generics (Generic)

import Ledger (TxIn, Value)
import Ledger.Constraints (MkTxError)
import Ledger.Tx.CardanoAPI (ToCardanoError)

import Network.HTTP.Req qualified as Req

import Plutus.ChainIndex (
  ChainIndexTxOutputs (..),
 )

import Prelude

import Prettyprinter (pretty)

type TxBodyContent = C.TxBodyContent C.ViewTx C.AlonzoEra

data TestError
  = HttpError Req.HttpException
  | DecoderError String
  | YamlError ParseException
  | ConfigurationError String
  | CardanoError ToCardanoError
  | ConversionError String
  | TxError MkTxError
  | -- HACK these errors come from @queryNodeLocalState@ and friends
    -- Should find a better way to represent them
    NodeError String
  deriving stock (Generic)

instance Show TestError where
  show = \case
    HttpError err -> show err
    DecoderError err -> err
    YamlError err -> show err
    ConfigurationError err -> err
    CardanoError err -> show $ pretty err
    ConversionError err -> show err
    TxError err -> show $ pretty err
    NodeError err -> err

instance Exception TestError

data BalanceInfo = BalanceInfo
  { txInFromWallet :: Set TxIn
  , fee :: Maybe Value
  , totalOutsValue :: Value
  , totalInsValue :: Value
  , unbalancedInsOuts :: (Set TxIn, ChainIndexTxOutputs)
  , balancedInsOuts :: (Set TxIn, ChainIndexTxOutputs)
  }
  deriving stock (Show, Eq, Generic)

data SignInfo = SignInfo
  { balancedWitnesses :: [C.KeyWitness C.AlonzoEra]
  , signedWitnesses :: [C.KeyWitness C.AlonzoEra]
  , witnessDiff :: [C.KeyWitness C.AlonzoEra]
  }
  deriving stock (Show, Eq, Generic)
