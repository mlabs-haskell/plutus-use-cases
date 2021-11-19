module Mlabs.IntegrationTest.PabWbe.Pab (
  TestContracts (..),
) where

import PlutusTx.Prelude

import Prelude qualified as Hask

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi.Schema qualified as Schema
import Data.Row (Empty)

import GHC.Generics (Generic)

import Ledger (PubKeyHash(..))

import Language.PureScript.Bridge (
  argonaut,
  equal,
  genericShow,
  mkSumType,
 )

import Mlabs.IntegrationTest.PabWbe.Contracts.IntegrationTest qualified as IntegrationTest
import Mlabs.IntegrationTest.PabWbe.Contracts.Mint (Mint, MintSchema)
import Mlabs.IntegrationTest.PabWbe.Contracts.Mint qualified as Mint
import Mlabs.IntegrationTest.PabWbe.Contracts.Payment (Payment, PaymentSchema)
import Mlabs.IntegrationTest.PabWbe.Contracts.Payment qualified as Payment

import Plutus.PAB.Effects.Contract.Builtin (
  HasDefinitions (..),
  SomeBuiltin (..),
 )
import Plutus.PAB.Effects.Contract.Builtin qualified as Builtin
import Plutus.PAB.Run.PSGenerator (HasPSTypes (..))

import Prettyprinter (Pretty (pretty), viaShow)

data TestContracts
  = BalanceAndSignContract
  | PaymentContract Payment
  | MintContract Mint
  deriving stock (Hask.Eq, Hask.Ord, Hask.Show, Generic)
  deriving anyclass (FromJSON, ToJSON, Schema.ToSchema)

instance Pretty TestContracts where
  pretty = viaShow

instance HasPSTypes TestContracts where
  psTypes =
    [ equal . genericShow . argonaut $ mkSumType @TestContracts
    ]

instance HasDefinitions TestContracts where
  getDefinitions = 
     [ BalanceAndSignContract
     , MintContract someMint
     ]
     where
       someMint = Mint.Mint (PubKeyHash "ff") ("testToken", 10)

  getContract = \case
    BalanceAndSignContract -> SomeBuiltin IntegrationTest.runTests
    PaymentContract payment -> SomeBuiltin $ Payment.payFromTo payment
    MintContract mint -> SomeBuiltin $ Mint.runMintTest mint

  getSchema = \case
    BalanceAndSignContract -> Builtin.endpointsToSchemas @Empty
    PaymentContract {} -> Builtin.endpointsToSchemas @PaymentSchema
    MintContract {} -> Builtin.endpointsToSchemas @MintSchema
