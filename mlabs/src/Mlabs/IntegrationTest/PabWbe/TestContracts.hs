module Mlabs.IntegrationTest.PabWbe.TestContracts (
  TestContracts (..),
) where

import PlutusTx.Prelude
import Prelude qualified as Hask

import Data.Aeson (
  FromJSON (parseJSON),
  Options (sumEncoding),
  SumEncoding (TaggedObject),
  ToJSON,
  defaultOptions,
  genericParseJSON,
 )
import Data.OpenApi.Schema qualified as OpenApi
import Data.Text.Prettyprint.Doc (Pretty (pretty), viaShow)

import GHC.Generics (Generic)

import Language.PureScript.Bridge (equal, genericShow, mkSumType)

import Mlabs.IntegrationTest.PabWbe.TestContracts.Balance qualified as Contract.Balance

import Plutus.PAB.Effects.Contract.Builtin (HasDefinitions (..), SomeBuiltin (..))
import Plutus.PAB.Effects.Contract.Builtin qualified as Builtin
import Plutus.PAB.Run.PSGenerator (HasPSTypes (..))

data TestContracts = BalanceContract
  deriving stock (Hask.Eq, Hask.Ord, Hask.Show, Generic)
  deriving anyclass (ToJSON, OpenApi.ToSchema)

instance FromJSON TestContracts where
  parseJSON x =
    genericParseJSON
      defaultOptions {sumEncoding = TaggedObject "tag" (Hask.show x)}
      x

instance Pretty TestContracts where
  pretty = viaShow

instance HasPSTypes TestContracts where
  psTypes p =
    [ equal p (genericShow p $ mkSumType p)
    ]

instance HasDefinitions TestContracts where
  getDefinitions =
    [ BalanceContract
    ]

  getContract = \case
    BalanceContract -> SomeBuiltin Contract.Balance.endpoints

  getSchema = \case
    BalanceContract -> Builtin.endpointsToSchemas @Contract.Balance.BalanceSchema
