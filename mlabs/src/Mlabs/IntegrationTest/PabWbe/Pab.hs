module Mlabs.IntegrationTest.PabWbe.Pab (
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
import Data.Row (Empty)

import GHC.Generics (Generic)

import Language.PureScript.Bridge (argonaut, equal, genericShow, mkSumType)

import Mlabs.IntegrationTest.PabWbe.TestStand qualified as TestStand

import Plutus.PAB.Effects.Contract.Builtin (
  HasDefinitions (..),
  SomeBuiltin (..),
 )
import Plutus.PAB.Effects.Contract.Builtin qualified as Builtin
import Plutus.PAB.Run.PSGenerator (HasPSTypes (..))

import Prettyprinter (Pretty (pretty), viaShow)

data TestContracts = BalanceAndSignContract
  deriving stock (Hask.Eq, Hask.Ord, Hask.Show, Generic)
  deriving anyclass (ToJSON, OpenApi.ToSchema)

instance FromJSON TestContracts where
  parseJSON x =
    genericParseJSON
      defaultOptions {sumEncoding = TaggedObject "tag" $ Hask.show x}
      x

instance Pretty TestContracts where
  pretty = viaShow

instance HasPSTypes TestContracts where
  psTypes =
    [ equal . genericShow . argonaut $ mkSumType @TestContracts
    ]

instance HasDefinitions TestContracts where
  getDefinitions = [BalanceAndSignContract]

  getContract _ = SomeBuiltin TestStand.runTests

  getSchema _ = Builtin.endpointsToSchemas @Empty
