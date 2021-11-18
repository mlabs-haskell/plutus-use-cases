module Mlabs.DemoPAB.PAB (
  DemoPABContracts
) where

import PlutusTx.Prelude
import Prelude qualified as Hask

import Data.Aeson (
  FromJSON,
  ToJSON,
 )
import Data.OpenApi.Schema qualified as OpenApi

import GHC.Generics (Generic)

import Prettyprinter (Pretty (..), viaShow)

import Language.PureScript.Bridge (argonaut, equal, genericShow, mkSumType)

import Plutus.PAB.Effects.Contract.Builtin (HasDefinitions (..), SomeBuiltin (..))
import Plutus.PAB.Effects.Contract.Builtin qualified as Builtin
import Plutus.PAB.Run.PSGenerator (HasPSTypes (..))


import Mlabs.DemoPAB.DemoContract qualified as DemoContract
import Ledger (PubKeyHash(..))

data DemoPABContracts
  = DebugLog
  | PayToContract PubKeyHash
  deriving stock (Hask.Eq, Hask.Ord, Hask.Show, Generic)
  deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema)

instance Pretty DemoPABContracts where
  pretty = viaShow

instance HasPSTypes DemoPABContracts where
  psTypes =
    [ equal . genericShow . argonaut $ mkSumType @DemoPABContracts
    ]

instance HasDefinitions DemoPABContracts where
  getDefinitions =
    [ PayToContract somePkh
    , DebugLog
    ]
    where
      somePkh = PubKeyHash "ff"

  getContract = \case
    PayToContract pkh ->
      SomeBuiltin $ DemoContract.demoParamEndpoints pkh
    DebugLog ->  SomeBuiltin $ DemoContract.demoEndpoints
    

  getSchema = \case
    PayToContract _ -> Builtin.endpointsToSchemas @DemoContract.DemoSchema
    DebugLog -> Builtin.endpointsToSchemas @DemoContract.DemoSchema
