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

import Mlabs.NFT.Api qualified as Contract.NFT

import Plutus.V1.Ledger.Value (CurrencySymbol (..))
import Mlabs.DemoPAB.DemoContract qualified as DemoContract
import Ledger (PubKeyHash(..))


data DemoPABContracts
  = PayToContract PubKeyHash
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
    ]
    where
      somePkh = PubKeyHash "ff"

  getContract = \case
    PayToContract pkh ->
      SomeBuiltin $ DemoContract.demoEndpoints pkh

  getSchema = \case
    PayToContract _ -> Builtin.endpointsToSchemas @DemoContract.DemoSchema