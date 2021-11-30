module Mlabs.Roundtrip.PAB.Contracts (
  DemoContracts
) where

import Prelude
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.OpenApi.Schema qualified as OpenApi
import Prettyprinter

import Plutus.PAB.Effects.Contract.Builtin (Builtin, BuiltinHandler (..), HasDefinitions (..), SomeBuiltin (..))
import Plutus.PAB.Effects.Contract.Builtin qualified as Builtin
import Plutus.PAB.Run.PSGenerator (HasPSTypes (..))
import Ledger (PubKeyHash(..))
import Language.PureScript.Bridge (argonaut, equal, genericShow, mkSumType, order)

import Mlabs.Roundtrip.PKH qualified as Mlabs.Contracts
import Mlabs.Roundtrip.DemoContract qualified as Mlabs.Contracts


data DemoContracts 
  = Roundtrip Mlabs.Contracts.ContractArgs
  | LogPKH
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema)


instance Pretty DemoContracts where
    pretty = viaShow

instance HasPSTypes DemoContracts where
    psTypes =
        [ equal . genericShow . argonaut $ mkSumType @DemoContracts
        ]

instance HasDefinitions DemoContracts where
    getDefinitions = [Roundtrip someArgs]
      where someArgs = Mlabs.Contracts.ContractArgs (PubKeyHash "ff")

    getSchema = \case
      LogPKH -> Builtin.endpointsToSchemas @Builtin.Empty
      Roundtrip _ -> Builtin.endpointsToSchemas @Mlabs.Contracts.DemoSchema

    getContract = \case
      LogPKH -> SomeBuiltin Mlabs.Contracts.getPKH
      Roundtrip args -> SomeBuiltin (Mlabs.Contracts.runDemo args)