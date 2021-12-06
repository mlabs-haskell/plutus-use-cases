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
import Mlabs.Roundtrip.RountripP2W qualified as Mlabs.Contracts
import Mlabs.Roundtrip.RoundtripSpending qualified as Mlabs.Contracts


data DemoContracts 
  = Roundtrip Mlabs.Contracts.ContractArgs
  | LockSpend Mlabs.Contracts.SContractArgs
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
    getDefinitions = [LogPKH, Roundtrip someArgs]
      where someArgs = Mlabs.Contracts.ContractArgs "addr_test1qq7e0hr837nwr799y7gk7nzs4gq603c6lx2lufvjt5jyqrskgm69l6d3fjd3lkp0knc97t8rsk9r35jrg88kc0m9sj3q52xml6"

    getSchema = \case
      LogPKH -> Builtin.endpointsToSchemas @Builtin.Empty
      Roundtrip _ -> Builtin.endpointsToSchemas @Mlabs.Contracts.DemoSchema
      LockSpend _ -> Builtin.endpointsToSchemas @Mlabs.Contracts.LockSpendSchema

    getContract = \case
      LogPKH -> SomeBuiltin Mlabs.Contracts.getPKH
      Roundtrip args -> SomeBuiltin (Mlabs.Contracts.runDemo args)
      LockSpend args -> SomeBuiltin (Mlabs.Contracts.lockSpendEndpoints args)