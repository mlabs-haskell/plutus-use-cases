module Mlabs.NFTBuySetPrice.PAB (
  BuySetContract,
  runBuySetMarketplace,
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

import Mlabs.NFTBuySetPrice.Api
import Mlabs.NFT.Contract.Init (uniqueTokenName)
import Mlabs.NFT.Types (UniqueToken)

import Plutus.Contracts.Currency ()
import Plutus.V1.Ledger.Value (CurrencySymbol (..), TokenName (..), assetClass)

import Plutus.PAB.Effects.Contract.Builtin qualified as Builtin
import Plutus.PAB.Run (runWith)


-- | Start PAB for NFT contract
runBuySetMarketplace :: Hask.IO ()
runBuySetMarketplace = runWith (Builtin.handleBuiltin @BuySetContract)


{- | Contracts available through PAB.
 For concrete endpoints see `getContract`
-}
data BuySetContract
  = BuySetContract UniqueToken
  deriving stock (Hask.Eq, Hask.Ord, Hask.Show, Generic)
  deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema)

instance Pretty BuySetContract where
  pretty = viaShow

instance HasPSTypes BuySetContract where
  psTypes =
    [ equal . genericShow . argonaut $ mkSumType @BuySetContract
    ]

instance HasDefinitions BuySetContract where
  getDefinitions =
    [ BuySetContract exampleUT
    ]
    where
      exampleUT = assetClass (CurrencySymbol "ff") (TokenName uniqueTokenName)

  getContract = \case
    BuySetContract uT -> SomeBuiltin $ buySetEndpoints uT

  getSchema = \case
    BuySetContract _ -> Builtin.endpointsToSchemas @NFTBySetPriceSchema
