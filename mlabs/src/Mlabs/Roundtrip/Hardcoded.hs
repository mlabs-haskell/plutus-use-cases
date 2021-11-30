module Mlabs.Roundtrip.Hardcoded(
  unsafeProtocolParams
) where

import PlutusTx.Prelude
import Prelude qualified as Hask
import Data.Aeson (eitherDecodeFileStrict)
import Data.Either (fromRight)
import System.IO.Unsafe (unsafePerformIO)
import GHC.Stack

import Cardano.Api.Shelley (ProtocolParameters)

unsafeProtocolParams :: HasCallStack => ProtocolParameters
unsafeProtocolParams = unsafePerformIO $ do
  get Hask.<$> (eitherDecodeFileStrict "./local-cluster-mainnet-params.json")
  where
    get = fromRight (Hask.error "Could not parse protocol params")