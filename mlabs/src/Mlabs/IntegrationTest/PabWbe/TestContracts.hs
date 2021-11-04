module Mlabs.IntegrationTest.PabWbe.TestContracts (
  
) where


{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

import PlutusTx.Prelude
import Prelude qualified as Hask

import           Control.Monad.Freer
import           Data.Aeson                                (FromJSON, ToJSON)
import           Data.Default                              (Default (def))
import           Data.Text.Prettyprint.Doc
import           GHC.Generics                              (Generic)

import           Data.Data                                 (Proxy (Proxy))
import qualified Data.OpenApi.Schema                       as OpenApi
import           Data.Row
import           Language.PureScript.Bridge                (equal, genericShow, mkSumType)
import           Language.PureScript.Bridge.TypeParameters (A)
import           Ledger                                    (TxId)
import           Playground.Types                          (FunctionSchema)
import           Plutus.PAB.Effects.Contract.Builtin       (Builtin, BuiltinHandler (..), HasDefinitions (..),
                                                            SomeBuiltin (..))
import qualified Plutus.PAB.Effects.Contract.Builtin       as Builtin
import           Plutus.PAB.Run.PSGenerator                (HasPSTypes (..))
import           Plutus.PAB.Simulator                      (SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator                      as Simulator
import qualified SandboxContracts.SimpleContract           as Contracts.Simple
import           Schema                                    (FormSchema)

import Mlabs.IntegrationTest.PabWbe.TestContracts.Balance qualified as Contract.Balance


data TestContracts 
  = ToJsonFix Int -- FIXME: added to make correct aeson instance
  | BalanceContract
  deriving stock (Hask.Eq, Hask.Ord, Hask.Show, Generic)
  deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema)

instance Pretty SandboxContracts where
  pretty = viaShow

instance HasPSTypes SandboxContracts where
  psTypes p =
    [ (equal <*> (genericShow <*> mkSumType)) p
    ]

instance HasDefinitions SandboxContracts where
  getDefinitions = [ BalanceContract
                   ]

  getContract = \case
    BalanceContract -> SomeBuiltin Contract.Balance.endpoints

  getSchema = \case
    BalanceContract -> Builtin.endpointsToSchemas @Contract.Balance.BalanceSchema
