{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:debug-context #-}

module Mlabs.IntegrationTest.PabWbe.TestContracts.Balance (
    SimpleSchema
  , endpoints
) where

import           Control.Lens
import           Control.Monad                       (forever)
import           PlutusTx.Prelude                    hiding (Applicative (..), check)


import           Data.Aeson                          (FromJSON, ToJSON, decode)
import qualified Data.Map                            as Map
import           Data.Text                           (Text, pack, unpack)
import           GHC.Generics                        (Generic)
import qualified Ledger.Ada                          as Ada
import qualified Ledger.Constraints                  as Constraints
import           Ledger.Constraints.OffChain         (mkTx)
import           Ledger.Scripts                      (Datum (..), Redeemer (..), unitRedeemer)
import           Ledger.Tx                           (getCardanoTxId)
import           Ledger.Typed.Scripts                as Scripts
import           Plutus.Contract

import qualified Data.OpenApi.Schema                 as OpenApi
import           Data.Text.Prettyprint.Doc           (Pretty, pretty, viaShow)
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, BuiltinHandler (..), HasDefinitions (..),
                                                      SomeBuiltin (..))
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import           Plutus.PAB.Run.PSGenerator          (HasPSTypes (..))
-- import           Data.Row
import           Language.PureScript.Bridge          (equal, genericShow, mkSumType)

import           Plutus.Contract.Request             as R

import           Data.ByteString.Lazy.Char8          (ByteString)
import           Data.Void                           (Void (..))
import           Ledger                              (PubKeyHash)
import qualified Plutus.Contracts.PubKey             as PubKey
import           Plutus.V1.Ledger.Ada                (adaValueOf)
import qualified Prelude                             as Hask

type BalanceSchema =
  Endpoint "run-balance" ()


endpoints :: Contract () SimpleSchema Text ()
endpoints = forever . selectList $
  [ runBalance
  ]

runBalance :: Promise () SimpleSchema Text ()
runBalance = endpoint @"run-balance" $ \() -> do
  ownPkh <- ownPubKeyHash
  logInfo @Hask.String $ "Running balance with wallet PKH " ++ (Hask.show ownPkh)
  
  -- (flip handleError)
  --   getOwnPkh
  --   (\(e :: PubKey.PubKeyError) -> logInfo @Haskell.String $ "Failed to get own PKH")
  -- let to_pkh = decodePkh
  --         "{\"getPubKeyHash\" : \"d19278d36a31eec98aca5d1cc226fcf5aee6451bb9d0123bb60c1b5b\"}"
  --     txC = Constraints.mustPayToPubKey to_pkh (adaValueOf 5)
  --     etx = mkTx @Void Haskell.mempty txC
  -- case etx of
  --   Left e -> logInfo @Haskell.String (Haskell.show e)
  --   Right unbTx -> do
  --     logInfo @Haskell.String (Haskell.show unbTx)
  --     bcd <- R.balanceTx unbTx
  --     logInfo @Haskell.String (Haskell.show bcd)
  --     submitted <- submitBalancedTx bcd
  --     logInfo @Haskell.String (Haskell.show submitted)
  -- where
  --   getOwnPkh = do
  --     pkh <- ownPubKeyHash
  --     logInfo @Haskell.String $ "Own PKH: " <> Haskell.show pkh


-- decodePkh :: ByteString -> PubKeyHash
-- decodePkh = fromMaybe theImpossible . decode
--   where
--     theImpossible = Haskell.error "The impossible happened: failed to decode PKH"
