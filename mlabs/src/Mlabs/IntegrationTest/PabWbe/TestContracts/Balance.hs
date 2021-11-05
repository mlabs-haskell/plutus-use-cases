{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:debug-context #-}

module Mlabs.IntegrationTest.PabWbe.TestContracts.Balance (
  BalanceSchema,
  endpoints,
) where

import Control.Monad (forever)

import Data.Text (Text)

import Plutus.Contract

import PlutusTx.Prelude hiding (Applicative (..), check)

import Prelude qualified as Hask

type BalanceSchema = Endpoint "run-balance" ()

endpoints :: Contract () BalanceSchema Text ()
endpoints =
  forever . selectList $
    [ runBalance
    ]

runBalance :: Promise () BalanceSchema Text ()
runBalance = endpoint @"run-balance" $ \() -> do
  ownPkh <- ownPubKeyHash
  logInfo @Hask.String $ "Running balance with wallet PKH " ++ Hask.show ownPkh
  
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
