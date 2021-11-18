module Mlabs.DemoPAB.DemoContract (
    demoEndpoints
  , demoParamEndpoints
  , DemoSchema
) where


import Data.Aeson (FromJSON, ToJSON)
import Data.Map qualified as Map
import GHC.Generics (Generic)
import Ledger (PubKeyHash(..), pubKeyHashAddress)
import Ledger.Ada qualified as Ada
import Ledger.Constraints qualified as Constraints
import Ledger.Scripts (Datum (..), Redeemer (..), unitRedeemer)
import Ledger.Tx (getCardanoTxId)
import Ledger.Typed.Scripts as Scripts
import Plutus.Contract
import Plutus.Contracts.PubKey qualified as PubKey
import Prelude as Haskell
import Mlabs.Plutus.Contract (selectForever)
import Schema (ToSchema)

import Control.Lens

data SendParams = SendParams 
  { lovelaceAmount :: Integer
  }
  deriving stock (Haskell.Show, Generic, Haskell.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

type DemoSchema 
  = Endpoint "debug-log" ()
  .\/  Endpoint "send-to" SendParams

data IError =
    PKError PubKey.PubKeyError
    | CError ContractError
    deriving stock (Eq, Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''IError
instance AsContractError IError where
    _ContractError = _CError

type DemoContract a = Contract () DemoSchema IError a


demoEndpoints :: DemoContract ()
demoEndpoints = 
  selectForever 
    [ endpoint @"debug-log" (\_ -> debugLog)
    ]

debugLog :: DemoContract ()
debugLog = runError contr >>= \case
    Left err -> logWarn @Haskell.String (show err)
    Right () -> pure ()
  where 
    contr = do
      pkh <- mapError CError ownPubKeyHash
      logInfo @Haskell.String "Own PKH"
      logInfo @Haskell.String (show pkh)
      utxos <- utxosAt (pubKeyHashAddress pkh)
      logInfo @Haskell.String "UTXOs"
      logInfo @Haskell.String (show utxos)




demoParamEndpoints :: PubKeyHash -> DemoContract ()
demoParamEndpoints pkh = 
  selectForever 
    [ endpoint @"send-to" (sendTo pkh) 
    ]

sendTo :: PubKeyHash -> SendParams -> DemoContract ()
sendTo toPkh sendParams = runError (run' toPkh sendParams) >>= \case
    Left err -> logWarn @Haskell.String (show err)
    Right () -> pure ()

run' :: PubKeyHash -> SendParams -> DemoContract ()
run' toPkh sendParams = do
    logInfo @Haskell.String "Running PAB demo contract"
    pkh <- mapError CError ownPubKeyHash
    logInfo @Haskell.String "Locking funds at PubKeyContract script address"
    let payValue = Ada.lovelaceValueOf  $ lovelaceAmount sendParams
    (txOutRef, ciTxOut, pkInst) <- mapError PKError (PubKey.pubKeyContract pkh payValue)
    logInfo @Haskell.String "UTxO locked:"
    logInfo txOutRef
    let lookups = mconcat 
            [ Constraints.otherData (Datum $ getRedeemer unitRedeemer)
            , Constraints.unspentOutputs (maybe mempty (Map.singleton txOutRef) ciTxOut)
            , Constraints.otherScript (Scripts.validatorScript pkInst)
            ]
        constraints = mconcat 
            [ Constraints.mustSpendScriptOutput txOutRef unitRedeemer
            , Constraints.mustBeSignedBy pkh
            , Constraints.mustPayToPubKey toPkh payValue
            ]
    logInfo @Haskell.String
      $ "Starting second transaction: spending utxo from script and paying to wallet " <> (show toPkh)
    result <- runError @_ @_ @ContractError $ submitTxConstraintsWith @Scripts.Any lookups constraints
    case result of
        Left err -> do
            logWarn @Haskell.String "PayToPubKey transaction failed"
            logWarn err
        Right redeemingTx -> do
            let txi = getCardanoTxId redeemingTx
            logInfo @Haskell.String $ "Waiting for tx " <> show txi <> " to complete"
            mapError CError $ awaitTxConfirmed txi
            logInfo @Haskell.String "Tx confirmed."

