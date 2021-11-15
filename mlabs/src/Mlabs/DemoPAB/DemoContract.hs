module Mlabs.DemoPAB.DemoContract (
  demoEndpoints
  , DemoSchema
) where


import Data.Aeson (FromJSON, ToJSON)
import Data.Map qualified as Map
import GHC.Generics (Generic)
import Ledger (PubKeyHash(..))
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
  { adaAmount :: Integer
  }
  deriving stock (Haskell.Show, Generic, Haskell.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

type DemoSchema 
  = Endpoint "send-to" SendParams

data IError =
    PKError PubKey.PubKeyError
    | CError ContractError
    deriving stock (Eq, Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''IError
instance AsContractError IError where
    _ContractError = _CError


type DemoContract a = Contract () DemoSchema IError a






demoEndpoints :: PubKeyHash -> DemoContract ()
demoEndpoints pkh = 
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
    let payValue = Ada.lovelaceValueOf  $ adaAmount sendParams
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
    result <- runError @_ @_ @ContractError $ submitTxConstraintsWith @Scripts.Any lookups constraints
    case result of
        Left err -> do
            logWarn @Haskell.String "PayToPubKey transaction failed"
            logWarn err
        Right redeemingTx -> do
            let txi = getCardanoTxId redeemingTx
            logInfo @Haskell.String $ "Waiting for tx " <> show txi <> " to complete"
            mapError CError $ awaitTxConfirmed txi
            logInfo @Haskell.String "Tx confirmed. Integration test complete."
