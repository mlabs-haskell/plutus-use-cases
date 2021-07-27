module SDK (activate, test_activate, getState, initMarket, module PAB.Types) where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Class (liftEffect)

import Effect.Aff as Aff
import Effect.Aff (Aff)
import Effect.Console (log)
import Affjax as AX
import Affjax.ResponseFormat as AJRF
import Affjax.RequestBody as AJRB

import Data.Argonaut as A
import Data.Time.Duration (Milliseconds(..))

import PAB.Types (ActiveContract, BaseURL, ContractInstanceClientState, ContractInstanceId, ContractInstanceRef, ContractPath, ContractState, HaskellUnit, Wallet, haskellUnit, lovelaceValueOf)

test_activate :: Effect Unit
test_activate = Aff.launchAff_ $ do
  ci <- activate
    "http://localhost:9090"
    { contractPath: "/home/emiflake/work/liqwid/liqwid-contracts/.stack-work/install/x86_64-linux/034ae249d66fca67c4ad1d9129c6b52cdc60074870ec8ce68526664f5d9dddd8/8.10.3/bin/liqwid-app" }
  let datum =
        { initMarketExchangeRate: [5, 1]
        , initMarketStateToken: { unTokenName: "qADAStateToken" }
        , initMarketAcceptedValueType:
            { marketValueTypeSymbol: { unCurrencySymbol: "" }
            , marketValueTypeTokenName: { unTokenName: ""}
            }
        , initMarketToken: { unTokenName: "qADA" }
        }
  liftEffect $ log $ "Waiting for activation"
  ci' <- initMarket ci datum
  _ <- Aff.delay (Milliseconds 5000.0)
  liftEffect $ log $ A.stringify $ A.encodeJson $ ci'.contractInstanceRef.csCurrentState

activate
  :: BaseURL
  -> ContractPath
  -> Aff ActiveContract
activate baseURL cp = do
  let url = baseURL <> "/api/contract/activate"
  result <- AX.post AJRF.json url $ Just $ AJRB.Json $ A.encodeJson $ cp
  case result of
    Left err -> liftEffect $ throw $ "Activate failed: " <> (AX.printError err)
    Right response -> do
      case A.decodeJson response.body of
        Left e -> liftEffect $ throw $ "Activate Response Parse failed: " <> A.printJsonDecodeError e
        Right contractInstanceRef -> do
          let activeContract
                = { contractInstanceRef
                  , baseURL
                  }
          pure activeContract

type CurrencySymbol = { unCurrencySymbol :: String }

type TokenName = { unTokenName :: String }
-- this probably needs to be BigInt, instead of Int
type Value = { getValue :: Array (Tuple CurrencySymbol (Array (Tuple TokenName Int))) }

type MarketAcceptedValueType =
  { marketValueTypeSymbol :: CurrencySymbol
  , marketValueTypeTokenName :: TokenName
  }

type InitMarketDatum =
  { initMarketExchangeRate :: Array Int
  , initMarketStateToken :: TokenName
  , initMarketAcceptedValueType :: MarketAcceptedValueType
  , initMarketToken :: TokenName
  }

getState :: ActiveContract -> Aff (Maybe ContractState)
getState contract = do
  let url = contract.baseURL <> "/api/contract/" <> contract.contractInstanceRef.csContract.unContractInstanceId <> "/status"
  liftEffect $ log $ "requesting url: " <> url
  result <- AX.post AJRF.json url $ Nothing
  case result of
    Left err ->  do
      liftEffect $ log $ "getState failed: " <> (AX.printError err)
      pure Nothing
    Right response -> do
      pure (Just response.body)

initMarket
  :: ActiveContract
  -> InitMarketDatum
  -> Aff ActiveContract
initMarket contract initMarketDatum = do
  let url = contract.baseURL <> "/api/contract/" <> contract.contractInstanceRef.csContract.unContractInstanceId <> "/endpoint/InitMarket"
  result <- AX.post AJRF.json url $ Just $ AJRB.Json $ A.encodeJson $ initMarketDatum
  case result of
    Left err -> do
      liftEffect $ throw $ "initMarket failed: " <> (AX.printError err)
    Right response -> do
      case A.decodeJson response.body of
        Right contractInstanceRef -> do
          let activeContract
                = { contractInstanceRef
                  , baseURL: contract.baseURL
                  }
          pure activeContract
        Left e ->
          liftEffect $ throw $ "Activate Response Parse failed: " <> A.printJsonDecodeError e