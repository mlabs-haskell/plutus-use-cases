module Lendex.API where

import Prelude
import Data.Argonaut as A
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Console (log)
import Effect.Class (liftEffect)

import PAB.Api (PABConnectionInfo, callEndpoint)
import PAB.Types (ContractInstanceId)

instanceId :: ContractInstanceId
instanceId = { unContractInstanceId: "ba60d693-c038-4557-bcc5-8096a44fce72" }

connectionInfo :: PABConnectionInfo
connectionInfo = {
    baseURL: "http://localhost:8080"
}

type Deposit = 
    { deposit'amount :: Int
  , deposit'asset :: Maybe Int
  }

deposit :: PABConnectionInfo 
  -> ContractInstanceId 
  -> Deposit
  -> Aff Unit
deposit ci cii deposit = do 
    json <- callEndpoint ci cii "deposit" deposit
    liftEffect $ log $ A.stringify json
    pure unit

testDeposit :: Deposit
testDeposit = {
    deposit'amount: 10000
  , deposit'asset: Just $ 500000
}

testDeposit_ :: Effect Unit
testDeposit_ = runAff_ (log <<< show) $ deposit connectionInfo instanceId testDeposit