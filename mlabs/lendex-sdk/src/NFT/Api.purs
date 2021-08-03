module NFT.API where

import Prelude
import Data.Argonaut as A
import Data.Maybe
import Effect
import Effect.Aff (Aff, runAff_)
import Effect.Console (log)
import Effect.Class (liftEffect)

import PAB.Api (PABConnectionInfo, callEndpoint)
import PAB.Types (ContractInstanceId)

type Buy = 
  { buy'price     :: String
  , buy'newPrice  :: Maybe String
  }

buyNft :: PABConnectionInfo 
  -> ContractInstanceId 
  -> Buy
  -> Aff Unit
buyNft ci cii buy = do 
    json <- callEndpoint ci cii "Buy" buy
    liftEffect $ log $ A.stringify json
    pure unit

instanceId :: ContractInstanceId
instanceId = { unContractInstanceId: "44c6bb8c-67a3-4efa-97c0-c3d468cef7d7" }

connectionInfo :: PABConnectionInfo
connectionInfo = {
    baseURL: "localhost:8080"
}

testBuy :: Buy
testBuy = {
    buy'price: "1000",
    buy'newPrice: Just $ "5000"
}

testBuyNft :: Effect Unit
testBuyNft = runAff_ $ buyNft connectionInfo instanceId testBuy