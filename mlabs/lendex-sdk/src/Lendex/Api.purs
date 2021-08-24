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
instanceId = { unContractInstanceId: "9fe7819c-c842-4fe9-8ef7-62b6ece8abba" }

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


type Borrow = {
  borrow'amount :: Int
  , borrow'asset :: Int
  , borrow'rate  :: Int
}

borrow :: PABConnectionInfo 
  -> ContractInstanceId 
  -> Borrow
  -> Aff Unit
borrow ci cii borrow = do 
    json <- callEndpoint ci cii "borrow" borrow
    liftEffect $ log $ A.stringify json
    pure unit

testBorrow :: Borrow
testBorrow = {
   borrow'amount: 200000
  , borrow'asset: 100000
  , borrow'rate: 2000000
}

testBorrow_ :: Effect Unit
testBorrow_ = runAff_ (log <<< show) $ borrow connectionInfo instanceId testBorrow


type Repay = { 
  repay'amount :: Int
  , repay'asset :: Int
  , repay'rate :: Int
  }


repay :: PABConnectionInfo 
  -> ContractInstanceId 
  -> Repay
  -> Aff Unit
repay ci cii repay = do 
    json <- callEndpoint ci cii "repay" repay
    liftEffect $ log $ A.stringify json
    pure unit

testRepay :: Repay
testRepay = {
   repay'amount: 200000
  , repay'asset: 100000
  , repay'rate: 2000000
}

testRepay_ :: Effect Unit
testRepay_ = runAff_ (log <<< show) $ repay connectionInfo instanceId testRepay

type SwapBorrowRateModel = { 
  swapRate'asset :: Int
  , swapRate'rate :: Int
}

swapBorrowRateModel :: PABConnectionInfo 
  -> ContractInstanceId 
  -> SwapBorrowRateModel
  -> Aff Unit
swapBorrowRateModel ci cii swapBorrowRateModel = do 
    json <- callEndpoint ci cii "swap-borrow-rate-model" swapBorrowRateModel
    liftEffect $ log $ A.stringify json
    pure unit

testSwapBorrowRateModel :: SwapBorrowRateModel
testSwapBorrowRateModel = {
   swapRate'asset: 200000
  , swapRate'rate: 100000
}

testSwapBorrowRateModel_ :: Effect Unit
testSwapBorrowRateModel_ = runAff_ (log <<< show) $ swapBorrowRateModel connectionInfo instanceId testSwapBorrowRateModel

type SetUserReserveAsCollateral = { 
  setCollateral'asset :: Int
  , setCollateral'useAsCollateral :: Int
  ,  setCollateral'portion :: Int
}

setUserReserveAsCollateral :: PABConnectionInfo 
  -> ContractInstanceId 
  -> SetUserReserveAsCollateral
  -> Aff Unit
setUserReserveAsCollateral ci cii setUserReserveAsCollateral = do 
    json <- callEndpoint ci cii "set-user-reserve-as-collateral" setUserReserveAsCollateral
    liftEffect $ log $ A.stringify json
    pure unit

testSetUserReserveAsCollateral :: SetUserReserveAsCollateral
testSetUserReserveAsCollateral = {
   setCollateral'asset : 200000
  , setCollateral'useAsCollateral: 100000
  , setCollateral'portion: 300000
}

testSetUserReserveAsCollateral_ :: Effect Unit
testSetUserReserveAsCollateral_ = runAff_ (log <<< show) $ setUserReserveAsCollateral connectionInfo instanceId testSetUserReserveAsCollateral

type Withdraw = { 
  withdraw'amount :: Int
  , withdraw'asset :: Int
}

withdraw :: PABConnectionInfo 
  -> ContractInstanceId 
  -> Withdraw
  -> Aff Unit
withdraw ci cii withdraw = do 
    json <- callEndpoint ci cii "withdraw" withdraw
    liftEffect $ log $ A.stringify json
    pure unit

testWithdraw :: Withdraw
testWithdraw = {
   withdraw'amount : 200000
  , withdraw'asset: 100000
}

testWithdraw_ :: Effect Unit
testWithdraw_ = runAff_ (log <<< show) $ withdraw connectionInfo instanceId testWithdraw

type LiquidationCall =   { 
  liquidationCall'collateral :: Int 
  , liquidationCall'debtUser :: Int
  , liquidationCall'debtAsset :: Int
  , liquidationCall'debtToCover :: Int
  , liquidationCall'receiveAToken  :: Int
  }


liquidationCall :: PABConnectionInfo 
  -> ContractInstanceId 
  -> LiquidationCall
  -> Aff Unit
liquidationCall ci cii liquidationCall = do 
    json <- callEndpoint ci cii "liquidation-call" liquidationCall
    liftEffect $ log $ A.stringify json
    pure unit

testLiquidationCall :: LiquidationCall
testLiquidationCall = {
  liquidationCall'collateral: 10000
  , liquidationCall'debtUser: 150000
  , liquidationCall'debtAsset: 1900000
  , liquidationCall'debtToCover: 1000000
  , liquidationCall'receiveAToken: 10000
}

testLiquidationCall_ :: Effect Unit
testLiquidationCall_ = runAff_ (log <<< show) $ liquidationCall connectionInfo instanceId testLiquidationCall