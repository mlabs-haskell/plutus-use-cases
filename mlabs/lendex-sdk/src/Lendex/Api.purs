module Lendex.API where

import Prelude
import Data.Argonaut as A
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Console (log)
import Effect.Class (liftEffect)

import PAB.Api (PABConnectionInfo, callEndpoint)
import PAB.Types (ContractInstanceId)

instanceId :: ContractInstanceId
instanceId = { unContractInstanceId: "2dbc6f8f-cd84-48dd-be65-a0220c9d8844" }

connectionInfo :: PABConnectionInfo
connectionInfo = {
    baseURL: "http://localhost:8080"
}

type UnCurrencySymbol = {
  unCurrencySymbol :: String
}

type UnTokenName = {
  unTokenName :: String
}

type UnAssetClass = Tuple UnCurrencySymbol UnTokenName

type AssetClass = {
  unAssetClass :: UnAssetClass
}

type Deposit = 
    { deposit'amount :: Int
  , deposit'asset :: AssetClass
  }

deposit :: PABConnectionInfo 
  -> ContractInstanceId 
  -> Deposit
  -> Aff Unit
deposit ci cii depositValue = do 
    json <- callEndpoint ci cii "deposit" depositValue
    liftEffect $ log $ A.stringify json
    pure unit

testDeposit :: Deposit
testDeposit = {
    deposit'amount: 200
  , deposit'asset: {
    unAssetClass: (Tuple {unCurrencySymbol: "d57ff37f80a4f7ba1e62fcbb12a7d5f04556e2e90ec6ce4faea3c27d"} { unTokenName: "Euro"})
  }
}

testDeposit_ :: Effect Unit
testDeposit_ = runAff_ (log <<< show) $ deposit connectionInfo instanceId testDeposit


type Borrow = {
  borrow'amount :: Int
  , borrow'asset :: AssetClass
  , borrow'rate  :: Int
}

borrow :: PABConnectionInfo 
  -> ContractInstanceId 
  -> Borrow
  -> Aff Unit
borrow ci cii borrowValue = do 
    json <- callEndpoint ci cii "borrow" borrowValue
    liftEffect $ log $ A.stringify json
    pure unit

testBorrow :: Borrow
testBorrow = {
   borrow'amount: 30
  , borrow'asset:  {
    unAssetClass: (Tuple {unCurrencySymbol: "d57ff37f80a4f7ba1e62fcbb12a7d5f04556e2e90ec6ce4faea3c27d"} { unTokenName: "Euro"})
  }
  , borrow'rate: 0
}

testBorrow_ :: Effect Unit
testBorrow_ = runAff_ (log <<< show) $ borrow connectionInfo instanceId testBorrow


type Repay = { 
  repay'amount :: Int
  , repay'asset :: AssetClass
  , repay'rate :: Int
  }

repay :: PABConnectionInfo 
  -> ContractInstanceId 
  -> Repay
  -> Aff Unit
repay ci cii repayValue = do 
    json <- callEndpoint ci cii "repay" repayValue
    liftEffect $ log $ A.stringify json
    pure unit

testRepay :: Repay
testRepay = {
   repay'amount: 50
  , repay'asset: {
    unAssetClass: (Tuple {unCurrencySymbol: "d57ff37f80a4f7ba1e62fcbb12a7d5f04556e2e90ec6ce4faea3c27d"} { unTokenName: "Euro"})
  }
  , repay'rate: 0
}

testRepay_ :: Effect Unit
testRepay_ = runAff_ (log <<< show) $ repay connectionInfo instanceId testRepay

type SwapBorrowRateModel = { 
  swapRate'asset :: AssetClass
  , swapRate'rate :: Int
}

swapBorrowRateModel :: PABConnectionInfo 
  -> ContractInstanceId 
  -> SwapBorrowRateModel
  -> Aff Unit
swapBorrowRateModel ci cii swapBorrowRateModelValue = do 
    json <- callEndpoint ci cii "swap-borrow-rate-model" swapBorrowRateModelValue
    liftEffect $ log $ A.stringify json
    pure unit

testSwapBorrowRateModel :: SwapBorrowRateModel
testSwapBorrowRateModel = {
   swapRate'asset:  {
    unAssetClass: (Tuple {unCurrencySymbol: "d57ff37f80a4f7ba1e62fcbb12a7d5f04556e2e90ec6ce4faea3c27d"} { unTokenName: "Euro"})
  }
  , swapRate'rate: 0
}

testSwapBorrowRateModel_ :: Effect Unit
testSwapBorrowRateModel_ = runAff_ (log <<< show) $ swapBorrowRateModel connectionInfo instanceId testSwapBorrowRateModel

type Withdraw = { 
  withdraw'amount :: Int
  , withdraw'asset :: AssetClass
}

withdraw :: PABConnectionInfo 
  -> ContractInstanceId 
  -> Withdraw
  -> Aff Unit
withdraw ci cii withdrawValue = do 
    json <- callEndpoint ci cii "withdraw" withdrawValue
    liftEffect $ log $ A.stringify json
    pure unit

testwithdraw :: Withdraw
testwithdraw = { 
   withdraw'asset : {
    unAssetClass: (Tuple {unCurrencySymbol: "d57ff37f80a4f7ba1e62fcbb12a7d5f04556e2e90ec6ce4faea3c27d"} { unTokenName: "Euro"})
  },
   withdraw'amount: 25
}

testwithdraw_ :: Effect Unit
testwithdraw_ = runAff_ (log <<< show) $ withdraw connectionInfo instanceId testwithdraw


type LiquidationCall =   { 
  liquidationCall'collateral :: AssetClass 
  , liquidationCall'debtUser :: {getPubKeyHash :: String}
  , liquidationCall'debtAsset :: AssetClass
  , liquidationCall'debtToCover :: Int
  , liquidationCall'receiveAToken  :: Boolean
  }


liquidationCall :: PABConnectionInfo 
  -> ContractInstanceId 
  -> LiquidationCall
  -> Aff Unit
liquidationCall ci cii liquidationCallValue = do 
    json <- callEndpoint ci cii "liquidation-call" liquidationCallValue
    liftEffect $ log $ A.stringify json
    pure unit

testLiquidationCall :: LiquidationCall
testLiquidationCall = {
  liquidationCall'collateral:  {
    unAssetClass: (Tuple {unCurrencySymbol: "d57ff37f80a4f7ba1e62fcbb12a7d5f04556e2e90ec6ce4faea3c27d"} { unTokenName: "Euro"})
  }
  , liquidationCall'debtUser: {
    getPubKeyHash: "Abcd"
  }
  , liquidationCall'debtAsset:  {
    unAssetClass: (Tuple {unCurrencySymbol: ""} { unTokenName: ""})
  }
  , liquidationCall'debtToCover: 10
  , liquidationCall'receiveAToken: false
}

testLiquidationCall_ :: Effect Unit
testLiquidationCall_ = runAff_ (log <<< show) $ liquidationCall connectionInfo instanceId testLiquidationCall


type AddCollateral =   { 
  addCollateral'asset :: AssetClass
  , addCollateral'amount :: Int
  }

addCollateral :: PABConnectionInfo 
  -> ContractInstanceId 
  -> AddCollateral
  -> Aff Unit
addCollateral ci cii addCollateralValue = do 
    json <- callEndpoint ci cii "add-collateral" addCollateralValue
    liftEffect $ log $ A.stringify json
    pure unit

testAddCollateral :: AddCollateral
testAddCollateral = {
  addCollateral'asset:  {
    unAssetClass: (Tuple {unCurrencySymbol: "d57ff37f80a4f7ba1e62fcbb12a7d5f04556e2e90ec6ce4faea3c27d"} { unTokenName: "Euro"})
  }
  , addCollateral'amount: 50
}

testAddCollateral_ :: Effect Unit
testAddCollateral_ = runAff_ (log <<< show) $ addCollateral connectionInfo instanceId testAddCollateral


type RemoveCollateral =   { 
  removeCollateral'asset :: AssetClass
  , removeCollateral'amount :: Int
  }

removeCollateral :: PABConnectionInfo 
  -> ContractInstanceId 
  -> RemoveCollateral
  -> Aff Unit
removeCollateral ci cii removeCollateralValue = do 
    json <- callEndpoint ci cii "remove-collateral" removeCollateralValue
    liftEffect $ log $ A.stringify json
    pure unit

testRemoveCollateral :: RemoveCollateral
testRemoveCollateral = {
  removeCollateral'asset:  {
    unAssetClass: (Tuple {unCurrencySymbol: "d57ff37f80a4f7ba1e62fcbb12a7d5f04556e2e90ec6ce4faea3c27d"} { unTokenName: "Euro"})
  }
  , removeCollateral'amount: 50
}

testRemoveCollateral_ :: Effect Unit
testRemoveCollateral_ = runAff_ (log <<< show) $ removeCollateral connectionInfo instanceId testRemoveCollateral
