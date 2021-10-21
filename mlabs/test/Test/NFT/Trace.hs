module Test.NFT.Trace where

import PlutusTx.Prelude
import Prelude qualified as Hask

import Data.Monoid (Last (..))
import Data.Text (Text)
import Data.Default (def)

import Control.Monad (void)
import Control.Monad.Freer.Extras.Log as Extra (logInfo)

import Plutus.Trace.Emulator (EmulatorTrace, activateContractWallet, callEndpoint, runEmulatorTraceIO)
import Plutus.Trace.Emulator qualified as Trace
import Wallet.Emulator qualified as Emulator
import Ledger.TimeSlot (slotToBeginPOSIXTime)

import Mlabs.Utils.Wallet (walletFromNumber)

import Mlabs.NFT.Contract
import Mlabs.NFT.Types

-- | Generic application Trace Handle.
type AppTraceHandle = Trace.ContractHandle (Last NftId) NFTAppSchema Text

-- | Emulator Trace 1. Mints Some NFT.
eTrace1 :: EmulatorTrace ()
eTrace1 = do
  let wallet1 = walletFromNumber 1 :: Emulator.Wallet
      wallet2 = walletFromNumber 2 :: Emulator.Wallet
  h1 :: AppTraceHandle <- activateContractWallet wallet1 endpoints
  h2 :: AppTraceHandle <- activateContractWallet wallet2 endpoints
  callEndpoint @"mint" h1 artwork
  -- callEndpoint @"mint" h2 artwork2

  void $ Trace.waitNSlots 1
  oState <- Trace.observableState h1
  nftId <- case getLast oState of
    Nothing -> Trace.throwError (Trace.GenericError "NftId not found")
    Just nid -> return nid
  void $ Trace.waitNSlots 1
  callEndpoint @"buy" h2 (buyParams nftId)

  logInfo @Hask.String $ Hask.show oState
  where
    --  callEndpoint @"mint" h1 artwork
    artwork =
      MintParams
        { mp'content = Content "A painting."
        , mp'title = Title "Fiona Lisa"
        , mp'share = 1 % 10
        , mp'price = Just 5
        , mp'purchaseAfter = Nothing
        }
    -- artwork2 = artwork {mp'content = Content "Another Painting"}

    buyParams nftId = BuyRequestUser nftId 6 (Just 200) Nothing

setPriceTrace :: EmulatorTrace ()
setPriceTrace = do
  let wallet1 = walletFromNumber 1 :: Emulator.Wallet
      wallet2 = walletFromNumber 5 :: Emulator.Wallet
  authMintH <- activateContractWallet wallet1 endpoints
  callEndpoint @"mint" authMintH artwork
  void $ Trace.waitNSlots 2
  oState <- Trace.observableState authMintH
  nftId <- case getLast oState of
    Nothing -> Trace.throwError (Trace.GenericError "NftId not found")
    Just nid -> return nid
  logInfo $ Hask.show nftId
  void $ Trace.waitNSlots 1
  authUseH :: AppTraceHandle <- activateContractWallet wallet1 endpoints
  callEndpoint @"set-price" authUseH (SetPriceParams nftId (Just 20) Nothing)
  void $ Trace.waitNSlots 1
  callEndpoint @"set-price" authUseH (SetPriceParams nftId (Just (-20)) Nothing)
  void $ Trace.waitNSlots 1
  userUseH :: AppTraceHandle <- activateContractWallet wallet2 endpoints
  callEndpoint @"set-price" userUseH (SetPriceParams nftId Nothing Nothing)
  void $ Trace.waitNSlots 1
  callEndpoint @"set-price" userUseH (SetPriceParams nftId (Just 30) Nothing)
  void $ Trace.waitNSlots 1
  where
    artwork =
      MintParams
        { mp'content = Content "A painting."
        , mp'title = Title "Fiona Lisa"
        , mp'share = 1 % 10
        , mp'price = Just 100
        , mp'purchaseAfter = Nothing
        }

queryPriceTrace :: EmulatorTrace ()
queryPriceTrace = do
  let wallet1 = walletFromNumber 1 :: Emulator.Wallet
      wallet2 = walletFromNumber 5 :: Emulator.Wallet
  authMintH :: AppTraceHandle <- activateContractWallet wallet1 endpoints
  callEndpoint @"mint" authMintH artwork
  void $ Trace.waitNSlots 2
  oState <- Trace.observableState authMintH
  nftId <- case getLast oState of
    Nothing -> Trace.throwError (Trace.GenericError "NftId not found")
    Just nid -> return nid
  logInfo $ Hask.show nftId
  void $ Trace.waitNSlots 1

  authUseH <- activateContractWallet wallet1 endpoints
  callEndpoint @"set-price" authUseH (SetPriceParams nftId (Just 20) Nothing)
  void $ Trace.waitNSlots 2

  queryHandle <- activateContractWallet wallet2 queryEndpoints
  callEndpoint @"query-current-price" queryHandle nftId
  -- hangs if this is not called before `observableState`
  void $ Trace.waitNSlots 1
  queryState <- Trace.observableState queryHandle
  queriedPrice <- case getLast queryState of
    Nothing -> Trace.throwError (Trace.GenericError "QueryResponse not found")
    Just resp -> case resp of
      QueryCurrentOwner _ -> Trace.throwError (Trace.GenericError "wrong query state, got owner instead of price")
      QueryCurrentPrice price -> return price
  logInfo $ "Queried price: " <> Hask.show queriedPrice

  callEndpoint @"query-current-owner" queryHandle nftId
  void $ Trace.waitNSlots 1
  queryState2 <- Trace.observableState queryHandle
  queriedOwner <- case getLast queryState2 of
    Nothing -> Trace.throwError (Trace.GenericError "QueryResponse not found")
    Just resp -> case resp of
      QueryCurrentOwner owner -> return owner
      QueryCurrentPrice _ -> Trace.throwError (Trace.GenericError "wrong query state, got price instead of owner")
  logInfo $ "Queried owner: " <> Hask.show queriedOwner

  void $ Trace.waitNSlots 1
  where
    artwork =
      MintParams
        { mp'content = Content "A painting."
        , mp'title = Title "Fiona Lisa"
        , mp'share = 1 % 10
        , mp'price = Just 100
        , mp'purchaseAfter = Nothing
        }

validPurchaseAfterTrace :: EmulatorTrace ()
validPurchaseAfterTrace = do
  let wallet1 = walletFromNumber 1 :: Emulator.Wallet
      wallet2 = walletFromNumber 2 :: Emulator.Wallet
  h1 :: AppTraceHandle <- activateContractWallet wallet1 endpoints
  h2 :: AppTraceHandle <- activateContractWallet wallet2 endpoints
  callEndpoint @"mint" h1 artwork

  void $ Trace.waitNSlots 1
  oState <- Trace.observableState h1
  nftId <- case getLast oState of
    Nothing -> Trace.throwError (Trace.GenericError "NftId not found")
    Just nid -> return nid
  void $ Trace.waitNSlots 5

  callEndpoint @"buy" h2 (buyParams nftId)
  where
    slotFiveTime = slotToBeginPOSIXTime def 5
    artwork =
      MintParams
        { mp'content = Content "A painting."
        , mp'title = Title "Fiona Lisa"
        , mp'share = 1 % 10
        , mp'price = Just 5
        , mp'purchaseAfter = Just slotFiveTime
        }

    buyParams nftId = BuyRequestUser nftId 6 (Just 200) Nothing

invalidPurchaseAfterTrace :: EmulatorTrace ()
invalidPurchaseAfterTrace = do
  let wallet1 = walletFromNumber 1 :: Emulator.Wallet
      wallet2 = walletFromNumber 2 :: Emulator.Wallet
  h1 :: AppTraceHandle <- activateContractWallet wallet1 endpoints
  h2 :: AppTraceHandle <- activateContractWallet wallet2 endpoints
  callEndpoint @"mint" h1 artwork

  void $ Trace.waitNSlots 1
  oState <- Trace.observableState h1
  nftId <- case getLast oState of
    Nothing -> Trace.throwError (Trace.GenericError "NftId not found")
    Just nid -> return nid
  void $ Trace.waitNSlots 1

  callEndpoint @"buy" h2 (buyParams nftId)
  where
    slotFiveTime = slotToBeginPOSIXTime def 5
    artwork =
      MintParams
        { mp'content = Content "A painting."
        , mp'title = Title "Fiona Lisa"
        , mp'share = 1 % 10
        , mp'price = Just 5
        , mp'purchaseAfter = Just slotFiveTime
        }

    buyParams nftId = BuyRequestUser nftId 6 (Just 200) Nothing

validPurchaseAfterBuyTrace :: EmulatorTrace ()
validPurchaseAfterBuyTrace = do
  let wallet1 = walletFromNumber 1 :: Emulator.Wallet
      wallet2 = walletFromNumber 2 :: Emulator.Wallet
      wallet3 = walletFromNumber 3 :: Emulator.Wallet
      wallet4 = walletFromNumber 4 :: Emulator.Wallet
  h1 :: AppTraceHandle <- activateContractWallet wallet1 endpoints
  h2 :: AppTraceHandle <- activateContractWallet wallet2 endpoints
  h3 :: AppTraceHandle <- activateContractWallet wallet3 endpoints
  h4 :: AppTraceHandle <- activateContractWallet wallet4 endpoints
  callEndpoint @"mint" h1 artwork

  void $ Trace.waitNSlots 1
  oState <- Trace.observableState h1
  nftId <- case getLast oState of
    Nothing -> Trace.throwError (Trace.GenericError "NftId not found")
    Just nid -> return nid
  void $ Trace.waitNSlots 1

  callEndpoint @"buy" h2 (buyParams1 nftId)
  void $ Trace.waitNSlots 7

  callEndpoint @"buy" h3 (buyParams2 nftId)
  void $ Trace.waitNSlots 2

  callEndpoint @"set-price" h3 (SetPriceParams nftId (Just 200) (Just slotTwentyTime))
  void $ Trace.waitNSlots 2

  callEndpoint @"buy" h4 (buyParams3 nftId)
  void $ Trace.waitNSlots 1
  where
    slotTenTime = slotToBeginPOSIXTime def 10
    slotTwentyTime = slotToBeginPOSIXTime def 20
    artwork =
      MintParams
        { mp'content = Content "A painting."
        , mp'title = Title "Fiona Lisa"
        , mp'share = 1 % 10
        , mp'price = Just 5
        , mp'purchaseAfter = Nothing
        }

    buyParams1 nftId = BuyRequestUser nftId 6 (Just 200) (Just slotTenTime)
    buyParams2 nftId = BuyRequestUser nftId 200 (Just 250) Nothing
    buyParams3 nftId = BuyRequestUser nftId 200 Nothing Nothing

-- | Test for prototyping.
test :: Hask.IO ()
test = runEmulatorTraceIO eTrace1

testSetPrice :: Hask.IO ()
testSetPrice = runEmulatorTraceIO setPriceTrace

testQueryPrice :: Hask.IO ()
testQueryPrice = runEmulatorTraceIO queryPriceTrace

testValidPurchaseAfter :: Hask.IO ()
testValidPurchaseAfter = runEmulatorTraceIO validPurchaseAfterTrace

testInvalidPurchaseAfter :: Hask.IO ()
testInvalidPurchaseAfter = runEmulatorTraceIO invalidPurchaseAfterTrace

testValidPurchaseAfterBuy :: Hask.IO ()
testValidPurchaseAfterBuy = runEmulatorTraceIO validPurchaseAfterBuyTrace
