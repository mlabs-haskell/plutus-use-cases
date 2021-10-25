module Test.NFT.Trace where

import PlutusTx.Prelude
import Prelude qualified as Hask

import Data.Monoid (Last (..))
import Data.Text (Text)

import Control.Monad (void)
import Control.Monad.Freer.Extras.Log as Extra (logInfo)

import Plutus.Trace.Emulator (EmulatorTrace, activateContractWallet, callEndpoint, runEmulatorTraceIO)
import Plutus.Trace.Emulator qualified as Trace
import Wallet.Emulator qualified as Emulator

import Mlabs.Utils.Wallet (walletFromNumber)

import Mlabs.NFT.Api
import Mlabs.NFT.Types

-- | Generic application Trace Handle.
type AppTraceHandle = Trace.ContractHandle (Last NftId) NFTAppSchema Text

type AppInitHandle = Trace.ContractHandle (Last NftAppSymbol) NFTAppSchema Text

-- | Initialise the Application
appInitTrace :: EmulatorTrace NftAppSymbol
appInitTrace = do
  let admin = walletFromNumber 3 :: Emulator.Wallet
  hAdmin :: AppInitHandle <- activateContractWallet admin adminEndpoints
  callEndpoint @"app-init" hAdmin ()
  void $ Trace.waitNSlots 2
  oState <- Trace.observableState hAdmin
  aSymbol <- case getLast oState of
    Nothing -> Trace.throwError $ Trace.GenericError "App Symbol Could not be established."
    Just aS -> return aS
  void $ Trace.waitNSlots 1
  return aSymbol

-- | Emulator Trace 1. Mints one NFT.
mint1Trace :: EmulatorTrace ()
mint1Trace = do
  aSymb <- appInitTrace
  let wallet1 = walletFromNumber 1 :: Emulator.Wallet
  h1 :: AppTraceHandle <- activateContractWallet wallet1 $ endpoints aSymb
  callEndpoint @"mint" h1 artwork
  void $ Trace.waitNSlots 1
  where
    artwork =
      MintParams
        { mp'content = Content "A painting."
        , mp'title = Title "Fiona Lisa"
        , mp'share = 1 % 10
        , mp'price = Just 5
        }

-- | Tries to Mint the same NFT twice. Should fail
mint2Trace :: EmulatorTrace ()
mint2Trace = do
  aSymb <- appInitTrace
  let wallet1 = walletFromNumber 1 :: Emulator.Wallet
  h1 :: AppTraceHandle <- activateContractWallet wallet1 $ endpoints aSymb
  callEndpoint @"mint" h1 artwork
  void $ Trace.waitNSlots 1
  callEndpoint @"mint" h1 artwork2
  void $ Trace.waitNSlots 1
  callEndpoint @"mint" h1 artwork2
  where
    artwork =
      MintParams
        { mp'content = Content "A painting."
        , mp'title = Title "Fiona Lisa"
        , mp'share = 1 % 10
        , mp'price = Just 5
        }
    artwork2 =
      MintParams
        { mp'content = Content "Another painting."
        , mp'title = Title "Fiona Lisa"
        , mp'share = 1 % 10
        , mp'price = Just 5
        }

-- | Emulator Trace 1. Mints one NFT.
eTrace1 :: EmulatorTrace ()
eTrace1 = do
  let wallet1 = walletFromNumber 1 :: Emulator.Wallet
      wallet2 = walletFromNumber 2 :: Emulator.Wallet
  aSymb <- appInitTrace
  h1 :: AppTraceHandle <- activateContractWallet wallet1 $ endpoints aSymb
  h2 :: AppTraceHandle <- activateContractWallet wallet2 $ endpoints aSymb
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
        }
    buyParams nftId = BuyRequestUser nftId 6 (Just 200)

setPriceTrace :: EmulatorTrace ()
setPriceTrace = do
  let wallet1 = walletFromNumber 1 :: Emulator.Wallet
      wallet2 = walletFromNumber 5 :: Emulator.Wallet
  authMintH <- activateContractWallet wallet1 (endpoints $ error ())
  void $ Trace.waitNSlots 2
  oState <- Trace.observableState authMintH
  nftId <- case getLast oState of
    Nothing -> Trace.throwError (Trace.GenericError "NftId not found")
    Just nid -> return nid
  logInfo $ Hask.show nftId
  void $ Trace.waitNSlots 1
  authUseH :: AppTraceHandle <- activateContractWallet wallet1 (endpoints $ error ())
  callEndpoint @"set-price" authUseH (SetPriceParams nftId (Just 20))
  void $ Trace.waitNSlots 1
  callEndpoint @"set-price" authUseH (SetPriceParams nftId (Just (-20)))
  void $ Trace.waitNSlots 1
  userUseH :: AppTraceHandle <- activateContractWallet wallet2 (endpoints $ error ())
  callEndpoint @"set-price" userUseH (SetPriceParams nftId Nothing)
  void $ Trace.waitNSlots 1
  callEndpoint @"set-price" userUseH (SetPriceParams nftId (Just 30))
  void $ Trace.waitNSlots 1
  where
    artwork =
      MintParams
        { mp'content = Content "A painting."
        , mp'title = Title "Fiona Lisa"
        , mp'share = 1 % 10
        , mp'price = Just 100
        }

eTrace2 :: EmulatorTrace ()
eTrace2 = do
  let wallet1 = walletFromNumber 1 :: Emulator.Wallet
  authMintH <- activateContractWallet wallet1 $ endpoints (error ()) --FIXME
  void $ Trace.waitNSlots 1

-- | Test for initialising the App
testInit :: Hask.IO ()
testInit = runEmulatorTraceIO $ void appInitTrace

-- | Test for Minting one token
testMint = runEmulatorTraceIO mint1Trace

testMint2 = runEmulatorTraceIO mint2Trace

-- | Test for prototyping.
test :: Hask.IO ()
test = runEmulatorTraceIO eTrace1

-- | New Test
test1 :: Hask.IO ()
test1 = runEmulatorTraceIO eTrace2

-- | Mint Test
test2 = runEmulatorTraceIO eTrace2

testSetPrice :: Hask.IO ()
testSetPrice = runEmulatorTraceIO setPriceTrace
