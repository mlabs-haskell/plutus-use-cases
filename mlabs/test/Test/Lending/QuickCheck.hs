{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Lending.QuickCheck where

import PlutusTx.Prelude hiding (fmap, length, (<$>), (<*>))
import Prelude (
  Int,
  Show,
  String, 
  abs,
  drop,
  fmap,
  length,
  zip3,
  (<$>),
  (<*>),
 )

import qualified Prelude as Prelude 

import Control.Monad (guard)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.String (fromString)
import Plutus.V1.Ledger.Value qualified as Value
import Test.Lending.Logic (coin1, coin2, coin3, fromToken, testAppConfig, user1, user2, user3)
import Test.QuickCheck qualified as QC
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import qualified Test.QuickCheck.Gen as Gen 

import Mlabs.Emulator.App (App (..), lookupAppWallet)
import Mlabs.Emulator.Blockchain (BchWallet (..))
import Mlabs.Emulator.Types (Coin, UserId (..), adaCoin)
import Mlabs.Lending.Logic.App (AppConfig (..), Script, runLendingApp, userAct, toCoin)
import Mlabs.Lending.Logic.Types (UserAct (..), CoinCfg (CoinCfg), defaultInterestModel)
import Plutus.V1.Ledger.Api (PubKeyHash(PubKeyHash))
import PlutusTx.Builtins.Class ()
import Control.Monad.Reader (replicateM)
import Plutus.V1.Ledger.Value (AssetClass(unAssetClass))
import Test.QuickCheck (elements)

allUsers :: [UserId]
allUsers = [Self, user1, user2, user3]

users :: [UserId]
users = drop 1 allUsers

coins :: [Coin]
coins = [adaCoin, coin1, coin2, coin3]

nonNativeCoins :: [Coin]
nonNativeCoins = drop 1 coins

aToken :: Coin -> Value.TokenName
aToken (Value.AssetClass (_, Value.TokenName tn)) = Value.TokenName ("a" <> tn)

aCoin :: Coin -> Coin
aCoin coin = fromToken (aToken coin)

-- Various integer generators
smallGenSize :: Int
smallGenSize = 100

bigGenSize :: Int
bigGenSize = 1_000_000_000_000_000_000

positiveSmallInteger :: QC.Gen Integer
positiveSmallInteger = fmap QC.getPositive (QC.resize smallGenSize QC.arbitrary)

positiveBigInteger :: QC.Gen Integer
positiveBigInteger = (*) <$> gen <*> gen
  where
    gen = fmap QC.getPositive (QC.resize bigGenSize QC.arbitrary)

nonPositiveSmallInteger :: QC.Gen Integer
nonPositiveSmallInteger = fmap (negate . abs) (QC.resize smallGenSize QC.arbitrary)

nonPositiveBigInteger :: QC.Gen Integer
nonPositiveBigInteger = (\x y -> negate (abs (x * y))) <$> gen <*> gen
  where
    gen = fmap negate (QC.resize bigGenSize QC.arbitrary)

positiveInteger :: QC.Gen Integer
positiveInteger = QC.frequency [(1, positiveSmallInteger), (1, positiveBigInteger)]

nonPositiveInteger :: QC.Gen Integer
nonPositiveInteger = QC.frequency [(1, nonPositiveSmallInteger), (1, nonPositiveBigInteger)]

-- | Contains parameters that deposit test cases can be generalized over
newtype DepositTestInput = DepositTestInput
  {deposits :: [(UserId, Coin, Integer)]}
  deriving (Show)


-- | Construct a `Script`
createDepositScript :: DepositTestInput -> Script
createDepositScript (DepositTestInput ds) =
  mapM_ (\(user, coin, amt) -> userAct user $ DepositAct amt coin) ds

noErrorsProp :: App st act -> Bool
noErrorsProp app = null @[] (    "app'log"        )

someErrorsProp :: App st act -> Bool
someErrorsProp app = not (null @[] (    "app'log"        ))

hasWallet :: App st act -> UserId -> BchWallet -> Bool
hasWallet app uid wal = lookupAppWallet uid app == Just wal

checkWalletsProp :: (Show act, Show st) => [(UserId, BchWallet)] -> App st act -> Bool
checkWalletsProp wals app = all (uncurry $ hasWallet app) wals

-- Map manipulation helper functions
walletListToNestedMap :: [(UserId, BchWallet)] -> Map UserId (Map Coin Integer)
walletListToNestedMap wals =
  addNestedMaps $ map (\(user, BchWallet wal) -> Map.singleton user wal) wals

nestedMapToWalletList :: Map UserId (Map Coin Integer) -> [(UserId, BchWallet)]
nestedMapToWalletList m = Map.toAscList (Map.map BchWallet m)

addNestedMaps :: [Map UserId (Map Coin Integer)] -> Map UserId (Map Coin Integer)
addNestedMaps = Map.unionsWith (Map.unionWith (+))

-- | Calculate expected balances after running deposit script
expectedWalletsDeposit :: AppConfig -> DepositTestInput -> [(UserId, BchWallet)]
expectedWalletsDeposit appCfg (DepositTestInput ds) =
  let startingBalances = walletListToNestedMap (appConfig'users appCfg)
      depositedCoins = map (\(user, coin, amt) -> Map.singleton user (Map.singleton coin (negate amt))) ds
      aCoins = map (\(user, coin, amt) -> Map.singleton user (Map.singleton (aCoin coin) amt)) ds
      appCoins = Map.singleton Self $ Map.unionsWith (+) (map (\(_, coin, amt) -> Map.singleton coin amt) ds)
      appAcoins = Map.singleton Self $ Map.fromList $ map (\(_, coin, _) -> (aCoin coin, 0)) ds
      allWallets = addNestedMaps ([startingBalances] ++ depositedCoins ++ aCoins ++ [appCoins] ++ [appAcoins])
   in Map.toAscList (Map.map BchWallet allWallets)

-- | Check that the balances after deposit script run correspond to the expected balances
testWalletsProp :: [(UserId, BchWallet)] -> Script -> Bool
testWalletsProp expectedWals script =
  let app = runLendingApp testAppConfig script
   in noErrorsProp app && checkWalletsProp expectedWals app

testWalletsProp' :: DepositTestInput -> Bool
testWalletsProp' d =
  let script = createDepositScript d
   in testWalletsProp (expectedWalletsDeposit testAppConfig d) script


depositInputGen :: Int -> QC.Gen Integer -> QC.Gen DepositTestInput
depositInputGen n intgen = fmap DepositTestInput $ replicateM n $ do 
  user <- retry ((UserId . PubKeyHash . fromString) <$> QC.arbitrary) (\(UserId (PubKeyHash x)) -> x == mempty) 
  coin <- retry ((toCoin . fromString) <$> QC.arbitrary) (\(Value.AssetClass (Value.CurrencySymbol x, Value.TokenName y)) -> x == mempty || y == mempty)
  int <- intgen 
  return $ (user, coin, int)

retry ma p = do 
  a <- ma 
  if p a then retry ma p else return a 

 


testDepositLogic :: QC.Property
testDepositLogic = QC.forAll (depositInputGen 1 (QC.choose (1, 100))) testWalletsProp'

test :: TestTree
test = testGroup "QuickCheck" [testGroup "Logic" [testProperty "deposit" testDepositLogic]]

mkUser :: QC.Gen UserId
mkUser = UserId <$> PubKeyHash <$> fromString @BuiltinByteString <$> Gen.scale (Prelude.+3) QC.arbitrary


mkName :: QC.Gen BuiltinByteString 
mkName = fromString <$> Gen.scale (Prelude.+3) QC.arbitrary 

mkCoinBase :: Gen.Gen Coin
mkCoinBase = Value.AssetClass <$> do { cur <- mkName; tn <- mkName; return (Value.CurrencySymbol cur, Value.TokenName tn)}

mkCoin :: Gen.Gen Coin
mkCoin = retry mkCoinBase (\(Value.AssetClass (Value.CurrencySymbol  x, Value.TokenName y)) -> x==mempty || y == mempty)


coinToCoinCfg :: Coin -> CoinCfg 
coinToCoinCfg assetclass = CoinCfg assetclass (fromInteger 0) (snd . unAssetClass $ assetclass) defaultInterestModel (fromInteger 0) 

--assumes no duplicate users 
-- and a nonempty list 
depositTestInputToAppConfig :: DepositTestInput -> Maybe AppConfig  
depositTestInputToAppConfig (DepositTestInput []) = Nothing 
depositTestInputToAppConfig (DepositTestInput usercoinamt@(first:_)) = Just $ AppConfig 
  (coinToCoinCfg . (\(_, x, _) -> x) <$> usercoinamt) 
  userswallets 
  cs 
  [] 
  [] 
  where 
    userswallets :: [(UserId, BchWallet)]
    userswallets = (\(user, coin, amt) -> (user, toWallet coin amt)) <$> usercoinamt 
    toWallet coin amt = BchWallet $ Map.fromList [(coin, amt)]
    cs = fst . unAssetClass $ (\(_, x, _) -> x)  first 
