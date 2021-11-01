module Test.NFT.Contract (
  test,
) where

import PlutusTx.Prelude hiding (check, mconcat)
import Test.Tasty (TestTree, testGroup)
import Prelude (mconcat)

import Mlabs.Emulator.Scene (checkScene)
import Mlabs.NFT.Types
import Test.NFT.Init

test :: TestTree
test =
  testGroup
    "Contract"
    [ testBuyOnce
    , testBuyTwice
    , testChangePriceWithoutOwnership
    , testBuyLockedScript
    , testBuyNotEnoughPriceScript
    -- , testQueryPrice
    -- , testQueryOwner
    ]

-- | User 2 buys from user 1
testBuyOnce :: TestTree
testBuyOnce = check "Buy once" (checkScene scene) w1 script
  where
    script = do
      nft1 <- userMint w1 artwork1
      userSetPrice w1 $ SetPriceParams nft1 (Just 1_000_000)
      userBuy w2 $ BuyRequestUser nft1 1_000_000 Nothing
      userSetPrice w2 $ SetPriceParams nft1 (Just 2_000_000)
    scene =
      mconcat
        [ w1 `ownsAda` 1_000_000
        , w2 `ownsAda` (-1_000_000)
        ]

{- |
- * User 2 buys from user 1
- * User 3 buys from user 2
-}
testBuyTwice :: TestTree
testBuyTwice = check "Buy twice" (checkScene scene) w1 script
  where
    script = do
      nft1 <- userMint w1 artwork1
      userSetPrice w1 $ SetPriceParams nft1 (Just 1_000_000)
      userBuy w2 $ BuyRequestUser nft1 1_000_000 Nothing
      userSetPrice w2 $ SetPriceParams nft1 (Just 2_000_000)
      userBuy w3 $ BuyRequestUser nft1 2_000_000 Nothing
    scene =
      mconcat
        [ w1 `ownsAda` 1_200_000
        , w2 `ownsAda` 800_000
        , w3 `ownsAda` (-2_000_000)
        ]

-- | User 1 tries to set price after user 2 owned the NFT.
testChangePriceWithoutOwnership :: TestTree
testChangePriceWithoutOwnership = check "Sets price without ownership" (checkScene scene) w1 script
  where
    script = do
      nft1 <- userMint w1 artwork1
      userSetPrice w1 $ SetPriceParams nft1 (Just 1_000_000)
      userBuy w2 $ BuyRequestUser nft1 1_000_000 Nothing
      userSetPrice w1 $ SetPriceParams nft1 (Just 2_000_000)
    scene =
      mconcat
        [ w1 `ownsAda` 1_000_000
        , w2 `ownsAda` (-1_000_000)
        ]

-- | User 2 tries to buy NFT which is locked (no price is set)
testBuyLockedScript :: TestTree
testBuyLockedScript = check "Buy locked NFT" (checkScene noChangesScene) w1 script
  where
    script = do
      nft1 <- userMint w1 artwork1
      userBuy w2 $ BuyRequestUser nft1 1_000_000 Nothing

-- | User 2 tries to buy open NFT with not enough money
testBuyNotEnoughPriceScript :: TestTree
testBuyNotEnoughPriceScript = check "Buy not enough price" (checkScene noChangesScene) w1 script
  where
    script = do
      nft1 <- userMint w1 artwork1
      userSetPrice w1 $ SetPriceParams nft1 (Just 1_000_000)
      userBuy w2 $ BuyRequestUser nft1 500_000 Nothing

-- testQueryPrice :: TestTree
-- testQueryPrice =
--   checkPredicateOptions
--     checkOptions
--     "Query price"
--     (assertAccumState queryEndpoints (walletInstanceTag w2) predicate "")
--     script
--   where
--     script = do
--       nftId <- callStartNft w1 mp
--       void $ waitNSlots 10

--       hdl1 <- activateContractWallet w1 endpoints
--       void $ callEndpoint @"mint" hdl1 mp
--       void $ waitNSlots 10

--       void $ callEndpoint @"set-price" hdl1 (SetPriceParams nftId (Just 100))
--       void $ waitNSlots 10

--       hdl2 <- activateContractWallet w2 queryEndpoints
--       void $ callEndpoint @"query-current-price" hdl2 nftId
--       void $ waitNSlots 10
--     predicate = \case
--       Last (Just (QueryCurrentPrice (Just x))) -> x == 100
--       _ -> False

-- testQueryOwner :: TestTree
-- testQueryOwner =
--   checkPredicateOptions
--     checkOptions
--     "Query owner"
--     (assertAccumState queryEndpoints (walletInstanceTag w2) predicate "")
--     script
--   where
--     script = do
--       nftId <- callStartNft w1 mp
--       void $ waitNSlots 10

--       hdl1 <- activateContractWallet w1 endpoints
--       void $ callEndpoint @"mint" hdl1 mp
--       void $ waitNSlots 10

--       void $ callEndpoint @"set-price" hdl1 (SetPriceParams nftId (Just 100))
--       void $ waitNSlots 10

--       hdl2 <- activateContractWallet w2 queryEndpoints
--       void $ callEndpoint @"query-current-owner" hdl2 nftId
--       void $ waitNSlots 10
--     predicate = \case
--       Last (Just (QueryCurrentOwner (UserId hash))) -> hash == pubKeyHash (walletPubKey w1)
--       _ -> False
