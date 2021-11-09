module Test.NFT.Script.Auction (
  testAuctionBeforeDeadline,
  testAuctionAfterDeadline
) where

import Data.Semigroup ((<>))
import Ledger qualified
import Mlabs.NFT.Types qualified as NFT
import Mlabs.NFT.Validation qualified as NFT
import Plutus.V1.Ledger.Ada qualified as Ada
import PlutusTx qualified
import PlutusTx.IsData.Class (ToData (toBuiltinData))
import PlutusTx.Prelude hiding ((<>))
import PlutusTx.Prelude qualified as PlutusPrelude
import Test.NFT.Script.Values as TestValues
import Test.Tasty (TestTree, localOption)
import Test.Tasty.Plutus.Context
import Test.Tasty.Plutus.Script.Unit
import Ledger.TimeSlot (slotToBeginPOSIXTime)
import Data.Default (def)
import Plutus.V1.Ledger.Interval qualified as Interval

slotFiveTime :: Ledger.POSIXTime
slotFiveTime = slotToBeginPOSIXTime def 5

slotTenTime :: Ledger.POSIXTime
slotTenTime = slotToBeginPOSIXTime def 10

slotElevenTime :: Ledger.POSIXTime
slotElevenTime = slotToBeginPOSIXTime def 11

testAuctionBeforeDeadline :: TestTree
testAuctionBeforeDeadline = localOption (TestTxId TestValues.testTxId) $
  localOption (TimeRange $ Interval.to slotFiveTime) $
    withValidator "Test NFT dealing validator (auction before deadline)" dealingValidator $ do
      shouldn'tValidate "Author can't close auction if not owner" closeAuctionData1 closeAuctionContext1
      shouldValidate "Owner can start auction" validOpenAuctionData validOpenAuctionContext
      shouldn'tValidate "Owner can't close auction before deadline" validCloseAuctionData validCloseAuctionContext

-- FIXME
-- shouldValidate "Can bid before deadline" validBidData validBidContext
-- shouldValidate "Can make higher bid" validSecondBidData validSecondBidContext

testAuctionAfterDeadline :: TestTree
testAuctionAfterDeadline = localOption (TimeRange $ Interval.from slotElevenTime) $
  withValidator "Test NFT dealing validator (auction after deadline)" dealingValidator $ do
    shouldValidate "Owner can close auction" validCloseAuctionData validCloseAuctionContext
    shouldn'tValidate "Can't bid after deadline" validBidData validBidContext
    shouldValidate "Can close auction with a bid" closeAuctionWithBidData closeAuctionWithBidContext
    shouldn'tValidate "Can't close auction if author not paid" closeAuctionWithBidData closeAuctionWithBidNoAuthorContext
    shouldn'tValidate "Can't close auction if owner not paid" closeAuctionWithBidData closeAuctionWithBidNoOwnerContext
    shouldn'tValidate "Can't close auction if owner=author not paid" closeAuctionWithBidAuthorData closeAuctionWithBidAuthorContext
    shouldn'tValidate "Can't close auction if datum illegaly altered" closeAuctionInconsistentData closeAuctionInconsistentContext

initialNode :: NFT.NftListNode
initialNode =
  NFT.NftListNode
    { node'information =
        NFT.InformationNft
          { info'id = TestValues.testNftId
          , info'share = 1 % 2
          , info'author = NFT.UserId TestValues.authorPkh
          , info'owner = NFT.UserId TestValues.authorPkh
          , info'price = Just (100 * 1_000_000)
          , info'auctionState = Nothing
          }
    , node'next = Nothing
    , node'appInstance = TestValues.appInstance
    }

initialAuthorDatum :: NFT.DatumNft
initialAuthorDatum = NFT.NodeDatum initialNode

ownerUserOneNode :: NFT.NftListNode
ownerUserOneNode =
    initialNode
      { NFT.node'information =
        (NFT.node'information initialNode)
          { NFT.info'owner = NFT.UserId TestValues.userOnePkh
          }
      }

ownerUserOneDatum :: NFT.DatumNft
ownerUserOneDatum =
  NFT.NodeDatum ownerUserOneNode

openAuctionState :: NFT.AuctionState
openAuctionState = NFT.AuctionState
  { as'highestBid = Nothing
  , as'deadline = slotTenTime
  , as'minBid = 100 * 1_000_000
  }

bidAuctionState :: NFT.AuctionState
bidAuctionState = NFT.AuctionState
  { as'highestBid = Just (NFT.AuctionBid (300 * 1_000_000) (NFT.UserId TestValues.userTwoPkh))
  , as'deadline = slotTenTime
  , as'minBid = 100 * 1_000_000
  }

secondBidAuctionState :: NFT.AuctionState
secondBidAuctionState = NFT.AuctionState
  { as'highestBid = Just (NFT.AuctionBid (500 * 1_000_000) (NFT.UserId TestValues.userThreePkh))
  , as'deadline = slotTenTime
  , as'minBid = 100 * 1_000_000
  }

ownerUserOneAuctionOpenDatum :: NFT.DatumNft
ownerUserOneAuctionOpenDatum =
  NFT.NodeDatum $
  ownerUserOneNode
    { NFT.node'information =
      (NFT.node'information ownerUserOneNode)
        { NFT.info'auctionState = Just openAuctionState
        }
    }

ownerUserOneAuctionBidDatum :: NFT.DatumNft
ownerUserOneAuctionBidDatum =
  NFT.NodeDatum $
  ownerUserOneNode
    { NFT.node'information =
      (NFT.node'information ownerUserOneNode)
        { NFT.info'auctionState = Just bidAuctionState
        }
    }

ownerUserOneAuctionSecondBidDatum :: NFT.DatumNft
ownerUserOneAuctionSecondBidDatum =
  NFT.NodeDatum $
  ownerUserOneNode
    { NFT.node'information =
      (NFT.node'information ownerUserOneNode)
        { NFT.info'auctionState = Just secondBidAuctionState
        }
    }

auctionWithBidCloseDatum :: NFT.DatumNft
auctionWithBidCloseDatum =
  NFT.NodeDatum $
  ownerUserOneNode
    { NFT.node'information =
      (NFT.node'information ownerUserOneNode)
        { NFT.info'owner = NFT.UserId TestValues.userTwoPkh
        }
    }

auctionWithBidAuthorNode :: NFT.NftListNode
auctionWithBidAuthorNode =
  initialNode
    { NFT.node'information =
      (NFT.node'information initialNode)
        { NFT.info'auctionState = Just bidAuctionState
        }
    }

auctionWithBidAuthorDatum :: NFT.DatumNft
auctionWithBidAuthorDatum =
  NFT.NodeDatum $ auctionWithBidAuthorNode

auctionCloseInconsistentDatum :: NFT.DatumNft
auctionCloseInconsistentDatum =
  NFT.NodeDatum $
    auctionWithBidAuthorNode
      { NFT.node'information =
        (NFT.node'information auctionWithBidAuthorNode)
          { NFT.info'auctionState = Nothing
          , NFT.info'author = NFT.UserId TestValues.userOnePkh
          , NFT.info'owner = NFT.UserId TestValues.userTwoPkh
          }
      }

-- case 1
openAuctionData1 :: TestData 'ForSpending
openAuctionData1 = SpendingTest dtm redeemer val
  where
    dtm = ownerUserOneDatum

    redeemer =
      NFT.OpenAuctionAct
        { act'symbol = TestValues.appSymbol
        }

    val = TestValues.oneNft

openAuctionContext1 :: ContextBuilder 'ForSpending
openAuctionContext1 =
  paysOther NFT.txValHash TestValues.oneNft ownerUserOneAuctionOpenDatum
  <> paysSelf mempty ownerUserOneAuctionOpenDatum

-- case 2
closeAuctionData1 :: TestData 'ForSpending
closeAuctionData1 = SpendingTest dtm redeemer val
  where
    dtm = ownerUserOneAuctionOpenDatum

    redeemer =
      NFT.CloseAuctionAct
        { act'symbol = TestValues.appSymbol
        }

    val = TestValues.oneNft

closeAuctionContext1 :: ContextBuilder 'ForSpending
closeAuctionContext1 =
  paysOther NFT.txValHash TestValues.oneNft ownerUserOneDatum
  <> paysSelf mempty ownerUserOneDatum

-- case 3
validOpenAuctionData :: TestData 'ForSpending
validOpenAuctionData = SpendingTest dtm redeemer val
  where
    dtm = ownerUserOneDatum

    redeemer =
      NFT.OpenAuctionAct
        { act'symbol = TestValues.appSymbol
        }

    val = TestValues.oneNft

validOpenAuctionContext :: ContextBuilder 'ForSpending
validOpenAuctionContext =
  paysOther NFT.txValHash TestValues.oneNft ownerUserOneAuctionOpenDatum
  <> paysSelf mempty ownerUserOneAuctionOpenDatum
  <> signedWith userOnePkh

-- case 4
validCloseAuctionData :: TestData 'ForSpending
validCloseAuctionData = SpendingTest dtm redeemer val
  where
    dtm = ownerUserOneAuctionOpenDatum

    redeemer =
      NFT.CloseAuctionAct
        { act'symbol = TestValues.appSymbol
        }

    val = TestValues.oneNft

validCloseAuctionContext :: ContextBuilder 'ForSpending
validCloseAuctionContext =
  paysOther NFT.txValHash TestValues.oneNft ownerUserOneDatum
  <> signedWith userOnePkh
  -- a hack to get `getContinuingOutputs` working
  <> paysSelf mempty ownerUserOneDatum

validBidData :: TestData 'ForSpending
validBidData = SpendingTest dtm redeemer val
  where
    dtm = ownerUserOneAuctionOpenDatum

    redeemer =
      NFT.BidAuctionAct
        { act'bid = 300 * 1_000_000
        , act'symbol = TestValues.appSymbol
        }

    val = TestValues.oneNft

validBidContext :: ContextBuilder 'ForSpending
validBidContext =
  paysOther NFT.txValHash TestValues.oneNft ownerUserOneDatum
  <> paysSelf (mempty PlutusPrelude.<> TestValues.adaValue 300) ownerUserOneAuctionBidDatum

validSecondBidData :: TestData 'ForSpending
validSecondBidData = SpendingTest dtm redeemer val
  where
    dtm = ownerUserOneAuctionBidDatum

    redeemer =
      NFT.BidAuctionAct
        { act'bid = 500 * 1_000_000
        , act'symbol = TestValues.appSymbol
        }

    val = TestValues.oneNft <> (TestValues.adaValue 300)

validSecondBidContext :: ContextBuilder 'ForSpending
validSecondBidContext =
  paysOther NFT.txValHash TestValues.oneNft ownerUserOneAuctionSecondBidDatum
  <> paysSelf (mempty PlutusPrelude.<> TestValues.adaValue 500) ownerUserOneAuctionSecondBidDatum
  <> paysToWallet TestValues.userTwoWallet (TestValues.adaValue 300)

closeAuctionWithBidData :: TestData 'ForSpending
closeAuctionWithBidData = SpendingTest dtm redeemer val
  where
    dtm = ownerUserOneAuctionBidDatum

    redeemer =
      NFT.CloseAuctionAct
        { act'symbol = TestValues.appSymbol
        }

    -- TODO: correctInputValue check for all redeemers?
    val = TestValues.oneNft -- <> (TestValues.adaValue 300)

closeAuctionWithBidContext :: ContextBuilder 'ForSpending
closeAuctionWithBidContext =
  paysOther NFT.txValHash TestValues.oneNft auctionWithBidCloseDatum
  <> paysSelf mempty auctionWithBidCloseDatum
  <> signedWith userOnePkh
  <> paysToWallet TestValues.authorWallet (TestValues.adaValue 150)
  <> paysToWallet TestValues.userOneWallet (TestValues.adaValue 150)

closeAuctionWithBidNoAuthorContext :: ContextBuilder 'ForSpending
closeAuctionWithBidNoAuthorContext =
  paysOther NFT.txValHash TestValues.oneNft auctionWithBidCloseDatum
  <> paysSelf mempty auctionWithBidCloseDatum
  <> signedWith userOnePkh
  <> paysToWallet TestValues.userOneWallet (TestValues.adaValue 150)

closeAuctionWithBidNoOwnerContext :: ContextBuilder 'ForSpending
closeAuctionWithBidNoOwnerContext =
  paysOther NFT.txValHash TestValues.oneNft auctionWithBidCloseDatum
  <> paysSelf mempty auctionWithBidCloseDatum
  <> signedWith userOnePkh
  <> paysToWallet TestValues.authorWallet (TestValues.adaValue 150)

closeAuctionWithBidAuthorData :: TestData 'ForSpending
closeAuctionWithBidAuthorData = SpendingTest dtm redeemer val
  where
    dtm = auctionWithBidAuthorDatum

    redeemer =
      NFT.CloseAuctionAct
        { act'symbol = TestValues.appSymbol
        }

    val = TestValues.oneNft

closeAuctionWithBidAuthorContext :: ContextBuilder 'ForSpending
closeAuctionWithBidAuthorContext =
  paysOther NFT.txValHash TestValues.oneNft auctionWithBidCloseDatum
  <> paysSelf mempty auctionWithBidCloseDatum
  <> signedWith authorPkh
  <> paysToWallet TestValues.authorWallet (TestValues.adaValue 150)

closeAuctionInconsistentData :: TestData 'ForSpending
closeAuctionInconsistentData = SpendingTest dtm redeemer val
  where
    dtm = auctionWithBidAuthorDatum

    redeemer =
      NFT.CloseAuctionAct
        { act'symbol = TestValues.appSymbol
        }

    val = TestValues.oneNft

closeAuctionInconsistentContext :: ContextBuilder 'ForSpending
closeAuctionInconsistentContext =
  paysOther NFT.txValHash TestValues.oneNft auctionCloseInconsistentDatum
  <> paysSelf mempty auctionCloseInconsistentDatum
  <> signedWith authorPkh

dealingValidator :: Ledger.Validator
dealingValidator =
  Ledger.mkValidatorScript $
    $$(PlutusTx.compile [||wrap||])
      `PlutusTx.applyCode` $$(PlutusTx.compile [||NFT.mkTxPolicy||])
  where
    wrap ::
      (NFT.DatumNft -> NFT.UserAct -> Ledger.ScriptContext -> Bool) ->
      (BuiltinData -> BuiltinData -> BuiltinData -> ())
    wrap = toTestValidator
