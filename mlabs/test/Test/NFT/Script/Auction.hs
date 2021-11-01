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
testAuctionBeforeDeadline = localOption (TimeRange $ Interval.to slotFiveTime) $
  withValidator "Test NFT dealing validator (for auction)" dealingValidator $ do
    shouldn'tValidate "Author can't start auction if not owner" openAuctionData1 openAuctionContext1
    shouldn'tValidate "Author can't close auction if not owner" closeAuctionData1 closeAuctionContext1
    shouldValidate "Owner can start auction" validOpenAuctionData validOpenAuctionContext
    shouldn'tValidate "Owner can't close auction before deadline" validCloseAuctionData validCloseAuctionContext
    shouldValidate "Can bid before deadline" validBidData validBidContext

testAuctionAfterDeadline :: TestTree
testAuctionAfterDeadline = localOption (TimeRange $ Interval.from slotElevenTime) $
  withValidator "Test NFT dealing validator (for auction, time dependent)" dealingValidator $ do
    shouldValidate "Owner can close auction" validCloseAuctionData validCloseAuctionContext
    shouldn'tValidate "Can't bid after deadline" validBidData validBidContext

initialAuthorDatum :: NFT.DatumNft
initialAuthorDatum =
  NFT.DatumNft
    { dNft'id = TestValues.testNftId
    , dNft'share = 1 % 2
    , dNft'author = NFT.UserId TestValues.authorPkh
    , dNft'owner = NFT.UserId TestValues.authorPkh
    , dNft'price = Just (100 * 1_000_000)
    , dNft'auctionState = Nothing
    }

ownerUserOneDatum :: NFT.DatumNft
ownerUserOneDatum = initialAuthorDatum {NFT.dNft'owner = NFT.UserId TestValues.userOnePkh}

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

ownerUserOneAuctionOpenDatum :: NFT.DatumNft
ownerUserOneAuctionOpenDatum =
  ownerUserOneDatum
    { NFT.dNft'auctionState = Just openAuctionState }

ownerUserOneAuctionBidDatum :: NFT.DatumNft
ownerUserOneAuctionBidDatum =
  ownerUserOneDatum
    { NFT.dNft'auctionState = Just bidAuctionState }

-- case 1
openAuctionData1 :: TestData 'ForSpending
openAuctionData1 = SpendingTest dtm redeemer val
  where
    dtm = ownerUserOneDatum

    redeemer =
      NFT.OpenAuctionAct
        { act'cs = TestValues.nftCurrencySymbol
        }

    val = TestValues.oneNft

openAuctionContext1 :: ContextBuilder 'ForSpending
openAuctionContext1 =
  paysSelf oneNft ownerUserOneAuctionOpenDatum

-- case 2
closeAuctionData1 :: TestData 'ForSpending
closeAuctionData1 = SpendingTest dtm redeemer val
  where
    dtm = ownerUserOneAuctionOpenDatum

    redeemer =
      NFT.CloseAuctionAct
        { act'cs = TestValues.nftCurrencySymbol
        }

    val = TestValues.oneNft

closeAuctionContext1 :: ContextBuilder 'ForSpending
closeAuctionContext1 =
  paysSelf oneNft ownerUserOneDatum

-- case 3
validOpenAuctionData :: TestData 'ForSpending
validOpenAuctionData = SpendingTest dtm redeemer val
  where
    dtm = ownerUserOneDatum

    redeemer =
      NFT.OpenAuctionAct
        { act'cs = TestValues.nftCurrencySymbol
        }

    val = TestValues.oneNft

validOpenAuctionContext :: ContextBuilder 'ForSpending
validOpenAuctionContext =
    paysSelf oneNft ownerUserOneAuctionOpenDatum
  <> signedWith userOnePkh

-- case 4
validCloseAuctionData :: TestData 'ForSpending
validCloseAuctionData = SpendingTest dtm redeemer val
  where
    dtm = ownerUserOneAuctionOpenDatum

    redeemer =
      NFT.CloseAuctionAct
        { act'cs = TestValues.nftCurrencySymbol
        }

    val = TestValues.oneNft

validCloseAuctionContext :: ContextBuilder 'ForSpending
validCloseAuctionContext =
  paysSelf oneNft ownerUserOneDatum
  <> signedWith userOnePkh

validBidData :: TestData 'ForSpending
validBidData = SpendingTest dtm redeemer val
  where
    dtm = ownerUserOneAuctionOpenDatum

    redeemer =
      NFT.BidAuctionAct
        { act'bid = 300 * 1_000_000
        , act'cs = TestValues.nftCurrencySymbol
        }

    val = TestValues.oneNft

validBidContext :: ContextBuilder 'ForSpending
validBidContext =
  paysSelf (oneNft PlutusPrelude.<> TestValues.adaValue 300) ownerUserOneAuctionBidDatum

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
