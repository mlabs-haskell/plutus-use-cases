{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Test.NFT.QuickCheck where

import Control.Lens (at, makeLenses, set, view, (^.))
import Control.Monad (void, when)
import Data.Default (def)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Monoid (Last (..))
import Data.String (IsString (..))
import Data.Text (Text)
import Ledger (getPubKeyHash)
import Ledger.TimeSlot (slotToBeginPOSIXTime)
import Plutus.Contract.Test (Wallet (..))
import Plutus.Contract.Test.ContractModel (
  Action,
  Actions,
  ContractInstanceSpec (..),
  ContractModel (..),
  contractState,
  currentSlot,
  deposit,
  getModelState,
  propRunActionsWithOptions,
  transfer,
  wait,
  withdraw,
  ($=),
  ($~),
 )
import Plutus.Trace.Emulator (callEndpoint)
import Plutus.Trace.Emulator qualified as Trace
import Plutus.V1.Ledger.Ada (lovelaceValueOf)
import Plutus.V1.Ledger.Slot (Slot (..))
import Plutus.V1.Ledger.Value (AssetClass (..), TokenName (..), Value, assetClassValue)
import PlutusTx.Prelude hiding ((<$>), (<*>), (==))
import Test.QuickCheck qualified as QC
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Prelude ((<$>), (<*>), (==))
import Prelude qualified as Hask

import Mlabs.NFT.Api (NFTAppSchema, adminEndpoints, endpoints)
import Mlabs.NFT.Contract (hashData)
import Mlabs.NFT.Types (
  AuctionBidParams (..),
  AuctionCloseParams (..),
  AuctionOpenParams (..),
  BuyRequestUser (..),
  Content (..),
  MintParams (..),
  NftAppInstance,
  NftAppSymbol (..),
  NftId (..),
  QueryResponse,
  SetPriceParams (..),
  Title (..),
  UniqueToken,
  UserId (..),
 )
import Mlabs.NFT.Validation (calculateShares)
import Test.NFT.Init (checkOptions, toUserId, w1, w2, w3, wA)

data MockAuctionState = MockAuctionState
  { _auctionHighestBid :: Maybe (Integer, Wallet)
  , _auctionDeadline :: Slot
  , _auctionMinBid :: Integer
  }
  deriving (Hask.Show, Hask.Eq)
makeLenses ''MockAuctionState

-- Mock content for overriding Show instance, to show hash instead, useful for debugging
newtype MockContent = MockContent {getMockContent :: Content}
  deriving (Hask.Eq)

instance Hask.Show MockContent where
  show x = Hask.show (hashData . getMockContent $ x, getMockContent x)

-- We cannot use InformationNft because we need access to `Wallet`
-- `PubKeyHash` is not enough for simulation
data MockNft = MockNft
  { _nftId :: NftId
  , _nftPrice :: Maybe Integer
  , _nftOwner :: Wallet
  , _nftAuthor :: Wallet
  , _nftShare :: Rational
  , _nftAuctionState :: Maybe MockAuctionState
  }
  deriving (Hask.Show, Hask.Eq)
makeLenses ''MockNft

data NftModel = NftModel
  { _mMarket :: Map NftId MockNft
  , _mMintedCount :: Integer
  , _mStarted :: Bool
  }
  deriving (Hask.Show, Hask.Eq)
makeLenses ''NftModel

instance ContractModel NftModel where
  data Action NftModel
    = ActionInit
    | ActionMint
        { aPerformer :: Wallet
        , aContent :: MockContent
        , aTitle :: Title
        , aNewPrice :: Maybe Integer
        , aShare :: Rational
        }
    | ActionSetPrice
        { aPerformer :: Wallet
        , aNftId :: NftId
        , aNewPrice :: Maybe Integer
        }
    | ActionBuy
        { aPerformer :: Wallet
        , aNftId :: NftId
        , aPrice :: Integer
        , aNewPrice :: Maybe Integer
        }
    | ActionAuctionOpen
        { aPerformer :: Wallet
        , aNftId :: NftId
        , aDeadline :: Slot
        , aMinBid :: Integer
        }
    | ActionAuctionBid
        { aPerformer :: Wallet
        , aNftId :: NftId
        , aBid :: Integer
        }
    | ActionAuctionClose
        { aPerformer :: Wallet
        , aNftId :: NftId
        }
    deriving (Hask.Show, Hask.Eq)

  data ContractInstanceKey NftModel w s e where
    InitKey :: Wallet -> ContractInstanceKey NftModel (Last NftAppInstance) NFTAppSchema Text
    UserKey :: Wallet -> ContractInstanceKey NftModel (Last (Either NftId QueryResponse)) NFTAppSchema Text

  instanceTag key _ = fromString $ Hask.show key

  arbitraryAction model =
    let nfts = Map.keys (model ^. contractState . mMarket)
        genWallet = QC.elements wallets
        genNonNeg = ((* 1_000_000) . (+ 1)) . QC.getNonNegative <$> QC.arbitrary
        genDeadline = Slot . QC.getNonNegative <$> QC.arbitrary
        -- genDeadline = Hask.pure @QC.Gen (Slot 9999)
        genMaybePrice = QC.oneof [Hask.pure Nothing, Just <$> genNonNeg]
        genString = QC.listOf (QC.elements [Hask.minBound .. Hask.maxBound])
        genContent = MockContent . Content . fromString . ('x' :) <$> genString
        -- genTitle = Title . fromString <$> genString
        genTitle = Hask.pure (Title "")
        genShare = (% 100) <$> QC.elements [1 .. 99]
        genNftId = QC.elements nfts
     in QC.oneof
          [ Hask.pure ActionInit
          , ActionMint
              <$> genWallet
              <*> genContent
              <*> genTitle
              <*> genMaybePrice
              <*> genShare
          , ActionSetPrice
              <$> genWallet
              <*> genNftId
              <*> genMaybePrice
          , ActionBuy
              <$> genWallet
              <*> genNftId
              <*> genNonNeg
              <*> genMaybePrice
          , ActionAuctionOpen
              <$> genWallet
              <*> genNftId
              <*> genDeadline
              <*> genNonNeg
          , ActionAuctionBid
              <$> genWallet
              <*> genNftId
              <*> genNonNeg
          , ActionAuctionClose
              <$> genWallet
              <*> genNftId
          ]

  initialState = NftModel Map.empty 0 False

  precondition s ActionInit {} = not (s ^. contractState . mStarted)
  precondition s ActionMint {..} =
    (s ^. contractState . mStarted)
      && (s ^. contractState . mMintedCount <= 5)
      && not (Map.member (NftId . hashData . getMockContent $ aContent) (s ^. contractState . mMarket))
  precondition s ActionBuy {..} =
    (s ^. contractState . mStarted)
      && (s ^. contractState . mMintedCount > 0)
      && isJust ((s ^. contractState . mMarket . at aNftId) >>= _nftPrice)
      && (Just aPrice >= ((s ^. contractState . mMarket . at aNftId) >>= _nftPrice))
  precondition s ActionSetPrice {..} =
    (s ^. contractState . mStarted)
      && (s ^. contractState . mMintedCount > 0)
      && (Just aPerformer == (view nftOwner <$> (s ^. contractState . mMarket . at aNftId)))
      && isNothing ((s ^. contractState . mMarket . at aNftId) >>= _nftAuctionState)
  precondition s ActionAuctionOpen {..} =
    (s ^. contractState . mStarted)
      && (s ^. contractState . mMintedCount > 0)
      && (Just aPerformer == (view nftOwner <$> (s ^. contractState . mMarket . at aNftId)))
      && isNothing ((s ^. contractState . mMarket . at aNftId) >>= _nftAuctionState)
  precondition s ActionAuctionBid {..} =
    (s ^. contractState . mStarted)
      && (s ^. contractState . mMintedCount > 0)
      && isJust ((s ^. contractState . mMarket . at aNftId) >>= _nftAuctionState)
      && (Just aBid > (view auctionMinBid <$> ((s ^. contractState . mMarket . at aNftId) >>= _nftAuctionState)))
      && (Just aBid > (fst <$> ((s ^. contractState . mMarket . at aNftId) >>= _nftAuctionState >>= _auctionHighestBid)))
      && (Just (s ^. currentSlot + 1) < (view auctionDeadline <$> ((s ^. contractState . mMarket . at aNftId) >>= _nftAuctionState)))
  precondition s ActionAuctionClose {..} =
    (s ^. contractState . mStarted)
      && (s ^. contractState . mMintedCount > 0)
      && isJust ((s ^. contractState . mMarket . at aNftId) >>= _nftAuctionState)
      && (Just (s ^. currentSlot) > (view auctionDeadline <$> ((s ^. contractState . mMarket . at aNftId) >>= _nftAuctionState)))

  nextState ActionInit {} = do
    mStarted $= True
    wait 5
  nextState ActionMint {..} = do
    s <- view contractState <$> getModelState
    let nft =
          MockNft
            { _nftId = NftId . hashData . getMockContent $ aContent
            , _nftPrice = aNewPrice
            , _nftOwner = aPerformer
            , _nftAuthor = aPerformer
            , _nftShare = aShare
            , _nftAuctionState = Nothing
            }
    let nft' = s ^. mMarket . at (nft ^. nftId)
    case nft' of
      Nothing -> do
        mMarket $~ Map.insert (nft ^. nftId) nft
        mMintedCount $~ (+ 1)
      Just _ -> Hask.pure () -- NFT is already minted
    wait 5
  nextState ActionSetPrice {..} = do
    s <- view contractState <$> getModelState
    let nft' = s ^. mMarket . at aNftId
    case nft' of
      Nothing -> Hask.pure () -- NFT not found
      Just nft -> do
        when ((nft ^. nftOwner) == aPerformer && isNothing (nft ^. nftAuctionState)) $ do
          let newNft = set nftPrice aNewPrice nft
          mMarket $~ Map.insert aNftId newNft
    wait 5
  nextState ActionBuy {..} = do
    s <- view contractState <$> getModelState
    let nft' = s ^. mMarket . at aNftId
    case nft' of
      Nothing -> Hask.pure () -- NFT not found
      Just nft -> case nft ^. nftPrice of
        Nothing -> Hask.pure () -- NFT is locked
        Just nftPrice' -> do
          when (nftPrice' <= aPrice) $ do
            let newNft = set nftOwner aPerformer . set nftPrice aNewPrice $ nft
                feeValue = round $ fromInteger aPrice * feeRate
                (ownerShare, authorShare) = calculateShares (aPrice - feeValue) (nft ^. nftShare)
            mMarket $~ Map.insert aNftId newNft
            transfer aPerformer (nft ^. nftOwner) ownerShare
            transfer aPerformer (nft ^. nftAuthor) authorShare
            withdraw aPerformer (lovelaceValueOf feeValue)
            deposit aPerformer (mkFreeGov aPerformer feeValue)
    wait 5
  nextState ActionAuctionOpen {..} = do
    s <- view contractState <$> getModelState
    let nft' = s ^. mMarket . at aNftId
    case nft' of
      Nothing -> Hask.pure () -- NFT not found
      Just nft -> do
        when ((nft ^. nftOwner) == aPerformer && isNothing (nft ^. nftAuctionState)) $ do
          let ac =
                MockAuctionState
                  { _auctionHighestBid = Nothing
                  , _auctionDeadline = aDeadline
                  , _auctionMinBid = aMinBid
                  }
              newNft =
                set nftAuctionState (Just ac) $
                  set nftPrice Nothing nft
          mMarket $~ Map.insert aNftId newNft
    wait 5
  nextState ActionAuctionBid {..} = do
    s <- view contractState <$> getModelState
    curSlot <- view currentSlot <$> getModelState
    let nft' = s ^. mMarket . at aNftId
    case nft' of
      Nothing -> Hask.pure () -- NFT not found
      Just nft -> case nft ^. nftAuctionState of
        Nothing -> Hask.pure () -- NFT not on auction
        Just ac -> do
          when (ac ^. auctionDeadline > curSlot) $ do
            let newAc = set auctionHighestBid (Just (aBid, aPerformer)) ac
                newNft = set nftAuctionState (Just newAc) nft
            case ac ^. auctionHighestBid of
              Nothing -> do
                -- First bid
                when (ac ^. auctionMinBid <= aBid) $ do
                  mMarket $~ Map.insert aNftId newNft
                  withdraw aPerformer (lovelaceValueOf aBid)
              Just hb ->
                -- Next bid
                when (fst hb < aBid) $ do
                  mMarket $~ Map.insert aNftId newNft
                  deposit (snd hb) (lovelaceValueOf . fst $ hb)
                  withdraw aPerformer (lovelaceValueOf aBid)
    wait 10
  nextState ActionAuctionClose {..} = do
    s <- view contractState <$> getModelState
    curSlot <- view currentSlot <$> getModelState
    let nft' = s ^. mMarket . at aNftId
    case nft' of
      Nothing -> Hask.pure () -- NFT not found
      Just nft -> case nft ^. nftAuctionState of
        Nothing -> Hask.pure () -- NFT not on auction
        Just ac -> do
          when (ac ^. auctionDeadline < curSlot) $ do
            case ac ^. auctionHighestBid of
              Nothing -> do
                -- No bids
                let newNft = set nftAuctionState Nothing nft
                mMarket $~ Map.insert aNftId newNft
              Just hb -> do
                let newOwner = snd hb
                    newNft = set nftOwner newOwner $ set nftAuctionState Nothing nft
                    price = fst hb
                    feeValue = round $ fromInteger price * feeRate
                    (ownerShare, authorShare) = calculateShares (price - feeValue) (nft ^. nftShare)
                mMarket $~ Map.insert aNftId newNft
                deposit (nft ^. nftOwner) ownerShare
                deposit (nft ^. nftAuthor) authorShare
                deposit newOwner (mkFreeGov newOwner feeValue)
    wait 5

  perform h _ = \case
    ActionInit -> do
      let hAdmin = h $ InitKey wAdmin
      callEndpoint @"app-init" hAdmin [toUserId wAdmin]
      void $ Trace.waitNSlots 5
    ActionMint {..} -> do
      let h1 = h $ UserKey aPerformer
      callEndpoint @"mint" h1 $
        MintParams
          { mp'content = getMockContent aContent
          , mp'title = aTitle
          , mp'share = aShare
          , mp'price = aNewPrice
          }
      void $ Trace.waitNSlots 5
    ActionSetPrice {..} -> do
      let h1 = h $ UserKey aPerformer
      callEndpoint @"set-price" h1 $
        SetPriceParams
          { sp'nftId = aNftId
          , sp'price = aNewPrice
          }
      void $ Trace.waitNSlots 5
    ActionBuy {..} -> do
      let h1 = h $ UserKey aPerformer
      callEndpoint @"buy" h1 $
        BuyRequestUser
          { ur'nftId = aNftId
          , ur'newPrice = aNewPrice
          , ur'price = aPrice
          }
      void $ Trace.waitNSlots 5
    ActionAuctionOpen {..} -> do
      let h1 = h $ UserKey aPerformer
      callEndpoint @"auction-open" h1 $
        AuctionOpenParams
          { op'nftId = aNftId
          , op'deadline = slotToBeginPOSIXTime def aDeadline
          , op'minBid = aMinBid
          }
      void $ Trace.waitNSlots 5
    ActionAuctionBid {..} -> do
      let h1 = h $ UserKey aPerformer
      callEndpoint @"auction-bid" h1 $
        AuctionBidParams
          { bp'nftId = aNftId
          , bp'bidAmount = aBid
          }
      void $ Trace.waitNSlots 9
      callEndpoint @"query-list-nfts" h1 ()
      void $ Trace.waitNSlots 1
    ActionAuctionClose {..} -> do
      let h1 = h $ UserKey aPerformer
      callEndpoint @"auction-close" h1 $
        AuctionCloseParams
          { cp'nftId = aNftId
          }
      void $ Trace.waitNSlots 5

deriving instance Hask.Eq (ContractInstanceKey NftModel w s e)
deriving instance Hask.Show (ContractInstanceKey NftModel w s e)

feeRate :: Rational
feeRate = 5 % 1000

-- We do not have any better way for testing GOV than hardcoding GOV currency.
-- If tests fail after updating validator change currency here.
mkFreeGov :: Wallet -> Integer -> Plutus.V1.Ledger.Value.Value
mkFreeGov wal = assetClassValue (AssetClass (cur, tn))
  where
    tn = TokenName . ("freeGov" <>) . getPubKeyHash . getUserId . toUserId $ wal
    cur = "8db955eed8cebff614f8aff4a6dac4c99f4714e2fe282dd80143912a"

appSymbol :: UniqueToken
appSymbol = AssetClass ("038ecf2f85dcb99b41d7ebfcbc0d988f4ac2971636c3e358aa8d6121", "Unique App Token")

wallets :: [Wallet]
wallets = [w1, w2, w3]

wAdmin :: Wallet
wAdmin = wA

instanceSpec :: [ContractInstanceSpec NftModel]
instanceSpec =
  [ ContractInstanceSpec (InitKey wAdmin) wAdmin adminEndpoints
  ]
    <> Hask.fmap (\w -> ContractInstanceSpec (UserKey w) w (endpoints appSymbol)) wallets

propContract :: Actions NftModel -> QC.Property
propContract =
  QC.withMaxSuccess 50
    . propRunActionsWithOptions -- Keeping 50 tests limits time to ~1m, 100 tests took ~8m
      checkOptions
      instanceSpec
      (const $ Hask.pure True)

test :: TestTree
test = testGroup "QuickCheck" [testProperty "Contract" propContract]
