{-# LANGUAGE GADTs #-}

module Test.NFT.QuickCheck where

import Control.Lens hiding (elements)
import Control.Monad (unless, void, when)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Monoid (Last (..))
import Data.String (IsString (..))
import Data.Text (Text)
import Plutus.Contract.Test (Wallet (..))
import Plutus.Contract.Test.ContractModel (Action, Actions, ContractInstanceSpec (..), ContractModel (..), contractState, getModelState, propRunActionsWithOptions, wait, ($=), ($~))
import Plutus.Trace.Emulator (EmulatorRuntimeError (..), activateContractWallet, callEndpoint, observableState, throwError, waitNSlots)
import PlutusTx.Prelude hiding (fmap, length, mconcat, unless, (<$>), (<*>), (==))
import Test.QuickCheck qualified as QC
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Prelude (div, fmap, (<$>), (<*>), (==))
import Prelude qualified as Hask

import Mlabs.NFT.Contract
import Mlabs.NFT.Types
import Test.NFT.Init

data NftModel = NftModel
  { _mPrice :: Maybe Integer
  , _mOwner :: Wallet
  , _mAuthor :: Wallet
  , _mMinted :: Bool
  , _mWallets :: Map Wallet Integer
  }
  deriving (Hask.Show, Hask.Eq)
makeLenses ''NftModel

instance ContractModel NftModel where
  data Action NftModel
    = Mint
    | SetPrice Wallet (Maybe Integer)
    | Buy Wallet Integer (Maybe Integer)
    deriving (Hask.Show, Hask.Eq)

  data ContractInstanceKey NftModel w s e where
    Key :: Wallet -> ContractInstanceKey NftModel (Last NftId) NFTAppSchema Text

  instanceTag key _ = fromString $ Hask.show key

  arbitraryAction _ =
    QC.oneof
      [ Hask.pure Mint
      , SetPrice <$> genWallet <*> QC.oneof [Hask.pure Nothing, Just <$> genNonNeg]
      , Buy <$> genWallet <*> genNonNeg <*> QC.oneof [Hask.pure Nothing, Just <$> genNonNeg]
      ]

  initialState = NftModel Nothing w1 w1 False $ Map.fromList [(w1, 1000), (w2, 1000), (w3, 1000)]

  nextState Mint = do
    mMinted $= True
  nextState (SetPrice wal price) = do
    s <- view contractState <$> getModelState
    if s ^. mOwner == wal && s ^. mMinted
      then mPrice $= price
      else Hask.pure ()
    wait 10
  nextState (Buy wal price newPrice) = do
    s <- view contractState <$> getModelState
    let currPrice = s ^. mPrice
    let authorShare = price `div` 10
    let ownerShare = price - authorShare
    if s ^. mWallets . at wal >= Just price && Just price >= currPrice && isJust currPrice && s ^. mMinted
      then do
        (mWallets . at (s ^. mAuthor)) $~ fmap (+ authorShare)
        (mWallets . at (s ^. mOwner)) $~ fmap (+ ownerShare)
        (mWallets . at wal) $~ fmap (Hask.subtract price)
        mOwner $= wal
        mPrice $= newPrice
      else Hask.pure ()
    wait 10

  precondition s Mint = not (s ^. contractState . mMinted)
  precondition s _ = s ^. contractState . mMinted

  perform _ s Mint = do
    unless (s ^. contractState . mMinted) $ do
      hdl <- activateContractWallet w1 endpoints
      void $ callEndpoint @"mint" hdl mp
      void $ waitNSlots 10
      Last _ <- observableState hdl
      Hask.pure ()
  perform _ s (Buy wal price newPrice) = do
    when (s ^. contractState . mMinted) $ do
      hdl <- activateContractWallet wal endpoints
      Last nid <- observableState hdl
      case nid of
        Just nftId -> callEndpoint @"buy" hdl (BuyRequestUser nftId price newPrice Nothing)
        Nothing -> throwError $ GenericError "NFT not minted"
      void $ waitNSlots 10
  perform _ s (SetPrice wal price) = do
    when (s ^. contractState . mMinted) $ do
      hdl <- activateContractWallet wal endpoints
      Last nid <- observableState hdl
      case nid of
        Just nftId -> callEndpoint @"set-price" hdl (SetPriceParams nftId price Nothing)
        Nothing -> throwError $ GenericError "NFT not minted"
      void $ waitNSlots 10

deriving instance Hask.Eq (ContractInstanceKey NftModel w s e)
deriving instance Hask.Show (ContractInstanceKey NftModel w s e)

wallets :: [Wallet]
wallets = [w1, w2, w3]

-- | Random wallet
genWallet :: QC.Gen Wallet
genWallet = QC.elements wallets

-- | Random non negative integer
genNonNeg :: QC.Gen Integer
genNonNeg = QC.getNonNegative <$> QC.arbitrary

instanceSpec :: [ContractInstanceSpec NftModel]
instanceSpec = Hask.pure $ ContractInstanceSpec (Key w1) w1 endpoints

propContract :: Actions NftModel -> QC.Property
propContract =
  propRunActionsWithOptions
    checkOptions
    instanceSpec
    (const $ Hask.pure True)

test :: TestTree
test = testGroup "QuickCheck" [testProperty "Contract" propContract]
