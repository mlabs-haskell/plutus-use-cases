module Test.NFT.Script.Values where

import qualified Ledger
import qualified Ledger.Address         as Ledger
import qualified Wallet.Emulator.Wallet as Emu
import qualified Plutus.V1.Ledger.Ada as Ada
import qualified Mlabs.NFT.Contract            as NFT
import           Mlabs.NFT.Types               (NftId (..), Content(..), Title(..))
import qualified Mlabs.NFT.Validation          as NFT
import qualified Data.Aeson                    as Aeson
import Data.Maybe (fromJust)
import qualified Ledger.Value as Value
import Ledger.Value (TokenName (..))
import           PlutusTx.Prelude hiding ((<>))

-- test values

-- NFT Author
authorWallet :: Emu.Wallet
authorWallet = Emu.fromWalletNumber (Emu.WalletNumber 1)

authorAddr :: Ledger.Address
authorAddr = Emu.walletAddress authorWallet

authorPk :: Ledger.PubKey
authorPk = Emu.walletPubKey authorWallet

authorPkh :: Ledger.PubKeyHash
authorPkh = Ledger.pubKeyHash authorPk

-- User 1
userOneWallet :: Emu.Wallet
userOneWallet = Emu.fromWalletNumber (Emu.WalletNumber 2)

-- User 2
userTwoWallet :: Emu.Wallet
userTwoWallet = Emu.fromWalletNumber (Emu.WalletNumber 3)

--
testOref :: Ledger.TxOutRef
testOref = Ledger.TxOutRef txId 1
  where txId = fromJust $ Aeson.decode
             -- FIXME: this is taken out of the first tx generated by tasty-plutus
             $ "{\"getTxId\" : \"61626364\"}"

testTokenName :: TokenName
testTokenName = TokenName hData
  where
    hData = NFT.hashData $ Content "A painting."

testNftId :: NftId
testNftId = NftId { nftId'title = Title "Fiona Lisa"
                  , nftId'token = testTokenName
                  , nftId'outRef = testOref
                  }

nftPolicy :: Ledger.MintingPolicy
nftPolicy = NFT.mintPolicy testStateAddr testOref testNftId

oneNft :: Value.Value
oneNft = Value.singleton (Ledger.scriptCurrencySymbol nftPolicy) testTokenName 1

oneAda :: Value.Value
oneAda = Ada.lovelaceValueOf 1_000_000

adaValue :: Integer -> Value.Value
adaValue = Ada.lovelaceValueOf . (* 1_000_000)

testStateAddr :: Ledger.Address
testStateAddr = NFT.txScrAddress