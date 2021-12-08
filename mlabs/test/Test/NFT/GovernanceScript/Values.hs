module Test.NFT.GovernanceScript.Values where

import Data.Aeson qualified as Aeson
import Data.Maybe (fromJust)
import Ledger qualified

import Ledger.Value (TokenName (..))
import Ledger.Value qualified as Value

import Ledger.CardanoWallet qualified as CardanoWallet
import Mlabs.NFT.Contract.Aux qualified as NFT

import Mlabs.NFT.Governance qualified as Gov
import Mlabs.NFT.Types (Content (..), NftAppInstance (..), NftAppSymbol (..), NftId (..), UserId (..))

import Mlabs.NFT.Validation qualified as NFT
import Plutus.V1.Ledger.Ada qualified as Ada
import PlutusTx.Prelude
import Wallet.Emulator.Wallet qualified as Emu

-- test values

-- NFT Author
authorWallet :: Emu.Wallet
authorWallet = Emu.fromWalletNumber (CardanoWallet.WalletNumber 1)

authorAddr :: Ledger.Address
authorAddr = Emu.walletAddress authorWallet

authorPkh :: Ledger.PubKeyHash
authorPkh = Emu.walletPubKeyHash authorWallet

-- User 1
userOneWallet :: Emu.Wallet
userOneWallet = Emu.fromWalletNumber (CardanoWallet.WalletNumber 2)

userOnePkh :: Ledger.PubKeyHash
userOnePkh = Emu.walletPubKeyHash userOneWallet

-- User 2
userTwoWallet :: Emu.Wallet
userTwoWallet = Emu.fromWalletNumber (CardanoWallet.WalletNumber 3)

userTwoPkh :: Ledger.PubKeyHash
userTwoPkh = Emu.walletPubKeyHash userTwoWallet

-- User 3
userThreeWallet :: Emu.Wallet
userThreeWallet = Emu.fromWalletNumber (CardanoWallet.WalletNumber 4)

userThreePkh :: Ledger.PubKeyHash
userThreePkh = Emu.walletPubKeyHash userThreeWallet

testTxId :: Ledger.TxId
testTxId = fromJust $ Aeson.decode "{\"getTxId\" : \"61626364\"}"

testTokenName :: TokenName
testTokenName = TokenName emptyByteString

oneProofToken :: Value.Value
oneProofToken = Value.singleton nftCurrencySymbol testTokenName 1

oneUniqueToken :: Value.Value
oneUniqueToken = Value.assetClassValue uniqueAsset 1

uniqueAndProofTokens :: Value.Value
uniqueAndProofTokens = oneProofToken <> oneUniqueToken

uniqueAndProofTokensPlus1Ada :: Value.Value
uniqueAndProofTokensPlus1Ada = uniqueAndProofTokens <> (adaValue 1)

nftPolicy :: Ledger.MintingPolicy
nftPolicy = NFT.mintPolicy appInstance

nftCurrencySymbol :: Value.CurrencySymbol
nftCurrencySymbol = app'symbol appSymbol

oneAda :: Value.Value
oneAda = Ada.lovelaceValueOf 1_000_000

adaValue :: Integer -> Value.Value
adaValue = Ada.lovelaceValueOf . (* 1_000_000)

userOneListGov :: Value.AssetClass
userOneListGov = Value.AssetClass (nftCurrencySymbol, TokenName ("listGov" <> (Ledger.getPubKeyHash userOnePkh)))

userOneFreeGov :: Value.AssetClass
userOneFreeGov = Value.AssetClass (nftCurrencySymbol, TokenName ("freeGov" <> (Ledger.getPubKeyHash userOnePkh)))

listGovTokens :: Value.Value
listGovTokens = Value.assetClassValue userOneListGov 1_000_000

freeGovTokens :: Value.Value
freeGovTokens = Value.assetClassValue userOneFreeGov 1_000_000

listAndFreeGovTokens :: Value.Value
listAndFreeGovTokens = listGovTokens <> freeGovTokens

-- testStateAddr :: Ledger.Address
testStateAddr = NFT.txScrAddress

{-
   We can't get rid of hard-coding the CurrencySymbol of UniqueToken at the moment since the mintContract produces it
   which works inside the Contract monad. Getting this value from our initApp endpoint need to encapsulate almost everything here
   to a Contract monad or using a similar approach such as ScriptM, which is operationally heavy and isn't worth doing.
   We can almost make sure that this value won't change unless upgrading weird things in plutus, or predetermining
   the initial state UTxOs to something other than the default.
-}

govScriptAddress :: Ledger.Address
govScriptAddress = Gov.govScrAddress uniqueAsset

govMintValidatorHash :: Ledger.ValidatorHash
govMintValidatorHash = fromJust $ Ledger.toValidatorHash govScriptAddress

uniqueAsset :: Value.AssetClass
uniqueAsset = Value.AssetClass ("00a6b45b792d07aa2a778d84c49c6a0d0c0b2bf80d6c1c16accdbe01", "Unique App Token")

appInstance :: NftAppInstance
appInstance = NftAppInstance (testStateAddr uniqueAsset) uniqueAsset govScriptAddress [UserId userOnePkh]

appSymbol :: NftAppSymbol
appSymbol = NftAppSymbol . NFT.curSymbol $ appInstance
