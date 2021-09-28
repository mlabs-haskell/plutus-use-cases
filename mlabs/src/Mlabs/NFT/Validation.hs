{-# LANGUAGE UndecidableInstances #-}

module Mlabs.NFT.Validation ()
where
import qualified Prelude as Hask
import qualified PlutusTx 
import qualified Plutus.V1.Ledger.Scripts as Scripts
import qualified Plutus.Contract as Contract
import qualified Ledger.Value as Value
import qualified Ledger.Typed.Scripts as Scripts
import qualified Ledger.Constraints as Constraints
import PlutusTx.Prelude
import Plutus.V1.Ledger.Value (TokenName (..), tokenName, flattenValue, CurrencySymbol)
import Plutus.V1.Ledger.Tx (TxOutRef(..))
import Plutus.V1.Ledger.Crypto (PubKeyHash (..))
import Plutus.V1.Ledger.Contexts (TxInInfo(..), ScriptContext(..),TxInfo(..))
import Plutus.Trace.Emulator (activateContractWallet, callEndpoint, EmulatorTrace, runEmulatorTraceIO)
import Plutus.Contract ( endpoint, Endpoint, type (.\/), Contract )
import Playground.Contract (ToSchema, TxOutRef, mkSchemaDefinitions, TokenName (TokenName))
import Mlabs.Utils.Wallet (walletFromNumber)
import Mlabs.Plutus.Contract (getEndpoint, selectForever, Call, IsEndpoint(..))
import GHC.Generics (Generic)
import Plutus.V1.Ledger.Address (Address)
import Ledger.Address (pubKeyAddress)
import Ledger.Contexts (scriptCurrencySymbol)
import Data.Void (Void)
import Data.Aeson (FromJSON, ToJSON, Value (Bool))
import qualified Data.Map as Map
import           Data.Text              (Text)
import           Text.Printf            (printf)
import Control.Monad ( void )


newtype UserId = UserId {getUserId :: PubKeyHash}
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''UserId

-- | Unique identifier of NFT.
data NftId = NftId
  { -- | token name is identified by content of the NFT (it's hash of it)
    nftId'token :: TokenName
  , -- | TxOutRef that is used for minting of NFT,
    -- with it we can guarantee uniqueness of NFT
    nftId'outRef :: TxOutRef
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''NftId
PlutusTx.makeLift ''NftId

-- | Data for NFTs
data Nft = Nft
  { -- | token name, unique identifier for NFT
    nft'id :: NftId
  , -- | data (media, audio, photo, etc)
    nft'data :: BuiltinByteString
  , -- | share for the author on each sell
    nft'share :: Rational
  , -- | author
    nft'author :: UserId
  , -- | current owner
    nft'owner :: UserId
  , -- | price in ada, if it's nothing then nobody can buy
    nft'price :: Maybe Integer
  }
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
PlutusTx.unstableMakeIsData ''Nft

{-# INILINABLE mkMintPolicy #-}
mkMintPolicy :: Address -> NftId -> () -> ScriptContext -> Bool
mkMintPolicy _ nid _ context = 
    traceIfFalse "UTxO not consumed" hasUTXo
    && traceIfFalse "Wrong amount minted" checkMintedAmount
    
    where
        info :: TxInfo
        info = scriptContextTxInfo context

        oref = nid.nftId'outRef

        hasUTXo :: Bool 
        hasUTXo = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info
        
        checkMintedAmount :: Bool 
        checkMintedAmount = case flattenValue $ txInfoMint info of
            [(_,_,v)] -> v == 1
            _ -> False

mintPolicy :: Address -> NftId -> Scripts.MintingPolicy
mintPolicy stateAddr nid =
  Scripts.mkMintingPolicyScript $
    $$(PlutusTx.compile [||\x y -> Scripts.wrapMintingPolicy (mkMintPolicy x y)||])
      `PlutusTx.applyCode` PlutusTx.liftCode stateAddr
      `PlutusTx.applyCode` PlutusTx.liftCode nid

-- newtype Mint = Mint Nft
--   deriving stock (Hask.Show, Generic, Hask.Eq)
--   deriving newtype (FromJSON, ToJSON)
--   deriving anyclass (ToSchema)

-- instance IsEndpoint Mint where
--   type EndpointSymbol Mint = "mint"

type NFTSchema = Endpoint "mint" Nft

mkSchemaDefinitions ''NFTSchema

-- | Calculate the currency symbol of the NFT.
curSymbol :: Address -> NftId -> CurrencySymbol
curSymbol stateAddr nid = scriptCurrencySymbol $ mintPolicy stateAddr nid

-- | Mints an NFT an returns it.
mint :: Nft -> Contract w NFTSchema Text ()
mint nft = do
    pk    <- Contract.ownPubKey
    utxos <- Contract.utxosAt (pubKeyAddress pk)
    case Map.keys utxos of
        []       -> Contract.logError @Hask.String "no utxo found"
        oref : _ -> do
            let nftid   = NftId  (tokenName $ nft.nft'dat) oref
                val     = Value.singleton (curSymbol nftid) nftid.token 1
                lookups = Constraints.mintingPolicy (mintPolicy $ NftId oref nft) <> Constraints.unspentOutputs utxos
                tx      = Constraints.mustMintValue val <> Constraints.mustSpendPubKeyOutput oref
            ledgerTx <- Contract.submitTxConstraintsWith @Void lookups tx
            void $ Contract.awaitTxConfirmed $ txId ledgerTx
            Contract.logInfo @Hask.String $ printf "forged %s" (Hask.show val)

endpoints :: Contract w NFTSchema Text ()
endpoints = Contract.awaitPromise $ endpoint @"mint" mint

nftInit :: TxOutRef -> UserId -> Nft
nftInit oref user = Nft 
    { nft'id = NftId 
        { nftId'token = (TokenName "some random hash")
        , nftId'outRef = oref}
    , nft'data = "Audio"
    , nft'share = 1 % 10
    , nft'author = user 
    , nft'owner = user 
    , nft'price = Just 10
    }

eTrace1 :: EmulatorTrace ()
eTrace1 = do
    let nft = nftInit
    h1 <- activateContractWallet (walletFromNumber 1) endpoints
    --callEndpoint h1 @"mint"
    return ()

test :: Hask.IO ()
test = runEmulatorTraceIO $ eTrace1

