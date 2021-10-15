module Mlabs.NFT.Endpoints.Init (
  initApp
) where

import PlutusTx.Prelude hiding (mconcat, (<>))
import Prelude (mconcat, (<>))
import Prelude qualified as Hask

import Control.Lens (filtered, to, traversed, (^.), (^..), _Just, _Right)
import Control.Monad (join, void)
import Data.List qualified as L
import Data.Map qualified as Map
import Data.Monoid (Last (..))
import Data.Text (Text, pack)
import Text.Printf (printf)

import Plutus.ChainIndex.Tx (ChainIndexTx)
import Plutus.Contract (Contract, mapError, ownPubKey, utxosTxOutTxAt)
import Plutus.Contract qualified as Contract
import PlutusTx qualified

import Plutus.Contracts.Currency (CurrencyError, mintContract, mintedValue)
import Plutus.Contracts.Currency qualified as MC
import Plutus.V1.Ledger.Value (TokenName (..), assetClass, currencySymbol, flattenValue, symbols)

import Ledger (
  Address,
  AssetClass,
  ChainIndexTxOut,
  Datum (..),
  Redeemer (..),
  TxOutRef,
  Value,
  ciTxOutDatum,
  ciTxOutValue,
  getDatum,
  pubKeyAddress,
  pubKeyHash,
  scriptCurrencySymbol,
  txId,
 )

import Ledger.Constraints qualified as Constraints
import Ledger.Typed.Scripts (validatorScript)
import Ledger.Value as Value (singleton, unAssetClass, valueOf)

import Mlabs.NFT.Types {- (
  BuyRequestUser (..),
  Content (..),
  MintAct (..),
  MintParams (..),
  NftId (..),
  QueryResponse (..),
  SetPriceParams (..),
  UserId (..),
 ) -}

import Mlabs.Plutus.Contract (readDatum')

import Mlabs.NFT.Validation

import Mlabs.NFT.Types
import Mlabs.NFT.Endpoints.Aux
import Mlabs.NFT.Validation

-- | A Generic Contract used for aux functions and helpers.
type GenericContract a = forall w s. Contract w s Text a

--------------------------------------------------------------------------------
-- Init --

initApp :: GenericContract ()
initApp = do
  createListHead
  Contract.logInfo @Hask.String $ printf "Finished Initialisation"

{- | Initialise the application at the address of the script by creating the
 HEAD of the list, and coupling the one time token with the Head of the list.
-}
createListHead :: GenericContract NftAppInstance
createListHead = do
  (uniqueToken, uniqueTokenValue) <- generateUniqueToken
  let appInstance = NftAppInstance txScrAddress uniqueToken
  headDatum <- nftHeadInit appInstance
  head <- mintListHead appInstance uniqueTokenValue headDatum
  return appInstance
  where
    -- Mint the Linked List Head and its associated token.
    mintListHead :: NftAppInstance -> Value -> DatumNft -> GenericContract ()
    mintListHead appInstance uniqueTokenValue headDatum = do
      utxos <- getUserUtxos
      let headPolicy = mintPolicy appInstance
          proofTokenValue = Value.singleton (scriptCurrencySymbol headPolicy) "HEAD" 1
          initRedeemer = asRedeemer Initialise
          (lookups, tx) =
            ( mconcat
                [ Constraints.typedValidatorLookups txPolicy
                , Constraints.mintingPolicy headPolicy
                ]
            , mconcat
                [ Constraints.mustPayToTheScript headDatum (uniqueTokenValue <> proofTokenValue)
                , Constraints.mustMintValueWithRedeemer initRedeemer proofTokenValue
                ]
            )
      void $ Contract.submitTxConstraintsWith @NftTrade lookups tx
      Contract.logInfo @Hask.String $ printf "forged HEAD for %s" (Hask.show appInstance)

    -- Contract that mints a unique token to be used in the minting of the head
    generateUniqueToken :: GenericContract (AssetClass, Value)
    generateUniqueToken = do
      self <- Ledger.pubKeyHash <$> ownPubKey
      let nftTokenName = TokenName PlutusTx.Prelude.emptyByteString
      x <-
        mapError
          (pack . Hask.show @CurrencyError)
          (mintContract self [(nftTokenName, 1)])
      return (assetClass (MC.currencySymbol x) nftTokenName, MC.mintedValue x)


-- | Initialise an NFT using the current wallet.
nftHeadInit :: NftAppInstance -> GenericContract DatumNft
nftHeadInit appInst = do
  pure .
    HeadDatum $
      NftListHead
        { head'next = Nothing
        , head'appInstance = appInst
        }
        