module Mlabs.NFT.Contract (
  QueryContract,
  UserContract,
  GenericContract,
  buy,
  queryCurrentOwner,
  queryCurrentPrice,
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

-- | A contract used exclusively for query actions.
type QueryContract a = forall s. Contract QueryResponse s Text a

-- | A contract used for all user actions.
type UserContract a = forall s. Contract (Last NftId) s Text a

{- | BUY.
 Attempts to buy a new NFT by changing the owner, pays the current owner and
 the author, and sets a new price for the NFT.
-}
buy :: BuyRequestUser -> UserContract ()
buy (BuyRequestUser nftId bid newPrice) = error ()

--   oldDatum' <- getNftDatum nftId
--   case oldDatum' of
--     Nothing -> Contract.logError @Hask.String "NFT Cannot be found."
--     Just oldDatum -> do
--       case dNft'price oldDatum of
--         Nothing -> Contract.logError @Hask.String "NFT not for sale."
--         Just price ->
--           if bid < price
--             then Contract.logError @Hask.String "Bid Price is too low."
--             else do
--               let scrAddress = txScrAddress
--                   mintORef = nftId'outRef . dNft'id $ oldDatum
--                   nftPolicy = mintPolicy scrAddress mintORef nftId
--                   prevTokenName = nftTokenName oldDatum
--               user <- getUId
--               userUtxos <- getUserUtxos
--               scriptUtxos <- Map.map fst <$> getScriptAddrUtxos
--               (nftOref, ciTxOut, _) <- findNft txScrAddress nftId
--               let -- Unserialised Datum
--                   newDatum' =
--                     DatumNft
--                       { dNft'id = dNft'id oldDatum
--                       , dNft'share = dNft'share oldDatum
--                       , dNft'author = dNft'author oldDatum
--                       , dNft'owner = user
--                       , dNft'price = newPrice
--                       }
--                   newCurrency = nftCurrency newDatum'
--                   newTokenName = nftTokenName newDatum'
--                   action =
--                     BuyAct
--                       { act'bid = bid
--                       , act'newPrice = newPrice
--                       , act'cs = newCurrency
--                       }
--
--                   -- Serialised Datum.
--                   newDatum = Datum . PlutusTx.toBuiltinData $ newDatum'
--                   (paidToAuthor, paidToOwner) = calculateShares bid $ dNft'share oldDatum
--
--                   -- TEST MINT
--                   -- nftPolicy = mintPolicy scrAddress nftOref nftId
--                   mintRedeemer = asRedeemer $ TxAction prevTokenName newTokenName
--
--                   valAAA = Value.singleton (scriptCurrencySymbol nftPolicy) prevTokenName 1
--                   negAAA = negate valAAA
--
--                   valBBB = Value.singleton (scriptCurrencySymbol nftPolicy) newTokenName 1
--                   newValue = valBBB
--
--                   (lookups, tx) =
--                     ( mconcat
--                         [ Constraints.unspentOutputs userUtxos
--                         , Constraints.typedValidatorLookups txPolicy
--                         , Constraints.mintingPolicy nftPolicy
--                         , Constraints.otherScript (validatorScript txPolicy)
--                         , Constraints.unspentOutputs $ Map.singleton nftOref ciTxOut -- first way of getting them
--                         , Constraints.unspentOutputs scriptUtxos -- second way of getting them
--                         -- , Constraints.unspentOutputs $ scriptUtxos' -- third way of getting them
--                         ]
--                     , mconcat
--                         [ Constraints.mustPayToTheScript newDatum' newValue
--                         , Constraints.mustIncludeDatum newDatum
--                         , Constraints.mustMintValueWithRedeemer mintRedeemer negAAA
--                         , Constraints.mustMintValueWithRedeemer mintRedeemer valBBB
--                         , Constraints.mustPayToPubKey (getUserId . dNft'owner $ oldDatum) paidToOwner
--                         , Constraints.mustPayToPubKey (getUserId . dNft'author $ oldDatum) paidToAuthor
--                         , Constraints.mustSpendScriptOutput nftOref (Redeemer . PlutusTx.toBuiltinData $ action)
--                         ]
--                     )
--               void $ Contract.logInfo @Hask.String $ printf "NEW VALUE %s" $ Hask.show newValue
--               void $ Contract.logInfo @Hask.String $ printf "SCRIPT VALUE %s" $ Hask.show (ciTxOut ^. ciTxOutValue)
--               void $ Contract.logInfo @Hask.String $ printf "TO AUTH %s" $ Hask.show paidToAuthor
--               void $ Contract.logInfo @Hask.String $ printf "TO OWNER %s" $ Hask.show paidToOwner
--               void $ Contract.logInfo @Hask.String $ printf "NEG AAA %s" $ Hask.show negAAA
--               void $ Contract.submitTxConstraintsWith @NftTrade lookups tx
--
-- -- void $ Contract.logInfo @Hask.String $ printf "Bought %s" $ Hask.show val

-- SET PRICE --
-- setPrice :: SetPriceParams -> UserContract ()
-- setPrice spParams = error ()
--   result <-
--     Contract.runError $ do
--       (oref, ciTxOut, datum) <- findNft txScrAddress $ sp'nftId spParams
--       runOffChainChecks datum
--       let (tx, lookups) = mkTxLookups oref ciTxOut datum
--       ledgerTx <- Contract.submitTxConstraintsWith @NftTrade lookups tx
--       void $ Contract.awaitTxConfirmed $ Ledger.txId ledgerTx
--   either Contract.logError (const $ Contract.logInfo @Hask.String "New price set") result
--   where
--     mkTxLookups oref ciTxOut datum =
--       let newDatum = datum {dNft'price = sp'price spParams}
--           redeemer = asRedeemer $ SetPriceAct (sp'price spParams) $ nftCurrency datum
--           newValue = ciTxOut ^. ciTxOutValue
--           lookups =
--             mconcat
--               [ Constraints.unspentOutputs $ Map.singleton oref ciTxOut
--               , Constraints.typedValidatorLookups txPolicy
--               , Constraints.otherScript (validatorScript txPolicy)
--               ]
--           tx =
--             mconcat
--               [ Constraints.mustSpendScriptOutput oref redeemer
--               , Constraints.mustPayToTheScript newDatum newValue
--               ]
--        in (tx, lookups)
--
--     runOffChainChecks :: DatumNft -> UserContract ()
--     runOffChainChecks datum = do
--       ownPkh <- pubKeyHash <$> Contract.ownPubKey
--       if isOwner datum ownPkh
--         then pure ()
--         else Contract.throwError "Only owner can set price"
--       if priceNotNegative (sp'price spParams)
--         then pure ()
--         else Contract.throwError "New price can not be negative"
--
--     isOwner datum pkh = pkh == (getUserId . dNft'owner) datum
--

{- | Query the current price of a given NFTid. Writes it to the Writer instance
 and also returns it, to be used in other contracts.
-}
queryCurrentPrice :: NftId -> NftAppSymbol -> QueryContract QueryResponse
queryCurrentPrice nftid cSymbol = error ()

--  price <- wrap <$> getsNftDatum dNft'price nftid
--  Contract.tell price >> log price >> return price
--  where
--    wrap = QueryCurrentPrice . Last . join
--    log price =
--      Contract.logInfo @Hask.String $
--        "Current price of: " <> Hask.show nftid <> " is: " <> Hask.show price

{- | Query the current owner of a given NFTid. Writes it to the Writer instance
 and also returns it, to be used in other contracts.
-}
queryCurrentOwner :: NftId -> NftAppSymbol -> QueryContract QueryResponse
queryCurrentOwner nftid cSymbol = error ()

--   ownerResp <- wrap <$> getsNftDatum dNft'owner nftid
--   Contract.tell ownerResp >> log ownerResp >> return ownerResp
--   where
--     wrap = QueryCurrentOwner . Last
--     log owner =
--       Contract.logInfo @Hask.String $
--         "Current owner of: " <> Hask.show nftid <> " is: " <> Hask.show owner
