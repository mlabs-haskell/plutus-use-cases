module Mlabs.NFT.Contract (
  QueryContract,
  UserContract,
  GenericContract,
  mint,
  buy,
  setPrice,
  initApp,
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

import Mlabs.NFT.Types (
  BuyRequestUser (..),
  Content (..),
  MintAct (..),
  MintParams (..),
  NftId (..),
  QueryResponse (..),
  SetPriceParams (..),
  UserId (..),
 )

import Mlabs.Plutus.Contract (readDatum')

import Mlabs.NFT.Validation

-- | A contract used exclusively for query actions.
type QueryContract a = forall s. Contract QueryResponse s Text a

-- | A contract used for all user actions.
type UserContract a = forall s. Contract (Last NftId) s Text a

-- | A Generic Contract used for aux functions and helpers.
type GenericContract a = forall w s. Contract w s Text a

--------------------------------------------------------------------------------
-- Init --

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

initApp :: GenericContract ()
initApp = do
  createListHead
  Contract.logInfo @Hask.String $ printf "Finished Initialisation"

--------------------------------------------------------------------------------
-- MINT --

-- | Mints an NFT and sends it to the App Address.
mint ::  a -> UserContract ()
mint nftContent = error ()

--  addr <- getUserAddr
--  datum <- nftInit nftContent
--  utxos <- Contract.utxosAt addr
--  oref <- fstUtxo addr
--  let nftId = dNft'id datum
--      scrAddress = txScrAddress
--      nftPolicy = mintPolicy scrAddress oref nftId
--      tokenName = nftTokenName datum
--      mintRedeemer = asRedeemer $ Mint tokenName
--      val = Value.singleton (scriptCurrencySymbol nftPolicy) tokenName 1
--      (lookups, tx) =
--        ( mconcat
--            [ Constraints.unspentOutputs utxos
--            , Constraints.mintingPolicy nftPolicy
--            , Constraints.typedValidatorLookups txPolicy
--            ]
--        , mconcat
--            [ Constraints.mustMintValueWithRedeemer mintRedeemer val
--            , Constraints.mustSpendPubKeyOutput oref
--            , Constraints.mustPayToTheScript datum val
--            ]
--        )
--  void $ Contract.submitTxConstraintsWith @NftTrade lookups tx
--  Contract.tell . Last . Just $ nftId
--  Contract.logInfo @Hask.String $ printf "forged %s" (Hask.show val)

{- | Request tells if a Datum and its coin were produced correctly.
 testAuthenticNFT :: NftId -> GenericContract Bool
 testAuthenticNFT nftid = error ()
-}
getScriptAddrUtxos :: GenericContract (Map.Map TxOutRef (ChainIndexTxOut, ChainIndexTx))
getScriptAddrUtxos = utxosTxOutTxAt txScrAddress

-- | Initialise an NFT using the current wallet.
nftHeadInit :: NftAppInstance -> GenericContract DatumNft
nftHeadInit appInst = do
  pure .
    HeadDatum $
      NftListHead
        { head'next = Nothing
        , head'appInstance = appInst
        }

-- | Initialise new NftId
nftIdInit :: MintParams -> Contract w s Text NftId
nftIdInit mP = do
  userAddress <- getUserAddr
  let hData = hashData $ mp'content mP
  pure $ NftId { nftId'contentHash = hData }

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
setPrice :: SetPriceParams -> UserContract ()
setPrice spParams = error ()
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

-- HELPER FUNCTIONS AND CONTRACTS --

-- | Get the current Wallet's publick key.
getUserAddr :: Contract w s Text Address
getUserAddr = pubKeyAddress <$> Contract.ownPubKey

-- | Get the current wallet's utxos.
getUserUtxos :: Contract w s Text (Map.Map TxOutRef Ledger.ChainIndexTxOut)
getUserUtxos = getAddrUtxos =<< getUserAddr

-- | Get the current wallet's userId.
getUId :: Contract w s Text UserId
getUId = UserId . pubKeyHash <$> Contract.ownPubKey

-- | Get the ChainIndexTxOut at an address.
getAddrUtxos :: Address -> Contract w s Text (Map.Map TxOutRef ChainIndexTxOut)
getAddrUtxos adr = Map.map fst <$> utxosTxOutTxAt adr

-- | Get the ChainIndexTxOut at an address.
getAddrValidUtxos :: NftAppSymbol -> Contract w s Text (Map.Map TxOutRef (ChainIndexTxOut, ChainIndexTx))
getAddrValidUtxos appSymbol = Map.filter validTx <$> utxosTxOutTxAt txScrAddress
  where
    -- | Check if the given transactions contain the correct Currency Symbol.
    validTx (cIxTxOut, _) = elem (app'symbol appSymbol) $ symbols (cIxTxOut ^. ciTxOutValue)

-- | Serialise Datum
serialiseDatum :: PlutusTx.ToData a => a -> Datum
serialiseDatum = Datum . PlutusTx.toBuiltinData

-- | Returns the Datum of a specific nftId from the Script address.
getNftDatum ::  NftId -> NftAppSymbol -> Contract w s Text (Maybe DatumNft)
getNftDatum nftId appSymbol = do
   utxos :: [Ledger.ChainIndexTxOut] <- fmap fst . Map.elems <$> getAddrValidUtxos appSymbol
   let datums :: [DatumNft] =
         utxos
           ^.. traversed . Ledger.ciTxOutDatum
             . _Right
             . to (PlutusTx.fromBuiltinData @DatumNft . getDatum)
             . _Just
             . filtered (
                \d -> case d of
                  HeadDatum _ -> False
                  NodeDatum node ->
                    let nftId' = info'id . node'information $ node
                    in nftId' == nftId
                )
   Contract.logInfo @Hask.String $ Hask.show $ "Datum Found:" <> Hask.show datums
   Contract.logInfo @Hask.String $ Hask.show $ "Datum length:" <> Hask.show (Hask.length datums)
   case datums of
     [x] ->
       pure $ Just x
     [] -> do
       Contract.logError @Hask.String "No suitable Datum can be found."
       pure Nothing
     _ : _ -> do
       Contract.logError @Hask.String "More than one suitable Datum can be found. This should never happen."
       pure Nothing

{- | Gets the Datum of a specific nftId from the Script address, and applies an
  extraction function to it.
 -}
getsNftDatum :: (DatumNft -> b) -> NftId -> NftAppSymbol -> Contract a s Text (Maybe b)
getsNftDatum f nftId  = fmap (fmap f) . getNftDatum nftId 

-- | A hashing function to minimise the data to be attached to the NTFid.
hashData :: Content -> BuiltinByteString
hashData (Content b) = sha2_256 b

-- | Find NFTs at a specific Address. Will throw an error if none or many are found.
findNft :: NftId -> NftAppSymbol -> Contract w s Text (TxOutRef, ChainIndexTxOut, DatumNft)
findNft nftId cSymbol = do
   utxos <-  getAddrValidUtxos cSymbol
   case findData utxos of
     [v] -> do
       Contract.logInfo @Hask.String $ Hask.show $ "NFT Found:" <> Hask.show v
       pure v
     [] -> Contract.throwError $ "DatumNft not found for " <> (pack . Hask.show) nftId
     _ ->
       Contract.throwError $
         "Should not happen! More than one DatumNft found for "
           <> (pack . Hask.show) nftId
   where
     findData =
       L.filter hasCorrectNft -- filter only datums with desired NftId
         . mapMaybe readTxData -- map to Maybe (TxOutRef, ChainIndexTxOut, DatumNft)
         . Map.toList

     readTxData (oref, (ciTxOut, _)) = (oref,ciTxOut,) <$> readDatum' ciTxOut

     hasCorrectNft (_, ciTxOut, datum) =
       let (cs, tn) = unAssetClass $ nftAsset datum
        in tn == nftTokenName datum -- sanity check
           && case datum of
                NodeDatum datum' ->
                  (info'id . node'information $ datum') == nftId -- check that Datum has correct NftId
                  && valueOf (ciTxOut ^. ciTxOutValue) cs tn == 1 -- check that UTXO has single NFT in Value
                HeadDatum _ -> False
