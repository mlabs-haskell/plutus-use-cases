module Mlabs.NFT.Contract where

import PlutusTx.Prelude hiding (mconcat, (<>))
import Prelude (mconcat, (<>))
import Prelude qualified as Hask

import Control.Lens (filtered, to, traversed, (^.), (^..), _Just, _Right)
import Control.Monad (forever, void)
import Data.List qualified as L
import Data.Map qualified as Map
import Data.Monoid (Last (..))
import Data.Text (Text, pack)

import Text.Printf (printf)

import Plutus.Contract (Contract, Endpoint, endpoint, type (.\/))
import Plutus.Contract qualified as Contract
import PlutusTx qualified

import Ledger (
  Address,
  AssetClass (..),
  ChainIndexTxOut,
  Datum (..),
  Redeemer (..),
  TxOutRef,
  ciTxOutDatum,
  ciTxOutValue,
  getDatum,
  pubKeyAddress,
  pubKeyHash,
  scriptCurrencySymbol,
  txId,
 )

import Ledger.Constraints qualified as Constraints
import Ledger.Typed.Scripts (
  validatorScript,
 )
import Ledger.Value as Value (TokenName (..), singleton, unAssetClass, valueOf)
import Playground.Contract (mkSchemaDefinitions)
import Plutus.V1.Ledger.Ada qualified as Ada

import Mlabs.NFT.Types
import Mlabs.NFT.Validation
import Mlabs.Plutus.Contract (readDatum', selectForever)

type AuthorContract a = Contract (Last NftId) NFTAppSchema Text a

type NFTAppSchema =
  Endpoint "buy" BuyRequestUser
    .\/ Endpoint "mint" MintParams
    .\/ Endpoint "set-price" SetPriceParams

mkSchemaDefinitions ''NFTAppSchema

-- MINT --

-- | Mints an NFT and sends it to the App Address.
mint :: MintParams -> AuthorContract ()
mint nftContent = do
  addr <- getUserAddr
  nft <- nftInit nftContent
  utxos <- Contract.utxosAt addr
  oref <- fstUtxo addr
  let nftId = nft.dNft'id
      scrAddress = txScrAddress
      nftPolicy = mintPolicy scrAddress oref nftId
      val = Value.singleton (scriptCurrencySymbol nftPolicy) nftId.nftId'token 1
      (lookups, tx) =
        ( mconcat
            [ Constraints.unspentOutputs utxos
            , Constraints.mintingPolicy nftPolicy
            , Constraints.typedValidatorLookups txPolicy
            ]
        , mconcat
            [ Constraints.mustMintValue val
            , Constraints.mustSpendPubKeyOutput oref
            , Constraints.mustPayToTheScript nft val
            ]
        )
  void $ Contract.submitTxConstraintsWith @NftTrade lookups tx
  Contract.tell $ Last . Just $ nftId
  Contract.logInfo @Hask.String $ printf "forged %s" (Hask.show val)

-- | Initialise an NFT using the current wallet.
nftInit :: MintParams -> Contract w s Text DatumNft
nftInit mintP = do
  user <- getUId
  nftId <- nftIdInit mintP
  pure $
    DatumNft
      { dNft'id = nftId
      , dNft'share = mintP.mp'share
      , dNft'author = user
      , dNft'owner = user
      , dNft'price = mintP.mp'price
      }

-- | Initialise new NftId
nftIdInit :: MintParams -> Contract w s Text NftId
nftIdInit mintP = do
  userAddress <- getUserAddr
  oref <- fstUtxo userAddress
  let hData = hashData mintP.mp'content
  pure $
    NftId
      { nftId'title = mintP.mp'title
      , nftId'token = TokenName hData
      , nftId'outRef = oref
      }

-- BUY --
buy :: BuyRequestUser -> Contract w NFTAppSchema Text ()
buy req@(BuyRequestUser nftId bid newPrice) = do
  oldDatum' <- getNftDatum nftId
  case oldDatum' of
    Nothing -> Contract.logError @Hask.String "NFT Cannot be found."
    Just oldDatum -> do
      let scrAddress = txScrAddress
          oref = oldDatum.dNft'id.nftId'outRef
          nftPolicy = mintPolicy scrAddress oref nftId
          val = Value.singleton (scriptCurrencySymbol nftPolicy) nftId.nftId'token 1
      case oldDatum.dNft'price of
        Nothing -> Contract.logError @Hask.String "NFT not for sale."
        Just price ->
          if bid Hask.< price
            then Contract.logError @Hask.String "Bid Price is too low."
            else do
              user <- getUId
              userUtxos <- getUserUtxos
              (oref, ciTxOut, datum) <- findNft txScrAddress nftId
              oref' <- fstUtxo =<< getUserAddr
              utxosAddr <- getScriptUtxos

              -- Unserialised Datum
              let newDatum' =
                    DatumNft
                      { dNft'id = oldDatum.dNft'id
                      , dNft'share = oldDatum.dNft'share
                      , dNft'author = oldDatum.dNft'author
                      , dNft'owner = user
                      , dNft'price = newPrice
                      }

                  action =
                    BuyAct
                      { act'bid = bid
                      , act'newPrice = newPrice
                      }
                  -- Serialised Datum
                  newDatum = Datum . PlutusTx.toBuiltinData $ newDatum'

                  convertToAda = Ada.toValue . Ada.Lovelace . (Hask.*) 1_000_000

                  authorShare' = round $ fromInteger bid * oldDatum.dNft'share
                  authorShare = convertToAda authorShare'
                  ownerShare' = bid Hask.- authorShare'
                  ownerShare = convertToAda ownerShare'

                  nftPolicy' = mintPolicy scrAddress oref' nftId
                  val' = Value.singleton (scriptCurrencySymbol nftPolicy') nftId.nftId'token 1

                  newValue = ciTxOut ^. ciTxOutValue

                  (lookups, tx) =
                    ( mconcat
                        [ Constraints.unspentOutputs userUtxos
                        , Constraints.typedValidatorLookups txPolicy
                        , Constraints.mintingPolicy nftPolicy'
                        , Constraints.otherScript (validatorScript txPolicy)
                        , Constraints.unspentOutputs $ Map.singleton oref ciTxOut
                        ]
                    , mconcat
                        [ Constraints.mustPayToTheScript newDatum' newValue
                        , Constraints.mustIncludeDatum newDatum
                        , Constraints.mustPayToPubKey (getUserId oldDatum.dNft'owner) ownerShare
                        , Constraints.mustPayToPubKey (getUserId oldDatum.dNft'author) authorShare
                        , Constraints.mustSpendScriptOutput oref (Redeemer . PlutusTx.toBuiltinData $ action)
                        ]
                    )
              void $ Contract.submitTxConstraintsWith @NftTrade lookups tx
              Contract.logInfo @Hask.String $ printf "Bought %s" $ Hask.show val

-- SET PRICE --
setPrice :: SetPriceParams -> Contract w NFTAppSchema Text ()
setPrice spParams = do
  result <-
    Contract.runError $ do
      ownPkh <- pubKeyHash <$> Contract.ownPubKey
      (oref, ciTxOut, datum) <- findNft txScrAddress (spParams.sp'nftId)
      if isOwner datum ownPkh
        then pure ()
        else Contract.throwError "Only owner can set price"
      let (tx, lookups) = mkTxLookups oref ciTxOut datum
      ledgerTx <- Contract.submitTxConstraintsWith @NftTrade lookups tx
      void $ Contract.awaitTxConfirmed $ Ledger.txId ledgerTx
  case result of
    Hask.Left e -> Contract.logError e
    Hask.Right _ -> Contract.logInfo @Hask.String "New price set"
  where
    mkTxLookups oref ciTxOut datum =
      let newDatum = datum {dNft'price = spParams.sp'price}
          redeemer = asRedeemer (SetPriceAct spParams.sp'price)
          newValue = ciTxOut ^. ciTxOutValue
          vScript = txPolicy
          lookups =
            mconcat
              [ Constraints.unspentOutputs $ Map.singleton oref ciTxOut
              , Constraints.typedValidatorLookups vScript
              , Constraints.otherScript (validatorScript vScript)
              ]
          tx =
            mconcat
              [ Constraints.mustSpendScriptOutput oref redeemer
              , Constraints.mustPayToTheScript newDatum newValue
              ]
       in (tx, lookups)
    isOwner datum pkh = pkh == datum.dNft'owner.getUserId

-- ENDPOINTS --
endpoints :: Contract (Last NftId) NFTAppSchema Text ()
endpoints = forever $ do
  Contract.awaitPromise $
    Hask.foldr1
      Contract.select
      [ endpoint @"mint" mint
      , endpoint @"buy" buy
      ]

userEndpoints :: Contract w NFTAppSchema Text ()
userEndpoints =
  selectForever [endpoint @"set-price" setPrice]

-- HELPER FUNCTIONS AND CONTRACTS --
getUserAddr :: Contract w s Text Address
getUserAddr = pubKeyAddress <$> Contract.ownPubKey

getUserUtxos :: Contract w s Text (Map.Map TxOutRef Ledger.ChainIndexTxOut)
getUserUtxos = Contract.utxosAt =<< getUserAddr

getScriptUtxos :: Contract w s Text (Map.Map TxOutRef Ledger.ChainIndexTxOut)
getScriptUtxos = Contract.utxosAt txScrAddress

getUId :: Contract w s Text UserId
getUId = UserId . pubKeyHash <$> Contract.ownPubKey

-- | Get the user's ChainIndexTxOut
getAddrUtxos :: Address -> Contract w s Text [Ledger.ChainIndexTxOut]
getAddrUtxos adr = fmap fst . Map.elems <$> Contract.utxosTxOutTxAt adr

-- | Get first utxo at address.
fstUtxo :: Address -> Contract w s Text TxOutRef
fstUtxo address = do
  utxos <- Contract.utxosAt address
  case Map.keys utxos of
    [] -> Contract.throwError @Text "No utxo found at address."
    x : _ -> pure x

getNftDatum :: NftId -> Contract w s Text (Maybe DatumNft)
getNftDatum nftId = do
  utxos :: [Ledger.ChainIndexTxOut] <- getAddrUtxos txScrAddress
  let datums :: [DatumNft] =
        utxos
          ^.. traversed . Ledger.ciTxOutDatum
            . _Right
            . to (PlutusTx.fromBuiltinData @DatumNft . getDatum)
            . _Just
            . filtered (\d -> d.dNft'id Hask.== nftId)
  Contract.logInfo @Hask.String $ Hask.show $ "Datum Found:" <> Hask.show datums
  Contract.logInfo @Hask.String $ Hask.show $ "Datum length:" <> Hask.show (Hask.length datums)
  case datums of
    [x] -> pure $ Just x
    [] -> Contract.throwError "No Datum can be found."
    _ : _ -> Contract.throwError "More than one suitable datums can be found."

-- | A hashing function to minimise the data to be attached to the NTFid.
hashData :: Content -> BuiltinByteString
hashData (Content b) = sha2_256 b

-- | Find NFTs at a specific Address.
findNft :: Address -> NftId -> Contract w s Text (TxOutRef, ChainIndexTxOut, DatumNft)
findNft addr nftId = do
  utxos <- Contract.utxosTxOutTxAt addr
  case findData utxos of
    [v] -> pure v
    [] -> Contract.throwError $ "DatumNft not found for " <> pack (Hask.show nftId)
    _ ->
      Contract.throwError $
        "Should not happen! More than one DatumNft found for "
          <> pack (Hask.show nftId)
  where
    findData =
      L.filter hasCorrectNft -- filter only datums with desired NftId
        . mapMaybe readTxData -- map to Maybe (TxOutRef, ChainIndexTxOut, DatumNft)
        . Map.toList
    readTxData (oref, (ciTxOut, _)) = (oref,ciTxOut,) <$> readDatum' ciTxOut
    hasCorrectNft (_, ciTxOut, datum) =
      let (cs, tn) = unAssetClass $ nftAsset nftId
       in tn Hask.== nftId'token nftId -- sanity check
            && datum.dNft'id Hask.== nftId -- check that Datum has correct NftId
            && valueOf (ciTxOut ^. ciTxOutValue) cs tn Hask.== 1 -- check that UTXO has single NFT in Value
