{-# LANGUAGE UndecidableInstances #-}

module Mlabs.NFT.Contract.Mint (
  mint,
  getDatumsTxsOrdered,
  hashData,
) where

import PlutusTx.Prelude hiding (mconcat, mempty, (<>))
import Prelude (mconcat, mempty)
import Prelude qualified as Hask

import Control.Lens ((^.))
import Control.Monad (void)
import Data.Function (on)
import Data.List qualified as L
import Data.Map qualified as Map
import Data.Monoid (Last (..), (<>))
import Data.Text (Text, pack)
import Text.Printf (printf)

import Mlabs.Plutus.Contract (readDatum')
import Plutus.Contract (Contract)
import Plutus.Contract qualified as Contract

import Ledger (
  ChainIndexTxOut,
  MintingPolicy,
  PubKeyHash (..),
  Redeemer,
  TxOutRef,
  ciTxOutValue,
  pubKeyHash,
  scriptCurrencySymbol,
 )

import Ledger.Constraints qualified as Constraints
import Ledger.Typed.Scripts (validatorScript)
import Ledger.Value as Value (AssetClass (..), TokenName (..), assetClass, assetClassValue, singleton)
import Plutus.ChainIndex.Tx (ChainIndexTx)

import Mlabs.NFT.Types {-(
                        BuyRequestUser (..),
                        Content (..),
                        MintAct (..),
                        MintParams (..),
                        NftId (..),
                        QueryResponse (..),
                        SetPriceParams (..),
                        UserId (..),
                       ) -}

import Mlabs.NFT.Validation {-(
                             DatumNft (..),
                             NftTrade,
                             UserAct (..),
                             asRedeemer,
                             calculateShares,
                             mintPolicy,
                             nftAsset,
                             nftCurrency,
                             nftTokenName,
                             priceNotNegative,
                             txPolicy,
                             txScrAddress,
                            )-}

import Mlabs.NFT.Contract.Aux
import Plutus.V1.Ledger.Value (TokenName (TokenName))

--------------------------------------------------------------------------------
-- MINT --

-- | Two positions in on-chain list between which new NFT will be "inserted"
data InsertPoint = InsertPoint
  { prev :: PointInfo
  , next :: Maybe PointInfo
  }

---- | Mints an NFT and sends it to the App Address.
mint :: NftAppSymbol -> MintParams -> Contract (Last NftId) s Text ()
mint symbol params = do
  user <- getUId
  head' <- getHead symbol
  scrUtxos <- Map.map fst <$> getAddrValidUtxos symbol
  case head' of
    Nothing -> Contract.throwError @Text "Couldn't find head"
    Just head -> do
      let appInstance = getAppInstance $ pi'datum head
          newNode = createNewNode appInstance params user
          nftPolicy = mintPolicy appInstance
      (InsertPoint lNode rNode) <- findInsertPoint symbol newNode
      (lLk, lCx) <- updateNodePointer appInstance scrUtxos symbol lNode newNode
      (nLk, nCx) <- mintNode symbol nftPolicy newNode rNode
      let lookups = mconcat [lLk, nLk]
          tx = mconcat [lCx, nCx]
      void $ Contract.submitTxConstraintsWith @NftTrade lookups tx
      Contract.logInfo @Hask.String $ printf "mint successful!"
  where
    nftIdInit = NftId . hashData

    createNewNode :: NftAppInstance -> MintParams -> UserId -> NftListNode
    createNewNode appInstance mp author =
      NftListNode
        { node'information = mintParamsToInfo mp author
        , node'next = Nothing
        , node'appInstance = appInstance
        }

    findInsertPoint :: NftAppSymbol -> NftListNode -> GenericContract InsertPoint
    findInsertPoint aSymbol node = do
      list <- getDatumsTxsOrdered aSymbol
      case list of
        [] -> Contract.throwError "This Should never happen."
        x : xs -> findPoint x xs
      where
        findPoint :: PointInfo -> [PointInfo] -> GenericContract InsertPoint
        findPoint x = \case
          [] -> pure $ InsertPoint x Nothing
          (y : ys) ->
            case compare (pi'datum y) (NodeDatum node) of
              LT -> findPoint y ys
              EQ -> Contract.throwError @Text "NFT already minted."
              GT -> pure $ InsertPoint x (Just y)

    mintNode ::
      NftAppSymbol ->
      MintingPolicy ->
      NftListNode ->
      Maybe PointInfo ->
      GenericContract (Constraints.ScriptLookups NftTrade, Constraints.TxConstraints i0 DatumNft)
    mintNode appSymbol mintingP newNode nextNode = pure (lookups, tx)
      where
        newTokenValue = Value.singleton (app'symbol appSymbol) (TokenName . getDatumValue . NodeDatum $ newNode) 1
        symbol = app'symbol appSymbol
        newTokenDatum =
          NodeDatum $
            newNode
              { node'next = Pointer . assetClass symbol . TokenName . getDatumValue . pi'datum <$> nextNode
              }

        mintRedeemer = asRedeemer . Mint . NftId . getDatumValue . NodeDatum $ newNode

        lookups =
          mconcat
            [ Constraints.typedValidatorLookups txPolicy
            , Constraints.otherScript (validatorScript txPolicy)
            , Constraints.mintingPolicy mintingP
            ]
        tx =
          mconcat
            [ Constraints.mustPayToTheScript newTokenDatum newTokenValue
            , Constraints.mustMintValueWithRedeemer mintRedeemer newTokenValue
            ]

    updateNodePointer ::
      NftAppInstance ->
      Map.Map TxOutRef ChainIndexTxOut ->
      NftAppSymbol ->
      PointInfo ->
      NftListNode ->
      GenericContract (Constraints.ScriptLookups NftTrade, Constraints.TxConstraints i0 DatumNft)
    updateNodePointer appInstance scrAddrUtxos appSymbol insertPoint newNode = do
      pure (lookups, tx)
      where
        token = Value.singleton (app'symbol appSymbol) (TokenName . getDatumValue . pi'datum $ insertPoint) 1
        newToken = assetClass (app'symbol appSymbol) (TokenName .getDatumValue . NodeDatum $ newNode)
        newDatum = updatePointer (Pointer newToken)
        oref = pi'TOR insertPoint
        redeemer = asRedeemer $ MintAct . NftId . getDatumValue . NodeDatum $ newNode
        oldDatum = pi'datum insertPoint
        uniqueToken = assetClassValue (appInstance'AppAssetClass appInstance) 1

        updatePointer :: Pointer -> DatumNft
        updatePointer newPointer =
          case oldDatum of
            HeadDatum (NftListHead _ a) -> HeadDatum $ NftListHead (Just newPointer) a
            NodeDatum (NftListNode i _ a) -> NodeDatum $ NftListNode i (Just newPointer) a

        lookups =
          mconcat
            [ Constraints.typedValidatorLookups txPolicy
            , Constraints.otherScript (validatorScript txPolicy)
            , Constraints.unspentOutputs scrAddrUtxos
            ]
        tx =
          mconcat
            [ case oldDatum of
                NodeDatum _ -> Constraints.mustPayToTheScript newDatum token
                HeadDatum _ -> Constraints.mustPayToTheScript newDatum (token <> uniqueToken)
            , Constraints.mustSpendScriptOutput oref redeemer
            ]

    mintParamsToInfo :: MintParams -> UserId -> InformationNft
    mintParamsToInfo MintParams {..} author =
      InformationNft
        { info'id = nftIdInit mp'content
        , info'share = mp'share
        , info'price = mp'price
        , info'owner = author
        , info'author = author
        }

    mkNode :: NftAppInstance -> NftId -> MintParams -> PubKeyHash -> NftListNode
    mkNode inst nid MintParams {..} pkh =
      let author = UserId pkh
          info = InformationNft nid mp'share author author mp'price
       in NftListNode info Nothing inst

-- | A hashing function to minimise the data to be attached to the NTFid.
hashData :: Content -> BuiltinByteString
hashData (Content b) = sha2_256 b

lookupNext :: Maybe PointInfo -> Constraints.ScriptLookups a2
lookupNext = \case
  Just (PointInfo _ oref toOut _) -> Constraints.unspentOutputs $ Map.fromList [(oref, toOut)]
  Nothing -> mempty

mustSpendNext :: Maybe PointInfo -> Redeemer -> Constraints.TxConstraints i o
mustSpendNext maybeNext redeemer = case maybeNext of
  Just (PointInfo _ oref _ _) -> Constraints.mustSpendScriptOutput oref redeemer
  Nothing -> mempty

mustPayNext :: Maybe PointInfo -> Constraints.TxConstraints i DatumNft
mustPayNext = \case
  Just (PointInfo datum _ txOut _) -> Constraints.mustPayToTheScript datum (txOut ^. ciTxOutValue)
  Nothing -> mempty
