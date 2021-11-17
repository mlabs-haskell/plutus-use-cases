module Mlabs.IntegrationTest.Wbe.CardanoAPI (getUTXOs) where

import Cardano.Api qualified as C

import Data.Bifunctor (bimap, first)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

import Ledger (TxIn (..), TxOut)

import Prelude

import Mlabs.IntegrationTest.Types

import Plutus.Contract.CardanoAPI (
  fromCardanoTxIn,
  fromCardanoTxOut,
  toCardanoTxIn,
 )

-- | Get UTxO from wallet by Ledger's `TxIn`
getUTXOs ::
  C.LocalNodeConnectInfo C.CardanoMode ->
  Set TxIn ->
  IO (Either TestError (Map TxIn TxOut))
getUTXOs nodeInfo txIns = (>>= convert) <$> getApiUTXOs nodeInfo txIns

getApiUTXOs ::
  C.LocalNodeConnectInfo C.CardanoMode ->
  Set TxIn ->
  IO (Either TestError (C.UTxO C.AlonzoEra))
getApiUTXOs nodeInfo txIns = do
  let cardanoTxIns =
        bimap (const $ DecoderError "Failed to parse TxIn") Set.fromList $
          traverse (toCardanoTxIn . txInRef) $ Set.toList txIns
  case cardanoTxIns of
    Right ins -> get ins
    Left err -> return $ Left err
  where
    get ins = do
      let query =
            C.QueryInEra C.AlonzoEraInCardanoMode $
              C.QueryInShelleyBasedEra
                C.ShelleyBasedEraAlonzo
                (C.QueryUTxO . C.QueryUTxOByTxIn $ ins)
      res <- C.queryNodeLocalState nodeInfo Nothing query
      case res of
        Right (Right utxo) ->
          return $ Right utxo
        err ->
          return . Left . DecoderError . show $ err

-- | Converts UTxO of Cardano API to map of map of Plutus `TxIn` -> `TxOut`
convert :: C.UTxO C.AlonzoEra -> Either TestError (Map TxIn TxOut)
convert (C.UTxO utxoMap) =
  first (DecoderError . show) $
    traverse fromCardanoTxOut
      . Map.mapKeys
        ( (`TxIn` Nothing)
            . fromCardanoTxIn
        )
      $ toTxCtx utxoMap
  where
    toTxCtx ::
      Map C.TxIn (C.TxOut C.CtxUTxO C.AlonzoEra) ->
      Map C.TxIn (C.TxOut C.CtxTx C.AlonzoEra)
    toTxCtx = error "FIXME"