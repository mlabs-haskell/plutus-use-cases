{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE MonoLocalBinds     #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}

module Main where

import Prelude (IO, FilePath, print, fail, show, (<$>), (<*>), pure, mempty, putStrLn)
import PlutusTx.Prelude hiding ((<$>),(<*>), pure, mempty)

import System.Exit (die)

import qualified Cardano.Api                    as C
import qualified Cardano.Api.Shelley            as C

import           Ledger.Bytes                (LedgerBytes (..))
import qualified Ledger.Constraints          as LC
import           Ledger.Constraints.OffChain (UnbalancedTx (..), mkTx)
import           Ledger.Crypto               (PubKey (..), pubKeyHash)
import           Ledger.Tx                   (Tx (..), TxOutRef, txInRef)
import qualified Plutus.Contract.CardanoAPI  as CardanoAPI
import qualified Ledger                      as Plutus
import Plutus.V1.Ledger.Ada (adaValueOf)
import Ledger.Typed.Scripts.Validators (RedeemerType, DatumType)
import Plutus.V1.Ledger.Api (ToData, FromData)
import Plutus.V1.Ledger.Tx as Tx


import           Data.Aeson                     (FromJSON (..), ToJSON (..), Value (Object), object, (.:), (.=))
import qualified Data.Aeson                     as Aeson
import           Data.Aeson.Encode.Pretty       (encodePretty)
import qualified Data.ByteString.Lazy        as BSL
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Text                   (Text)
import qualified Data.Text.Encoding          as Text
import           Data.Typeable               (Typeable)
import           Data.Void                   (Void)
import           GHC.Generics                (Generic)
import           Data.ByteArray.Encoding     (Base (Base16), convertToBase)
import           Data.Bitraversable          (bitraverse)
import           Data.Proxy                  (Proxy (..))
import           Data.Text.Prettyprint.Doc   (Pretty (..))
import Control.Lens ((&), (.~), (^.))

import Data.Maybe (fromJust)
import Data.Set as Set

paramsJson = "/home/mike/dev/mlabs/plutus-use-cases/mlabs/tx-builder/pparams.json"
tx_raw_file = "/home/mike/dev/mlabs/contract_deploy/node_mnt/transactions/tx.raw"

main :: IO ()
main = do
  netParams <- readNetParams paramsJson
  netMagic  <- getTestNetMagic
  let
    pkh :: Plutus.PubKeyHash = fromJust $ Aeson.decode $
        "{\"getPubKeyHash\" : \"4cebc6f2a3d0111ddeb09ac48e2053b83b33b15f29182f9b528c6491\"}" 
    testTxConstr :: LC.TxConstraints Void Void
    testTxConstr = LC.mustPayToPubKey pkh $ adaValueOf 10
    lookups :: LC.ScriptLookups Void
    lookups = mempty
  txUnbalanced <- case LC.mkTx lookups testTxConstr of
                      Left e  -> die $ show e
                      Right t -> return t
  putStrLn "\nUnbalanced:"
  print txUnbalanced
  putStrLn "\nUnbalanced pretty:"
  pprint txUnbalanced
  putStrLn "\nBalanced:"
  let balanced = balance txUnbalanced
  print balanced
  putStrLn "\nBalanced pretty:"
  pprint balanced
  body <- case mkPartialTxBody netParams netMagic (unBalancedTxTx txUnbalanced) of
              Left e  -> die $ show $ pretty e
              Right tx -> return tx
  putStrLn "\nBalanced serialized:"
  let serialized = encodePretty $ C.serialiseToTextEnvelope Nothing body
  BSL.putStrLn serialized
  BSL.writeFile tx_raw_file serialized



balance :: UnbalancedTx -> Tx
balance unbTx = 
  let
    tx = unBalancedTxTx unbTx
    txInId = fromJust $ Aeson.decode 
             $ "{\"getTxId\" : \"2b70547a10b256f4693564692ac5fabd29ae48a6797b4f92884736d39922daab\"}"
    input_1 = TxIn (TxOutRef txInId 0 ) Nothing 
    txCollateralId = fromJust $ Aeson.decode 
                     $ "{\"getTxId\" : \"b66803306188d89ba0a4a3d5138ac442248219314b361bcaae8a6ee1f74d0a05\"}"
    collateral_1 = TxIn (TxOutRef txCollateralId 1 ) Nothing 
    txBalanced = tx
      { txFee = adaValueOf 200000,
        txInputs = Set.singleton input_1,
        txCollateral = Set.singleton collateral_1
      }
  in
    txBalanced
    


pprint :: Pretty a => a -> IO ()
pprint = print . pretty

readNetParams :: FilePath -> IO C.ProtocolParameters 
readNetParams file = do
  bs <- BSL.readFile file
  case Aeson.eitherDecode bs of
    Left err -> die $ "Can't read net params: " ++ err
    Right p -> return p

getTestNetMagic :: IO C.NetworkId
getTestNetMagic = return . C.Testnet . C.NetworkMagic $ 8

mkPartialTxBody :: C.ProtocolParameters -> C.NetworkId -> Plutus.Tx -> Either CardanoAPI.ToCardanoError (C.TxBody C.AlonzoEra)
mkPartialTxBody params networkId = CardanoAPI.toCardanoTxBody (Just params) networkId


-- From iohks' Extract
data ExportTx =
        ExportTx
            { partialTx   :: C.Tx C.AlonzoEra -- ^ The transaction itself
            , lookups     :: [ExportTxInput] -- ^ The tx outputs for all inputs spent by the partial tx
            , signatories :: [Text] -- ^ Key(s) that we expect to be used for balancing & signing. (Advisory) See note [Keys in ExportT]
            }
    deriving stock (Generic, Typeable)

data ExportTxInput = ExportTxInput{txIn :: C.TxIn, txOut :: C.TxOut C.AlonzoEra}
    deriving stock (Generic, Typeable)
    deriving anyclass (ToJSON)

instance ToJSON ExportTx where
    toJSON ExportTx{partialTx, lookups, signatories} =
        object
            [ "transaction" .= toJSON (C.serialiseToTextEnvelope Nothing partialTx)
            , "inputs"      .= toJSON lookups
            , "signatories" .= toJSON signatories
            ]

instance FromJSON ExportTx where
    parseJSON (Object v) =
        ExportTx
            <$> ((v .: "transaction") >>= either (fail . show) pure . C.deserialiseFromTextEnvelope (C.proxyToAsType Proxy))
            <*> pure mempty -- FIXME: How to deserialise Utxo / [(TxIn, TxOut)] ) see https://github.com/input-output-hk/cardano-node/issues/3051
            <*> v .: "signatories"
    parseJSON _ = fail "Expexted Object"

export :: C.ProtocolParameters -> C.NetworkId -> UnbalancedTx -> Either CardanoAPI.ToCardanoError ExportTx
export params networkId UnbalancedTx{unBalancedTxTx, unBalancedTxUtxoIndex, unBalancedTxRequiredSignatories} =
    ExportTx
        <$> mkPartialTx params networkId unBalancedTxTx
        <*> mkLookups networkId unBalancedTxUtxoIndex
        <*> mkSignatories unBalancedTxRequiredSignatories


mkPartialTx :: C.ProtocolParameters -> C.NetworkId -> Plutus.Tx -> Either CardanoAPI.ToCardanoError (C.Tx C.AlonzoEra)
mkPartialTx params networkId = fmap (C.makeSignedTransaction []) . CardanoAPI.toCardanoTxBody (Just params) networkId

mkLookups :: C.NetworkId -> Map Plutus.TxOutRef Plutus.TxOut -> Either CardanoAPI.ToCardanoError [ExportTxInput]
mkLookups networkId = fmap (fmap $ uncurry ExportTxInput) . traverse (bitraverse CardanoAPI.toCardanoTxIn (CardanoAPI.toCardanoTxOut networkId)) . Map.toList

mkSignatories :: Map Plutus.PubKeyHash (Maybe Plutus.PubKey) -> Either CardanoAPI.ToCardanoError [Text]
mkSignatories =
    -- see note [Keys in ExportTx]
    Right . fmap (\(PubKey (LedgerBytes k)) -> Text.decodeUtf8 $ convertToBase Base16 (k <> k)) . mapMaybe snd . Map.toList

{- Note [Keys in ExportTx]
The wallet backend (receiver of 'ExportTx' values) expectes the public keys in the
'signatories' field to be 'Cardano.Crypto.Wallet.XPub' keys - extended public keys
of 64 bytes. In the emulator we only deal with ED25519 keys of 32 bytes. Until that
is changed (https://jira.iohk.io/browse/SCP-2644) we simply append each of our keys
to itself in order to get a key of the correct length.
-}