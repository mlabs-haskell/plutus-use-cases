{-# LANGUAGE  NamedFieldPuns #-}

-- | Simple pay to wallet contract. No scripts used.
module Mlabs.Roundtrip.RoundtripSpending(
    lock
    , lockSpendEndpoints
    , SContractArgs(..)
    , LockSpendSchema
    ) where

import Prelude qualified as Hask
import PlutusTx.Prelude

import Control.Lens
import Control.Monad.Error.Lens
import Data.Aeson (FromJSON, ToJSON)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Schema (ToSchema)
import Data.OpenApi.Schema qualified as OpenApi
import Data.Void (Void)

import Ledger hiding (initialise, to)
import Ledger.Contexts as V
import Ledger.Typed.Scripts (TypedValidator)
import Ledger.Typed.Scripts qualified as Scripts
import PlutusTx qualified
import Plutus.V1.Ledger.Ada qualified as Ada

import Plutus.V1.Ledger.Scripts (unitDatum)

import Ledger.Constraints qualified as Constraints
import Plutus.Contract as Contract

import Cardano.Api (AsType (AsAddressInEra, AsAlonzoEra))
import Cardano.Api qualified
import Plutus.Contract.CardanoAPI (FromCardanoError, fromCardanoAddress, toCardanoAddress)
import PlutusTx.Builtins.Class (stringToBuiltinByteString)


import Mlabs.Roundtrip.PreBalance (PrebalancedTx(..), preBalanceTxFrom)
import Mlabs.Plutus.Contract (selectForever)

newtype UserAddress = UserAddress {getNamiAddr :: BuiltinByteString}
  deriving stock (Hask.Show)
PlutusTx.makeLift ''UserAddress

newtype AddressRedeemer = AddressRedeemer {
  unAddrRedeemer :: BuiltinByteString
}
  -- deriving stock (Hask.Show, Generic, Hask.Eq, Hask.Ord)
  -- deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''AddressRedeemer
-- PlutusTx.makeLift ''AddressRedeemer

newtype AddressDatum = AddressDatum {
  unAddrDatum :: BuiltinByteString
}
  -- deriving stock (Hask.Show, Generic, Hask.Eq, Hask.Ord)
  -- deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''AddressDatum
-- PlutusTx.makeLift ''AddressDatum

mkValidator :: UserAddress -> AddressDatum -> AddressRedeemer -> ScriptContext -> Bool
mkValidator _ _ _ _ = True

data AddressContract

instance Scripts.ValidatorTypes AddressContract where
    type instance RedeemerType AddressContract = AddressRedeemer
    type instance DatumType AddressContract = AddressDatum

typedValidator :: UserAddress -> Scripts.TypedValidator AddressContract
typedValidator = Scripts.mkTypedValidatorParam @AddressContract
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator

-- |Contract activation args
data SContractArgs = SContractArgs
  { ownAddress :: Text
  }
  deriving (Hask.Eq, Hask.Ord, Hask.Show, Generic)
  deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema)

-- Contract API
data LockParams =
    LockParams
        { lovelaceAmount:: Integer
        , collateralRef :: TxOutRef
        , spendableUtxos :: [TxOutRef]
        }
        deriving stock (Hask.Eq, Hask.Show, Generic)
        deriving anyclass (ToJSON, FromJSON, ToSchema)

type LockSpendSchema = 
  Endpoint "lock" LockParams
  .\/ Endpoint "spend" ()

lockSpendEndpoints :: SContractArgs -> Contract () LockSpendSchema ContractError ()
lockSpendEndpoints cArgs = 
  selectForever [ endpoint @"lock" (lock cArgs)
                , endpoint @"spend" (\_ -> spendAny cArgs)
                ]


lock:: SContractArgs
    -> LockParams
    -> Contract () LockSpendSchema ContractError ()
lock (SContractArgs namiAddr) lp = do
    runError (run lp) >>= \case
      Left err -> logWarn @Hask.String (Hask.show @ContractError err)
      Right () -> pure ()
    where 
      run LockParams{lovelaceAmount, collateralRef, spendableUtxos} = do
        logInfo @Hask.String $ "UDH"
        logInfo @Hask.String $ Hask.show (datumHash unitDatum)
        
        let inst = typedValidator . UserAddress . stringToBuiltinByteString . T.unpack $ namiAddr
            scrAddress = Scripts.validatorAddress inst
            datum = AddressDatum "Test 1"
            value = Ada.lovelaceValueOf lovelaceAmount
            tx = Constraints.mustPayToTheScript datum value
            ls = Constraints.typedValidatorLookups inst

        logInfo @Hask.String $ "datum hash: " Hask.++ Hask.show (datumHash unitDatum)
        scrAddrUtxos <- utxosAt scrAddress
        logInfo @Hask.String $ "All UTXOs from address:"
        mapM_ (logInfo @Hask.String . Hask.show) (Map.toList scrAddrUtxos)

        utx <- mkTxConstraints @AddressContract ls tx

        addr <- parseAddress namiAddr

        PrebTx pUtx <- preBalanceTxFrom addr spendableUtxos collateralRef utx
        logInfo @Hask.String $ "Yielding tx"
        -- logInfo @Hask.String $ "Lock Datum hash: " Hask.++ (Hask.show $ datumHash datum)
        yieldUnbalancedTx pUtx


spendAny 
  :: SContractArgs
  -> Contract () LockSpendSchema ContractError ()
spendAny (SContractArgs namiAddr) = 
    runError run >>= \case
      Left err -> logWarn @Hask.String (Hask.show @ContractError err)
      Right () -> pure ()
    where 
      run = do
        let inst = typedValidator . UserAddress . stringToBuiltinByteString . T.unpack $ namiAddr
            scrAddress = Scripts.validatorAddress inst
        scrAddrUtxos <- utxosAt scrAddress
        logInfo @Hask.String $ "All UTXOs from address:"
        mapM_ (logInfo @Hask.String . Hask.show) (Map.toList scrAddrUtxos)
        -- TBD






parseAddress :: Text -> Contract () LockSpendSchema ContractError Address
parseAddress addr = 
  case fmap fromCardanoAddress $ Cardano.Api.deserialiseAddress (AsAddressInEra AsAlonzoEra) addr of
    Just (Right a) -> pure a
    _ -> throwError $ OtherError "Can not parse address"

  -- mapError (review _PubKeyError   ) $ do
  --   let inst = typedValidator pk
  --       address = Scripts.validatorAddress inst
  --       tx = Constraints.mustPayToTheScript () vl

  --   ledgerTx <- mkTxConstraints (Constraints.typedValidatorLookups inst) tx
  --       >>= submitUnbalancedTx . Constraints.adjustUnbalancedTx

  --   _ <- awaitTxConfirmed (getCardanoTxId ledgerTx)
  --   let refs = Map.keys
  --              $ Map.filter ((==) address . txOutAddress)
  --              $ getCardanoTxUnspentOutputsTx ledgerTx

  --   case refs of
  --       []                   -> throwing _ScriptOutputMissing pk
  --       [outRef] -> do
  --           ciTxOut <- txOutFromRef outRef
  --           pure (outRef, ciTxOut, inst)
  --       _                    -> throwing _MultipleScriptOutputs pk
