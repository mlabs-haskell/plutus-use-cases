{-# LANGUAGE  NamedFieldPuns #-}

-- | Simple pay to wallet contract. No scripts used.
module Mlabs.Roundtrip.RoundtripSpending(
    lock
    -- , SContractArgs(..)
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

--     ScriptOutputMissing PubKeyHash
--     | MultipleScriptOutputs PubKeyHash
--     | PKContractError ContractError
--     deriving stock (Eq, Show, Generic)
--     deriving anyclass (ToJSON, FromJSON)

-- makeClassyPrisms ''PubKeyError

-- instance AsContractError PubKeyError where
--     _ContractError = _PKContractError

-- | Lock some funds in a 'PayToPubKey' contract, returning the output's address
--   and a 'TxIn' transaction input that can spend it.

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
        }
        deriving stock (Hask.Eq, Hask.Show, Generic)
        deriving anyclass (ToJSON, FromJSON, ToSchema)

type SpendingSchema = Endpoint "lock" LockParams

lock
    :: SContractArgs
    -> LockParams
    -> Contract () SpendingSchema ContractError ()
lock (SContractArgs namiAddr) lp = do
    runError (run lp) >>= \case
      Left err -> logWarn @Hask.String (Hask.show @ContractError err)
      Right () -> pure ()
    where 
      run LockParams{lovelaceAmount, collateralRef} = do
        let inst = typedValidator . UserAddress . stringToBuiltinByteString . T.unpack $ namiAddr
            scrAddress = Scripts.validatorAddress inst
            datum = AddressDatum "Test 1"
            value = Ada.lovelaceValueOf lovelaceAmount
            tx = Constraints.mustPayToTheScript datum value

        utx <- mkTxConstraints @AddressContract (Hask.mempty) tx

        addr <- parseAddress namiAddr

        PrebTx pUtx <- preBalanceTxFrom addr collateralRef utx
        logInfo @Hask.String $ "Yielding tx"
        yieldUnbalancedTx pUtx
      
      -- logInfo @Hask.String $ "Submitting lock tx"
      -- ledgerTx <- mkTxConstraints (Constraints.typedValidatorLookups inst) tx
      --   >>= submitUnbalancedTx . Constraints.adjustUnbalancedTx
      -- _ <- awaitTxConfirmed (getCardanoTxId ledgerTx)
      -- addrUtxos <- utxosAt addr




parseAddress :: Text -> Contract () SpendingSchema ContractError Address
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
