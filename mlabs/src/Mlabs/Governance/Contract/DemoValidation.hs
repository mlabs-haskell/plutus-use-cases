{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fobject-code #-}

-- | Validation, on-chain code for governance application
module Mlabs.Governance.Contract.DemoValidation (
  govAddress,
  govInstance,
  govValidator,
  govSingleton,
  xgovSingleton,
  xGovMintingPolicy,
  xGovCurrencySymbol,
  Governance,
  GovernanceDatum (..),
  GovernanceRedeemer (..),
  AssetClassGov (..),
) where

import PlutusTx.Prelude hiding (Semigroup (..), unless)
import Prelude qualified as Hask

import Data.Bifunctor (first)
import Data.Coerce (coerce)
import GHC.Generics (Generic)

import Playground.Contract (FromJSON, ToJSON, ToSchema)
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AssocMap

import Ledger hiding (after, before)
import Ledger.Typed.Scripts (wrapMintingPolicy)
import Ledger.Typed.Scripts.Validators qualified as Validators
import Plutus.V1.Ledger.Contexts qualified as Contexts
import Plutus.V1.Ledger.Credential (Credential (..))
import Plutus.V1.Ledger.Value qualified as Value

-- TODO: Once AssetClass has a ToSchema instance, change this to a newtype.
--       or not. this is fine really.
data AssetClassGov = AssetClassGov
  { acGovCurrencySymbol :: !CurrencySymbol
  , acGovTokenName :: !TokenName
  }
  deriving (Hask.Show, Hask.Eq, Generic, ToJSON, FromJSON, ToSchema)

instance Eq AssetClassGov where
  {-# INLINEABLE (==) #-}
  n1 == n2 =
    acGovCurrencySymbol n1 == acGovCurrencySymbol n2
      && acGovTokenName n1 == acGovTokenName n2

PlutusTx.unstableMakeIsData ''AssetClassGov
PlutusTx.makeLift ''AssetClassGov

data GovernanceRedeemer
  = GRDeposit !Integer
  | GRWithdraw !Integer
  deriving (Hask.Show)

instance Eq GovernanceRedeemer where
  {-# INLINEABLE (==) #-}
  (GRDeposit n1) == (GRDeposit n2) = n1 == n2
  (GRWithdraw n1) == (GRWithdraw n2) = n1 == n2
  _ == _ = False

PlutusTx.unstableMakeIsData ''GovernanceRedeemer
PlutusTx.makeLift ''GovernanceRedeemer

data GovernanceDatum = GovernanceDatum
  { gdPubKeyHash :: !PubKeyHash
  , gdxGovCurrencySymbol :: !CurrencySymbol
  }
  deriving (Hask.Show)

PlutusTx.unstableMakeIsData ''GovernanceDatum
PlutusTx.makeLift ''GovernanceDatum

data Governance
instance Validators.ValidatorTypes Governance where
  type DatumType Governance = BuiltinData
  type RedeemerType Governance = BuiltinData

-- data Governance
-- instance Validators.ValidatorTypes Governance where
--   type DatumType Governance = GovernanceDatum
--   type RedeemerType Governance = GovernanceRedeemer

-- | governance validator
{-# INLINEABLE govValidator #-}
mkValidator :: AssetClassGov -> BuiltinData -> BuiltinData -> ScriptContext -> Bool
mkValidator ac _ _ _ = True

govInstance :: AssetClassGov -> Validators.TypedValidator Governance
govInstance gov =
  Validators.mkTypedValidator @Governance
    ( $$(PlutusTx.compile [||mkValidator||])
        `PlutusTx.applyCode` PlutusTx.liftCode gov
    )
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Validators.wrapValidator @BuiltinData @BuiltinData
    -- wrap = Validators.wrapValidator @GovernanceDatum @GovernanceRedeemer

govValidator :: AssetClassGov -> Validator
govValidator = Validators.validatorScript . govInstance

govAddress :: AssetClassGov -> Ledger.Address
govAddress = scriptAddress . govValidator

{-# INLINEABLE govSingleton #-}
govSingleton :: AssetClassGov -> Integer -> Value
govSingleton AssetClassGov {..} = Value.singleton acGovCurrencySymbol acGovTokenName

xgovSingleton :: AssetClassGov -> PubKeyHash -> Integer -> Value
xgovSingleton gov pkh = Value.singleton (xGovCurrencySymbol gov) (coerce pkh)

-- xGOV minting policy
{-# INLINEABLE mkPolicy #-}
mkPolicy :: ValidatorHash -> AssetClassGov -> BuiltinData -> ScriptContext -> Bool
mkPolicy vh AssetClassGov {..} _ ctx = True

xGovMintingPolicy :: AssetClassGov -> MintingPolicy
xGovMintingPolicy gov =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||(wrapMintingPolicy .) . mkPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode (validatorHash $ govValidator gov)
      `PlutusTx.applyCode` PlutusTx.liftCode gov

-- | takes in the GOV CurrencySymbol, returns the xGOV CurrencySymbol
xGovCurrencySymbol :: AssetClassGov -> CurrencySymbol
xGovCurrencySymbol = scriptCurrencySymbol . xGovMintingPolicy
