module Mlabs.NFT.Governance.Types (
  GovAct (..),
  GovLHead (..),
  GovLNode (..),
  GovLList,
  GovDatum (..),
  LList (..),
  GovAppSymbol (..),
) where

import Data.OpenApi.Schema qualified as OpenApi
import Ledger (CurrencySymbol)
import Mlabs.Data.LinkedList (LList (..))
import Mlabs.NFT.Types (UserId)
import PlutusTx qualified
import Prelude qualified as Hask

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Plutus.V1.Ledger.Crypto (PubKeyHash)
import PlutusTx.Prelude

-- | Datum for utxo containing GovLList Head token.
data GovLHead = GovLHead
  { govLHead'feeRate :: Rational
  , govLHead'pkh :: PubKeyHash
  }
  deriving stock (Hask.Show, Generic, Hask.Eq, Hask.Ord)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''GovLHead
PlutusTx.makeLift ''GovLHead

newtype GovAppSymbol = GovAppSymbol {gov'symbol :: CurrencySymbol}
  deriving stock (Hask.Show, Generic, Hask.Eq, Hask.Ord)
  deriving anyclass (ToJSON, FromJSON, OpenApi.ToSchema)

PlutusTx.unstableMakeIsData ''GovAppSymbol
PlutusTx.makeLift ''GovAppSymbol

instance Eq GovAppSymbol where
  {-# INLINEABLE (==) #-}
  (GovAppSymbol a) == (GovAppSymbol a') = a == a'

instance Eq GovLHead where
  {-# INLINEABLE (==) #-}
  (GovLHead a b) == (GovLHead a' b') = a == a' && b == b'

-- | Datum for utxo containing GovLList Head token.
data GovLNode = GovLNode
  deriving stock (Hask.Show, Generic, Hask.Eq, Hask.Ord)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''GovLNode
PlutusTx.makeLift ''GovLNode

instance Eq GovLNode where
  {-# INLINEABLE (==) #-}
  _ == _ = True

type GovLList = LList UserId GovLHead GovLNode

newtype GovDatum = GovDatum {gov'list :: GovLList}
  deriving stock (Hask.Show, Generic, Hask.Eq, Hask.Ord)
  deriving newtype (Eq, Ord)
  deriving anyclass (ToJSON, FromJSON)
PlutusTx.unstableMakeIsData ''GovDatum
PlutusTx.makeLift ''GovDatum

data GovAct
  = -- | Mint Governance Tokens
    MintGov -- Gov Token is added / update on list, and as many xGov tokens are created and relelased.
  | -- | Use as Proof
    Proof -- Token is used as proof and must be returned unchanged to the application
  | -- | Initialises the Governance List at the given location
    InitialiseGov
  deriving stock (Hask.Show, Generic, Hask.Eq)
  deriving anyclass (ToJSON, FromJSON)
PlutusTx.unstableMakeIsData ''GovAct
PlutusTx.makeLift ''GovAct
