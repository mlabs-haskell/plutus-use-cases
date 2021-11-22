module Mlabs.NFT.Governance.Validation (
  govScript,
  govMintPolicy,
  govScrAddress,
  GovManage,
  findCurrSym,
) where

import Ledger (
  Address,
  MintingPolicy,
  ScriptContext,
  mkMintingPolicyScript,
  -- ownCurrencySymbol,
 )
import Ledger.Typed.Scripts (
  TypedValidator,
  ValidatorTypes (..),
  mkTypedValidator,
  validatorAddress,
  wrapMintingPolicy,
  wrapValidator,
 )

-- import Plutus.V1.Ledger.Value (AssetClass (unAssetClass),)
import PlutusTx qualified
import PlutusTx.Prelude (
  Bool (..),
  Maybe (..),
  -- fromMaybe,
  -- fst,
  -- traceError,
  traceIfFalse,
  ($),
  (&&),
  (.),
  -- (==),
  (||),
 )

import Mlabs.NFT.Governance.Types (GovAct (..), GovDatum, GovernanceToken)
import Mlabs.NFT.Types (NftAppInstance (..), UniqueToken)

data GovManage
instance ValidatorTypes GovManage where
  type DatumType GovManage = GovDatum
  type RedeemerType GovManage = GovAct

{-# INLINEABLE mkGovMintPolicy #-}

-- | Minting policy for GOV and xGOV tokens.
mkGovMintPolicy :: NftAppInstance -> GovAct -> ScriptContext -> Bool
mkGovMintPolicy _ act _ =
  let
   in -- ut = appInstance'AppAssetClass appInst
      -- err1 = traceError "Cannot find Governance AssetClass. Has List been initiated?"
      -- gToken = fromMaybe err1 $ findCurrSym ut sc
      traceIfFalse "Incorrect Minting Policy used." True -- (correctMintingPolicy gToken)
        && case act of
          InitialiseGov ->
            traceIfFalse "Only one head can be minted" singleHead
              && traceIfFalse "Head is not correctly managed." headSentToAddress
          MintGov ->
            traceIfFalse "List head is not present." headIsPresent
              && traceIfFalse
                "List node incorrectly minted or updated"
                (nodeCanBeUpdated && nodeCorrectlyUpdated)
                || (nodeCanBeInserted && nodeCorrectlyInserted)
              && traceIfFalse "Incorrect quantities minted." equalQuantities
              && traceIfFalse "List Nodes are not correctly managed." manageMint
          Proof ->
            traceIfFalse "Proof cannot mint or burn a token!" False
          ProofAndBurn ->
            traceIfFalse "List updated incorrectly" burnUpdate
  where
    singleHead = True

    -- correctMintingPolicy :: GovernanceToken -> Bool
    -- correctMintingPolicy = (ownCurrencySymbol sc ==) . fst . unAssetClass

    headIsPresent :: Bool
    headIsPresent = True

    -- Mint Gov

    nodeCanBeUpdated = True

    nodeCorrectlyUpdated = True

    equalQuantities = True

    nodeCanBeInserted = True

    nodeCorrectlyInserted = True

    manageMint = listGovSentToAddress && freeGovSentToWallet && headSentToAddress

    headSentToAddress = True

    listGovSentToAddress = True

    freeGovSentToWallet = True

    burnUpdate = True

{-# INLINEABLE govMintPolicy #-}

-- | Gov Minting Policy
govMintPolicy :: NftAppInstance -> MintingPolicy
govMintPolicy x =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||wrapMintingPolicy . mkGovMintPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode x

{-# INLINEABLE mkGovScript #-}

-- | Minting policy for GOV and xGOV tokens.
mkGovScript :: UniqueToken -> GovDatum -> GovAct -> ScriptContext -> Bool
mkGovScript _ _ act _ =
  traceIfFalse "" True
    && case act of
      InitialiseGov ->
        traceIfFalse "Only Gov head containing Unique Token is sent to Gov Address." onlyGovHead
          && traceIfFalse "Nothing is used from the script address." onlySending
      MintGov ->
        traceIfFalse "Fees are not correctly." correctFeesPaid
          && traceIfFalse "ListGov not minted correctly." correctListGovMinted
          && traceIfFalse "FreeGov not minted correctly." correctFreeGovMinted
          && traceIfFalse "Gov List head not managed correctly." headCorrectlyManaged
      Proof ->
        traceIfFalse "Free Gov tokens are required to unlock." freeGovPresent
          && traceIfFalse "ListGov must be sent bach unchanged." listGovSentBackUnchanged
          && traceIfFalse "FreeGov must be sent bach unchanged." freeGovSentBackUnchaged
      ProofAndBurn ->
        traceIfFalse "Free Gov tokens are required to unlock." freeGovPresent
          && traceIfFalse "Free Gov tokens must be burnt correctly." (freeGovBurnt && listGovBurnt)
  where
    -- OnlyGovHead containing Unique Token is sent to GovScript Address.
    onlyGovHead = True

    onlySending = True

    -- MintGov
    correctFeesPaid = True
    correctListGovMinted = True
    correctFreeGovMinted = True
    headCorrectlyManaged = True

    -- Proof
    freeGovPresent = True
    listGovSentBackUnchanged = True
    freeGovSentBackUnchaged = True

    -- Proof And Burn
    listGovBurnt = True
    freeGovBurnt = True

{-# INLINEABLE findCurrSym #-}

{- | Returns the AssetClass of the Governance list by finding the head of the
 list in the token and returning the other AssetClass except for the UniqueToken.
-}
findCurrSym :: UniqueToken -> ScriptContext -> Maybe GovernanceToken
findCurrSym _ _ = Nothing

{-# INLINEABLE govScript #-}
govScript :: UniqueToken -> TypedValidator GovManage
govScript x =
  mkTypedValidator @GovManage
    ($$(PlutusTx.compile [||mkGovScript||]) `PlutusTx.applyCode` PlutusTx.liftCode x)
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = wrapValidator @GovDatum @GovAct

{-# INLINEABLE govScrAddress #-}

-- | Address of Gov Script Logic.
govScrAddress :: UniqueToken -> Ledger.Address
govScrAddress = validatorAddress . govScript
