module Mlabs.NFT.Contract.InitMP (
  createListHead,
  getAppSymbol,
  initApp,
  uniqueTokenName,
) where

import Control.Monad (void)
import Data.Monoid (Last (..))
import Data.Text (Text, pack)
import Text.Printf (printf)

--import PlutusTx.Prelude hiding (mconcat, (<>))
import Prelude (mconcat, (<>))
import Prelude qualified as Hask

import Ledger (AssetClass, scriptCurrencySymbol)
import Ledger.Constraints qualified as Constraints
import Ledger.Typed.Scripts (validatorHash)
import Ledger.Value as Value (singleton)
import Plutus.Contract (EmptySchema, Contract, mapError, ownPubKeyHash)
import Plutus.Contract qualified as Contract

{- Drop-in replacement for
import Plutus.Contracts.Currency (CurrencyError, mintContract)
import Plutus.Contracts.Currency qualified as MC
till it will be fixed, see `Mlabs.Plutus.Contracts.Currency.mintContract`
for details -}
import Mlabs.Plutus.Contracts.Currency (CurrencyError, mintContractViaPkh)
import Mlabs.Plutus.Contracts.Currency qualified as MC

import Plutus.V1.Ledger.Value (TokenName (..), assetClass, assetClassValue)
import PlutusTx.Prelude hiding (mconcat, (<>))

import Mlabs.Data.LinkedList (LList (..))
import Mlabs.NFT.Contract.Aux (toDatum)
import Mlabs.NFT.Governance.Types (GovAct (..), GovDatum (..), GovLHead (..))
import Mlabs.NFT.Governance.Validation (govMintPolicy, govScrAddress, govScript)
import Mlabs.NFT.Types (
  UniqueToken,
  GenericContract, 
  InitParams (..), 
  MintAct (..), 
  NftAppInstance (..), 
  NftAppSymbol (..), 
  NftListHead (..))
import Mlabs.NFT.Validation (DatumNft (..), NftTrade, asRedeemer, curSymbol, mintPolicy, txPolicy, txScrAddress)
import Mlabs.Utils (submitTxConstraintsWithUnbalanced)

{- | The App Symbol is written to the Writter instance of the Contract to be
 recovered for future opperations, and ease of use in Trace.
-}
type InitContract a = Contract (Last NftAppInstance) EmptySchema Text a

{- |
  Initialise NFT marketplace, create HEAD of the list and unique token
-}
initApp :: UniqueToken -> InitParams -> InitContract ()
initApp ut params = do
  res <- Contract.runError (initApp' ut params)
  case res of
    Right _ -> pure ()
    Left e -> Contract.logInfo @Hask.String ("initApp error: " Hask.++ (Hask.show e))

initApp' :: UniqueToken -> InitParams -> InitContract ()
initApp' ut params = do
  ownPKH <- ownPubKeyHash
  Contract.logInfo @Hask.String ("PKH: " Hask.++ (Hask.show ownPKH))
  Contract.logInfo @Hask.String "Creating list head"
  appInstance <- createListHead ut params
  -- Contract.tell . Last . Just $ appInstance
  Contract.logInfo @Hask.String $ printf "Finished Initialisation: App Instance: %s" (Hask.show appInstance)

{- | Initialise the application at the address of the script by creating the
 HEAD of the list, and coupling the one time token with the Head of the list.
-}
createListHead :: UniqueToken -> InitParams -> GenericContract NftAppInstance
createListHead uToken InitParams {..} = do
  Contract.logInfo @Hask.String "Generating unique token"
  {- If we can achieve `awaitTxConfirmed` with MlabsPAB,
     we can uncomment line below and use that contract instead of passing
     `UniqueToken` as parameter. 
  -}
  -- uniqueToken <- generateUniqueToken
  let govAddr = govScrAddress uToken
      scrAddr = txScrAddress uToken
  Contract.logInfo @Hask.String "Minting head"
  mintListHead $ NftAppInstance scrAddr uToken govAddr ip'admins
  where
    -- Mint the Linked List Head and its associated token.
    mintListHead :: NftAppInstance -> GenericContract NftAppInstance
    mintListHead appInstance = do
      ownPKH <- ownPubKeyHash
      let -- Unique Token
          uniqueToken = appInstance'UniqueToken appInstance
          uniqueTokenValue = assetClassValue uniqueToken 1
          emptyTokenName = TokenName PlutusTx.Prelude.emptyByteString
      let -- Script Head Specific Information
          headDatum :: DatumNft = nftHeadInit appInstance
          headPolicy = mintPolicy appInstance
          proofTokenValue = Value.singleton (scriptCurrencySymbol headPolicy) emptyTokenName 1
          initRedeemer = asRedeemer Initialise
      let -- Gov App Head Specific information
          govHeadDatum :: GovDatum = govHeadInit
          govHeadPolicy = govMintPolicy appInstance
          govScr = govScript uniqueToken
          govProofTokenValue = Value.singleton (scriptCurrencySymbol govHeadPolicy) emptyTokenName 1
          govInitRedeemer = asRedeemer InitialiseGov

          -- NFT App Head
          (lookups, tx) =
            ( mconcat
                [ Constraints.typedValidatorLookups (txPolicy uniqueToken)
                , Constraints.mintingPolicy headPolicy
                , Constraints.mintingPolicy govHeadPolicy
                ]
            , mconcat
                [ Constraints.mustPayToTheScript headDatum (proofTokenValue <> uniqueTokenValue)
                , Constraints.mustPayToOtherScript (validatorHash govScr) (toDatum govHeadDatum) (govProofTokenValue <> uniqueTokenValue)
                , Constraints.mustMintValueWithRedeemer initRedeemer proofTokenValue
                , Constraints.mustMintValueWithRedeemer govInitRedeemer govProofTokenValue
                , Constraints.mustBeSignedBy ownPKH
                ]
            )

      void $ submitTxConstraintsWithUnbalanced @NftTrade lookups (tx <> Constraints.mustBeSignedBy ownPKH)
      Contract.logInfo @Hask.String $ printf "Forged Script Head & Governance Head for %s" (Hask.show appInstance)
      return appInstance

    -- Contract that mints a unique token to be used in the minting of the head
    generateUniqueToken :: GenericContract AssetClass
    generateUniqueToken = do
      self <- ownPubKeyHash
      let nftTokenName = TokenName uniqueTokenName 
      x <-
        mapError
          (pack . Hask.show @CurrencyError)
          (mintContractViaPkh self [(nftTokenName, 2)])
      return $ assetClass (MC.currencySymbol x) nftTokenName

    nftHeadInit :: NftAppInstance -> DatumNft
    nftHeadInit appInst =
      HeadDatum $
        NftListHead
          { head'next = Nothing
          , head'appInstance = appInst
          }

    govHeadInit =
      GovDatum $
        HeadLList
          { _head'info = GovLHead ip'feeRate ip'feePkh
          , _head'next = Nothing
          }

-- | Given an App Instance return the NftAppSymbol for that app instance.
getAppSymbol :: NftAppInstance -> NftAppSymbol
getAppSymbol = NftAppSymbol . curSymbol

{-# INLINEABLE uniqueTokenName #-}

-- | Token Name with which Unique Tokens are parametrised.
uniqueTokenName :: BuiltinByteString
uniqueTokenName = "UniqueAppToken"
