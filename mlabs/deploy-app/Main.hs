module Main where

import Prelude (String, IO, undefined, print, error)
import System.Environment (getArgs)
import PlutusTx.Prelude hiding (error)

import Mlabs.Nft.Contract.StateMachine as SM
import Mlabs.Nft.Logic.Types (Act(..), UserAct(..), Nft(..), NftId(..), toNftId, initNft)
import Mlabs.Nft.Contract.Forge as F
import Mlabs.Emulator.Types (UserId(..))

import           Cardano.Api
import           Cardano.Api.Shelley

import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import           Plutus.V1.Ledger.Api (Validator, MintingPolicy, TxOutRef)
import qualified Plutus.V1.Ledger.Api as Plutus
import           Codec.Serialise
import Ledger.Typed.Scripts.Validators as VS
import PlutusTx as PlutusTx


import qualified Data.ByteString.Lazy   as LB
import qualified Data.ByteString.Short  as SBS
import Data.Aeson as Json

import Data.ByteString as DB



main :: IO ()
main = do
  let
    txOutRef = Plutus.TxOutRef 
                (Plutus.TxId "8cf79ca29e5d62ad3c62c6f43c38a1eb1db75f0338e3c8ded5c7ca7be910eef3") 
                0
    userId         = UserId $ Plutus.PubKeyHash "4cebc6f2a3d0111ddeb09ac48e2053b83b33b15f29182f9b528c6491"
    initNftDatum   = initNft txOutRef userId "Content" (1 % 2) (Just 1000)
    nftId          = nft'id initNftDatum
    typedValidator = SM.scriptInstance nftId
    policy         = F.currencyPolicy (validatorAddress typedValidator) nftId
    redeemer = -- stop sell
      UserAct userId $ SetPriceAct Nothing
    stopSellDatum  = initNftDatum {nft'price = Nothing}

  [outDir] <- getArgs
  -- DB.writeFile 
  --   (outDir ++ "/token.name")
  --   (let (NftId (Plutus.TokenName n) _) = nftId in Plutus.fromBuiltin n)

  -- LB.writeFile 
  --   (outDir ++ "/init-datum.json")
  --   (toJson initNftDatum)

  -- LB.writeFile 
  --   (outDir ++ "/stop-sell-datum.json")
  --   (toJson stopSellDatum)

  -- LB.writeFile 
  --   (outDir ++ "/stop-sell-redeemer.json")
  --   (toJson redeemer)

  validatorToPlutus (outDir ++ "/NftScript.plutus") 
    (VS.validatorScript typedValidator)

  validatorToPlutus (outDir ++ "/NftPolicy.plutus") 
    $ (Plutus.Validator $ Plutus.unMintingPolicyScript policy)


validatorToPlutus file validator = do
  -- taken from here
  -- https://github.com/input-output-hk/Alonzo-testnet/blob/main/resources/plutus-sources/plutus-example/app/plutus-minting-purple-example.hs
  let (validatorPurpleScript, validatorAsSBS) = serializeValidator validator
  case Plutus.defaultCostModelParams of
        Just m ->
          let 
              Alonzo.Data pData = toAlonzoData (ScriptDataNumber 42)
              (logout, e) = 
                Plutus.evaluateScriptCounting Plutus.Verbose m (validatorAsSBS) [pData]
          in do print ("Log output" :: String) >> print logout
                case e of
                  Left evalErr -> print ("Eval Error" :: String) >> print evalErr
                  Right exbudget -> print ("Ex Budget" :: String) >> print exbudget
        Nothing -> error "defaultCostModelParams failed"
  result <- writeFileTextEnvelope file Nothing (validatorPurpleScript)
  case result of
    Left err -> print $ displayError err
    Right () -> return ()

serializeValidator :: Validator -> (PlutusScript PlutusScriptV1, SBS.ShortByteString)
serializeValidator validator =
  let 
    sbs :: SBS.ShortByteString
    sbs = SBS.toShort . LB.toStrict . serialise $ validator

    purpleScript :: PlutusScript PlutusScriptV1
    purpleScript = PlutusScriptSerialised sbs
  in
    (purpleScript, sbs)


serializeMintingPolicy :: MintingPolicy -> (PlutusScript PlutusScriptV1, SBS.ShortByteString)
serializeMintingPolicy policy =
  let 
    validator = Plutus.Validator $ Plutus.unMintingPolicyScript policy
    
  in
    serializeValidator validator

toJson :: ToData a => a -> LB.ByteString
toJson = 
  Json.encode 
  . scriptDataToJson ScriptDataJsonDetailedSchema 
  . fromPlutusData 
  . PlutusTx.toData     
        
