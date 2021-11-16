module Mlabs.Deploy.Utils (
  validatorToPlutus,
  policyToPlutus,
  writeData,
  toSchemeJson,
) where

import PlutusTx.Prelude hiding (error)
import Prelude qualified as Hask -- (FilePath, IO, String, error, print)

import Data.Aeson as Json (encode)
import Data.ByteString.Lazy qualified as LB
import Data.ByteString.Short qualified as SBS

import Cardano.Api.Shelley (
  Error (displayError),
  PlutusScript (..),
  PlutusScriptV1,
  ScriptData (ScriptDataNumber),
  ScriptDataJsonSchema (ScriptDataJsonDetailedSchema),
  fromPlutusData,
  scriptDataToJson,
  toAlonzoData,
  writeFileTextEnvelope,
 )

import Cardano.Ledger.Alonzo.Data qualified as Alonzo
import Codec.Serialise (serialise)
import Plutus.V1.Ledger.Api (Validator)
import Plutus.V1.Ledger.Api qualified as Plutus
import PlutusTx (ToData, toData)

validatorToPlutus :: Hask.FilePath -> Validator -> Hask.IO ()
validatorToPlutus file validator = do
  -- taken from here
  -- https://github.com/input-output-hk/Alonzo-testnet/blob/main/resources
  -- /plutus-sources/plutus-example/app/plutus-minting-purple-example.hs
  let (validatorPurpleScript, validatorAsSBS) = serializeValidator validator
  case Plutus.defaultCostModelParams of
    Just m ->
      let getAlonzoData d = case toAlonzoData d of
            Alonzo.Data pData -> pData
            _ -> Hask.error "Should not happen"
          (logout, e) =
            Plutus.evaluateScriptCounting
              Plutus.Verbose
              m
              validatorAsSBS
              [getAlonzoData (ScriptDataNumber 42)]
       in do
            Hask.print ("Log output" :: Hask.String) >> Hask.print logout
            case e of
              Left evalErr -> Hask.print ("Eval Error" :: Hask.String) >> Hask.print evalErr
              Right exbudget -> Hask.print ("Ex Budget" :: Hask.String) 
                >> Hask.print exbudget
    Nothing -> Hask.error "defaultCostModelParams failed"
  result <- writeFileTextEnvelope file Nothing validatorPurpleScript
  case result of
    Left err -> Hask.print $ displayError err
    Right () -> return ()

policyToPlutus :: Hask.FilePath -> Plutus.MintingPolicy -> Hask.IO ()
policyToPlutus file policy =
  validatorToPlutus
    file
    $ Plutus.Validator $ Plutus.unMintingPolicyScript policy

serializeValidator :: Validator -> (PlutusScript PlutusScriptV1, 
                                    SBS.ShortByteString)
serializeValidator validator =
  let sbs :: SBS.ShortByteString
      sbs = SBS.toShort . LB.toStrict . serialise $ validator

      purpleScript :: PlutusScript PlutusScriptV1
      purpleScript = PlutusScriptSerialised sbs
   in (purpleScript, sbs)

writeData :: ToData a => Hask.FilePath -> a -> Hask.IO ()
writeData file isData = LB.writeFile file (toSchemeJson isData)

toSchemeJson :: ToData a => a -> LB.ByteString
toSchemeJson =
  Json.encode
    . scriptDataToJson ScriptDataJsonDetailedSchema
    . fromPlutusData
    . PlutusTx.toData
