module Mlabs.Deploy.Governance where

import PlutusTx.Prelude hiding (error)
import Prelude (IO, String, error, print, undefined)

import Mlabs.Governance.Contract.DemoValidation

import Ledger
import Ledger.Typed.Scripts.Validators as VS
import Plutus.V1.Ledger.Api qualified as Plutus
import Plutus.V1.Ledger.Scripts qualified as Scripts
import Data.Aeson as Aeson (decode)
import Data.Maybe (fromJust)
import Ledger.Address (scriptAddress)

import Mlabs.Deploy.Utils
import Cardano.Api
import Cardano.CLI.Shelley.Script
import  Cardano.CLI.Shelley.Run.Address
import           Control.Monad.Trans.Except.Extra (firstExceptT, newExceptT)

import qualified Data.ByteString.Char8 as BS

outDir = "./deploy-app/deploy_scripts/node_mnt/governance/plutus_files"

-- serializeGovernance txId txIx ownerPkh content outDir = do
serializeGovernance = do
  let acGov =
        AssetClassGov
          (fromJust $ Aeson.decode "{\"unCurrencySymbol\" : \"fda1b6b487bee2e7f64ecf24d24b1224342484c0195ee1b7b943db50\"}") -- MintingPolicy.plutus
          (fromJust $ Aeson.decode "{\"unTokenName\" : \"GOV\"}")
      userOnePKH :: Plutus.PubKeyHash = fromJust $ Aeson.decode $
        "{\"getPubKeyHash\" : \"ab4693ba26d310cca1e7811e6df9e845c219b5fa70923a4ba305f5ab\"}" 

      validator = VS.validatorScript $ govInstance acGov
      policy = xGovMintingPolicy acGov
      xGovCurrSymbol = scriptCurrencySymbol policy
      initNftDatum = GovernanceDatum userOnePKH xGovCurrSymbol




  let validatorPath = outDir ++ "/GovScript.plutus"
  validatorToPlutus validatorPath validator
  scriptBytes <-  BS.readFile validatorPath
  case deserialiseScriptInAnyLang scriptBytes of
    Right (ScriptInAnyLang _lang script)  -> do
       print "Deserialized hash:"
       print $ hashScript script
    Left _ -> print "N-OK"

  print "Validator hash:"
  print $ VS.validatorHash $ govInstance acGov
  print "Validator address:"
  print $ govAddress acGov

  -- policyToPlutus (outDir ++ "/GovPolicy.plutus") policy
  -- writeData (outDir ++ "/user1-init-datum.json") initNftDatum
