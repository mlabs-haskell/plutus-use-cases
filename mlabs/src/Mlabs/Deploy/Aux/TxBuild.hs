{-# LANGUAGE UndecidableInstances #-}

module Mlabs.Deploy.Aux.TxBuild (
deposit
, UserName
, Amount
, ScriptInp(..)  
) where

import Prelude 

appDir = "/home/mike/dev/mlabs/plutus-use-cases/mlabs/deploy-app/application"
usersDir = appDir ++ "/users"
scriptsDir = appDir ++ "/scripts"
params = appDir ++ "/pparams.json"

type CliParams = [String]

withDefault :: CliParams -> CliParams
withDefault ps = ["--testnet-magic", "8", "--protocol-params-file", params] ++ ps

callCli :: CliParams -> IO ()
callCli ps = do
  let params = withDefault ps
  print params
  return ()

type UserName = String
type Amount = Integer
type UserAddr = String
type ScriptAddr = String
type GOVPolicyId = String
type XGOVPolicyId = String
type DatumHash = String

data ScriptInp = ScriptInp {sTxId :: String, balance :: Integer}

deposit :: UserName -> Amount -> ScriptInp -> IO ()
deposit userName amt scrInput = do
  unsignedBody <- buildTx userName amt scrInput
  signedBody   <- signTx userName unsignedBody
  submitTx userName signedBody
  

buildTx :: UserName -> Amount -> ScriptInp -> IO FilePath
buildTx userName amt scrInput = do 
  let govPolicy = scriptsDir ++ "/GOVPolicy.plutus"
      xGovPolicy = scriptsDir ++ "/XGOVPolicy.plutus"

  (adaInput, govInput) <- getUserInputs userName
  userAddr     <- getUserAddr userName
  script       <- getScriptFile
  datum        <- getDatumFile userName
  redeemer     <- getRedeemerFile userName
  scriptAddr   <- readScriptAddr script
  govPolicyId  <- getPolicyId govPolicy
  xGovPolicyId <- getPolicyId xGovPolicy
  datHash      <- getDatumHash datum
  let 
    unsignedBody = appDir ++ "/tx" ++ "/tx.raw"     
    params = 
        makeParams
          userName
          amt
          userAddr
          adaInput
          govInput
          scrInput
          script
          datum
          redeemer
          scriptAddr
          govPolicyId
          xGovPolicyId
          datHash
          xGovPolicy
          unsignedBody
    -- cli call here
  callCli params
  return unsignedBody


signTx :: FilePath -> UserName -> IO FilePath
signTx userName signedBody = 
  let signedBody = appDir ++ "/tx" ++ "/tx.sign" 
  in 
    -- cli call here
    return signedBody

submitTx  :: UserName -> FilePath -> IO ()
submitTx userName signedBody = 
  -- cli call here
  return ()


getUserAddr :: UserName -> IO UserAddr
getUserAddr userName = 
  readFile (usersDir ++ "/" ++ userName ++ "/payment.addr")

getScriptFile :: -> IO FilePath
getScriptFile = 
  return $ scriptsDir ++ "/" ++ "GovScript.plutus"

readScriptAddr :: FilePath -> IO String
readScriptAddr script = 
  return "dummy_script_addr"

getDatumFile :: UserName -> IO FilePath
getDatumFile userName = 
  return $ usersDir ++ "/" ++ userName ++ "/datum.json"

getRedeemerFile :: UserName -> IO FilePath
getRedeemerFile userName = 
  return $ usersDir ++ "/" ++ userName ++ "/redeemer.json"

getPolicyId :: FilePath -> IO String
getPolicyId policyFile = 
  -- node cli call here
  return "dummy_policy_id"

getDatumHash :: filePath -> IO String
getDatumHash datumFile = 
  -- node cli call here
  return "dummy_datum_hash"



makeParams 
  :: UserName ->
     Integer ->
     UserAddr -> 
     AdaInput -> 
     GovInput ->
     ScriptInp -> 
     FilePath -> 
     FilePath  -> 
     FilePath -> 
     ScriptAddr ->
     XGOVPolicyId -> 
     GOVPolicyId -> 
     DatumHash -> 
     FilePath  ->
     FilePath  ->
     CliParams
makeParams
  userName
  amount 
  userAddr 
  adaIn 
  govIn 
  scriptInp 
  scriptPath 
  datumPath 
  rdmrPath 
  scriptAddr
  govPolicyId
  xGovPolicyId
  datHash
  xGovMintPolicy 
  rawTxFile 
  =
    [ "--change-address",  userAddr
    , "--tx-in", (aTxId adaIn)
    , "--tx-in", (gTxId govIn) 
    , "--tx-in-collateral", aTxId adaIn
    , "--tx-in", (sTxId scriptInp)
    , "--tx-in-script-file", scriptPath
    , "--tx-in-datum-file", datumPath
    , "--tx-in-redeemer-file", rdmrPath
    , "--tx-out", toScriptOut
    , "--tx-out-datum-hash", datHash
    , "--tx-out",  govChangeOut
    , "--tx-out", xGovOut
    , "--mint", mint
    , "--mint-script-file", xGovMintPolicy
    , "--mint-redeemer-value",  "\"[]\""
    , "--out-file", rawTxFile

    ]
    where
      govAsset = govPolicyId ++ ".GOV"
      xGovAsset = xGovPolicyId ++ "." ++ userName ++ "XGov"
      toScriptOut = 
        ("\"" ++ scriptAddr ++ " 2000000 " ++ (show $ balance scriptInp + amount) ++ " " ++ govAsset ++ "\"")
      govChangeOut = 
        ("\"" ++ userAddr ++ " 2000000 " ++ (show $ currentGov govIn - amount) ++ " " ++ govAsset ++ "\"")
      xGovOut = ("\"" ++ userAddr ++ " 2000000 " ++ (show amount) ++ " " ++ xGovAsset ++ "\"")
      mint = "\"" ++ (show amount) ++ " " ++ xGovAsset ++ "\""
  
  



data AdaInput = AdaInput { aTxId :: String } deriving Show
data GovInput = GovInput {gTxId :: String, currentGov :: Integer} deriving Show

getUserInputs :: UserName -> IO (AdaInput, GovInput)
getUserInputs userName = 
  -- node cli call here
  return (AdaInput "ada_input", GovInput "gov_input" 100)