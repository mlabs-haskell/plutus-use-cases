#!/usr/bin/env bash
set -euo pipefail

# First initial deposit for user-1 
# when there is no utxo's with his datum at script address

WORKDIR=$PWD
GOV_POLICY_ID=$(cardano-cli transaction policyid --script-file $WORKDIR/plutus_files/MintingPolicy.plutus)
pparams=$WORKDIR/pparams.json



USER=$1
CURRENT_GOV_BALANCE=$2
AMOUNT=$3
GOV_CHANGE=$(( $CURRENT_GOV_BALANCE - $AMOUNT ))
USER_GOV_INPUT=$4
ADA_INPU=$5
SCRIPT_INPUT=$6
SCRIPT_BALANCE=$7
NEW_SCRIPT_BALANCE=$(( $AMOUNT + $SCRIPT_BALANCE ))

COLLATERAL=$ADA_INPU

XGOV_POLICY=$WORKDIR/plutus_files/GovPolicy.plutus
XGOV_POLICY_ID=$(cardano-cli transaction policyid --script-file "$XGOV_POLICY")
XGOV_TOKEN_NAME=$(cardano-cli address key-hash --payment-verification-key-file $WORKDIR/keys/$USER-payment.vkey)

# XGOV_ASSET=$XGOV_POLICY_ID.$XGOV_TOKEN_NAME
# ^ we suppose to use users PKH for TokenName
# but it gfails with
# TextEnvelope decode error: DecoderErrorDeserialiseFailure "Shelley TxBody" (DeserialiseFailure 554 "An error occured while decoding asset name exceeds 32 bytes:.\nError: 6234333565663231323263376161623532653963383336323437663064653533366338623766306264356638383735323435383437333162")
XGOV_ASSET=$XGOV_POLICY_ID.${USER}XGovToken

SCRIPT_ADDR=$(cat $WORKDIR/keys/gov-script-payment.addr)
USER_ADDR=$(cat $WORKDIR/keys/$USER-payment.addr)

SCRIPT=$WORKDIR/plutus_files/GovScript.plutus


echo "► Building transaction"
echo "  $USER deposits $AMOUNT"
echo "  xGOV token name for $USER is $XGOV_TOKEN_NAME"
echo "  xGOV policy id is $XGOV_POLICY_ID"
echo "  GOV policy id is $GOV_POLICY_ID"
echo "  GOV change to $USER is $GOV_CHANGE"

REDEEMER_VAL="[]"
CURRENT_DATUM=$WORKDIR/plutus_files/user1-init-datum.json
NEXT_DATUM_HASH=$(cardano-cli transaction hash-script-data --script-data-file $CURRENT_DATUM)


cardano-cli transaction build --alonzo-era --cardano-mode ${MAGIC} --protocol-params-file $pparams \
  --change-address "$USER_ADDR" \
  --tx-in "$ADA_INPU" \
  --tx-in "$USER_GOV_INPUT" \
  --tx-in-collateral "$COLLATERAL" \
  --tx-in "$SCRIPT_INPUT" \
  --tx-in-script-file "$SCRIPT" \
  --tx-in-datum-file "$CURRENT_DATUM" \
  --tx-in-redeemer-value "$REDEEMER_VAL" \
  --tx-out "$SCRIPT_ADDR + 2000000 + $NEW_SCRIPT_BALANCE $GOV_POLICY_ID.GOV" \
  --tx-out-datum-hash "$NEXT_DATUM_HASH" \
  --tx-out "$USER_ADDR + 2000000 + $GOV_CHANGE $GOV_POLICY_ID.GOV" \
  --tx-out "$USER_ADDR + 2000000 + $AMOUNT $XGOV_ASSET" \
  --mint "$AMOUNT $XGOV_ASSET" \
  --mint-script-file "$XGOV_POLICY" \
  --mint-redeemer-value "[]" \
  --out-file "$WORKDIR/tx/deposit.raw"

echo "► Signing transaction"
cardano-cli transaction sign ${MAGIC} \
  --tx-body-file "$WORKDIR/tx/deposit.raw" \
  --signing-key-file keys/"$USER"-payment.skey \
  --out-file "$WORKDIR/tx/$USER-deposit.sign"

echo "► Submitting transaction"
cardano-cli transaction submit ${MAGIC} \
  --tx-file "$WORKDIR/tx/$USER-deposit.sign" 

