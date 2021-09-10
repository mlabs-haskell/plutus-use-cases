#!/usr/bin/env bash
set -euo pipefail

WORKDIR=$PWD

USER=$1
AMOUNT=$2
IN_UTXO=$3
COLLATERAL_UTXO=$IN_UTXO
TOKEN_NAME=GOV

mintscript=$WORKDIR/plutus_files/MintingPolicy.plutus
policyid=$(cardano-cli transaction policyid --script-file $mintscript)
pparams=$WORKDIR/pparams.json


echo "► Building transaction"
cardano-cli transaction build --alonzo-era --cardano-mode ${MAGIC} --protocol-params-file $pparams \
  --change-address "$(cat $WORKDIR/keys/$USER-payment.addr)" \
  --tx-in "$IN_UTXO" \
  --tx-in-collateral "$COLLATERAL_UTXO" \
  --mint-redeemer-value "[]" \
  --mint-script-file "$mintscript" \
  --tx-out "$(cat $WORKDIR/keys/$USER-payment.addr) + 2000000 + $AMOUNT $policyid.$TOKEN_NAME" \
  --mint "$AMOUNT $policyid.$TOKEN_NAME" \
  --out-file "$WORKDIR/tx/gov-mint.raw"

echo "► Signing transaction"
cardano-cli transaction sign ${MAGIC} \
  --tx-body-file "$WORKDIR/tx/gov-mint.raw" \
  --signing-key-file $WORKDIR/keys/$USER-payment.skey \
  --out-file "$WORKDIR/tx/gov-mint.sign"

echo "► Submitting transaction"
cardano-cli transaction submit --tx-file "$WORKDIR/tx/gov-mint.sign" ${MAGIC}