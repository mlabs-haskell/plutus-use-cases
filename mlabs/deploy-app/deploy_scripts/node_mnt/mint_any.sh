#!/usr/bin/env bash
set -euo pipefail

USER=$1
TOKEN_NAME=$2
AMOUNT=$3
IN_UTXO=$4
COLLATERAL_UTXO=$IN_UTXO

mintscript=plutus_files/MintingPolicy.plutus
policyid=$(cardano-cli transaction policyid --script-file $mintscript)
pparams=params/pparams.json


echo "► Building transaction"
cardano-cli transaction build \
  --alonzo-era \
  --cardano-mode \
  ${MAGIC} \
  --change-address "$(cat addrs/$USER-payment.addr)" \
  --tx-in "$IN_UTXO" \
  --tx-in-collateral "$COLLATERAL_UTXO" \
  --mint-redeemer-value 42 \
  --mint-script-file "$mintscript" \
  --tx-out "$(cat addrs/$USER-payment.addr) + 2000000 + $AMOUNT $policyid.$TOKEN_NAME" \
  --mint "$AMOUNT $policyid.$TOKEN_NAME" \
  --protocol-params-file $pparams \
  --out-file "transactions/any-mint.raw"

echo "► Signing transaction"
cardano-cli transaction sign \
  --tx-body-file "transactions/any-mint.raw" \
   ${MAGIC} \
  --signing-key-file keys/$USER-payment.skey \
  --out-file "transactions/any-mint.sign"

# echo "► Submitting transaction"
# cardano-cli transaction submit --tx-file "transactions/any-mint.sign" ${MAGIC}