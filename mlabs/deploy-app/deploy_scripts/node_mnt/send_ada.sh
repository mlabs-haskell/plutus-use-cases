#!/usr/bin/env bash
set -xeuo pipefail

FROM=$1
TO=$2
AMOUNT=$3


FUNDS_FILE=transactions/$FROM-funds.json
cardano-cli query utxo ${MAGIC} --address $(cat addrs/$FROM-payment.addr) --out-file $FUNDS_FILE
UTXO=$(jq -r 'keys[]' $FUNDS_FILE)


cardano-cli transaction build \
  --alonzo-era \
  --tx-in ${UTXO} \
  --tx-out $(cat addrs/$TO-payment.addr)+$AMOUNT \
  --change-address $(cat addrs/$FROM-payment.addr) \
  ${MAGIC} \
  --out-file transactions/tx.raw

cardano-cli transaction sign \
  ${MAGIC} \
  --signing-key-file keys/$FROM-payment.skey \
  --tx-body-file transactions/tx.raw \
  --out-file transactions/tx.sign

cardano-cli transaction submit ${MAGIC}  --tx-file transactions/tx.sign