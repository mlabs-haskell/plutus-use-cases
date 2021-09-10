#!/usr/bin/env bash
set -xeuo pipefail

cardano-cli transaction sign \
  ${MAGIC} \
  --signing-key-file keys/alice-payment.skey \
  --tx-body-file transactions/tx.raw \
  --out-file transactions/tx.sign

cardano-cli transaction submit ${MAGIC}  --tx-file transactions/tx.sign