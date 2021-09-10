#!/usr/bin/env bash

set -euo pipefail

TO=$(cat addrs/bob-payment.addr)
CHANGE=$(cat addrs/alice-payment.addr)

cardano-cli transaction build-raw \
--alonzo-era \
--tx-in 8774492fb30d172c3c806951fce0c97e1652d3e9e219c8b136fb5300f5977f0a#1 \
--tx-out $TO+1000000 \
--tx-out  $CHANGE+9998000000 \
--fee 1000000 \
--protocol-params-file params/pparams.json \
--out-file transactions/raw-build.raw

cardano-cli transaction sign \
  ${MAGIC} \
  --signing-key-file keys/alice-payment.skey \
  --tx-body-file transactions/raw-build.raw \
  --out-file transactions/raw-build.sign

cardano-cli transaction submit ${MAGIC}  --tx-file transactions/raw-build.sign
