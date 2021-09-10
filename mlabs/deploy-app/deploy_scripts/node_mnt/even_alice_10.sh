#!/usr/bin/env bash
set -xeuo pipefail

FROM=alice
TO=$FROM
AMOUNT=10000000000


cardano-cli transaction build \
  --alonzo-era \
  --tx-in 2b70547a10b256f4693564692ac5fabd29ae48a6797b4f92884736d39922daab#0 \
  --tx-in 3ddaf88844265519237ecb9b5fed9a08ef9845dc5ab6cdae4402c49ebd90a478#1 \
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