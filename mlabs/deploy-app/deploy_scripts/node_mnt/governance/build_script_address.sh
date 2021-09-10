#!/usr/bin/env bash

set -euo pipefail

cardano-cli address build ${MAGIC} \
    --payment-script-file $PWD/plutus_files/GovScript.plutus \
    --out-file $PWD/keys/gov-script-payment.addr

chmod -R 777 .