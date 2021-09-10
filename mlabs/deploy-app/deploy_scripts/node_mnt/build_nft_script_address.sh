#!/usr/bin/env bash

set -euo pipefail

cardano-cli address build ${MAGIC} \
    --payment-script-file plutus_files/NftScript.plutus \
    --out-file addrs/nft-script-payment.addr

chmod -R 777 .