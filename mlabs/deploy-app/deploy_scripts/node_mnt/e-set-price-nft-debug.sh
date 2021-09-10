#!/usr/bin/env bash
set -euo pipefail


TOKEN_NAME=OneCoin
mintscript=plutus_files/MintingPolicy.plutus
policyid=$(cardano-cli transaction policyid --script-file $mintscript)

SCRIPT=plutus_files/NftScript.plutus
SCRIPT_ADDR=$(cat addrs/nft-script-payment.addr)
INIT_DATUM=nft_data/init-datum.json
SET_PRICE_DATUM_HASH=$(cardano-cli transaction hash-script-data --script-data-file nft_data/stop-sell-datum.json)
SET_PRICE_RMR=nft_data/stop-sell-redeemer.json

SPEND_UTXO=8cf79ca29e5d62ad3c62c6f43c38a1eb1db75f0338e3c8ded5c7ca7be910eef3#1
COLLATERAL=8774492fb30d172c3c806951fce0c97e1652d3e9e219c8b136fb5300f5977f0a#1


echo "Setting price"
echo "► Building spending transaction"

cardano-cli transaction build ${MAGIC} \
  --alonzo-era \
  --protocol-params-file params/pparams.json \
  --tx-in "$SPEND_UTXO" \
  --tx-in-script-file "$SCRIPT" \
  --tx-in-datum-file "$INIT_DATUM" \
  --tx-in-redeemer-file "$SET_PRICE_RMR" \
  --tx-in-collateral "$COLLATERAL" \
  --tx-out "$SCRIPT_ADDR + 2000000 + 1 $policyid.$TOKEN_NAME" \
  --tx-out-datum-hash "$SET_PRICE_DATUM_HASH" \
  --change-address "$SCRIPT_ADDR" \
  --out-file transactions/spend-tx.raw
