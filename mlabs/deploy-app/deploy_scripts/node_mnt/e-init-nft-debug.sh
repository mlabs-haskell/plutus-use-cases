#!/usr/bin/env bash
set -euo pipefail

pparams=params/pparams.json

# USER=$1
USER=alice

mintscript=plutus_files/NftPolicy.plutus
policyid=$(cardano-cli transaction policyid --script-file $mintscript)
TOKEN_NAME=$(cat nft_data/t_name)
AMOUNT=1

SCRIPT_ADDR=$(cat addrs/nft-script-payment.addr)
INIT_DATUM_HASH=$(cardano-cli transaction hash-script-data --script-data-file nft_data/init-datum.json)

NFT_MINT_UTXO=2b70547a10b256f4693564692ac5fabd29ae48a6797b4f92884736d39922daab#0
COLLATERAL_UTXO=8774492fb30d172c3c806951fce0c97e1652d3e9e219c8b136fb5300f5977f0a#1

echo "► Building transaction"
echo "  Inti datum hash: $INIT_DATUM_HASH"
cardano-cli transaction build \
  --alonzo-era \
  --cardano-mode \
  ${MAGIC} \
  --change-address "$(cat addrs/$USER-payment.addr)" \
  --tx-in "$NFT_MINT_UTXO" \
  --tx-in-collateral "$COLLATERAL_UTXO" \
  --tx-out "$SCRIPT_ADDR + 2000000 + $AMOUNT $policyid.$TOKEN_NAME" \
  --tx-out-datum-hash "$INIT_DATUM_HASH" \
  --mint "$AMOUNT $policyid.$TOKEN_NAME" \
  --mint-redeemer-value 42 \
  --mint-script-file "$mintscript" \
  --protocol-params-file $pparams \
  --out-file "transactions/nft-init.raw"

echo "► Signing transaction"
cardano-cli transaction sign \
  --tx-body-file "transactions/nft-init.raw" \
   ${MAGIC} \
  --signing-key-file keys/$USER-payment.skey \
  --out-file "transactions/nft-init.sign"

echo "► Submitting transaction"
cardano-cli transaction submit --tx-file "transactions/nft-init.sign" ${MAGIC}

