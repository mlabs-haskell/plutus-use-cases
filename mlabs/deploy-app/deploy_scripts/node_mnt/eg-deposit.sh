#!/usr/bin/env bash
set -euo pipefail

USER=$1

NFT_MINT_UTXO=3cd8e9aa9bb0880248b626e71600ba0d429bbefecc25aa04050b2b70a59170ae#0
COLLATERAL_UTXO=8774492fb30d172c3c806951fce0c97e1652d3e9e219c8b136fb5300f5977f0a#1

# ----- can't build as supposed
# TOKEN_NAME=$(cat nft_data/token.name)
# mintscript=plutus_files/NftPolicy.plutus
# ----- can't build as supposed - END

TOKEN_NAME=$(cardano-cli address key-hash --payment-verification-key-file keys/$USER-payment.vkey)
xgov_mintscript=plutus_files/GovPolicy.plutus
xgov_policyid=$(cardano-cli transaction policyid --script-file $mintscript)
SCRIPT_ADDR=$(cat addrs/gov-script-payment.addr)
INIT_DATUM_HASH=$(cardano-cli transaction hash-script-data --script-data-file nft_data/init-datum.json)
pparams=params/pparams.json


echo "Building transaction"
cardano-cli transaction build \
  --alonzo-era \
  --cardano-mode \
  ${MAGIC} \
  --change-address "$(cat addrs/$USER-payment.addr)" \
  --tx-in "$NFT_MINT_UTXO" \
  --tx-in-collateral "$COLLATERAL_UTXO" \
  --tx-out "$SCRIPT_ADDR + 2000000 + 1 $policyid.$TOKEN_NAME" \
  --tx-out-datum-hash "$INIT_DATUM_HASH" \
  --mint "1 $policyid.$TOKEN_NAME" \
  --mint-redeemer-value 42 \
  --mint-script-file "$mintscript" \
  --protocol-params-file $pparams \
  --out-file "transactions/gov-deposit.raw"

# echo "Signing transaction"
# cardano-cli transaction sign \
#   --tx-body-file "transactions/nft-mint.raw" \
#    ${MAGIC} \
#   --signing-key-file keys/$USER-payment.skey \
#   --out-file "transactions/nft-mint.sign"

# echo "Submitting transaction"
# cardano-cli transaction submit --tx-file "transactions/nft-mint.sign" ${MAGIC}

