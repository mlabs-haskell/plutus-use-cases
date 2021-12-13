#!/usr/bin/env bash
set -euo pipefail

rm -f ./pab-core.db
../result/bin/nft-buy-set-price --config ./plutus-pab.yaml migrate 
../result/bin/nft-buy-set-price --config ../nft-buy-set-price/plutus-pab.yaml webserver
