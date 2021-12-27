#!/usr/bin/env bash

set -euo pipefail

PKH=$1
address=$(cat signing_keys/address-"$PKH".addr)

cardano-cli query utxo --testnet-magic 1097911063 \
--address "$address"