#!/usr/bin/env bash

# Git rid of unwanted utxo's at test addresses by sending them to another known address

set -euo pipefail

MAGIC=1097911063
TO_ADDR=addr_test1qp23jfx787cet3vqq0t48mdj8ngync9xt59ccv7gyncvze2hj3rlvqt4h9ga75hd855t5zw25gj3muduerutyuv44nhqdnhhzn

# FROM_PUB_KEY_HASH=bcd6bceeb0d22a7ca6ba1cd00669f7eb60ca8938d853666d30d56a56
# Addr: addr_test1vz7dd08wkrfz5l9xhgwdqpnf7l4kpj5f8rv9xendxr2k54su7ztr4

FROM_PUB_KEY_HASH=25bd24abedaf5c68d898484d757f715c7b4413ad91a80d3cb0b3660d
# Addr: addr_test1vqjm6f9takh4c6xcnpyy6atlw9w8k3qn4kg6srfukzekvrgkd8gfg

key_file=signing_keys/signing-key-$FROM_PUB_KEY_HASH.skey

#  --tx-out "$TO_ADDR + 1379280 + 10 7d4b0a8f5e31f4939505914f3d175205a9c08073ce9465e4eecacf98.freeGovUser2" \
cardano-cli transaction build  --alonzo-era --testnet-magic $MAGIC \
  --tx-in "b134b71f93c2e24bcb13c6c7a225e916ecd2097366e42d0bc5e5487b84271018#1" \
  --tx-in "ff4bccbde40a475e71faeeb433866e2dd006b52a87e7da4e234f6f7c0f5b284d#3" \
  --tx-out "$TO_ADDR + 2000000 + 105 7d4b0a8f5e31f4939505914f3d175205a9c08073ce9465e4eecacf98.614bef312465629ed825bc23de97932a + 100000000 7d4b0a8f5e31f4939505914f3d175205a9c08073ce9465e4eecacf98.614bef312465629ed825bc23de97932a" \
  --change-address "$TO_ADDR" \
  --out-file transactions/bash-script-tx.raw

cardano-cli transaction sign  --testnet-magic $MAGIC \
  --signing-key-file $key_file \
  --tx-body-file transactions/bash-script-tx.raw \
  --out-file transactions/bash-script-tx.sign

cardano-cli transaction submit  --testnet-magic $MAGIC \
  --tx-file transactions/bash-script-tx.sign