#!/usr/bin/env bash

set -xeuo pipefail

NAME=$1

cardano-cli address key-gen \
--verification-key-file "keys/$NAME"-payment.vkey \
--signing-key-file "keys/$NAME"-payment.skey

cardano-cli stake-address key-gen \
--verification-key-file "keys/$NAME"-stake.vkey \
--signing-key-file "keys/$NAME"-stake.skey

cardano-cli address build \
--payment-verification-key-file "keys/$NAME"-payment.vkey \
--stake-verification-key-file "keys/$NAME"-stake.vkey \
--out-file "addrs/$NAME"-payment.addr \
${MAGIC}