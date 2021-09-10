#!/usr/bin/env bash
ADDR=$1

cardano-cli query utxo ${MAGIC} --address "$(cat $PWD/keys/$ADDR-payment.addr)"