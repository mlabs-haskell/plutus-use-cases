#!/usr/bin/env bash

NAME=$1

cardano-cli query utxo ${MAGIC} --address "$(cat addrs/"$NAME"-payment.addr)"