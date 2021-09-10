#!/usr/bin/env bash
NAME=$1

ADDR=$(cat addrs/$NAME-payment.addr)
API_KEY=jv3NBtZeaL0lZUxgqq8slTttX3BzViI7

curl -v -XPOST "https://faucet.alonzo-purple.dev.cardano.org/send-money/$ADDR?apiKey=$API_KEY"