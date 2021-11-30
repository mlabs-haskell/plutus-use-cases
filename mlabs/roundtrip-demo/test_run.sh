#!/usr/bin/env bash
set -euo pipefail

OWN_ADDRESS=addr_test1qq26x09u745wrmprz326jwgkmpkraggwu6d4rlgfq46esq2s6f75svcp9gdkl98xn2m08wun0xk0gm2gwahxl2xy9dkszzn8cm
RECEIVER_ADDRESS=addr_test1qq7e0hr837nwr799y7gk7nzs4gq603c6lx2lufvjt5jyqrskgm69l6d3fjd3lkp0knc97t8rsk9r35jrg88kc0m9sj3q52xml6

echo "Activating contract"
contract_id=$(curl --location --request POST 'localhost:9080/api/contract/activate' \
              --header 'Content-Type: application/json' \
              --data-raw "{
                  \"caID\": {
                      \"contents\": {
                          \"ownAddress\": \"$OWN_ADDRESS\"
                      },
                      \"tag\": \"Roundtrip\"
                  }
              }" | jq -r '.unContractInstanceId')

echo "Contract ID: " $contract_id
sleep 1

echo "Callig edpoint"
curl -H 'Content-Type: application/json'  -X POST \
--data-raw "{
  \"lovelaceAmount\": 7000000,
  \"receiverAddress\": \"$RECEIVER_ADDRESS\"
}" \
localhost:9080/api/contract/instance/$contract_id/endpoint/call-demo
sleep 3

echo "State:"
curl localhost:9080/api/contract/instance/$contract_id/status | jq

echo "-------------------------------------------------------------------------"
cbor=$(curl localhost:9080/api/contract/instance/$contract_id/status | jq -r '.cicYieldedExportTxs | .[0].transaction')
echo "CBOR: " $cbor


# encodedCbor=$(echo $cbor | xxd -r -p | base64 -w 0)
# echo "ENCODED CBOR: " $encodedCbor

# signed=$(curl -X POST 'localhost:8090/v2/wallets/8da9c0f87cc6feaca3f6c5e60f889842da9b4c53/transactions-sign' \
# --header 'Content-Type: application/json' \
# --data-raw "{
#     \"passphrase\": \"1234567891\",
#     \"transaction\": \"$encodedCbor\"
# }" | jq -r '.transaction')

# echo "SIGNED:" "$signed"

# file=./tx.bin
# rm -rf $file

# base64 -d > $file <<< "$signed"

# curl -X POST 'localhost:8090/v2/proxy/transactions' \
# --header 'Content-Type: application/octet-stream' \
# --data-binary "@/$file" \
# | jq