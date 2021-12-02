#!/usr/bin/env bash
set -euo pipefail

OWN_ADDRESS=addr_test1qp07kunx3edyal7nkdqg32jkul243a66lere3wpkffeevumspe9kny7tchnp97rhp2jnzfpm8kyg4qwq53k7t7jldfcqkngyzu
RECEIVER_ADDRESS=addr_test1qz3apv2ekuctf55fqa5psgaxeg24eeg0sc2wqqe9m259h5w6sk0nka6kca9ar7fwgxfg5khh4tkakp7cntexcat5x74q48ns3a

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
  \"receiverAddress\": \"$RECEIVER_ADDRESS\",
  \"collateral\": {
      \"txOutRefId\": {
          \"getTxId\": \"513aefa8cce5435985cef0795a96cbbd3937fce77ad1ed715ce6df77a15fe27f\"
          },
      \"txOutRefIdx\": 0
  }
}" \
localhost:9080/api/contract/instance/$contract_id/endpoint/call-demo
sleep 4

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
