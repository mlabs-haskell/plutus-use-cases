#!/usr/bin/env bash

# Performs contract activation and call to initialization endpooint
# As local cluster setup is fully reporducable, wallet ID and its PKH stayes the same

set -euo pipefail

WALLET_ID=2d4cc31a4b3116ab86bfe529d30d9c362acd0b44
WALLET_PKH=9ed7d88109a20fc0ffe82926040f0768a6bd1dbeceb8124a9050ff85

echo "Activating contract"
contract_id=$(curl -X POST 'localhost:9080/api/contract/activate' \
                   -H 'Content-Type: application/json' \
                   -d "{
                       \"caID\": {
                           \"tag\": \"NftAdminContract\"
                       },
                       \"caWallet\": {
                           \"getWalletId\": \"$WALLET_ID\"
                       }
                   }" \
              | jq -r '.unContractInstanceId')

echo "NftAdminContract Contract ID: " $contract_id
sleep 2

echo "Callig \"app-init\" edpoint"
curl -H 'Content-Type: application/json'  -X POST \
--data-raw "{
    \"ip'admins\": [
        {
            \"getUserId\": {
                \"getPubKeyHash\": \"$WALLET_PKH\"
            }
        }
    ],
    \"ip'feeRate\": [1,2],
    \"ip'feePkh\": {
        \"getPubKeyHash\": \"$WALLET_PKH\"
    }
}" \
localhost:9080/api/contract/instance/$contract_id/endpoint/app-init
# sleep 1

# echo "State:"
# curl localhost:9080/api/contract/instance/$contract_id/status | jq