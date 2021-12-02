#!/usr/bin/env bash
set -euo pipefail

rm -f /home/mike/dev/mlabs/plutus-use-cases/mlabs/roundtrip-demo/pab-core.db 
./result/bin/roundtrip-demo --config roundtrip-demo/plutus-pab.yaml migrate 
./result/bin/roundtrip-demo --config roundtrip-demo/plutus-pab.yaml --passphrase 1234567891 webserver