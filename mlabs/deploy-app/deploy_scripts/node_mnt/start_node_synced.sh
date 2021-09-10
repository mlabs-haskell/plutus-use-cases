#!/usr/bin/env bash
set -euo pipefail

echo "Starting node"
cardano-node run \
  --topology /home/cardano-my-node/alonzo-purple-topology.json \
  --database-path /home/cardano-my-node/db \
  --socket-path /home/cardano-my-node/db/socket \
  --host-addr 0.0.0.0 \
  --port 6000 \
  --config /home/cardano-my-node/alonzo-purple-config.json \
  > /dev/null 2>&1 &

sleep 10
echo "Node started"
echo "Waiting node to sync"

tip=$(cardano-cli query tip $MAGIC)
while  [ "$(echo $tip | jq '.syncProgress')" != "\"100.00\"" ]; do
        echo $tip
        echo "Waiting 20 seconds more..."
        sleep 20
        tip=$(cardano-cli query tip $MAGIC)
done
echo "Node synced"

echo "Initializing contract"