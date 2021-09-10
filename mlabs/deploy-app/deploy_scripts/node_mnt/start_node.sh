#!/usr/bin/env bash

cardano-node run \
    --topology /home/cardano-my-node/alonzo-purple-topology.json \
    --database-path /home/cardano-my-node/db \
    --socket-path /home/cardano-my-node/db/socket \
    --host-addr 0.0.0.0 \
    --port 6000 \
    --config /home/cardano-my-node/alonzo-purple-config.json