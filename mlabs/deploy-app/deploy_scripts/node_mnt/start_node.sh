#!/usr/bin/env bash

mkdir /home/cardano-my-node/sockets

cardano-node run \
    --topology /home/cardano-my-node/alonzo-purple-topology.json \
    --database-path /home/cardano-my-node/db \
    --socket-path /home/cardano-my-node/sockets/node.socket \
    --host-addr 0.0.0.0 \
    --port 6000 \
    --config /home/cardano-my-node/alonzo-purple-config.json