#!/usr/bin/env bash

IMAGE=stackchain/alonzopurple:1.29.0
CONTAINER=alonzo_purple

docker rm $CONTAINER

docker run -ti \
  --name $CONTAINER \
  -v ~/dev/mlabs/contract_deploy/node_mnt:/home/cardano-my-node/node_mnt \
  -e MAGIC="--testnet-magic 8" \
  $IMAGE bash
