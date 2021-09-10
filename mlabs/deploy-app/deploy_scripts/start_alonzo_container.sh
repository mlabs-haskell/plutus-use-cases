#!/usr/bin/env bash

IMAGE=stackchain/alonzopurple:1.29.0
CONTAINER=alonzo_purple

docker rm $CONTAINER

docker run -ti \
  --name $CONTAINER \
  -v $(echo $PWD)/node_mnt:/home/cardano-my-node/node_mnt \
  -e MAGIC="--testnet-magic 8" \
  -e NETWORK=mainnet \
  -p 6000:6000 \
  $IMAGE \
  bash
