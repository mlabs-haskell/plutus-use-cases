# Hosted PAB setup
This document describes how to prepare hosted PAB deployment from scratch that can operate on Alonzo purple testnet.

The following required to be run on host machine to use PAB with contracts in hosted scenario on testnet (for 2020-11-05 PAB Release):
- Cardano node connected to Alonzo testnet
- [cardano wallet](https://github.com/input-output-hk/cardano-wallet) connected to node
- [chain-index](https://github.com/input-output-hk/plutus-apps/tree/main/plutus-chain-index) connected to node
- PAB executable connected to node, cardano-wallet and chain-index

Note: we are using somewhat more involved setup with a bit custom docker compose, and wallet and chain-index being built from sources, but there is ready to go [node+wallet docker solution available](https://input-output-hk.github.io/cardano-wallet/user-guide/Docker)


## Starting cardano node
This section describes how to start Cardano node connected to Alonzo testnet using Docker (and docker-compose).
### Step 1
Make directory that will store node data and `cd` into it.

Create two files ([reference](https://github.com/input-output-hk/Alonzo-testnet/tree/main/Alonzo-solutions/exercise1/docker)):

`docker-compose.yaml`
```
version: '3.3'
services:
    cardano-node:
        image: inputoutput/cardano-node:${NODE_TAG}
        restart: always
        volumes:
          - ./configuration/config:/app/cardano/config
          - ./configuration/topology:/app/cardano/topology
          - ./configuration/sockets:/app/cardano/sockets
          - ./database:/db
          - ./../scripts:/deployment/scripts
        command: "run --topology /app/cardano/topology/alonzo-purple-topology.json --config /app/cardano/config/alonzo-purple-config.json --port 3001 --host-addr 0.0.0.0 --database-path /db --socket-path /app/cardano/sockets/node.socket"
        ports:
          - 3001
        environment:
          - CARDANO_NODE_SOCKET_PATH=/app/cardano/sockets/node.socket

```

`start.sh`
```
#!/bin/sh

export NODE_TAG=$1
CONFIG=https://hydra.iohk.io/build/7366583/download/1/alonzo-purple-config.json
BYRON_GENESIS=https://hydra.iohk.io/build/7366583/download/1/alonzo-purple-byron-genesis.json
SHELLEY_GENESIS=https://hydra.iohk.io/build/7366583/download/1/alonzo-purple-shelley-genesis.json
ALONZO_GENESIS=https://hydra.iohk.io/build/7366583/download/1/alonzo-purple-alonzo-genesis.json
TOPOLOGY=https://hydra.iohk.io/build/7189190/download/1/alonzo-purple-topology.json

##Making some folders
mkdir -p ./configuration/config/
mkdir -p ./configuration/topology/
mkdir -p ./configuration/sockets/

##Making DB Folder
mkdir -p ./database/

##Touch for a Socket
touch ./configuration/sockets/node.socket

##Getting Config
echo "--getting config"
wget  $CONFIG -P ./configuration/config
wget  $BYRON_GENESIS -P ./configuration/config/
wget  $SHELLEY_GENESIS -P ./configuration/config/
wget  $ALONZO_GENESIS -P ./configuration/config/

##Getting Topology
echo "--getting topology"
wget $TOPOLOGY -P ./configuration/topology/

##Starting Docker-Compose
docker-compose up
```

### Step 2
Start node with desired tag using `start.sh`. As currently working example:
```
./start.sh 1.30.1
```
This will download required files, pull image and run it providing node socket at  `./configuration/sockets/node.socket` location.

It will take some time for node to sync. Sync status could be checked with `cardano-cli` tool using `tip` query, e.g.:
```
cardano-cli query tip --testnet-magic 8
```
Tip of synced node looks something like this (note `syncProgress`):
```
{
    "epoch": 1005,
    "hash": "162d6541cc5aa6b0e098add8fa08a94660a08b9463c0a86fcf84661b5f63375f",
    "slot": 7232440,
    "block": 322985,
    "era": "Alonzo",
    "syncProgress": "100.00"
}
```
### Appendix
- [node monitoring tool](https://github.com/input-output-hk/cardano-rt-view/)


## Starting cardano wallet
:information_source: [GtiHub repo](https://github.com/input-output-hk/cardano-wallet)

### Step 1
[Install cardano wallet](https://input-output-hk.github.io/cardano-wallet/user-guide/installation) and start it.

```
cardano-wallet serve \
  --node-socket $CARDANO_NODE_SOCKET_PATH \
  --database /path/to/wallet/database \
  --testnet /path/to/node/configuration/config/alonzo-purple-byron-genesis.json \
  --listen-address 0.0.0.0 \
  --port 8090
```

### Step 2
Create wallet for tests (which PAB will use).
We are usually using [CLI](https://input-output-hk.github.io/cardano-wallet/user-guide/cli) to generate `recovery-phrase` and then create wallet `from-recovery-phrase`.

### Step 3
Get some funds on that wallet (not sure if faucet awailable now, we just had enough tAda on some addresses from previous tests - can share if required =) )

## Start chain-index
We building `chain-index` from sources from desired commit of [plutus-apps](https://github.com/input-output-hk/plutus-apps) and staring it, like:
```
cd plutus-apps
nix-shell
cd plutus-chain-index
cabal build
cabal run exe:plutus-chain-index -- --socket-path $CARDANO_NODE_SOCKET_PATH --db-path /path/to/chain-index.db --network-id 8 start-index

```
It will probably take some time for chain-index to sync. Request to `localhost:chain-index-port/tip` can be performed to see current status.

## Start PAB
### Step 1
Add PAB executable ([example](https://github.com/input-output-hk/plutus-apps/blob/main/plutus-pab/examples/Main.hs)) and build it.
### Step 2
Prepare config ([sample](https://github.com/input-output-hk/plutus-apps/blob/main/plutus-pab/plutus-pab.yaml.sample)) - here you specify where PAB should send requests to cardano-wallet, chain-index and node socket.
We currently have test config like this as example (note `mscNodeMode: AlonzoNode`):
```
dbConfig:
    dbConfigFile: pab-core.db
    dbConfigPoolSize: 20

pabWebserverConfig:
  baseUrl: http://localhost:9080
  staticDir: ./dist
  permissiveCorsPolicy: False
  # Optional timeout (in seconds) for calls to endpoints that are not currently
  # available. If this is not set, calls to unavailable endpoints fail
  # immediately.
  endpointTimeout: 5

walletServerConfig:
  baseUrl: http://localhost:8090
  wallet:
    getWallet: 1

nodeServerConfig:
  mscBaseUrl: http://localhost:9082
  mscSocketPath: /our/path/to/node.socket
  mscKeptBlocks: 100
  mscNetworkId: "8" # Testnet network ID (main net = empty string)
  mscSlotConfig:
    scSlotZeroTime: 1591566291000 # Wednesday, July 29, 2020 21:44:51 - shelley launch time in milliseconds
    scSlotLength: 1000 # In milliseconds
  mscFeeConfig:
    fcConstantFee:
      getLovelace: 10 # Constant fee per transaction in lovelace
    fcScriptsFeeFactor: 1.0 # Factor by which to multiply size-dependent scripts fee in lovelace
  mscInitialTxWallets:
    - getWallet: 1
    - getWallet: 2
    - getWallet: 3
  mscNodeMode: AlonzoNode

chainIndexConfig:
  ciBaseUrl: http://localhost:9083
  ciWatchedAddresses: []

requestProcessingConfig:
  requestProcessingInterval: 1

signingProcessConfig:
  spBaseUrl: http://localhost:9084
  spWallet:
    getWallet: 1

metadataServerConfig:
  mdBaseUrl: http://localhost:9085

# Optional EKG Server Config
# ----
# monitoringConfig:
#   monitoringPort: 9090
```

Now you cas start PAB and start serving your contracts at `pabWebserverConfig.baseUrl`, e.g.:
```
cabal exec nft-marketplace -- --config ./nft-marketplace/plutus-pab.yaml migrate (creates database)  
cabal exec nft-marketplace -- --config ./nft-marketplace/plutus-pab.yaml --passphrase WALLET_PASSPHRASE webserver  
```
For `WALLET_PASSPHRASE` specify passphrase of wallet created earlier with `cardano-wallet`.

After PAB started (watch for `Starting PAB backend server on port 9080` in logs) contracts can be activated. E.g.:
```
curl --location --request POST 'localhost:9080/api/contract/activate' \
--header 'Content-Type: application/json' \
--data-raw '{
    "caID": {
        "tag": "UserContract",
        "contents": {
            "someParam": "param"
        }
    },
    "caWallet": {
        "getWalletId": "WALLET_ID"
    }
}'
```
(for `WALLET_ID` we are using wallet which passphrase was provided to start PAB, as it will be dong transaction signing)