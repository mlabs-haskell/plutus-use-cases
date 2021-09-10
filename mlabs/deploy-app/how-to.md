# Readme

## How To Manually Do Transactions

-inside the docker image-

0. Start a passive node with the script at:
   `./node_mnt/start_node.sh`

1. In a new tab query the node with the script at:
   `~/cardano-my-node/gLiveView.sh`

2. Create a wallet address, providing as argument the user-name:
   `./node_mnt/make_wallet.sh tUser1`
     - it will save the user inside the folder named keys

3. Faucet some ADA to this wallet using the faucet script and the user-name.

4. Built the on-chain components of the script using `Deploy` module:
   `cabal new-run exe:deploy-app -- Governance`

5. Build a script address with:
   `mlabs/deploy-app/deploy_scripts/node_mnt/governance/build_script_address.sh`

## How to Faucet EUTXOs to an address

```bash
#!/usr/bin/env bash
NAME=$1

ADDR=$NAME-payment.adr
API_KEY=jv3NBtZeaL0lZUxgqq8slTttX3BzViI7

curl -v -XPOST "https://faucet.alonzo-purple.dev.cardano.org/send-money/$ADDR?apiKey=$API_KEY"
```

Also available in: `mlabs/deploy-app/deploy_scripts/node_mnt/faucet_to.sh`


## How to see the EUTXOs at an address

Run:

```bash
cardano-cli query \
    utxo ${MAGIC} \ 
    --address "user-payment.addr"
```

Also available at:
    `mlabs/deploy-app/deploy_scripts/node_mnt/utxos.sh`.
