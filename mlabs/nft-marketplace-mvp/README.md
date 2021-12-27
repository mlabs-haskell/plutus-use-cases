# NFT marketplace December demo

## Setup

### Prerequisites

- `cardano-cli` must be in `PATH`
- `CARDANO_NODE_SOCKET_PATH` environment variable must point to node socket
- node socket should have permissions for read and write
- `chain-index` should run at `localhost:9080` (it's hardcoded in `MLabsPAB.ChainIndex`, could changed there as well)
- `MlabsPAB` listens `localhost:9080`
- `MlabsPAB` expected to be added as git submodule; use branch `nft-december-demo` as it contains some critical tweaks that could be not merged to `main` yet - setup was prepared with `nft-december-demo` branch in mind
- setup was tested with `nix-shell` and then `cabal` to build and run, e.g.: `cabal build nft-marketplace-mvp && cabal exec nft-marketplace-mvp -- 25bd24abedaf5c68d898484d757f715c7b4413ad91a80d3cb0b3660d`
- to start MlabsPAB "user's" PubKeyHash must be provided, this PKH will be returned for PAB calls like `ownPubKeyHash`
- it will probably be a good ide to change validators a bit (e.g. with some extra `traceIfFalse "SomeThing" True`) to not to pollute address of real validator on tesntet with test or demo UTXOs 

### Users

User related keys and addresses reside in `signing_keys` dir.
There are two users at the moment:

```
PKH: bcd6bceeb0d22a7ca6ba1cd00669f7eb60ca8938d853666d30d56a56
Address: addr_test1vz7dd08wkrfz5l9xhgwdqpnf7l4kpj5f8rv9xendxr2k54su7ztr4
```

```
PKH: 25bd24abedaf5c68d898484d757f715c7b4413ad91a80d3cb0b3660d
Address: addr_test1vqjm6f9takh4c6xcnpyy6atlw9w8k3qn4kg6srfukzekvrgkd8gfg
```

There is helper script to get utxos at known user address by PHK: `pkh_utxos.sh`

## Calling contracts

Unlike IOG PAB, MlabsPAB doesn't start stateful web-services from Contract, it jsut executes contract effects once. So only `/api/contract/activate` endpoint is used. Also, there is currently no capabilities for awaiting transaction to complete, so NFT initialization contract was separated to two call - to mint `UniqueToken` and to mint list head.

Possible outcomes of Contract calls:
- Success: `Contract executed` printed to terminal
- Contract threw error: `Execution ERROR: Error message` printed to terminal
- `cardano-cli` threw error: error message from `cardano-cli` printed to terminal

Below is example how ti initiate NFT dApp, mint NFT, set price for it and buy.

(!) Example assumes, that user identified with `PubKeyHash` `bcd6bceeb0d22a7ca6ba1cd00669f7eb60ca8938d853666d30d56a56` is `Admin`, and `25bd24abedaf5c68d898484d757f715c7b4413ad91a80d3cb0b3660d` is `User`.

### Step 1 - Start PAB as `Admin`:
```
> nix-shell
> cabal build nft-marketplace-mvp && cabal exec nft-marketplace-mvp -- bcd6bceeb0d22a7ca6ba1cd00669f7eb60ca8938d853666d30d56a56
```
After `Starting MLabsPAB server` appears in the terminal, contract calls can be executed.

### Step 2  - Mint `UnuqueToken`

```
curl -X POST 'localhost:9080/api/contract/activate' \
-H 'Content-Type: application/json' \
-d '{
    "caID": {
        "tag": "GenerateUniqueToken"
    }
}'
```

After transaction submitted, query UTXOs for admin's PKH and get the `CyrrencySymbol` for `UniqueToken`:
```
./pkh_utxos.sh bcd6bceeb0d22a7ca6ba1cd00669f7eb60ca8938d853666d30d56a56
```
Result will be something like:
```
--------------------------------------------------------------------------------------
1db2cb2f7f4c42bfea760ccc098d63235520c60779ad07958d24053b79b9c43c     0        860307796 lovelace + TxOutDatumNone
1db2cb2f7f4c42bfea760ccc098d63235520c60779ad07958d24053b79b9c43c     1        2000000 lovelace + 2 eee39ea4d240fb25da720bc1c0c80aecf10722168adcf811b3465a79.UniqueAppToken + TxOutDatumNone
```

where `eee39ea4d240fb25da720bc1c0c80aecf10722168adcf811b3465a79` is the `CyrrencySymbol` of `UniqueToken`.

Further steps assume that we use Token `eee39ea4d240fb25da720bc1c0c80aecf10722168adcf811b3465a79.UniqueAppToken` to identify dApp instance.

### Step 3 - Mint List head

```
curl --location --request POST 'localhost:9080/api/contract/activate' \
--header 'Content-Type: application/json' \
--data-raw '{
    "caID": {
        "tag": "MintListHead",
        "contents": {
            "mhr'\''uniqueToken": {
                "unAssetClass": [
                    {
                        "unCurrencySymbol": "eee39ea4d240fb25da720bc1c0c80aecf10722168adcf811b3465a79"
                    },
                    {
                        "unTokenName": "UniqueAppToken"
                    }
                ]
            },
            "mhr'\''initParams": {
                "ip'\''admins": [
                    {
                        "getUserId": {
                            "getPubKeyHash": "bcd6bceeb0d22a7ca6ba1cd00669f7eb60ca8938d853666d30d56a56"
                        }
                    }
                ],
                "ip'\''feeRate": [
                    1,
                    2
                ],
                "ip'\''feePkh": {
                    "getPubKeyHash": "bcd6bceeb0d22a7ca6ba1cd00669f7eb60ca8938d853666d30d56a56"
                }
            }
        }
    }
}'
```

After transaction submitted you can verify that `UnuqueToken`'s no longer at `Admin`'s address with

```
./pkh_utxos.sh bcd6bceeb0d22a7ca6ba1cd00669f7eb60ca8938d853666d30d56a56
```

### Step 4 - Mint NFT

```
curl -X POST 'localhost:9080/api/contract/activate' \
-H 'Content-Type: application/json' \
-d '{
    "caID": {
        "tag": "MintNft",
        "contents": {
            "mnr'\''uniqueToken": {
                "unAssetClass": [
                    {
                        "unCurrencySymbol": "eee39ea4d240fb25da720bc1c0c80aecf10722168adcf811b3465a79"
                    },
                    {
                        "unTokenName": "UniqueAppToken"
                    }
                ]
            },
            "mnr'\''data": {
                "mnd'\''content": "Some Awesome NFT",
                "mnd'\''title": "NFT Title",
                "mnd'\''share": [
                    1,
                    2
                ],
                "mnd'\''price": null
            }
        }
    }
}'
```

This will mint NFT and put in the state "not for sale".

### Step 5 - Set Price

Note: originally to identify NFT contracts use `NftId`, which in turn is just wrapper for content hash, but to make life a bit easier for demo, NFT's content used for set price and buy calls to not to deal with hashes. Content gets converted to hash before passed to contract call.


```
curl --location --request POST 'localhost:9080/api/contract/activate' \
--header 'Content-Type: application/json' \
--data-raw '{
    "caID": {
        "tag": "SetNftPice",
        "contents": {
            "spr'\''uniqueToken": {
                "unAssetClass": [
                    {
                        "unCurrencySymbol": "eee39ea4d240fb25da720bc1c0c80aecf10722168adcf811b3465a79"
                    },
                    {
                        "unTokenName": "UniqueAppToken"
                    }
                ]
            },
            "spr'\''data": {
                "spd'\''content": "Some Awesome NFT",
                "spd'\''price": 200000000
            }
        }
    }
}'
```

### Step 5 - Buy NFT

Stop PAB and start it PAB as `User` (or start another instance on different port):


```
cabal exec nft-marketplace-mvp -- 25bd24abedaf5c68d898484d757f715c7b4413ad91a80d3cb0b3660d
```

Call buy contract:


```
curl -X POST 'localhost:9080/api/contract/activate' \
-H 'Content-Type: application/json' \
-d '{
    "caID": {
        "tag": "BuyNft",
        "contents": {
            "bnr'\''uniqueToken": {
                "unAssetClass": [
                    {
                        "unCurrencySymbol": "eee39ea4d240fb25da720bc1c0c80aecf10722168adcf811b3465a79"
                    },
                    {
                        "unTokenName": "UniqueAppToken"
                    }
                ]
            },
            "bnr'\''data": {
                "bnd'\''content": "Some Awesome NFT",
                "bnd'\''price": 200000000,
                "bnd'\''newPrice": null
            }
        }
    }
}'
```

As quick check, if transaction submitted successfully:

`Admin` won't be able to set price anymore.

`Admin` will get his shares: as admin, as author and as seller, something like:

```
> $ ./pkh_utxos.sh bcd6bceeb0d22a7ca6ba1cd00669f7eb60ca8938d853666d30d56a56
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
1db2cb2f7f4c42bfea760ccc098d63235520c60779ad07958d24053b79b9c43c     0        860307796 lovelace + TxOutDatumNone
ff4bccbde40a475e71faeeb433866e2dd006b52a87e7da4e234f6f7c0f5b284d     2        100000000 lovelace + TxOutDatumNone
ff4bccbde40a475e71faeeb433866e2dd006b52a87e7da4e234f6f7c0f5b284d     6        50000000 lovelace + TxOutDatumNone
ff4bccbde40a475e71faeeb433866e2dd006b52a87e7da4e234f6f7c0f5b284d     7        50000000 lovelace + TxOutDatumNone
```

`User` will get `Gov` tokens, something like:
```
> $ ./pkh_utxos.sh 25bd24abedaf5c68d898484d757f715c7b4413ad91a80d3cb0b3660d
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
b134b71f93c2e24bcb13c6c7a225e916ecd2097366e42d0bc5e5487b84271018     0        777648002 lovelace + TxOutDatumNone
ff4bccbde40a475e71faeeb433866e2dd006b52a87e7da4e234f6f7c0f5b284d     3        2000000 lovelace + 100000000 7d4b0a8f5e31f4939505914f3d175205a9c08073ce9465e4eecacf98.614bef312465629ed825bc23de97932a + TxOutDatumNone

```
