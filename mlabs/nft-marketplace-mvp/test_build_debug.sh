#!/usr/bin/env bash

set -euo pipefail

cardano-cli transaction build --alonzo-era \
  --testnet-magic 1097911063 \
  --protocol-params-file "./pparams.json" \
  --tx-in "1713a40fe4d756098a7f5410fa778b8b90b20a404cef891873379182fff6ecb3#0" \
  --tx-in "1713a40fe4d756098a7f5410fa778b8b90b20a404cef891873379182fff6ecb3#1" \
  --tx-in-collateral "1713a40fe4d756098a7f5410fa778b8b90b20a404cef891873379182fff6ecb3#0" \
  --tx-out "addr_test1wr29gcm6a0997e20ux73j5tw5rkduktmey495lus84r9fesnp7fvn+3103380 + 1 14c31c38813fde5d29cee3580e8273077b95127bcb2d6e8086e7d9a1 + 1 ea8f274c67da765590a2685c35e5d183d62e6067876bc07f91dcdcff.UniqueAppToken" \
  --tx-out-datum-embed-file "scripts_and_data/datum-86fa3210d0cb237d1d08ac0a866a20706207e5747448c75e43bb9e23d108a65e.json" \
  --tx-out "addr_test1wz6wqj5njjtlv9w6kec4546f5k38kqqw03wqeu29q3quaxc55mzx5+3103380 + 1 033895b5d52090ba31d1f56343549e8049fd2973174653730401a512 + 1 ea8f274c67da765590a2685c35e5d183d62e6067876bc07f91dcdcff.UniqueAppToken" \
  --tx-out-datum-embed-file "scripts_and_data/datum-881ab4ef335bbf2eb6a414d8daaecf6d2fa6ddf74af8e3ce57c24d5f35221da6.json" \
  --mint-script-file "scripts_and_data/policy-033895b5d52090ba31d1f56343549e8049fd2973174653730401a512.plutus" \
  --mint-redeemer-file "scripts_and_data/redeemer-67d8ed01e13f33438ea9059ac9be2e159f943cffe054283485e0300271e3e9f9.json" \
  --mint-script-file "scripts_and_data/policy-14c31c38813fde5d29cee3580e8273077b95127bcb2d6e8086e7d9a1.plutus" \
  --mint-redeemer-file "scripts_and_data/redeemer-8392f0c940435c06888f9bdb8c74a95dc69f156367d6a089cf008ae05caae01e.json" \
  --mint "1 033895b5d52090ba31d1f56343549e8049fd2973174653730401a512 + 1 14c31c38813fde5d29cee3580e8273077b95127bcb2d6e8086e7d9a1" \
  --required-signer "signing_keys/signing-key-bcd6bceeb0d22a7ca6ba1cd00669f7eb60ca8938d853666d30d56a56.skey" \
  --change-address "addr_test1vz7dd08wkrfz5l9xhgwdqpnf7l4kpj5f8rv9xendxr2k54su7ztr4" \
  --out-file "transactions/tx-541fbc250c370c3ab9e6ab33b4b654763f3c003e81e6ef1810e25ab3e10bd49a.raw"
