#!/usr/bin/env bash

set -euo pipefail

cardano-cli transaction calculate-min-required-utxo --alonzo-era \
  --tx-out "addr_test1wr29gcm6a0997e20ux73j5tw5rkduktmey495lus84r9fesnp7fvn+2000000 + 1 14c31c38813fde5d29cee3580e8273077b95127bcb2d6e8086e7d9a1 + 1 ea8f274c67da765590a2685c35e5d183d62e6067876bc07f91dcdcff.UniqueAppToken" \
  --tx-out "addr_test1wz6wqj5njjtlv9w6kec4546f5k38kqqw03wqeu29q3quaxc55mzx5+2000000 + 1 033895b5d52090ba31d1f56343549e8049fd2973174653730401a512 + 1 ea8f274c67da765590a2685c35e5d183d62e6067876bc07f91dcdcff.UniqueAppToken" \
  --protocol-params-file "pparams.json"