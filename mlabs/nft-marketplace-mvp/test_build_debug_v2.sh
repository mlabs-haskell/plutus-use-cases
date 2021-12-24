#!/usr/bin/env bash

set -euo pipefail

cardano-cli transaction build --alonzo-era \
  --tx-in "f509c91bcd5191b26505e200ddaf1b5dc36f5736b1fb2bed2026ebc2e953de32#0" \
  --tx-in "f509c91bcd5191b26505e200ddaf1b5dc36f5736b1fb2bed2026ebc2e953de32#1" \
  --tx-in-script-file "nft-marketplace-dec-demo/scripts_and_data/validator-0e57b0fdfc442e79858f36078ff9a8bb49b043a98b8c402694525cd4.plutus" \
  --tx-in-datum-file "nft-marketplace-dec-demo/scripts_and_data/datum-b64a1ced4f32532d87f7f6765fc560b7ea82e99594e0789208cecd567f26cfc3.json" \
  --tx-in-redeemer-file "nft-marketplace-dec-demo/scripts_and_data/redeemer-243e0db814eef8486b850141ebbf34becfc6319aa566cd44c0f0129a17ade73f.json" \
  --tx-in-collateral "f509c91bcd5191b26505e200ddaf1b5dc36f5736b1fb2bed2026ebc2e953de32#0" \
  --tx-out "addr_test1wq890v8al3zzu7v93umq0rle4za5nvzr4x9ccspxj3f9e4q527cwg+2930970 + 1 706d5839bc409d9367ddb1391fc11aff3d3e72c49d3898c2fc39c9a2.SomeContent" \
  --tx-out-datum-embed-file "nft-marketplace-dec-demo/scripts_and_data/datum-dc5339fccf5d52cee6f8c28324f8183a94dba9b0a4431ac34d06c33abc0674ec.json" \
  --tx-out "addr_test1wq890v8al3zzu7v93umq0rle4za5nvzr4x9ccspxj3f9e4q527cwg+3103380 + 1 3b17c803713079134c03b2de2a2dba0782f89778cc04ac2b92cf2fd8.UniqueAppToken + 1 706d5839bc409d9367ddb1391fc11aff3d3e72c49d3898c2fc39c9a2" \
  --tx-out-datum-embed-file "nft-marketplace-dec-demo/scripts_and_data/datum-8cc9686d2b63b2a72ceb61358f7b03373001ca66207b9ac38083c606ac565d36.json" \
  --mint-script-file "nft-marketplace-dec-demo/scripts_and_data/policy-706d5839bc409d9367ddb1391fc11aff3d3e72c49d3898c2fc39c9a2.plutus" \
  --mint-redeemer-file "nft-marketplace-dec-demo/scripts_and_data/redeemer-091c0a7bc744ee793dbaa24d152b872765fe4151e5b118fa58d231b3054f0db4.json" \
  --mint "1 706d5839bc409d9367ddb1391fc11aff3d3e72c49d3898c2fc39c9a2.SomeContent" \
  --required-signer "nft-marketplace-dec-demo/signing_keys/signing-key-bcd6bceeb0d22a7ca6ba1cd00669f7eb60ca8938d853666d30d56a56.skey" \
  --change-address "addr_test1vz7dd08wkrfz5l9xhgwdqpnf7l4kpj5f8rv9xendxr2k54su7ztr4" \
  --testnet-magic 1097911063 \
  "--protocol-params-file" "./nft-marketplace-dec-demo/pparams.json" \
  --out-file "nft-marketplace-dec-demo/transactions/tx-4d505ebc15737693641f5440b12c84008a2ff7d5b9a01fe288c89d95d22f6d59.raw"