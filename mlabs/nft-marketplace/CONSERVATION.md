# Conservation notes

Project was postponed as it's hard to make it work with PAB at PAB's current state.

To the moment of "conservation" `Mlabs.NFT.Contract.Init.initApp` was in process of testing with local cluster setup  as real testnet PAB takes forever to sync.

What was discovered:

- despite `Mlabs.NFT.Contract.Init.initApp` passes unit tests, w/o `Constraints.mustBeSignedBy ownPKH` being added to constraints, validation throws error `Only an admin can initialise app`, which (I suppose) means, that Tx wasn't signed by the WBE at all, coz contract was run by admin wallet. See comment at `Mlabs.NFT.Contract.Init` #L114-L115
- with signature being fixed submission fails with `ValueNotConservedUTxO` error: output Value has `217 750` lovalaces more than input Value, reason is unknown at the moment. See below full values

```
consumed: Value 100002111651 (fromList [(PolicyID {policyID = ScriptHash \"3354193ea355fdfd4160e679a93ff7042762236e61bbd947e63ea9c1\"},fromList [(\"\",1)]),(PolicyID {policyID = ScriptHash \"4059a55b11cb4b09a0122ed099aeaea527b2e5da9c354988e304a09a\"},fromList [(\"Unique App Token\",2)]),(PolicyID {policyID = ScriptHash \"f986f849561c9b20fe10e62099c6d0a719c2cc0cbd676f914488183b\"},fromList [(\"\",1)])])) 

produced: Value 100002329401 (fromList [(PolicyID {policyID = ScriptHash \"3354193ea355fdfd4160e679a93ff7042762236e61bbd947e63ea9c1\"},fromList [(\"\",1)]),(PolicyID {policyID = ScriptHash \"4059a55b11cb4b09a0122ed099aeaea527b2e5da9c354988e304a09a\"},fromList [(\"Unique App Token\",2)]),(PolicyID {policyID = ScriptHash \"f986f849561c9b20fe10e62099c6d0a719c2cc0cbd676f914488183b\"},fromList [(\"\",1)])]))
```

## Notes

- `Mlabs.NFT.Contract.Init.initApp` has a lot of debug logging that probably should be removed after current issues will be solved
- To build and start local cluster: `nix build -L .#mlabs-plutus-use-cases:exe:nft-marketplace-local-cluster && ./result/bin/nft-marketplace-local-cluster`
- Quick integration test for running initialization with local cluster can be done with `local_cluster_run_init.sh`
  