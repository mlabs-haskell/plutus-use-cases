 
# Running PAB
## Migrate
If this is the first run, database need to be created:
```
cabal exec pab-demo -- --config /path/to/config.yaml migrate
```
## Start webserver
After db created (or if db exists) run webserver
```
cabal exec pab-demo -- --config /path/to/config.yaml --passphrase $PASS webserver
```
`$PASS` - passphrase of wallet in `cardano-wallet` which will do transactions signing and submitting

Note:
When PAB starts it will print `Starting PAB backend server on port 9080`, but it will need some time to sync. If you want to see live how your transactions being processed, wait till it sync. Sync messages looks like this:
```
Current block: 327295. Current slot: 41749136.
Current block: 327295. Current slot: 41749150.
```
Wait till it reach current slot (on my machine for public testnet it takes 15-20 minutes for first run).