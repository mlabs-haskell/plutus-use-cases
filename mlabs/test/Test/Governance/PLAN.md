Currently `Governance` validator can return `Bool` or throw error via `traceError`. Errors can be asserted in tests as well, but the way current verision Plutus processes errors makes them to "general" - the error message will be
```
Exception: This must be replaced by the core-to-plc plugin during compilation: error
        CallStack (from HasCallStack):
          error, called at src/PlutusTx/Utils.hs:4:26 in plutus-tx-0.1.0.0-39dc1a20e0a7aedb83095ff764c78ccbca2343f1613f36c8748eac40f828f12f:PlutusTx.Utils
```

So, there is no way to distinguish between user provided and internal Plutus errors.

The problem with error message seems to be fixed already in newer version (https://github.com/input-output-hk/plutus/issues/3003), but maybe making validator throw no errors, but only return `Bool` will be more clear design anyway, as making assertions about concreete error message will mean tests adjustments every time the message changes.

`Governance` validator currently mostly commented out as a result of getting minimal validator test example to work.