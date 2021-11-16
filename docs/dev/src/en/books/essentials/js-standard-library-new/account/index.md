---
menuItem: mi-docs
---

# Account Object

# Methods

## attach

## deploy

This method creates a Reach contract object, starts deployment of the contract based on backend code, and returns the contract without waiting for deployment to complete. To wait for deployment to complete, see getInfo.

### Signature

```
deploy(backend) => contract
backend is a reference (e.g. build/index.main.mjs).
contract is a Reach contract object.
```

### Example Program

```
import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
(async () => {
  const stdlib = await loadStdlib();
  const balance = stdlib.parseCurrency(10);
  const account = await stdlib.newTestAccount(balance);
  const contract = account.deploy(backend);
  console.log(contract)
})();
```

## getAddress

## setDebugLabel

## setGasLimit