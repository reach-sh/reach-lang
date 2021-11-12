---
menuItem: mi-docs
---

# Support for JS Frontends

The Reach JavaScript Standard Library, a [Node.js package](https://www.npmjs.com/package/@reach-sh/stdlib), provides a generic consensus-network interface for JavaScript frontends and backends that make up Reach DApps. It also provides a variety of utility methods. The `reach run` command installs the package automatically. You can also install it manually:

```
$ npm install @reach-sh/stdlib
```

The library consists of objects (e.g. stdlib, account, contract) composed of properties and methods dealing with accounts, arithmetic, big numbers, comparisons, consensus network providers, contracts, debugging, encryption, randomization, and time. 

# Import

Reach provides one implementation of the standard library for each supported consensus network. There are several ways to import the library into your JavaScript module:

Import an Algorand instance:

```
import * as stdlib from '@reach-sh/stdlib/ALGO.mjs';
(async () => {
  console.log(stdlib);
})();
```

Import an Ethereum instance:

```
import * as stdlib from '@reach-sh/stdlib/ETH.mjs';
(async () => {
  console.log(stdlib);
})();
```

Import with implicit reference to process.env:

```
import { loadStdlib } from '@reach-sh/stdlib';
(async () => {
  const stdlib = await loadStdlib();
  console.log(stdlib);
})();
```

Import with explicit reference to process.env:

```
import { loadStdlib } from '@reach-sh/stdlib';
(async () => {
  const stdlib = await loadStdlib(process.env);
  console.log(stdlib);
})();
```

Import additional getter functions:

```
import { getConnector, getConnectorMode, loadStdlib } from '@reach-sh/stdlib';
(async () => {
  console.log(getConnector());
  console.log(getConnectorMode());
  const stdlib = await loadStdlib();
  console.log(stdlib);
})();
```

* `getConnector` returns a string indicating the consensus network (e.g. ALGO, ETH). 
* `getConnectorMode` returns a string indicating the consensus network mode (e.g. ETH-test-dockerized-geth).

# Inspect

The following steps can help you inspect these objects:

1. Create a directory:

    ```
    % mkdir test
    % cd test
    ```

1. Create a template Reach project which consists of two files: `index.mjs` and `index.rsh`.

    ```
    % reach init
    ```

1. Replace `index.mjs` with the following:

    ```
    import { loadStdlib } from '@reach-sh/stdlib';
    import * as backend from './build/index.main.mjs';
    (async () => {
      const stdlib = await loadStdlib();
      const balance = stdlib.parseCurrency(10);
      const account = await stdlib.newTestAccount(balance);
      const contract = account.deploy(backend);
      console.log(stdlib);
      console.log(account);
      console.log(contract);
    })();
    ```

1. Run the application with one or both of the following:

    ```
    $ REACH_CONNECTOR_MODE=ALGO reach run
    $ REACH_CONNECTOR_MODE=ETH reach run
    ```

    Output displays the properties and methods of the three objects which differ slightly depending on the target consensus network.