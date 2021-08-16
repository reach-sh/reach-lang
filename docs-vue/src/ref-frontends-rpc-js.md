



# {#ref-frontends-rpc-js} JavaScript (RPC)

::: note
This frontend library relies on the [Reach RPC Server](##ref-backends-rpc).
:::

A [JavaScript](https://www.javascript.com) client library for the
[Reach RPC protocol](##ref-backends-rpc) may be installed by running:
```
$ npm install --save @reach-sh/rpc-client
```


Once installed, add the following import line to your JavaScript file which will
connect to the XXX (seclink "ref-backends-rpc"):
```js
import { mkRPC } from '@reach-sh/rpc-client';
```


The library provides the following bindings:

---
<Ref :name="(quote js):mkRPC" />
```js
const { rpc, rpcCallbacks } = await mkRPC(opts);
```


`mkRPC` accepts the XXX (secref "ref-backends-rpc-opts") as an object and returns a Promise of an object with two fields, `rpc` and `rpcCallbacks`.

<Ref :name="(quote js):rpc" />
`rpc` is a function that invokes a synchronous value RPC method.
It takes a string, naming the RPC method, and some JSON values to provide as arguments.
It returns a Promise of a single JSON value as the result.

For example,

```js
await rpc(`/stdlib/formatCurrency`, i, 4);
```


calls `formatCurrency` with some value `i` and `4`.

<Ref :name="(quote js):rpcCallbacks" />
`rpcCallbacks` is a function that invokes an interactive RPC method, such as for a backend.
It takes a string, naming the RPC method, a JSON value as an argument, and dictionary from strings to JSON values or `async` functions.
The functions will be provided as interactive RPC callbacks to the RPC method and should expect JSON values as arguments and return a Promise of a JSON value as a result.
It returns a Promise that does not contain a value.

For example,

```js
const showX = async (xo) => {
  const x = await rpc(`/stdlib/bigNumberToNumber`, xo);
  console.log(`Alice saw that X is ${x}`);
};
const ms = {
  'price': 10,
  'showX': showX,
};
await rpcCallbacks(`/backend/Alice`, ctc, ms)
```


calls a backend named `Alice` with the contract `ctc` and a value named `price` and a method named `showX` that prints out a result from the Reach backend.

