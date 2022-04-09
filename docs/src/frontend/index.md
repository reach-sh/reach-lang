# {#ref-frontends} Frontends

This section describes the libraries provided by Reach version @{VERSION} to support developing frontends.

Frontends are implemented in [JavaScript](##ref-frontends-js)
or via the [the RPC server](##ref-backends-rpc) and one of the languages with an RPC client:
[C Sharp](##ref-frontends-rpc-cs),
[JavaScript](##ref-frontends-rpc-js),
[Go](##ref-frontends-rpc-go), and
[Python](##ref-frontends-rpc-py).

## {#ref-frontends-js} JavaScript

The Reach JavaScript standard library, @{ref("js", "stdlib")} `{!js} stdlib`, is provided the `{!js} async` function `{!js} loadStdlib` from the `@reach-sh/stdlib` package.

These modules are available in the [`@reach-sh/stdlib`](https://www.npmjs.com/package/@reach-sh/stdlib) [`npm`](https://www.npmjs.com/) package, which you can install via:

```cmd
$ npm install @reach-sh/stdlib
```

Although, if you use `reach run`, you don't need to install this package, because `reach` automatically manages your standard library install behind the scenes.
You only need to install the package directly if you are running your frontend without `reach` or using a tool like [webpack](https://webpack.js.org/) for deployment.

These libraries provide a standard interface that support developing frontends in JavaScript.

+ @{seclink("ref-frontends-js-loader")} discusses how to load the standard library.
+ @{seclink("ref-frontends-js-provider")} discusses how to adjust what provider is used to connect to a consensus network.
+ @{seclink("ref-frontends-js-network")} discuss some utility functions that provide access to consensus network parameters and resources.
+ @{seclink("ref-frontends-js-acc")} discusses how to construct an account handle, which is necessary to access contracts.
+ @{seclink("ref-frontends-js-ctc")} discusses how to construct a contract handle, which is used to interact with a DApp.
+ @{seclink("ref-frontends-js-types")} discusses the way that JavaScript values are used to represent Reach values.
+ @{seclink("ref-frontends-js-utils")} discusses some utility functions for working with JavaScript-represented Reach values.
+ @{seclink("ref-frontends-js-ask")} discusses a helper library for building text-based UIs.

### {#ref-frontends-js-types} Types

The table below shows the JavaScript representation of each of the Reach types:
@{ref("js", "Contract")}
```js
// Reach  => JavaScript
Null      => null
Bool      => 'boolean'
UInt      => 'BigNumber' or 'number'
Bytes     => 'string'
Digest    => 'BigNumber'
Address   => NetworkAccount
Contract  => Address on ETH; UInt on ALGO
Token     => Address on ETH; UInt on ALGO
Array     => array
Tuple     => array
Object    => object
Data      => ['variant', value]
Struct    => object
```

For example, the Reach type `{!rsh} MInt = Data({None: Null, Some: UInt})` inhabitant `{!rsh} MInt.Some(42)` is represented as `{!rsh} ['Some', 42]` in JavaScript.

---
@{ref("js", "Connector")}
```js
type Connector = 'ETH' | 'ALGO' | 'CFX'
```

A `{!rsh} Connector` is the abbreviated name of the network
being connected to.

### {#ref-frontends-js-loader} `loader.mjs`

The `loader.mjs` module exports the following functions
that might help you write code that is portable to multiple consensus networks.

---
@{ref("js", "canonicalizeConnectorMode")}
```js
canonicalizeConnectorMode(string) => string
```

Expands a connector mode prefix
to its full, canonical name. The canonical connector modes are:

+ `ETH-live`
+ `ETH-browser`
+ `ETH-devnet`, for `{!js} 'ETH'`.
+ `ALGO-live`
+ `ALGO-browser`
+ `ALGO-devnet`, for `{!js} 'ALGO'`.

---
@{ref("js", "getConnectorMode")}
```js
getConnectorMode() => string
```

Returns the canonicalized connector mode, based on the
`{!js} process.env.REACH_CONNECTOR_MODE` environment variable.
If the variable is missing or empty, it will return the canonicalized form of `{!js} 'ETH'`.

---
@{ref("js", "getConnector")}
```js
getConnector() => Connector
```

Returns the first piece of `{!js} getConnectorMode()`.

---
@{ref("js", "loadStdlib")}
```js
loadStdlib(env) => Promise<stdlib>
```

Returns a Promise for a standard library based on the provided `{!js} env` string or map.
In environments where the Reach standard library has implicit access to `{!js} process.env`,
you may omit the `{!js} env` argument, in which case `{!js} process.env` will be used.

If the standard library is being used with JavaScript bundlers like Webpack
---as it is with React, for example---
then Reach does **not** have implicit access to `{!js} process.env`.
In such scenarios, we recommend that you call this function like so:

```js
const reach = await loadStdlib(process.env);
```

Or construct a custom object that has all of the environment keys and fields you need.

On Algorand, this object may include the keys `ALGO_GENESIS_ID` or `ALGO_GENESIS_HASH` (or both),
which allows you to request that the user's [ARC-6 compliant wallet](https://github.com/algorandfoundation/ARCs/blob/main/ARCs/arc-0006.md) connect to a specific network.
When left unspecified, it allows the user to select one of their wallet's supported networks.
This object may also include the key `ALGO_ACCOUNT`,
which allows you to request the use of a specific account from the user's ARC-6 compliant wallet by address. This should usually be left unspecified, which allows the user to instead select their preferred account.

As a special case, you may instead pass in the string `'ETH'`, `'ALGO'`, or `'CFX'`,
to select the desired connector directly.

---

By default, this method allows a user to load a standard library for a single connector.
That is, this method may not be called multiple times with varying connectors.
To bypass this restriction, use `{!js} unsafeAllowMultipleStdlibs`.

---
@{ref("js", "unsafeAllowMultipleStdlibs")}
```js
unsafeAllowMultipleStdlibs() => null
```

Calling this function will lift the restriction that
`{!js} loadStdlib` imposes on loading multiple standard libraries.

### {#ref-frontends-js-acc} Account Handles

These functions create and interact with account representations.

---
@{ref("js", "getDefaultAccount")}
```js
getDefaultAccount() => Promise<acc>
```

Returns a Promise for a Reach account abstraction for a "default" account on the consensus network.
The meaning of "default account" varies between contexts.
When running in the browser, the default account will be connected to a wallet.
When running in Node while connected to one of Reach's standard devnets,
the default account will be connected to a faucet on the devnet.
This promise will be rejected with an exception if no sensible default account can be accessed for the current context.

---
@{ref("js", "newAccountFromSecret")}
```js
newAccountFromSecret(secret: string) => Promise<acc>
```

Returns a Promise for a Reach account abstraction for an account on the consensus network specified by the given secret.
The details of the secret encoding are specified uniquely to the consensus network.

---
@{ref("js", "newAccountFromMnemonic")}
```js
newAccountFromMnemonic(phrase: string) => Promise<acc>
```

Returns a Promise for a Reach account abstraction for an account on the consensus network specified by the given mnemonic phrase.
The details of the mnemonic phrase encoding are specified uniquely to the consensus network.

---
@{ref("js", "newTestAccount")}
```js
newTestAccount(balance) => Promise<acc>
```

Returns a Promise for a Reach account abstraction for a new account on the consensus network with a given balance of network tokens. This can only be used in private testing scenarios, as it uses a private faucet to issue network tokens.

`{!js} bigNumberify` is transparently applied to the `{!js} balance` argument.

---
@{ref("js", "newTestAccounts")}
```js
newTestAccounts(howMany, balance) => Promise<Array<acc>>
```

Returns a Promise for an array of `{!js} howMany` test accounts, using `{!js} newTestAccount`.

---
@{ref("js", "createAccount")}
```js
createAccount() => Promise<acc>
```

Returns a Promise for a Reach account abstraction for a new account on the consensus network. The account will have an empty balance of network tokens.

---
@{ref("js", "fundFromFaucet")}
```js
fundFromFaucet(account, balance) => Promise<void>
```

Adds the given balance of network tokens to a Reach account abstraction.
This can only be used in private testing scenarios,
as it uses a private faucet to issue network tokens,
as well as certain public TestNet scenarios.
You can use `{!js} canFundFromFaucet` to check if `{!js} fundFromFaucet` can be used.

`{!js} bigNumberify` is transparently applied to the `{!js} balance` argument.

---
@{ref("js", "canFundFromFaucet")}
```js
canFundFromFaucet() => Promise<boolean>
```

A Promise that resolves to `{!js} true` if `{!js} fundFromFaucet` can be used, `{!js} false` if not.

---
@{ref("js", "connectAccount")}
```js
connectAccount(networkAccount) => Promise<acc>
```

Returns a Promise for a Reach account abstraction for an existing account for the consensus network based on the connector-specific account specification provided by the `{!js} networkAccount` argument.

```js
// network => networkAccount type
ETH        => ethers.Wallet
ALGO       => {addr: string, sk: UInt8Array(64)}
```

---
@{ref("js", "networkAccount")}
```js
acc.networkAccount => networkAccount
```

Returns the connector-specific account specification of a Reach account abstraction.

---
@{ref("js", "getAddress")}
```js
acc.getAddress() => string
```

Returns the account's address as a string. The format of this string varies across connectors.

---
@{ref("js", "setDebugLabel")}
```js
acc.setDebugLabel(string) => acc
```

An account may set a distinguishing label to use in debug logs. If no label is provided, then the first four digits of the account address will be used.

---
@{ref("js", "tokenAccept")}
```js
acc.tokenAccept(token) => Promise<void>
```

Returns a Promise that completes when the Reach account abstraction is ready to accept non-network tokens specified by the `{!js} token`.
This does nothing on some consensus networks, but should always be used to ensure your frontend is blockchain agnostic.

---
@{ref("js", "tokenAccepted")}
```js
acc.tokenAccepted(token) => Promise<boolean>
```

Returns a Promise that returns if an account may accept a given token.
This does nothing on some consensus networks, but should always be used to ensure your frontend is blockchain agnostic.

---
@{ref("js", "tokenMetadata")}
```js
acc.tokenMetadata(token) => Promise<object>
```

Returns a Promise of the metadata for a non-network token specified by the `{!js} token`.

---
@{ref("js", "balanceOf")}
```js
balanceOf(acc, token?) => Promise<BigNumber>
```

Promises the balance of network tokens (or non-network tokens if `{!js} token` is provided) held by the account given by a Reach account abstraction provided by the `{!js} acc` argument.

---
@{ref("js", "balancesOf")}
```js
balancesOf(acc: Account, tokens: Array<Token | null>) => Promise<Array<BigNumber>>
```
Promises an array of balances that corresponds with the provided array of tokens, `{!js} tokens`,
for a given account `{!js} acc`.
If `{!js} tokens` contains a `{!js} null`,
the corresponding position in the output array will contain the account's balance of network tokens.
This function is more efficient for getting multiple token balances than repeated calls to `{!js} balanceOf`.

---
@{ref("js", "minimumBalanceOf")}
```js
minimumBalanceOf(acc) => Promise<BigNumber>
```

Promises the portion of `{!js} balanceOf(acc)` which may not be transferred by
the given account.
Some networks restrict the usage of an account's funds.
On networks that do not, this will always return zero.

---
@{ref("js", "transfer")}
```js
transfer(from:acc, to:acc, amount, token?) => Promise<void>
```

Performs a transfer of `{!js} amount` from `{!js} from` to `{!js} to`,
which are accounts, such as those returned by `{!js} connectAccount`.
If `{!js} token` is not provided, then the transfer is of network tokens;
otherwise, it is of the designated non-network token.
The returned `{!js} Promise` will only be resolved after the transfer completes.

`{!js} bigNumberify` is transparently applied to the `{!js} amount` argument.

#### {#ref-frontends-js-acc-eth} EVM-specific (Ethereum and Conflux)

When connected to an EVM-based consensus network, the standard library provides additional functionality.

---
@{ref("js", "setGasLimit")}
```js
acc.setGasLimit(n) => void
```

Modifies the gas limit for each transaction originating from the given account for the rest of the program.
`{!js} n` must be a value that `{!js} bigNumberify` will accept.

On EVM-based consensus networks, the Reach standard library will automatically estimate the required gas necessary to execute transactions, i.e. make publications.
However, sometimes this estimation process is inaccurate, especially when Reach programs interact with remote objects.
In those cases, it is sometimes useful to specify a particular gas limit.
It is common on Ethereum to use gas limits like `{!js} 5000000` in testing.
If you do this, you should inform your clients that they should pay attention to the gas stipend issued.

#### {#ref-frontends-js-acc-cfx} Conflux-specific

When connected to the Conflux consensus network, the standard library provides additional functionality.

---
@{ref("js", "setStorageLimit")}
```js
acc.setStorageLimit(n) => void
```

Modifies the storage limit for each transaction originating from the given account for the rest of the program.
`{!js} n` must be a value that `{!js} bigNumberify` will accept.

On the Conflux consensus networks, the Reach standard library will automatically use a storage limit of 2024 to execute transactions, i.e. make publications.
Storage fees are refunded once the storage space is no longer used by the contract.
The `{!js} setStorageLimit` function allows you to choose a different storage limit, as you see fit.

### {#ref-frontends-js-ctc} Contract Handles

In order to interact with a deployed contract, you must construct a contract handle from an account.

---
@{ref("js", "contract")}
```js
acc.contract(bin, ?info) => ctc
```

Returns a Reach contract handle based on the `{!js} bin` argument provided with access to the account `{!js} acc`.
This `{!js} bin` argument is the `index.main.mjs` module produced by the JavaScript backend.

If `{!js} info` is provided, it must be a `{!rsh} Contract` value, or a `{!js} Promise` that eventually yields a `{!rsh} Contract` value.
Typically, the deployer of a contract will not provide `{!js} info`, while users of a contract will.
In an automated, single instance program, `{!rsh} ctc.getInfo()` is typically used to acquire `{!js} info`;
while in non-automated programs, an application uses out-of-band communication, such as an external database or user input, to acquire the `{!js} info` argument.

The first publishing participant will attempt to deploy a contract for an application.
If `{!js} info` was provided, an error will be thrown.
This deployment can only happen one time, so subsequent attempts will fail with an error.

This function does not block.

---
@{ref("js", "deploy")}
```js
acc.deploy(bin) => ctc
```

This deprecated function is an abbreviation of `{!js} acc.contract(bin)`.

@{ref("js", "attach")}
```js
acc.attach(bin, info) => ctc
```

This deprecated function is an abbreviation of `{!js} acc.contract(bin, info)`.

---
@{ref("js", "getInfo")}
```js
ctc.getInfo() => Promise<ctcInfo>
```

Returns a Promise for a `{!rsh} Contract` value that may be given to `{!js} contract` to construct a Reach contract handle for this contract.
This object may be stringified with `{!js} JSON.stringify` for printing and parsed again with `{!js} JSON.parse` without any loss of information.

If `{!js} ctc` will deploy the program, then the Promise will only be resolved after the contract is actually deployed on the network,
thus you cannot block on this Promise with `{!js} await` until after the first `{!rsh} publish` has occurred.
Awaiting `{!rsh} getInfo` too early may cause your program to enter a state of deadlock.
It is safer to make an `{!rsh} interact` function that receives `{!rsh} getContract()` from the Reach program.

---
@{ref("js", "getContractAddress")}
```js
ctc.getContractAddress() => Promise<Address>
```

Returns a Promise for the `{!js} Address` of the connected Reach contract.

---
@{ref("js", "ctc.getABI")}
```js
ctc.getABI(showFull?: boolean) => any
```

Returns the ABI to the contract in a connector-specific format.
When the `{!js} showFull` argument is not `{!js} true`, internal implementation
details are omitted.

#### `ctc.participants`, `ctc.p`

Contract handles provide access to the interface of the compiled backend, `{!js} bin`, that they were constructed with.

```js
ctc.participants // {[name: string]: (interact:Object) => Promise}
ctc.p

ctc.p.Alice(interact)
```

An object where the keys are the participant names and the values are functions that accept an interact object and return a Promise that completes when the participant ends.

`{!js} acc.contract(backend).p.Alice(io)` is equivalent to `{!js} backend.Alice(acc.contract(backend), io)`, but does not require duplication of the `{!js} backend` component.

#### `ctc.apis`, `ctc.a`

```js
ctc.apis // {[name: string]: {[fun:string]: (...args) => Promise<res>}}
ctc.apis // {[name: string]: (...args) => Promise<result>}
ctc.a

ctc.a.Voter.cast("Pedro")
```

An object that mirrors the API hierarchy, so if `X.Y` is an API, then `{!js} ctc.apis.X.Y` is an @{defn("API function")}.
An API function accepts the arguments of the API and returns a `{!js} Promise` that results in the value of the API.
This function may throw an error if the API is not available.

If an API was specified without an `{!rsh} apiName`, for example `{!rsh} API({ cast: Fun([String], Null)})`, it may be accessed by its property name:

```js
ctc.a.cast("Pedro");
```

#### `ctc.safeApis`

```js
ctc.safeApis
ctc.safeApis.Voter.cast("Pedro")
```

This object is the same as `{!js} ctc.apis` except the API functions return a `{!rsh} Maybe` value.
If the call fails, then `{!js} ['None', null]` will be returned. If the call succeeds, the return value will
be wrapped with `{!js} Some`, e.g. `{!js} ['Some', 4]`.

#### `ctc.views`, `ctc.v`

:::note
Views are [defined in application initialization](##ref-programs-appinit-view) and then they are [set in consensus steps](##ref-programs-consensus-view). Both of these steps are in Reach. This section is about accessing them in JavaScript frontends.
:::

```js
ctc.views // {[name: string]: {[fun:string]: (...args) => Promise<res>}}
ctc.views // {[name: string]: (...args) => Promise<res>}
ctc.v

ctc.v.NFT.owner()
```

An object that mirrors the view hierarchy, so if `X.Y` is a view, then `{!js} ctc.views.X.Y` is a @{defn("view function")}.
A view function accepts the arguments of the view and returns a `{!js} Promise` that results in the value of the view wrapped in a `{!rsh} Maybe` type (because the view may not be bound).
For example, if `NFT.owner` is a view with no arguments that represents the `{!rsh} Address` that owns an NFT, then `{!js} await ctc.v.NFT.owner()` is either `{!js} ['Some', Owner]` or `{!js} ['None', null]`.

If a View was specified without a `{!rsh} viewName`, for example `{!rsh} View({ owner: Address })`, it may be accessed by its property name:

```js
ctc.v.owner();
```

#### `ctc.events`, `ctc.e`

```js
ctc.events // { [name: string]: EventStream }
ctc.e      // { [name: string]: EventStream }

await ctc.e.log.next();
```

An object that mirrors the event hierarchy, so if `X.Y` is an event, then `{!js} ctc.events.X.Y` is an @{defn("EventStream")}.
An EventStream supports the following operations for a given `{!rsh} Event`:

@{ref("js", "EventStream")}
```js
EventStream<T> : {
  next    : () => Promise<Event<T>>,
  seek    : (t: Time) => void,
  seekNow : () => Promise<void>,
  lastTime: () => Promise<Time>,
  monitor : ((Event<T>) => void) => Promise<void>,
}
```

where

@{ref("js", "Event")}
```js
Event<T> : { when: Time, what: T }
```

An `{!js} Event` is instantiated with it's corresponding type declared in Reach.

 `{!js} next` will wait for the next `{!rsh} Event` to occur, returning the time the event occured
and the arguments to the event.

 `{!js} seek` will set the internal time of the EventStream to the given argument.
The EventStream will use this time as the minimum bound when searching for `{!rsh} Event`s.

 `{!js} seekNow` will set the internal time of the EventStream to the latest network time.
The EventStream will use this time as the minimum bound when searching for `{!rsh} Event`s.

 `{!js} lastTime` will return the last network time that an `{!rsh} Event` was emitted.

 `{!js} monitor` accepts a function of type `{!js} Event<T> => void` as an argument. The provided function will be
called whenever the `{!rsh} Event` occurs.

---

@{ref("js", "getViews")}
```js
ctc.getViews() => Object
```

This deprecated function is an abbreviation of `{!js} ctc.views`.

#### `ctc.unsafeViews`

```js
ctc.unsafeViews
ctc.unsafeViews.NFT.owner()
```

This object is the same as `{!js} ctc.views` except the value of the view is not wrapped in a `{!rsh} Maybe` type.
If a view is set, the value will be returned as is, without being wrapped in `{!rsh} Some`.
If a view is not set, an error will be thrown.

### {#ref-frontends-js-network} Network Utilities

These functions interact with the consensus network itself.

---
@{ref("js", "getNetworkTime")}
```js
getNetworkTime() => Promise<time>
```

Returns a Promise for the current consensus network time,
represented as a `BigNumber`.

---
@{ref("js", "waitUntilTime")}
```js
waitUntilTime(time, onProgress?) => Promise<time>
```

Returns a Promise that will only be resolved after the specified consensus network time.
In isolated testing modes, this will also force time to pass on the network, usually by sending trivial transactions.
An @{defn("isolated testing mode")} is a `REACH_CONNECTOR_MODE` that matches
`$NET-devnet` for all valid `$NET`, or when `REACH_ISOLATED_NETWORK` is set.

You may provide an optional `{!js} onProgress` callback, used for reporting progress,
which may be called many times up until the specified network time.
It will receive an object with keys `{!js} current` and `{!js} target`,

---
@{ref("js", "getNetworkSecs")}
```js
getNetworkSecs() => Promise<secs>
```

Like `{!js} getNetworkTime`, but returns a network seconds Promise.

---
@{ref("js", "waitUntilSecs")}
```js
waitUntilSecs(secs, onProgress?) => Promise<secs>
```

Like `{!js} waitUntilSecs`, but waits for a certain network seconds deadline.

---
@{ref("js", "wait")}
```js
wait(timedelta, onProgress?) => Promise<networkTime>
```

Returns a Promise that will only be resolved after the specified time delta has elapsed.
The expression `{!js} await wait(delta, onProgress)` is the same as
`{!js} await waitUntilTime(add(await getNetworkTime(), delta), onProgress)`.
As with `{!js} waitUntilTime`, the `{!js} onProgress` callback is optional.

---
@{ref("js", "connector")}
```js
connector : Connector
```

Represents the `{!js} Connector` the `{!js} stdlib` uses.

---

@{ref("js", "setValidQueryWindow")}

```js
setValidQueryWindow(width: number|true) => void
```

Sets the maximum width of the query windows used to query the network for event logs.
The value `{!js} true` indicates that no window size should be used, and queries may span arbitrarily large window sizes.
While each connector has a default value that works for most common cases, tweaking this setting may be useful when dealing with layer two networks or custom endpoints that are more restrictive than normal nodes on the network.

### {#ref-frontends-js-provider} Provider Selection and Utilities

These functions allow you to choose which particular consensus network API provider to connect to.

---
@{ref("js", "setProviderByName")}
```js
setProviderByName(string) => void
```

Supported provider names are: `{!js} 'MainNet'`, `{!js} 'TestNet'`, and `{!js} 'LocalHost'`.

On Ethereum, `{!js} 'MainNet'` will connect to homestead, and `{!js} 'TestNet'` to ropsten.
Multiple free API providers are used behind the scenes, [as implemented by ethers.js](https://docs.ethers.io/v5/api/providers/#providers-getDefaultProvider).

On Algorand, `{!js} 'MainNet'` will connect to MainNet, and `{!js} 'TestNet'` to TestNet.
The free RandLabs API provider is used ([https://algoexplorerapi.io](https://algoexplorerapi.io)).

---
@{ref("js", "providerEnvByName")}
```js
providerEnvByName(string) => env
```

Retrieve configuration information about providers by name.

---
@{ref("js", "setProviderByEnv")}
```js
setProviderByEnv(env) => void
```

Select an API provider by supplying information about it.

This function's API is considered unstable.

`{!js} env` is a record with string keys and string values.

On Ethereum, `{!js} env` may include keys:
`{!js} 'ETH_NODE_URI'`

On Algorand, `{!js} env` may include keys:
`{!cmd} 'ALGO_SERVER'`, `{!cmd} 'ALGO_PORT'`, `{!cmd} 'ALGO_TOKEN'`, `{!cmd} 'ALGO_INDEXER_SERVER'`, `{!cmd} 'ALGO_INDEXER_PORT'`, `{!cmd} 'ALGO_INDEXER_TOKEN'`, `{!cmd} ALGO_NODE_WRITE_ONLY`.

---
@{ref("js", "setProvider")}
```js
setProvider(provider): void
```

Select an API provider by providing an object satisfying its interface.

This function's API is considered unstable.

On Ethereum, `{!js} provider` is an instance of `{!js} ethers.provider`.
See: [https://docs.ethers.io/v5/api/providers/provider/](https://docs.ethers.io/v5/api/providers/provider/)

On Algorand, `{!js} provider` is an object:
```js
interface Provider {
  algodClient: algosdk.Algodv2,
  indexer: algosdk.Indexer,
  getDefaultAddress: () => Promise<Address>,
  isIsolatedNetwork: boolean,
  signAndPostTxns: (txns:WalletTransaction[], opts?: any) => Promise<any>,
};
```

The `{!js} algodClient` and `{!js} indexer` values are as specified by the [Algorand JS SDK](https://algorand.github.io/js-algorand-sdk/).
The `{!js} signAndPostTxns` function obeys [ARC-0008](https://github.com/reach-sh/ARCs/blob/reach-wallet/ARCs/arc-0008.md).

---
@{ref("js", "getProvider")}
```js
getProvider(): Promise<provider>
```

Promises a provider, matching the interface specified above.

---
@{ref("js", "setWalletFallback")}
```js
setWalletFallback(make: () => wallet): void
```

When you call this function, if no browser wallet is available, then `{!js} make` will be called to construct one.
The value that `{!js} make` should return differs between connectors.

On Ethereum, it must match the interface of MetaMask.
On Conflux, it must match the interface of ConfluxPortal.
On Algorand, it must match the [ARC-0011](https://github.com/algorandfoundation/ARCs/blob/main/ARCs/arc-0011.md) standard.

---
@{ref("js", "walletFallback")}
```js
walletFallback(opts: object): () => wallet
```

This function returns a value that may be passed to `{!js} setWalletFallback` to synthesize a wallet for use in browsers that do not supply a compliant wallet.
Its customization options, `{!js} opts`, depend on the connector.

On Ethereum and Conflux, it always errors and cannot provide a wallet.

On Algorand, it can provide a wallet that directly connects to the Algorand network, like `{!js} setProviderByName` (& `{!js} setProviderByEnv`), but provide interactive signing.
The network connection is specified via the `providerEnv` key, which may be a string (which is used as an argument to `{!js} providerEnvByName`) or an environment (which is used as an argument to `{!js} setProviderByEnv`).
By default, signing is via an interactive browser window prompt, where the user repeatedly provides their mnemonic.

If the key `MyAlgoConnect` is provided, and bound to the `ALGO_MyAlgoConnect` export of `@reach-sh/stdlib`, then [MyAlgo](https://wallet.myalgo.com/home) will be used for signing.
For example, this sets the wallet fallback to be MyAlgo used with Algorand TestNet:
```js
import { ALGO_MyAlgoConnect as MyAlgoConnect } from '@reach-sh/stdlib';
stdlib.setWalletFallback(stdlib.walletFallback({
  providerEnv: 'TestNet', MyAlgoConnect }));
```

If the key `WalletConnect` is provided, and bound to the `ALGO_WalletConnect` export of `@reach-sh/stdlib`, then [WalletConnect](https://walletconnect.com/) is used to connect to the [Algorand Wallet](https://algorandwallet.com/) for signing.
For example, this sets the wallet fallback to be WalletConnect and the Algorand TestNet:
```js
import { ALGO_WalletConnect as WalletConnect } from '@reach-sh/stdlib';
stdlib.setWalletFallback(stdlib.walletFallback({
  providerEnv: 'TestNet', WalletConnect }));
```

Because these are fallbacks, you need to decide for your users which wallet they'll use, or make a user interface element to let them select which wallet fallback to use.

---
@{ref("js", "setMinMillisBetweenRequests")}
```js
setMinMillisBetweenRequests(ms: number): void
```

Setting this to a positive number forces outgoing requests to occur one at a time,
and limits them to occur no more frequently than one request every `ms` milliseconds.
This is only supported with certain connectors,
and applies to all Providers created by the Reach standard library.

---
@{ref("js", "setCustomHttpEventHandler")}
```js
setCustomHttpEventHandler(h: (e: any) => Promise<void>): void
```

Allows for the installation of a custom hook to observe outgoing HTTP requests.
The handler `h` will be called before and after every request.
The handler will be called with one argument: an object `e`.
Inspection of `e`'s fields should be considered an unstable API that may change over time.
Currently, it has a field `eventName` which may be `'before'`, `'success'`, or `'error'`,
as well as various other fields describing the HTTP event.
This is only supported with certain connectors,
and applies to all Providers created by the Reach standard library.

---
@{ref("js", "setSigningMonitor")}
```js
setSigningMonitor(h: (evt: any, pre:Promise<any>, post:Promise<any>) => void): void
```

Allows for the installation of a custom hook to observe signing requests.
The handler `h` will be called on every request.
The `evt` argument is an unstable object that describes the request.
The `pre` argument is a Promise of an unstable object with details about the request, available after the request has been made.
The `post` argument is a Promise of an unstable object with details about the completed request, available after the request has been completed.

### {#ref-frontends-js-utils} Utilities

These functions operate on JavaScript representations of Reach values.

---
@{ref("js", "protect")}
```js
protect(t, x) => x
```

Asserts that value `{!js} x` has Reach type `{!js} t`. An exception is thrown if this is not the case.

---
@{ref("js", "T_Null")}@{ref("js", "T_Bool")}@{ref("js", "T_UInt")}@{ref("js", "T_Bytes")}@{ref("js", "T_Address")}@{ref("js", "T_Array")}@{ref("js", "T_Tuple")}@{ref("js", "T_Object")}
```js
T_Null => ReachType
T_Bool => ReachType
T_UInt => ReachType
T_Bytes(number) => ReachType
T_Digest => ReachType
T_Address => ReachType
T_Array(ReachType, number) => ReachType
T_Tuple([ReachType ...]) => ReachType
T_Object({Key: ReachType ...}) => ReachType
T_Data({Variant: ReachType ...}) => ReachType
```

Each of these represent the corresponding Reach type.

---
@{ref("js", "assert")}
```js
assert(p)
```

Throws an exception if not given `{!js} true`.

---
@{ref("js", "Array_set")}
```js
Array_set(arr, idx, val)
```

Returns a new array identical to `{!js} arr`, except that index `{!js} idx` is `{!js} val`.

---
@{ref("js", "bigNumberify")}@{ref("js", "isBigNumber")}
```js
bigNumberify(x) => UInt
isBigNumber(x) => bool
bigNumberToNumber(x) => number
bigNumberToBigInt(x) => bigint
```

@{defn("bigNumberify")} converts a JavaScript number to a BigNumber,
the JavaScript representation of Reach's `{!rsh} UInt`.

@{defn("isBigNumber")} checks if its input is a BigNumber.

@{defn("bigNumberToNumber")} transparently applies `{!js} bigNumberify` to its
argument and returns a JavaScript number.

@{defn("bigNumberToBigInt")} transparently applies `{!js} bigNumberify` to its
argument and returns a JavaScript `bigint`.

---
@{ref("js", "isHex")}@{ref("js", "hexToBigNumber")}@{ref("js", "stringToHex")}@{ref("js", "bigNumberToHex")}@{ref("js", "uintToBytes")}@{ref("js", "bytesEq")}@{ref("js", "digestEq")}@{ref("js", "addressEq")}
```js
isHex(x) => bool
hexToBigNumber(bytes) => UInt
stringToHex(string) => bytes
bigNumberToHex(UInt) => bytes
uintToBytes(UInt) => bytes
bytesEq(bytes, bytes) => bool
digestEq(Digest, Digest) => bool
addressEq(Address, Address) => bool
```

These are additional conversion and comparison utilities.

---
@{ref("js", "digest")}
```js
digest(ty:Type, x:ty) => Digest
```

Hashes the value.

---
@{ref("js", "randomUInt")}
```js
randomUInt() => UInt
```

Generates random bits as a `{!rsh} UInt`.
The number of bits generated depends on the particular consensus network.

---
```js
hasRandom
```

@{defn("hasRandom (Frontend)")} A value suitable for use as a participant interact interface requiring a `random` function, such as `{!rsh} hasRandom`.
Reach does not natively support randomness and leaves random number generation to the frontend implementation.
This value is provided out of convenience; it is not mandatory to use this implementation.

---
```js
hasConsoleLogger
```

@{defn("hasConsoleLogger (Frontend)")} A value suitable for use as a participant interact interface requiring a `log` function, such as `{!rsh} hasConsoleLogger`.
The `{!js} log` function provided takes an arbitrary amount of elements and prints them to stdout.
This value is provided out of convenience; it is not mandatory to use this implementation.

---
@{ref("js", "parseFixedPoint")}
```js
parseFixedPoint(FixedPoint) => number
```

Parses a `{!rsh} FixedPoint` number into a JavaScript number.

---
@{ref("js", "numberToFixedPoint")}
```js
numberToFixedPoint(number) => FixedPoint
```

Parses a JavaScript number into a `{!rsh} FixedPoint`.

---
@{ref("js", "parseInt")}
```js
parseInt(Int) => number
```

Parses a signed `{!rsh} Int` into a JavaScript number.

---
@{ref("js", "numberToInt")}
```js
numberToInt(number) => Int
```

Parses a JavaScript number into an `{!rsh} Int`.

---
@{ref("js", "add")}@{ref("js", "sub")}@{ref("js", "mod")}@{ref("js", "mul")}@{ref("js", "div")}
```js
add(UInt, UInt) => UInt
sub(UInt, UInt) => UInt
mod(UInt, UInt) => UInt
mul(UInt, UInt) => UInt
div(UInt, UInt) => UInt
```

Integer arithmetic on `{!rsh} UInt`.

---
@{ref("js", "eq")}@{ref("js", "ge")}@{ref("js", "gt")}@{ref("js", "le")}@{ref("js", "lt")}
```js
eq(UInt, UInt) => bool
ge(UInt, UInt) => bool
gt(UInt, UInt) => bool
le(UInt, UInt) => bool
lt(UInt, UInt) => bool
```

Integer comparisons on `{!rsh} UInt`.

---
@{ref("js", "btoiLast8")}
```js
btoiLast8(Bytes) => UInt
```

Converts the last 8 bytes of a string to an `{!rsh} UInt`.
If the string is less than 8 bytes long, this function will convert the entire string to an `{!rsh} UInt`.

---
The following exports are for dealing with network tokens.

@{ref("js", "standardUnit")}@{ref("js", "atomicUnit")}@{ref("js", "minimumBalance")}@{ref("js", "parseCurrency")}@{ref("js", "formatCurrency")}@{ref("js", "formatWithDecimals")}
```js
standardUnit // string
atomicUnit // string
minimumBalance // atomicUnitAmount
parseCurrency(standardUnitAmount: int) => atomicUnitAmount
formatCurrency(atomicUnitAmount: int) => string  // display amount in standard unit
formatWithDecimals(atomicUnitAmount: int, tokenDecimals: int) => string  // display amount of atomic unit with custom decimal place
```

These functions handle amounts in a network's standard unit and its atomic unit.
A @{defn("standard unit")} is the network token unit most commonly associated with a network.
For example, the standard unit of Ethereum is ETH.
An @{defn("atomic unit")} is the smallest unit of measure for the standard unit.
For example, the atomic unit of Ethereum is WEI.
An atomic unit is @{defn("atomic")}, which means it cannot be divided into smaller units.

Some consensus networks, typically those with proof-of-stake, have minimum balances on their accounts, so this is exposed as `{!js} minimumBalance`.

Because there are 1,000,000,000,000,000,000 WEI in 1 ETH,
BigNumber is used to represet values in WEI.

Quantities of a network token should always be passed into Reach
in the token's atomic unit.

`{!js} bigNumberify` is transparently applied to `{!js} formatCurrency`'s and `{!js} formatWithDecimals`'s first arguments.

---
@{ref("js", "formatAddress")}
`{!js} formatAddress(acc) => string `

Formats the address in the way the user would expect to see it.

+ On Ethereum, it is a hex-encoded string starting with `{!js} '0x'`.
+ On Algorand, it is a base32-encoded string, ending with the checksum.

There is no corresponding `{!js} parseAddress` function because
the user-friendly form is also accepted from the frontend
in all places that Reach expects an address.

### {#ref-frontends-js-ask} `ask`

The Reach JavaScript standard library provides the `ask` object for constructing console interfaces to your frontends.

@{ref("js", "ask")}
```js
import {ask} from '@reach-sh/stdlib';
```

It provides the following exports:

```js
ask.ask(string, (string => result)) => Promise<result>
ask.yesno(string) => boolean
ask.done() => null
```

@{ref("js", "ask.ask")}
`{!js} ask.ask` is an asynchronous function that asks a question on the console and returns a Promise for the first result that its second argument does not error on.

@{ref("js", "ask.yesno")}
`{!js} ask.yesno` is an argument appropriate to give as the second argument to `{!js} ask.ask` that parses "Yes"/"No" answers.

@{ref("js", "ask.done")}
`{!js} ask.done` indicates that no more questions will be asked.

```js
(async () => {
  const isAlice = await ask.ask(
      `Are you Alice?`,
      ask.yesno
    );

    // Do something

  ask.done();
})();
```

Read the [Interaction and Independence](##tut-8) section the Rock, Paper, Scissors tutorial for a longer use case example of the `{!js} ask` object.

---
@{ref("js", "launchToken")}
```js
launchToken(accCreator: Account, name: string, sym: string, opts?: LaunchTokenOpts) => Promise<object>
```

Launches a non-network token with the given `{!js} name` and unit symbol `{!js} sym`.
Launched on the network by `{!js} accCreator`.

Possible options to provide in `{!js} opts` include:
+ `{!js} decimals` The number of digits to use after the decimal point when displaying the non-network token.
The default is the same as the network token.
+ `{!js} supply` The total number of atomic token units to create.
The default is the maximum possible on the network.
+ `{!js} url` A URL where more information about the non-network token can be retrieved.
The default is no url.
+ `{!js} metadataHash` A hash of some metadata that is relevant to your non-network token.
The default is no metadata hash.

Algorand-only options:
+ `{!js} clawback` Address that can claw back holdings of the token.
The default is no clawback address.
+ `{!js} note` A `{!js} Uint8Array` for the `Note` field of the asset creation transaction.

For more information on Algorand-only options, see
https://developer.algorand.org/docs/get-details/transactions/transactions/#asset-parameters.
