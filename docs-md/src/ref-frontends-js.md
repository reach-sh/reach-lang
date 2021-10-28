



# {#ref-frontends-js} JavaScript

The Reach JavaScript standard library, ${ref((quote js), "stdlib")} `stdlib`, is provided by either
+ the module `@reach-sh/stdlib/ETH.mjs`;
+ the module `@reach-sh/stdlib/ALGO.mjs`;
+ the `async` function `loadStdlib` from `@reach-sh/stdlib/loader.mjs`.


These modules are available in the [(tt @reach-sh/stdlib)](https://www.npmjs.com/package/@reach-sh/stdlib) [(tt npm)](https://www.npmjs.com/) package, which you can install via:
```
$ npm install @reach-sh/stdlib
```

Although, if you use `reach run`, you don't need to install this package, because `reach` automatically manages your standard library install behind the scenes.
You only need to install the package directly if you are running your frontend without `reach` or using a tool like [webpack](https://webpack.js.org/) for deployment.

These libraries provide a standard interface that support developing frontends.

${toc}

## {#ref-frontends-js-types} Types

The table below shows the JavaScript representation of each of the Reach types:
${ref((quote js), "Contract")}
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


For example, the Reach type `MInt = Data({None: Null, Some: UInt})` inhabitant `MInt.Some(42)` is represented as `['Some', 42]` in JavaScript.

---
${ref((quote js), "Connector")}
```js
type Connector = 'ETH' | 'ALGO' | 'CFX'
```


A `Connector` is the abbreviated name of the network
being connected to.


## {#ref-frontends-js-loader.mjs} `loader.mjs`

The `loader.mjs` module exports the following functions
that might help you write code that is portable to multiple consensus networks.

---
${ref((quote js), "canonicalizeConnectorMode")}
```js
canonicalizeConnectorMode(string) => string
```


Expands a connector mode prefix
to its full, canonical name. The canonical connector modes are:

+ `ETH-live`
+ `ETH-browser`
+ `ETH-devnet`, for `'ETH'`.
+ `ALGO-live`
+ `ALGO-browser`
+ `ALGO-devnet`, for `'ALGO'`.


---
${ref((quote js), "getConnectorMode")}
```js
getConnectorMode() => string
```


Returns the canonicalized connector mode, based on the
`process.env.REACH_CONNECTOR_MODE` environment variable.
If the variable is missing or empty, it will return the canonicalized form of `'ETH'`.

---
${ref((quote js), "getConnector")}
```js
getConnector() => Connector
```


Returns the first piece of `getConnectorMode()`.

---
${ref((quote js), "loadStdlib")}
```js
loadStdlib(env) => Promise<stdlib>
```


 Returns a Promise for a standard library based on the provided `env` string or map.
In environments where the Reach standard library has implicit access to `process.env`,
you may omit the `env` argument, in which case `process.env` will be used.

If the standard library is being used with JavaScript bundlers like Webpack
---as it is with React, for example---
then Reach does **not** have implicit access to `process.env`.
In such scenarios, we recommend that you call this function like so:

```js
const reach = await loadStdlib(process.env);
```


Or construct a custom object that has all of the environment keys and fields you need.

As a special case, you may instead pass in the string `'ETH'` or the string `'ALGO'`
to select the desired connector directly.

---

By default, this method allows a user to load a standard library for a single connector.
That is, this method may not be called multiple times with varying connectors.
To bypass this restriction, use `unsafeAllowMultipleStdlibs`.

---
${ref((quote js), "unsafeAllowMultipleStdlibs")}
```js
unsafeAllowMultipleStdlibs() => null
```


 Calling this function will lift the restriction that
`loadStdlib` imposes on loading multiple standard libraries.

## {#ref-frontends-js-acc} Account Handles

These functions create and interact with account representations.

---
${ref((quote js), "getDefaultAccount")}
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
${ref((quote js), "newAccountFromSecret")}
```js
newAccountFromSecret(secret: string) => Promise<acc> 
```


Returns a Promise for a Reach account abstraction for an account on the consensus network specified by the given secret.
The details of the secret encoding are specified uniquely to the consensus network.


---
${ref((quote js), "newAccountFromMnemonic")}
```js
newAccountFromMnemonic(phrase: string) => Promise<acc> 
```


Returns a Promise for a Reach account abstraction for an account on the consensus network specified by the given mnemonic phrase.
The details of the mnemonic phrase encoding are specified uniquely to the consensus network.


---
${ref((quote js), "newTestAccount")}
```js
newTestAccount(balance) => Promise<acc> 
```


Returns a Promise for a Reach account abstraction for a new account on the consensus network with a given balance of network tokens. This can only be used in private testing scenarios, as it uses a private faucet to issue network tokens.

`bigNumberify` is transparently applied to the `balance` argument.

---
${ref((quote js), "newTestAccounts")}
```js
newTestAccounts(howMany, balance) => Promise<Array<acc>> 
```


Returns a Promise for an array of `howMany` test accounts, using `newTestAccount`.

---
${ref((quote js), "createAccount")}
```js
createAccount() => Promise<acc> 
```


Returns a Promise for a Reach account abstraction for a new account on the consensus network. The account will have an empty balance of network tokens.

---
${ref((quote js), "fundFromFaucet")}
```js
fundFromFaucet(account, balance) => Promise<void>
```


Adds the given balance of network tokens to a Reach account abstraction.
This can only be used in private testing scenarios,
as it uses a private faucet to issue network tokens,
as well as certain public TestNet scenarios.
You can use `canFundFromFaucet` to check if `fundFromFaucet` can be used.

`bigNumberify` is transparently applied to the `balance` argument.

---
${ref((quote js), "canFundFromFaucet")}
```js
canFundFromFaucet() => Promise<boolean>
```


A Promise that resolves to `true` if `fundFromFaucet` can be used, `false` if not.

---
${ref((quote js), "connectAccount")}
```js
connectAccount(networkAccount) => Promise<acc> 
```


Returns a Promise for a Reach account abstraction for an existing account for the consensus network based on the connector-specific account specification provided by the `networkAccount` argument.

```js
// network => networkAccount type
ETH        => ethers.Wallet
ALGO       => {addr: string, sk: UInt8Array(64)}
```


---
${ref((quote js), "networkAccount")}
```js
acc.networkAccount => networkAccount 
```


 Returns the connector-specific account specification of a Reach account abstraction.

---
${ref((quote js), "getAddress")}
```js
acc.getAddress() => string
```


 Returns the account's address as a string. The format of this string varies across connectors.

---
${ref((quote js), "setDebugLabel")}
```js
acc.setDebugLabel(string) => acc 
```


 An account may set a distinguishing label to use in debug logs. If no label is provided, then the first four digits of the account address will be used.

---
${ref((quote js), "tokenAccept")}
```js
acc.tokenAccept(token) => Promise<void>
```


 Returns a Promise that completes when the Reach account abstraction is ready to accept non-network tokens specified by the `token`.
This does nothing on some consensus networks, but should always be used to ensure your frontend is blockchain agnostic.

---
${ref((quote js), "tokenMetadata")}
```js
acc.tokenMetadata(token) => Promise<object>
```


 Returns a Promise of the metadata for a non-network token specified by the `token`.

---
${ref((quote js), "balanceOf")}
```js
balanceOf(acc, token?) => Promise<amount> 
```


Returns a Promise for the balance of network tokens (or non-network tokens if `token` is provided) held by the account given by a Reach account abstraction provided by the `acc` argument.

---
${ref((quote js), "transfer")}
```js
transfer(from:acc, to:acc, amount, token?) => Promise<void> 
```


Performs a transfer of `amount` from `from` to `to`,
which are accounts, such as those returned by `connectAccount`.
If `token` is not provided, then the transfer is of network tokens;
otherwise, it is of the designated non-network token.
The returned `Promise` will only be resolved after the transfer completes.

`bigNumberify` is transparently applied to the `amount` argument.

### {#ref-frontends-js-acc-eth} EVM-specific (Ethereum and Conflux)

When connected to an EVM-based consensus network, the standard library provides additional functionality.

---
${ref((quote js), "setGasLimit")}
```js
acc.setGasLimit(n) => void 
```


 Modifies the gas limit for each transaction originating from the given account for the rest of the program.
`n` must be a value that `bigNumberify` will accept.

On EVM-based consensus networks, the Reach standard library will automatically estimate the required gas necessary to execute transactions, i.e. make publications.
However, sometimes this estimation process is inaccurate, especially when Reach programs interact with remote objects.
In those cases, it is sometimes useful to specify a particular gas limit.
It is common on Ethereum to use gas limits like `5000000` in testing.
If you do this, you should inform your clients that they should pay attention to the gas stipend issued.

### {#ref-frontends-js-acc-cfx} Conflux-specific

When connected to the Conflux consensus network, the standard library provides additional functionality.

---
${ref((quote js), "setStorageLimit")}
```js
acc.setStorageLimit(n) => void 
```


 Modifies the storage limit for each transaction originating from the given account for the rest of the program.
`n` must be a value that `bigNumberify` will accept.

On the Conflux consensus networks, the Reach standard library will automatically use a storage limit of 2024 to execute transactions, i.e. make publications.
Storage fees are refunded once the storage space is no longer used by the contract.
The `setStorageLimit` function allows you to choose a different storage limit, as you see fit.

## {#ref-frontends-js-ctc} Contract Handles

In order to interact with a deployed contract, you must construct a contract handle from an account.

---
${ref((quote js), "contract")}
```js
acc.contract(bin, ?info) => ctc 
```


 Returns a Reach contract handle based on the `bin` argument provided with access to the account `acc`.
This `bin` argument is the `input.mjs` module produced by the JavaScript backend.

If `info` is provided, it must be a `Contract` value, or a `Promise` that eventually yields a `Contract` value.
Typically, the deployer of a contract with not provide `info`, while users of a contract will.
In an automated, single instance program, `ctc.getInfo()` is typically used to acquire `info`;
while in non-automated programs, an application uses out-of-band communication, such as an external database or user input, to acquire the `info` argument.

The first publishing participant will attempt deploy a contract for application.
If `info` was provided, an error will be thrown.
This deployment can only happen one time, so subsequent attempts will fail with an error.

This function does not block.

---
${ref((quote js), "deploy")}
```js
acc.deploy(bin) => ctc
```



This deprecated function is an abbreviation of `acc.contract(bin)`.

${ref((quote js), "attach")}
```js
acc.attach(bin, info) => ctc 
```



This deprecated function is an abbreviation of `acc.contract(bin, info)`.

---
${ref((quote js), "getInfo")}
```js
ctc.getInfo() => Promise<ctcInfo> 
```


 Returns a Promise for a `Contract` value that may be given to `contract` to construct a Reach contract handle for this contract.
This object may be stringified with `JSON.stringify` for printing and parsed again with `JSON.parse` without any loss of information.

If `ctc` will deploy the program, then the Promise will only be resolved after the contract is actually deployed on the network,
thus you cannot block on this Promise with `await` until after the first `publish` has occurred.
Awaiting `getInfo` too early may cause your program to enter a state of deadlock.
It is safer to make an `interact` function that receives `getContract()` from the Reach program.

---
${ref((quote js), "getContractAddress")}
```js
ctc.getContractAddress() => Promise<Address> 
```


 Returns a Promise for the `Address` of the connected Reach contract.

---

Contract handles provide access to the interface of the compiled backend, `bin`, that they were constructed with.

```js
ctc.participants // {[name: string]: (interact:Object) => Promise}
ctc.p

ctc.p.Alice(interact)
```




An object where the keys are the participant names and the values are function that accept an interact object and return a Promise that completes when the participant ends.

`acc.contract(backend).p.Alice(io)` is equivalent to `backend.Alice(acc.contract(backend), io)`, but does not require duplication of the `backend` component.

```js
ctc.apis // {[name: string]: {[fun:string]: (...args) => Promise<res>}}
ctc.apis // {[name: string]: (...args) => Promise<result>}
ctc.a

ctc.a.Voter.cast("Pedro")
```




An object that mirrors the API hierarchy, so if `X.Y` is an API, then `ctc.apis.X.Y` is an ${defn("API function")}.
An API function accepts the arguments of the API and returns a `Promise` that results in the value of the API.
This function may throw an error if the API is not available.

::: note
Views are [defined in application initialization](##ref-programs-appinit-view) and then they are [set in consensus steps](##ref-programs-consensus-view). Both of these steps are in Reach. This section is about accessing them in JavaScript frontends.
:::

```js
ctc.views // {[name: string]: {[fun:string]: (...args) => Promise<res>}}
ctc.v

ctc.v.NFT.owner()
```




An object that mirrors the view hierarchy, so if `X.Y` is a view, then `ctc.views.X.Y` is a ${defn("view function")}.
A view function accepts the arguments of the view and returns a `Promise` that results in the value of the view wrapped in a `Maybe` type (because the view may not be bound.)
For example, if `NFT.owner` is a view with no arguments that represents the `Address` that owns an NFT, then `await ctc.v.NFT.owner()` is either `['Some', Owner]` or `['None', null]`.

${ref((quote js), "getViews")}
```js
ctc.getViews() => Object 
```



This deprecated function is an abbreviation of `ctc.views`.

## {#ref-frontends-js-network} Network Utilities

These functions interact with the consensus network itself.

---
${ref((quote js), "getNetworkTime")}
```js
getNetworkTime() => Promise<time>
```


Returns a Promise for the current consensus network time,
represented as a `BigNumber`.

---
${ref((quote js), "waitUntilTime")}
```js
waitUntilTime(time, onProgress?) => Promise<time>
```


Returns a Promise that will only be resolved after the specified consensus network time.
In isolated testing modes, this will also force time to pass on the network, usually by sending trivial transactions.
An ${defn("isolated testing mode")} is a `REACH_CONNECTOR_MODE` that matches
`$NET-devnet` for all valid `$NET`, or when `REACH_ISOLATED_NETWORK` is set.

You may provide an optional `onProgress` callback, used for reporting progress,
which may be called many times up until the specified network time.
It will receive an object with keys `current` and `target`,

---
${ref((quote js), "getNetworkSecs")}
```js
getNetworkSecs() => Promise<secs>
```


Like `getNetworkTime`, but returns a network seconds Promise.

---
${ref((quote js), "waitUntilSecs")}
```js
waitUntilSecs(secs, onProgress?) => Promise<secs>
```


Like `waitUntilSecs`, but waits for a certain network seconds deadline.

---
${ref((quote js), "wait")}
```js
wait(timedelta, onProgress?) => Promise<networkTime>
```


Returns a Promise that will only be resolved after the specified time delta has elapsed.
The expression `await wait(delta, onProgress)` is the same as
`await waitUntilTime(add(await getNetworkTime(), delta), onProgress)`.
As with `waitUntilTime`, the `onProgress` callback is optional.

---
${ref((quote js), "connector")}
```js
connector : Connector 
```


Represents the `Connector` the `stdlib` uses.

---

${ref((quote js), "setQueryLowerBound")}
```js
setQueryLowerBound(networkTime) => void
```


Sets the lower bound on querying the network for events.
The argument to this function is a network time.
By default, Reach will query for events from time 0.
This default is usually fine, but on certain networks like Conflux,
it can be very slow.
You can use this function to tell Reach to only query from a given network time onwards,
which can speed up event log querying significantly on Conflux.
If you use the reach stdlib to `deploy` or `attach` to a contract,
the specified lower bound must be no later than the time at which the contract was deployed.

---

${ref((quote js), "setValidQueryWindow")}

```js
setValidQueryWindow(width: number|true) => void
```


Sets the maximum width of the query windows used to query the network for event logs.
The value `true` indicates that no window size should be used, and queries may span arbitrarily large window sizes.
While each connector has a default value that works for most common cases, tweaking this setting may be useful when dealing with layer two networks or custom endpoints that are more restrictive than normal nodes on the network.

## {#ref-frontends-js-provider} Provider Selection

These functions allow you to choose which particular consensus network API provider to connect to.

---
${ref((quote js), "setProviderByName")}
```js
setProviderByName(string) => void 
```


Supported provider names are: `'MainNet'`, `'TestNet'`, and `'LocalHost'`.

On Ethereum, `'MainNet'` will connect to homestead, and `'TestNet'` to ropsten.
Multiple free API providers are used behind the scenes, [as implemented by ethers.js](https://docs.ethers.io/v5/api/providers/#providers-getDefaultProvider).

On Algorand, `'MainNet'` will connect to MainNet, and `'TestNet'` to TestNet.
The free RandLabs API provider is used ([https://algoexplorerapi.io](https://algoexplorerapi.io)).

---
${ref((quote js), "providerEnvByName")}
```js
providerEnvByName(string) => env 
```


Retrieve configuration information about providers by name.

---
${ref((quote js), "setProviderByEnv")}
```js
setProviderByEnv(env) => void 
```


Select an API provider by supplying information about it.

This function's API is considered unstable.

`env` is a record with string keys and string values.

On Ethereum, `env` may include keys:
`'ETH_NODE_URI'`

On Algorand, `env` may include keys:
`'ALGO_SERVER'`, `'ALGO_PORT'`, `'ALGO_TOKEN'`, `'ALGO_INDEXER_SERVER'`, `'ALGO_INDEXER_PORT'`, `'ALGO_INDEXER_TOKEN'`.

---
${ref((quote js), "setProvider")}
```js
setProvider(provider): void 
```


Select an API provider by providing an object satisfying its interface.

This function's API is considered unstable.

On Ethereum, `provider` is an instance of `ethers.provider`.
See: [https://docs.ethers.io/v5/api/providers/provider/](https://docs.ethers.io/v5/api/providers/provider/)

On Algorand, `provider` is an object:
```js
interface Provider {
  algodClient: algosdk.Algodv2,
  indexer: algosdk.Indexer,
  getDefaultAddress: () => Address,
  isIsolatedNetwork: boolean,
  signAndPostTxns: (txns:WalletTransaction[], opts?: any) => Promise<any>,
};
```


The `algodClient` and `indexer` values are as specified by the [Algorand JS SDK](https://algorand.github.io/js-algorand-sdk/).
The `signAndPostTxns` function obeys [ARC-0008](https://github.com/reach-sh/ARCs/blob/reach-wallet/ARCs/arc-0008.md).

---
${ref((quote js), "setWalletFallback")}
```js
setWalletFallback(make: () => wallet): void
```


When you call this function, if no browser wallet is available, then `make` will be called to construct one.
The value that `make` should return differs between connectors.

On Ethereum, it must match the interface of MetaMask.
On Conflux, it must match the interface of ConfluxPortal.
On Algorand, it must match the [ARC-0011](https://github.com/reach-sh/ARCs/blob/reach-wallet/ARCs/arc-0011.md) standard.

---
${ref((quote js), "walletFallback")}
```js
walletFallback(opts: object): () => wallet
```


This function returns a value that may be passed to `setWalletFallback` to synthesize a wallet for use in browsers that do not supply a compliant wallet.
Its customization options, `opts`, depend on the connector.

On Ethereum and Conflux, it always errors and cannot provide a wallet.

On Algorand, it can provide a wallet that directly connects to the Algorand network, like `setProviderByName` (& `setProviderByEnv`), but provide interactive signing.
The network connection is specified via the `providerEnv` key, which may be a string (which is used as an argument to `providerEnvByName`) or an environment (which is used as an argument to `setProviderByEnv`).
By default, signing is via an interactive browser window prompt, where the user repeatedly provides their mnemonic.
If the key `MyAlgoConnect` is provided, and bound to the export of `@reach-sh/stdlib/ALGO_MyAlgoConnect`, then [My Algo](https://wallet.myalgo.com/home) will be used for signing.
For example, this sets the wallet fallback to be My Algo used with Algorand TestNet:
```js
import MyAlgoConnect from '@reach-sh/stdlib/ALGO_MyAlgoConnect';
stdlib.setWalletFallback(stdlib.walletFallback({
  providerEnv: 'TestNet', MyAlgoConnect }));
```


## {#ref-frontends-js-utils} Utilities

These functions operate on JavaScript representations of Reach values.

---
${ref((quote js), "protect")}
```js
protect(t, x) => x 
```


Asserts that value `x` has Reach type `t`. An exception is thrown if this is not the case.

---
${ref((quote js), "T_Null")}${ref((quote js), "T_Bool")}${ref((quote js), "T_UInt")}${ref((quote js), "T_Bytes")}${ref((quote js), "T_Address")}${ref((quote js), "T_Array")}${ref((quote js), "T_Tuple")}${ref((quote js), "T_Object")}
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
${ref((quote js), "assert")}
```js
assert(p) 
```


Throws an exception if not given `true`.

---
${ref((quote js), "Array_set")}
```js
Array_set(arr, idx, val) 
```


Returns a new array identical to `arr`, except that index `idx` is `val`.

---
${ref((quote js), "bigNumberify")}${ref((quote js), "isBigNumber")}
```js
bigNumberify(x) => UInt
isBigNumber(x) => bool
bigNumberToNumber(x) => number
```


${defn("bigNumberify")} converts a JavaScript number to a BigNumber,
the JavaScript representation of Reach's `UInt`.

${defn("isBigNumber")} checks if its input is a BigNumber.

${defn("bigNumberToNumber")} transparently applies `bigNumberify` to its
argument and returns a JavaScript number.

---
${ref((quote js), "isHex")}${ref((quote js), "hexToBigNumber")}${ref((quote js), "stringToHex")}${ref((quote js), "bigNumberToHex")}${ref((quote js), "uintToBytes")}${ref((quote js), "bytesEq")}${ref((quote js), "digestEq")}${ref((quote js), "addressEq")}
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
${ref((quote js), "digest")}
```js
digest(ty:Type, x:ty) => Digest
```


Hashes the value.

---
${ref((quote js), "randomUInt")}
```js
randomUInt() => UInt
```


Generates random bits as a `UInt`.
The number of bits generated depends on the particular consensus network.

---
```js
hasRandom
```


${defn("hasRandom (Frontend)")} A value suitable for use as a participant interact interface requiring a `random` function, such as `hasRandom`.
Reach does not natively support randomness and leaves random number generation to the frontend implementation.
This value is provided out of convenience; it is not mandatory to use this implementation.

---
```js
hasConsoleLogger
```


${defn("hasConsoleLogger (Frontend)")} A value suitable for use as a participant interact interface requiring a `log` function, such as `hasConsoleLogger`.
The `log` function provided takes an arbitrary amount of elements and prints them to stdout.
This value is provided out of convenience; it is not mandatory to use this implementation.

---
${ref((quote js), "parseFixedPoint")}
```js
parseFixedPoint(FixedPoint) => number
```


Parses a `FixedPoint` number into a JavaScript number.

---
${ref((quote js), "numberToFixedPoint")}
```js
numberToFixedPoint(number) => FixedPoint
```


Parses a JavaScript number into a `FixedPoint`.

---
${ref((quote js), "parseInt")}
```js
parseInt(Int) => number
```


Parses a signed `Int` into a JavaScript number.

---
${ref((quote js), "numberToInt")}
```js
numberToInt(number) => Int
```


Parses a JavaScript number into an `Int`.

---
${ref((quote js), "add")}${ref((quote js), "sub")}${ref((quote js), "mod")}${ref((quote js), "mul")}${ref((quote js), "div")}
```js
add(UInt, UInt) => UInt
sub(UInt, UInt) => UInt
mod(UInt, UInt) => UInt
mul(UInt, UInt) => UInt
div(UInt, UInt) => UInt 
```


Integer arithmetic on `UInt`.

---
${ref((quote js), "eq")}${ref((quote js), "ge")}${ref((quote js), "gt")}${ref((quote js), "le")}${ref((quote js), "lt")}
```js
eq(UInt, UInt) => bool
ge(UInt, UInt) => bool
gt(UInt, UInt) => bool
le(UInt, UInt) => bool
lt(UInt, UInt) => bool
```


Integer comparisons on `UInt`.

---
The following exports are for dealing with network tokens.

${ref((quote js), "standardUnit")}${ref((quote js), "atomicUnit")}${ref((quote js), "minimumBalance")}${ref((quote js), "parseCurrency")}${ref((quote js), "formatCurrency")}
```js
standardUnit // string
atomicUnit // string
minimumBalance // atomicUnitAmount
parseCurrency(standardUnitAmount) => atomicUnitAmount
formatCurrency(atomicUnitAmount, int) => string  // display amount in standard unit
```


These functions handle amounts in a network's standard unit and its atomic unit.
A ${defn("standard unit")} is the network token unit most commonly associated with a network.
For example, the standard unit of Ethereum is ETH.
An ${defn("atomic unit")} is the smallest unit of measure for the standard unit.
For example, the atomic unit of Ethereum is WEI.
An atomic unit is ${defn("atomic")}, which means it cannot be divided into smaller units.

Some consensus networks, typically those with proof-of-stake, have minimum balances on their accounts, so this is exposed as `minimumBalance`.

Because there are 1,000,000,000,000,000,000 WEI in 1 ETH,
BigNumber is used to represet values in WEI.

Quantities of a network token should always be passed into Reach
in the token's atomic unit.

`bigNumberify` is transparently applied to `formatCurrency`'s first argument.

---
${ref((quote js), "formatAddress")}
`formatAddress(acc) => string `

Formats the address in the way the user would expect to see it.

+ On Ethereum, it is a hex-encoded string starting with `'0x'`.
+ On Algorand, it is a base32-encoded string, ending with the checksum.


There is no corresponding `parseAddress` function because
the user-friendly form is also accepted from the frontend
in all places that Reach expects an address.

## {#ref-frontends-js-ask.mjs} `ask.mjs`

The Reach JavaScript standard library also provides the helper module `@reach-sh/stdlib/ask.mjs` for constructing console interfaces to your frontends.

${ref((quote js), "ask")}
```js
import * as ask from '@reach-sh/stdlib/ask.mjs';
```


It provides the following exports:

${ref((quote js), "ask")}${ref((quote js), "yesno")}${ref((quote js), "done")}
```js
ask(string, (string => result)) => Promise<result>
yesno(string) => boolean
done() => null
```


`ask` is an asynchronous function that asks a question on the console and returns a Promise for the first result that its second argument does not error on.

`yesno` is an argument appropriate to give as the second argument to `ask` that parses "Yes"/"No" answers.

`done` indicates that no more questions will be asked.
