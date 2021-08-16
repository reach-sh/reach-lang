



# {#ref-frontends-js} JavaScript

The Reach JavaScript standard library, <Ref :name="(quote js):stdlib" /> `stdlib`, is provided by either
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

[[toc]]

## {#ref-frontends-js-types} Types

The table below shows the JavaScript representation of each of the Reach types:
```js
// Reach  => JavaScript
Null      => null
Bool      => 'boolean'
UInt      => 'BigNumber' or 'number'
Bytes     => 'string'
Digest    => 'BigNumber'
Address   => NetworkAccount
Token     => Address on ETH; UInt on ALGO
Array     => array
Tuple     => array
Object    => object
Data      => ['variant', value]
Struct    => object
```


For example, the Reach type `MInt = Data({None: Null, Some: UInt})` inhabitant `MInt.Some(42)` is represented as `['Some', 42]` in JavaScript.

---
<Ref :name="(quote js):Connector" />
```js
type Connector = 'ETH' | 'ALGO'
```


A `Connector` is the abbreviated name of the network
being connected to.


## {#ref-frontends-js-loader.mjs} `loader.mjs`

The `loader.mjs` module exports the following functions
that might help you write code that is portable to multiple consensus networks.

---
<Ref :name="(quote js):canonicalizeConnectorMode" />
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
<Ref :name="(quote js):getConnectorMode" />
```js
getConnectorMode() => string
```


Returns the canonicalized connector mode, based on the
`process.env.REACH_CONNECTOR_MODE` environment variable.
If the variable is missing or empty, it will return the canonicalized form of `'ETH'`.

---
<Ref :name="(quote js):getConnector" />
```js
getConnector() => Connector
```


Returns the first piece of `getConnectorMode()`.

---
<Ref :name="(quote js):loadStdlib" />
```js
loadStdlib(env) => Promise<stdlib>
```


 Returns a Promise for a stlib based on the provided `env` string or map.
In environments where the reach stdlib has implicit access to `process.env`,
you may omit the `env` argument, in which case `process.env` will be used.

If the reach stdlib is being used with JavaScript bundlers like webpack
-- as it is with React, for example --
then the reach stdlib does not have implicit access to `process.env`.
In such scenarios, we recommend that you call this function like so:

```js
const reach = await loadStdlib(process.env);
```


You may instead pass in the string `'ETH'` or the string `'ALGO'`
to select the desired stdlib directly.

By default, this method allows a user to load a standard library for a single connector.
That is, this method may not be called multiple times with varying connectors.
To bypass this restriction, use `unsafeAllowMultipleStdlibs`.

---
<Ref :name="(quote js):unsafeAllowMultipleStdlibs" />
```js
unsafeAllowMultipleStdlibs() => null
```


 Calling this function will lift the restriction that
`loadStdlib` imposes on loading multiple standard libraries.

## {#ref-frontends-js-acc} Accounts

These functions create and interact with account representations.

---
<Ref :name="(quote js):getDefaultAccount" />
```js
getDefaultAccount() => Promise<acc> 
```


Returns a Promise for a Reach account abstraction for a "default" account on the consensus network.
The meaning of "default account" varies between contexts.
When running in the browser,
the default account will be connected to a wallet such as MetaMask or AlgoSigner.
When running in node.js while connected to one of reach's standard devnets,
the default account will be connected to a faucet on the devnet.
This promise will be rejected with an exception
if no sensible default account can be accessed for the current context.

---
<Ref :name="(quote js):newAccountFromSecret" />
```js
newAccountFromSecret(secret: string) => Promise<acc> 
```


Returns a Promise for a Reach account abstraction for an account on the consensus network specified by the given secret.
The details of the secret encoding are specified uniquely to the consensus network.


---
<Ref :name="(quote js):newAccountFromMnemonic" />
```js
newAccountFromMnemonic(phrase: string) => Promise<acc> 
```


Returns a Promise for a Reach account abstraction for an account on the consensus network specified by the given mnemonic phrase.
The details of the mnemonic phrase encoding are specified uniquely to the consensus network.


---
<Ref :name="(quote js):newTestAccount" />
```js
newTestAccount(balance) => Promise<acc> 
```


Returns a Promise for a Reach account abstraction for a new account on the consensus network with a given balance of network tokens. This can only be used in private testing scenarios, as it uses a private faucet to issue network tokens.

`bigNumberify` is transparently applied to the `balance` argument.

---
<Ref :name="(quote js):newTestAccounts" />
```js
newTestAccounts(howMany, balance) => Promise<Array<acc>> 
```


Returns a Promise for an array of `howMany` test accounts, using `newTestAccount`.

---
<Ref :name="(quote js):createAccount" />
```js
createAccount() => Promise<acc> 
```


Returns a Promise for a Reach account abstraction for a new account on the consensus network. The account will have an empty balance of network tokens.

---
<Ref :name="(quote js):fundFromFaucet" />
```js
fundFromFaucet(account, balance) => Promise<void>
```


Adds the given balance of network tokens to a Reach account abstraction. This can only be used in private testing scenarios, as it uses a private faucet to issue network tokens.

`bigNumberify` is transparently applied to the `balance` argument.

---
<Ref :name="(quote js):connectAccount" />
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
<Ref :name="(quote js):networkAccount" />
```js
acc.networkAccount => networkAccount 
```


 Returns the connector-specific account specification of a Reach account abstraction.

---
<Ref :name="(quote js):getAddress" />
```js
acc.getAddress() => string
```


 Returns the account's address as a string. The format of this string varies across connectors.

---
<Ref :name="(quote js):setDebugLabel" />
```js
acc.setDebugLabel(string) => acc 
```


 An account may set a distinguishing label to use in debug logs. If no label is provided, then the first four digits of the account address will be used.

---
<Ref :name="(quote js):tokenAccept" />
```js
acc.tokenAccept(token) => Promise<void>
```


 Returns a Promise that completes when the Reach account abstraction is ready to accept non-network tokens specified by the `token`.
This does nothing on some consensus networks, but should always be used to ensure your frontend is blockchain agnostic.

---
<Ref :name="(quote js):tokenMetadata" />
```js
acc.tokenMetadata(token) => Promise<object>
```


 Returns a Promise of the metadata for a non-network tokens specified by the `token`.

---
<Ref :name="(quote js):balanceOf" />
```js
balanceOf(acc, token?) => Promise<amount> 
```


Returns a Promise for the balance of network tokens (or non-network tokens if `token` is provided) held by the account given by a Reach account abstraction provided by the `acc` argument.

---
<Ref :name="(quote js):transfer" />
```js
transfer(from:acc, to:acc, amount, token?) => Promise<void> 
```


Performs a transfer of `amount` from `from` to `to`,
which are accounts, such as those returned by `connectAccount`.
If `token` is not provided, then the transfer is of network tokens;
otherwise, it is of the designated non-network token.
The returned `Promise` will only be resolved after the transfer completes.

`bigNumberify` is transparently applied to the `amount` argument.

### {#ref-frontends-js-acc-eth} Ethereum-specific

When connected to an EVM-based consensus network, the standard library provides additional functionality.

---
<Ref :name="(quote js):setGasLimit" />
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

## {#ref-frontends-js-ctc} Contracts

These functions create and interact with contract representations.

---
<Ref :name="(quote js):deploy" />
```js
acc.deploy(bin) => ctc 
```


 Returns a Reach contract abstraction after starting the deployment of a Reach DApp contract based on the `bin` argument provided.
This `bin` argument is the `input.mjs` module produced by the JavaScript backend.
This function does not block on the completion of deployment.
To wait for deployment, see `ctc.getInfo`.

---
<Ref :name="(quote js):getInfo" />
```js
ctc.getInfo() => Promise<ctcInfo> 
```


 Returns a Promise for an object that may be given to `attach` to construct a Reach contract abstraction representing this contract.
This object may be stringified with `JSON.stringify` for printing and parsed again with `JSON.parse` without any loss of information.
The Promise will only be resolved after the contract is actually deployed on the network.
If you are using `{deployMode: 'firstMsg'}`,
avoid blocking on this Promise with `await` until after the first `publish` has occurred.
Awaiting `getInfo` too early may cause your program to enter a state of deadlock.

---

<Ref :name="(quote js):attach" />
```js
acc.attach(bin, ctcInfoP) => ctc 
```


 Returns a Reach contract abstraction based on a deployed Reach DApp contract provided in the `ctcInfo` argument (or a Promise for ctcInfo) and the `bin` argument.
This `bin` argument is the `input.mjs` module produced by the JavaScript backend.

---

### {#ref-frontends-js-view} View Access
XXX (note-view-xref)

<Ref :name="(quote js):getViews" />
```js
ctc.getViews() => Object 
```


 Returns an object representing the views of the contract.
This object mirrors the view hierarchy, so if `X.Y` is a view, then `ctc.getViews().X.Y` is a <Defn :name="view function">view function</Defn>.
A view function accepts the arguments of the view and returns a `Promise` that results in the value of the view wrapped in a `Maybe` type (because the view may not be bound.)
For example, if `NFT.owner` is a view with no arguments that represents the `Address` that owns an NFT, then `await ctc.getViews().NFT.owner()` is either `['Some', Owner]` or `['None', null]`.

## {#ref-frontends-js-network} Network Utilities

These functions interact with the consensus network itself.

---
<Ref :name="(quote js):getNetworkTime" />
```js
getNetworkTime() => Promise<time>
```


Returns a Promise for the current consensus network time,
represented as a `BigNumber`.

---
<Ref :name="(quote js):waitUntilTime" />
```js
waitUntilTime(time, onProgress?) => Promise<time>
```


Returns a Promise that will only be resolved after the specified consensus network time.
In isolated testing modes, this will also force time to pass on the network, usually by sending trivial transactions.
An <Defn :name="isolated testing mode">isolated testing mode</Defn> is a `REACH_CONNECTOR_MODE` that matches
`$NET-devnet` for all valid `$NET`, or when `REACH_ISOLATED_NETWORK` is set.

You may provide an optional `onProgress` callback, used for reporting progress,
which may be called many times up until the specified network time.
It will receive an object with keys `current` and `target`,

---
<Ref :name="(quote js):getNetworkSecs" />
```js
getNetworkSecs() => Promise<secs>
```


Like `getNetworkTime`, but returns a network seconds Promise.

---
<Ref :name="(quote js):waitUntilSecs" />
```js
waitUntilSecs(secs, onProgress?) => Promise<secs>
```


Like `waitUntilSecs`, but waits for a certain network seconds deadline.

---
<Ref :name="(quote js):wait" />
```js
wait(timedelta, onProgress?) => Promise<time>
```


Returns a Promise that will only be resolved after the specified time delta has elapsed.
The expression `await wait(delta, onProgress)` is the same as
`await waitUntilTime(add(await getNetworkTime(), delta), onProgress)`.
As with `waitUntilTime`, the `onProgress` callback is optional.

---
<Ref :name="(quote js):connector" />
```js
connector : Connector 
```


Represents the `Connector` the `stdlib` uses.

## {#ref-frontends-js-provider} Provider Selection

These functions allow you to choose which particular consensus network API provider to connect to.

::: note
On Ethereum, if you would like to use the wallet from the user's browser,
the `setProvider` functions are not needed.
The Reach standard library uses `window.ethereum` by default.

On Algorand, if you would like to use AlgoSigner,
the `setProvider` functions are necessary for all but `'LocalHost'`.
However, this will eventually become unnecessary.
Reach is working with Algorand wallets to develop standards
that will put provider selection in control of the end user,
instead of the DApp developer.
:::

---
<Ref :name="(quote js):setProviderByName" />
```js
setProviderByName(string) => void 
```


Supported provider names are: `'MainNet'`, `'TestNet'`, and `'LocalHost'`.

On Ethereum, `'MainNet'` will connect to homestead, and `'TestNet'` to ropsten.
Multiple free API providers are used behind the scenes, [as implemented by ethers.js](https://docs.ethers.io/v5/api/providers/#providers-getDefaultProvider).

On Algorand, `'MainNet'` will connect to MainNet, and `'TestNet'` to TestNet.
The free RandLabs API provider is used ( [https://algoexplorerapi.io](https://algoexplorerapi.io) ).

---
<Ref :name="(quote js):providerEnvByName" />
```js
providerEnvByName(string) => env 
```


Retrieve configuration information about providers by name.

---
<Ref :name="(quote js):setProviderByEnv" />
```js
setProviderByEnv(env) => void 
```


Select an API provider by supplying information about it.

This function's API is considered unstable.

`env` is a record with string keys and string values.

On Ethereum, env may include keys:
`'ETH_NODE_URI'`

On Algorand, env may include keys:
`'ALGO_LEDGER'`, `'ALGO_SERVER'`, `'ALGO_PORT'`, `'ALGO_TOKEN'`, `'ALGO_INDEXER_SERVER'`, `'ALGO_INDEXER_PORT'`, `'ALGO_INDEXER_TOKEN'`.

`'ALGO_LEDGER'` is used by AlgoSigner. Configuration for the indicated ledger should match the configuration present in AlgoSigner.

---
<Ref :name="(quote js):setProvider" />
```js
setProvider(provider): void 
```


Select an API provider by providing an object satisfying its interface.

This function's API is considered unstable.

On Ethereum, provider is an instance of ethers.provider.
See: [https://docs.ethers.io/v5/api/providers/provider/](https://docs.ethers.io/v5/api/providers/provider/)

On Algorand, provider is an object:
```js
{
  ledger: string,
  algodClient: algosdk.Algodv2,
  indexer: algosdk.Indexer,
}
```


See: [https://algorand.github.io/js-algorand-sdk/](https://algorand.github.io/js-algorand-sdk/)

## {#ref-frontends-js-utils} Utilities

These functions operate on JavaScript representations of Reach values.

---
<Ref :name="(quote js):protect" />
```js
protect(t, x) => x 
```


Asserts that value `x` has Reach type `t`. An exception is thrown if this is not the case.

---
<Ref :name="(quote js):T_Null" /><Ref :name="(quote js):T_Bool" /><Ref :name="(quote js):T_UInt" /><Ref :name="(quote js):T_Bytes" /><Ref :name="(quote js):T_Address" /><Ref :name="(quote js):T_Array" /><Ref :name="(quote js):T_Tuple" /><Ref :name="(quote js):T_Object" />
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
<Ref :name="(quote js):assert" />
```js
assert(p) 
```


Throws an exception if not given `true`.

---
<Ref :name="(quote js):Array_set" />
```js
Array_set(arr, idx, val) 
```


Returns a new array identical to `arr`, except that index `idx` is `val`.

---
<Ref :name="(quote js):bigNumberify" /><Ref :name="(quote js):isBigNumber" />
```js
bigNumberify(x) => UInt
isBigNumber(x) => bool
bigNumberToNumber(x) => number
```


<Defn :name="bigNumberify">bigNumberify</Defn> converts a JavaScript number to a BigNumber,
the JavaScript representation of Reach's `UInt`.

<Defn :name="isBigNumber">isBigNumber</Defn> checks if its input is a BigNumber.

<Defn :name="bigNumberToNumber">bigNumberToNumber</Defn> transparently applies `bigNumberify` to its
argument and returns a JavaScript number.

---
<Ref :name="(quote js):isHex" /><Ref :name="(quote js):hexToBigNumber" /><Ref :name="(quote js):stringToHex" /><Ref :name="(quote js):bigNumberToHex" /><Ref :name="(quote js):uintToBytes" /><Ref :name="(quote js):bytesEq" /><Ref :name="(quote js):digestEq" /><Ref :name="(quote js):addressEq" />
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
<Ref :name="(quote js):digest" />
```js
digest(ty:Type, x:ty) => Digest
```


Hashes the value.

---
<Ref :name="(quote js):randomUInt" />
```js
randomUInt() => UInt
```


Generates random bits as a `UInt`.
The number of bits generated depends on the particular consensus network.

---
```js
hasRandom
```


<Defn :name="hasRandom (Frontend)">hasRandom (Frontend)</Defn> A value suitable for use as a participant interact interface requiring a `random` function, such as `hasRandom`.
Reach does not natively support randomness and leaves random number generation to the frontend implementation.
This value is provided out of convenience; it is not mandatory to use this implementation.

---
```js
hasConsoleLogger
```


<Defn :name="hasConsoleLogger (Frontend)">hasConsoleLogger (Frontend)</Defn> A value suitable for use as a participant interact interface requiring a `log` function, such as `hasConsoleLogger`.
The `log` function provided takes an arbitrary amount of elements and prints them to stdout.
This value is provided out of convenience; it is not mandatory to use this implementation.

---
<Ref :name="(quote js):parseFixedPoint" />
```js
parseFixedPoint(FixedPoint) => number
```


Parses a `FixedPoint` number into a JavaScript number.

---
<Ref :name="(quote js):numberToFixedPoint" />
```js
numberToFixedPoint(number) => FixedPoint
```


Parses a JavaScript number into a `FixedPoint`.

---
<Ref :name="(quote js):parseInt" />
```js
parseInt(Int) => number
```


Parses a signed `Int` into a JavaScript number.

---
<Ref :name="(quote js):numberToInt" />
```js
numberToInt(number) => Int
```


Parses a JavaScript number into an `Int`.

---
<Ref :name="(quote js):add" /><Ref :name="(quote js):sub" /><Ref :name="(quote js):mod" /><Ref :name="(quote js):mul" /><Ref :name="(quote js):div" />
```js
add(UInt, UInt) => UInt
sub(UInt, UInt) => UInt
mod(UInt, UInt) => UInt
mul(UInt, UInt) => UInt
div(UInt, UInt) => UInt 
```


Integer arithmetic on `UInt`.

---
<Ref :name="(quote js):eq" /><Ref :name="(quote js):ge" /><Ref :name="(quote js):gt" /><Ref :name="(quote js):le" /><Ref :name="(quote js):lt" />
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

<Ref :name="(quote js):standardUnit" /><Ref :name="(quote js):atomicUnit" /><Ref :name="(quote js):minimumBalance" /><Ref :name="(quote js):parseCurrency" /><Ref :name="(quote js):formatCurrency" />
```js
standardUnit // string
atomicUnit // string
minimumBalance // atomicUnitAmount
parseCurrency(standardUnitAmount) => atomicUnitAmount
formatCurrency(atomicUnitAmount, int) => string  // display amount in standard unit
```


These functions handle amounts in a network's standard unit and its atomic unit.
A <Defn :name="standard unit">standard unit</Defn> is the network token unit most commonly associated with a network.
For example, the standard unit of Ethereum is ETH.
An <Defn :name="atomic unit">atomic unit</Defn> is the smallest unit of measure for the standard unit.
For example, the atomic unit of Ethereum is WEI.
An atomic unit is <Defn :name="atomic">atomic</Defn>, which means it cannot be divided into smaller units.

Some consensus networks, typically those with proof-of-stake, have minimum balances on their accounts, so this is exposed as `minimumBalance`.

Because there are 1,000,000,000,000,000,000 WEI in 1 ETH,
BigNumber is used to represet values in WEI.

Quantities of a network token should always be passed into Reach
in the token's atomic unit.

`bigNumberify` is transparently applied to `formatCurrency`'s first argument.

---
<Ref :name="(quote js):formatAddress" />
`formatAddress(acc) => string `

Formats the address in the way the user would expect to see it.

+ On Ethereum, it is a hex-encoded string starting with `'0x'`.
+ On Algorand, it is a base32-encoded string, ending with the checksum.


There is no corresponding `parseAddress` function because
the user-friendly form is also accepted from the frontend
in all places that Reach expects an address.

## {#ref-frontends-js-ask.mjs} `ask.mjs`

The Reach JavaScript standard library also provides the helper module `@reach-sh/stdlib/ask.mjs` for constructing console interfaces to your frontends.

<Ref :name="(quote js):ask" />
```js
import * as ask from '@reach-sh/stdlib/ask.mjs';
```


It provides the following exports:

<Ref :name="(quote js):ask" /><Ref :name="(quote js):yesno" /><Ref :name="(quote js):done" />
```js
ask(string, (string => result)) => Promise<result>
yesno(string) => boolean
done() => null
```


`ask` is an asynchronous function that asks a question on the console and returns a Promise for the first result that its second argument does not error on.

`yesno` is an argument appropriate to give as the second argument to `ask` that parses "Yes"/"No" answers.

`done` indicates that no more questions will be asked.
