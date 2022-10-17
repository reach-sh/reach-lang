# {#ref-frontends} Frontends

This section describes the libraries provided by Reach version @{VERSION} to support developing frontends.

Frontends are implemented in [JavaScript](##ref-frontends-js)
or via the [the RPC server](##ref-backends-rpc) and one of the languages with an RPC client:
[C Sharp](##ref-frontends-rpc-cs),
[JavaScript](##ref-frontends-rpc-js),
[Go](##ref-frontends-rpc-go), and
[Python](##ref-frontends-rpc-py).

# {#ref-frontends-js} JavaScript

You import the Reach JavaScript standard library by writing
```js
import { loadStdlib } from '@reach-sh/stdlib';
const stdlib = loadStdlib();

// or, equivalently
import { Reach } from '@reach-sh/stdlib';
const stdlib = new Reach(env);
```

When you use `{!cmd} reach run` or `{!cmd} reach react`, you don't need to do anything else.

However, most developers when they stop learning and experimenting will use their own testing or deployment infrastructure.
At that point, you should install the [`@reach-sh/stdlib`](https://www.npmjs.com/package/@reach-sh/stdlib) [`npm`](https://www.npmjs.com/) package however you prefer installing packages.
For example, you can use `{!cmd} npm`:
```cmd
$ npm install @reach-sh/stdlib
```

You only need to install the package directly if you are running your frontend without `reach` or using a tool like [webpack](https://webpack.js.org/) for deployment.

# {#ref-frontends-js-types} Types

Whenever you are interacting with the Reach standard library, you need to remember the JavaScript representation of each of the Reach types:

@{ref("js", "Contract")}
```js
// Reach   => JavaScript
Null       => null
Bool       => 'boolean'
UInt       => 'BigNumber'
UInt256    => 'BigNumber'
Bytes(len) => Uint8Array | 'string'
BytesDyn   => Uint8Array | 'string'
String     => 'string'
Digest     => 'BigNumber'
Address    => NetworkAccount
Contract   => Address on ETH; UInt on ALGO
Token      => Address on ETH; UInt on ALGO
Array      => array
Tuple      => array
Object     => object
Data       => ['variant', value]
Struct     => object
```

For example, the Reach type `{!rsh} MInt = Data({None: Null, Some: UInt})` inhabitant `{!rsh} MInt.Some(42)` is represented as `{!rsh} ['Some', 42]` in JavaScript.

`{!rsh} Bytes(len)` and `{!rsh} BytesDyn` may be represented as either a `'string'` or a `Uint8Array` byte array.
Both types are accepted as input.
If the input string is prefixed with `{!rsh} 0x`, the string will be interpreted as hex.
Reach will output a value as a `'string'` if the value is able to be encoded in UTF-8.
Otherwise, the value will be represented as `Uint8Array` byte array.

# {#ref-frontends-js-loader} Loading the Standard Library

@{ref("js", "loadStdlib")}@{ref("js", "Reach")}
```js
import { loadStdlib } from '@reach-sh/stdlib';
const stdlib = loadStdlib(env);

// or, equivalently
import { Reach } from '@reach-sh/stdlib';
const stdlib = new Reach(env);
```

Returns the standard library based on the provided `{!js} env`.

You may call this function multiple times and get completely separate standard libraries, with different (and incompatible!) configuration options.

---

In environments where the Reach standard library has implicit access to `{!js} process.env`,
you may omit the `{!js} env` argument, in which case `{!js} process.env` will be used.
If the standard library is being used with JavaScript bundlers like Webpack
---as it is with React, for example---
then Reach does **not** have implicit access to `{!js} process.env`.
In such scenarios, we recommend that you call this function like so:

```js
const stdlib = loadStdlib(process.env);
```

Alternatively, you can construct a custom object that has all of the environment keys and fields you need.
Each network supports different keys; see @{seclink("ref-networks")} for details.

As a special case, you may instead pass in the string `'ETH'`, `'ALGO'`, or `'CFX'`,
to select the desired connector directly.

---

By default, this method allows a user to load a standard library for a single connector.
That is, this method may not be called multiple times with varying connectors.
To bypass this restriction, use `{!js} unsafeAllowMultipleStdlibs`.

@{ref("js", "unsafeAllowMultipleStdlibs")}
```js
import { unsafeAllowMultipleStdlibs } from '@reach-sh/stdlib';
unsafeAllowMultipleStdlibs() => null
```

Calling this function will lift the restriction that
`{!js} loadStdlib` imposes on loading multiple standard libraries.

# {#ref-frontends-js-provider} Provider Selection

The first thing you should do in a frontend is decide if you need to specify a provider.

---

If you are building a browser-based DApp, then you may need to set up a fallback for users that do not have a wallet.

@{ref("js", "stdlib.setWalletFallback")}
```js
stdlib.setWalletFallback(make: () => wallet): void
```

When you call this function, if no browser wallet is available, then `{!js} make` will be called to construct one and it will be installed as if there were always a browser wallet.

The value that `{!js} make` should return differs between connectors:
- On Ethereum, it must match the interface of MetaMask.
- On Conflux, it must match the interface of ConfluxPortal.
- On Algorand, it must match the [ARC-0011](https://github.com/algorandfoundation/ARCs/blob/main/ARCs/arc-0011.md) standard.

---

:::note
Please refer to the [`algo-wallet-demo`](https://github.com/reach-sh/reach-lang/tree/master/examples/algo-wallet-demo) example to see a full walkthrough of this discussion.
:::


Since this function installs a new value as the browser wallet, if you call `{!js} stdlib.setWalletFallback` twice, then the second is guaranteed to do nothing, _even if you are using different standard library instances_, because this modifies a global property of the browser.
If you want to work around that, then you'd have to delete the property so that Reach cannot find a browser wallet on the second time.

For example, this is a bad program that is useless:
```js
const stdlib1 = loadStdlib();
stdlib1.setWalletFallback( make1 ); // maybe does something

const stdlib2 = loadStdlib();
stdlib2.setWalletFallback( make2 ); // does nothing!
```
because the second call is useless.
If you were using a connector like Algorand, then you'd want to do:
```js
const stdlib1 = loadStdlib();
stdlib1.setWalletFallback( make1 ); // maybe does something

const stdlib2 = loadStdlib();
delete window.algorand;
stdlib2.setWalletFallback( make2 ); // does something

f( stdlib1 ); // uses make2's result
```
But even this will not do what you think, because in `f`, `stdlib1` will still use the global variable `{!js} window.algorand`, which is the value that `make2` returns.

The solution to this is to explicitly access the wallet using `stdlib1`, before you reset the wallet:
```js
const stdlib1 = loadStdlib();
stdlib1.setWalletFallback( make1 ); // maybe does something
await stdlib1.getProvider(); // look at wallet

const stdlib2 = loadStdlib();
delete window.algorand;
stdlib2.setWalletFallback( make2 ); // does something
await stdlib2.getProvider(); // look at wallet

f( stdlib1 ); // uses make1's result
```

---
@{ref("js", "stdlib.walletFallback")}
```js
stdlib.walletFallback(opts: object): () => wallet
```

This function returns a value that may be passed to `{!js} setWalletFallback` to synthesize a wallet for use in browsers that do not supply a compliant wallet.
Its customization options, `{!js} opts`, depend on the connector.

On Ethereum and Conflux, it always errors and cannot provide a wallet.

On Algorand, it can provide a wallet that directly connects to the Algorand network, like `{!js} setProviderByName` (& `{!js} setProviderByEnv`), but provide interactive signing.
The network connection is specified via the `providerEnv` key, which may be a string (which is used as an argument to `{!js} providerEnvByName`) or an environment (which is used as an argument to `{!js} setProviderByEnv`).
Alternatively, a `{!js} BasicProvider` value can be provided with the key `provider` (see `{!js} stdlib.setProvider`'s documentation to understand the type of this value).

By default, signing is via an interactive browser window prompt, where the user repeatedly provides their mnemonic.
But, other fallbacks can be synthesized by providing special arguments in `opts`.


---

If the key `MyAlgoConnect` is provided, and bound to the `ALGO_MyAlgoConnect` export of `@reach-sh/stdlib`, then [MyAlgo](https://wallet.myalgo.com/home) will be used for signing.
For example, this sets the wallet fallback to be MyAlgo used with Algorand TestNet:
```js
import { ALGO_MyAlgoConnect as MyAlgoConnect } from '@reach-sh/stdlib';
stdlib.setWalletFallback(stdlib.walletFallback({
  providerEnv: 'TestNet', MyAlgoConnect }));
```

---

If the key `WalletConnect` is provided, and bound to the `ALGO_WalletConnect` export of `@reach-sh/stdlib`, then [WalletConnect](https://walletconnect.com/) is used to connect to the [PeraWallet](https://perawallet.app/) for signing.
For example, this sets the wallet fallback to be WalletConnect and the Algorand TestNet:
```js
import { ALGO_WalletConnect as WalletConnect } from '@reach-sh/stdlib';
stdlib.setWalletFallback(stdlib.walletFallback({
  providerEnv: 'TestNet', WalletConnect }));
```

Alternatively, you can call the `ALGO_MakePeraConnect` export of `@reach-sh/stdlib`, with `PeraWalletConnect` from `@perawallet/connect`, then PeraConnect is used to connect to the [PeraWallet](https://perawallet.app/) for signing.
For example, this sets the wallet fallback to be PeraConnect and the Algorand TestNet:
```js
import { ALGO_MakePeraConnect as MakePeraConnect } from '@reach-sh/stdlib';
import { PeraWalletConnect } from "@perawallet/connect";
stdlib.setWalletFallback(stdlib.walletFallback({
  providerEnv: 'TestNet', WalletConnect: MakePeraConnect(PeraWalletConnect) }));
```

This fallback exposes the underlying WalletConnect wrapper object as the `wc` property on the wallet.
Furthermore, it supports using an existing WalletConnect wrapper object by providing the key `WalletConnect_wc` in the `{!js} stdlib.walletFallback` options object.
(When you're using PeraConnect, the same object/keys/etc are used for the `PeraWalletConnect` object.)

---

Because these are fallbacks, you need to decide for your users which wallet they'll use, or make a user interface element to let them select which wallet fallback to use.

:::note
Please refer to the [`algo-wallet-demo`](https://github.com/reach-sh/reach-lang/tree/master/examples/algo-wallet-demo) example to see a full walkthrough of using wallet fallbacks and all their options.
:::

---

If you are not building a browser-based DApp, you may want to set the network provider by using a standard name or using environment variables:

---
@{ref("js", "stdlib.setProviderByName")}
```js
stdlib.setProviderByName(string) => void
```

Supported provider names are: `{!js} 'MainNet'`, `{!js} 'TestNet'`, and `{!js} 'LocalHost'`.

On Ethereum, `{!js} 'MainNet'` will connect to homestead, and `{!js} 'TestNet'` to ropsten.
Multiple free API providers are used behind the scenes, [as implemented by ethers.js](https://docs.ethers.io/v5/api/providers/#providers-getDefaultProvider).

On Algorand, `{!js} 'MainNet'` will connect to MainNet, and `{!js} 'TestNet'` to TestNet.
The free RandLabs API provider is used ([https://algoexplorerapi.io](https://algoexplorerapi.io)).

Provider connections are created with the code below:

`{!js} MainNet`

``` js
load: /examples/overview-networks/index.mjs
md5: a0d32a930197470096ccdd9fcaeace96
range: 9-9
```

`{!js} TestNet`

``` js
load: /examples/overview-networks/index.mjs
md5: a0d32a930197470096ccdd9fcaeace96
range: 11-11
```

`{!js} LocalHost`

``` js
load: /examples/overview-networks/index.mjs
md5: a0d32a930197470096ccdd9fcaeace96
range: 13-13
```

---
@{ref("js", "stdlib.providerEnvByName")}
```js
stdlib.providerEnvByName(string) => env
```

Retrieve configuration information about providers by name.

---
@{ref("js", "stdlib.setProviderByEnv")}
```js
stdlib.setProviderByEnv(env) => void
```

Select an API provider by supplying information about it.

`{!js} env` is a record with string keys and string values.

The environment object supports the same fields as `{!js} stdlib.loadStdlib`, which are documented in @{seclink("ref-networks")}.

---

Finally, Reach provides low-level access to defining a network provider.

---
@{ref("js", "stdlib.setProvider")}
```js
stdlib.setProvider(provider): void
```

Select an API provider by providing an object satisfying its interface.

This function's API is considered unstable.

On Ethereum, `{!js} provider` is an instance of `{!js} ethers.provider`.
See: [https://docs.ethers.io/v5/api/providers/provider/](https://docs.ethers.io/v5/api/providers/provider/)

On Algorand, `{!js} provider` is an object:
```js
interface BasicProvider {
  algod_bc: BaseHTTPClient,
  indexer_bc: BaseHTTPClient,
  algodClient: algosdk.Algodv2,
  indexer: algosdk.Indexer,
}
interface Provider extends BasicProvider {
  nodeWriteOnly: boolean,
  getDefaultAddress: () => Promise<Address>,
  isIsolatedNetwork: boolean,
  signAndPostTxns: (txns:WalletTransaction[], opts?: object) => Promise<unknown>,
};
```

The `{!js} algodClient` and `{!js} indexer` values are as specified by the [Algorand JS SDK](https://algorand.github.io/js-algorand-sdk/).
The `{!js} algod_bc` and `{!js} indexer_bc` are objects that represent HTTP connections to those values.
The `{!js} signAndPostTxns` function obeys [ARC-0008](https://github.com/reach-sh/ARCs/blob/reach-wallet/ARCs/arc-0008.md).

Example:

```js
load: /examples/ganache/index.mjs
md5: cca336d85844697eb44884226f9d2ce0
range: 10-13
```

---
@{ref("js", "getProvider")}
```js
stdlib.getProvider(): Promise<provider>
```

Promises a provider, matching the interface specified above.

## Provider Utilities

There are a few options to customize how Reach interacts with the Provider, however it is found.

---
@{ref("js", "setMinMillisBetweenRequests")}
```js
stdlib.setMinMillisBetweenRequests(ms: number): void
```

Setting this to a positive number forces outgoing requests to occur one at a time,
and limits them to occur no more frequently than one request every `ms` milliseconds.
This is only supported with certain connectors,
and applies to all Providers created by the Reach standard library.

---
@{ref("js", "setCustomHttpEventHandler")}
```js
stdlib.setCustomHttpEventHandler(h: (e: any) => Promise<void>): void
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
stdlib.setSigningMonitor(h: (evt: any, pre:Promise<any>, post:Promise<any>) => void): void
```

Allows for the installation of a custom hook to observe signing requests.
The handler `h` will be called on every request.
The `evt` argument is an unstable object that describes the request.
The `pre` argument is a Promise of an unstable object with details about the request, available after the request has been made.
The `post` argument is a Promise of an unstable object with details about the completed request, available after the request has been completed.

Example:

```js
load: /examples/signingMonitor/index.mjs
md5: ba025829c65235bef3ceddc4a5f0c150
range: 5-7
```

This sample has a hook of `{!js} async` and performs an `{!js} await` for the `{!js} Promise` of both `pre` and `post`.

# {#ref-frontends-js-acc} Creating an Account Handle

@{ref("js","acc")}
The second thing you should do in a frontend is create an account handle.

---

@{ref("js", "getDefaultAccount")}@{ref("js", "stdlib.getDefaultAccount")}
In real applications, you will use access the user's account via their wallet with

```js
stdlib.getDefaultAccount() => Promise<acc>
```

Returns a Promise for a Reach account abstraction for a "default" account on the consensus network.
The meaning of "default account" varies between contexts.
When running in the browser, the default account will be connected to a wallet.
This promise will be rejected with an exception if no sensible default account can be accessed for the current context.

Example:

```js
load: /examples/rsvp/index.js
md5: ac7d3caae5994352ef4cf445515730ab
range: 28-28
```

This code uses `{!js} stdlib.getDefaultAccount` to set `acc` to the account that was connected to the DApp earlier in the code.

---

@{ref("js", "newTestAccount")}@{ref("js", "stdlib.newTestAccount")}
But, when you are testing, you will use

```js
stdlib.newTestAccount(bal:UInt) => Promise<acc>
```

Returns a Promise for a Reach account abstraction for a new account on the consensus network with a given balance of network tokens.
This can only be used in private testing scenarios, because it uses a private faucet to issue network tokens.

```js
load: /examples/workshop-trust-fund/index.mjs
md5: 29f9f977d13d9651062bf18288b6c072
range: 29-33
```

This example from the Trust Fund workshop creates a `startingBalance` of 100 tokens, and then creates new accounts for the `funder`, `receiver` and `bystander`.
Each get funded 100 tokens by setting each `{!js} newTestAccount` balance to the `startingBalance` value.

@{ref("js", "newTestAccounts")}@{ref("js", "stdlib.newTestAccounts")}
```js
stdlib.newTestAccounts(howMany:number, balance:UInt) => Promise<Array<acc>>
```

Returns a Promise for an array of `{!js} howMany` test accounts, using `{!js} stdlib.newTestAccount`.

Example:

```js
load: /examples/minBalance/index.mjs
md5: 3c4457da0579671f1455423d0b44016b
range: 15-15
```

`{!js} newTestAccounts` sets up 3 new accounts (`accA`, `accB`, and `accC`) and loads them with the `startingBalance` of tokens so that they are usable for testing.

---

Reach also provides some low-level tools for creating account handles.
You will probably not use these.

---
@{ref("js", "newAccountFromSecret")}
```js
stdlib.newAccountFromSecret(secret: string | UInt8Array) => Promise<Account>
```

Returns a Promise for a Reach account abstraction for an account on the consensus network specified by the given secret.
The secret key must be either a hex string beginning with `{!js} '0x'`, or a `{!js} Uint8Array`.
The number of bytes in the secret differs between connectors.

Example:

```js
load: /examples/rps-8-interact/index.mjs
md5: 9f824fcd58e5fdda4f4761b99093cfdc
range: 18-25
```

In this code, if the user chooses to not create a new account, then the DApp uses `{!rsh} newAccountFromSecret` to create a new account.
The `acc` object `{!rsh} await`s the user to input the secret and then sets the provided account to itself.

---
@{ref("js", "newAccountFromMnemonic")}
```js
stdlib.newAccountFromMnemonic(phrase: string) => Promise<Account>
```

Returns a Promise for a Reach account abstraction for an account on the consensus network specified by the given mnemonic phrase.
The mnemonic phrase must be a string of whitespace-separated words from the BIP-39 English wordlist.
The number of words required for a mnemonic differs between connectors.

---
@{ref("js", "createAccount")}
```js
stdlib.createAccount() => Promise<acc>
```

Returns a Promise for a Reach account abstraction for a new account on the consensus network.
The account will have an empty balance of network tokens.

Example:

```js
load: /examples/minBalance/index.mjs
md5: 3c4457da0579671f1455423d0b44016b
range: 16 - 16
```

This code creates an account for `accD` without a starting balance.

---
@{ref("js", "fundFromFaucet")}
```js
stdlib.fundFromFaucet(acc: Account | Address, balance: BigNumber) => Promise<void>
```

Adds the given balance of network tokens to a Reach account abstraction.
This can only be used in private testing scenarios, as it uses a private faucet to issue network tokens, as well as certain public TestNet scenarios.
You can use `{!js} canFundFromFaucet` to check if `{!js} fundFromFaucet` can be used.

Example:

```js
load: /examples/rps-9-web/index.js
md5: 5e5c8976731882c06e7f094e05ca43b3
range: 32 - 34
```

This code funds the account with `fundAmount` worth of tokens from a faucet.
This is used in cases where funding from a normal faucet is difficult or impossible, such as when using large token quantities.

---
@{ref("js", "canFundFromFaucet")}
```js
stdlib.canFundFromFaucet() => Promise<boolean>
```

A Promise that resolves to `{!js} true` if `{!js} fundFromFaucet` can be used, `{!js} false` if not.

Example:

```js
load: /examples/rps-9-web/index.js
md5: 5e5c8976731882c06e7f094e05ca43b3
range: 26 - 34
```

This React frontend from the [Rock, Paper, Scissors](##tut) tutorial checks if the account can be funded from a faucet using `{!js} canFundFromFaucet`.
If the value returns true, then the account is funded by `{!js} fundFromFaucet`.

---
@{ref("js", "connectAccount")}
```js
stdlib.connectAccount(networkAccount) => Promise<acc>
```

Returns a Promise for a Reach account abstraction for an existing account for the consensus network based on the connector-specific account specification provided by the `{!js} networkAccount` argument.

```js
// network => networkAccount type
ETH        => ethers.Wallet
ALGO       => {addr: string, sk: UInt8Array(64)}
```

Example:

```js
load: /examples/ganache/index.mjs
md5: cca336d85844697eb44884226f9d2ce0
range: 10-16
```

Here, `faucet` is set to the previously coded `ganacheProvider` that has a `{!js} getSigner` method.
This allows `{!js} connectAccount` to be able to connect `faucet` to `{!js} setFaucet` to provide the testing funds.

# Using Account Handles

Once you have an account handle, you can inspect it

---
@{ref("js", "networkAccount")}
```js
acc.networkAccount => networkAccount
```

Returns the connector-specific account specification of a Reach account abstraction.

Example:

```js
load: /examples/clawback/index.mjs
md5: edcec408b29e3995381679f71381dc80
range: 26-26
```

This code sets `aliceAddr` to the consensus network format of the wallet address of `accAlice` using the `{!js} networkAccount` method.

---
@{ref("js", "getAddress")}
```js
acc.getAddress() => string
```

Returns the account's address as a string. The format of this string varies across connectors.

Example:

```js
load: /examples/interact-this/index.mjs
md5: 34eaa215660697ce8c5ba87387c8f0f1
range: 14-19
```

In this snippet, `{!js} getAddress` obtains the addresses of `Alice` and `Bob`, which were created with the `{!js} newTestAccounts` method.
Next, each address is logged so that participants can see both addresses.
This can be useful to verify that the address receiving the payment is the correct address.

---
@{ref("js", "setDebugLabel")}
```js
acc.setDebugLabel(string) => acc
```

An account may set a distinguishing label to use in debug logs. If no label is provided, then the first four digits of the account address will be used.

Example:

```js
load: /examples/ctc-address/index.mjs
md5: 0a4b59a9ff0f11d45e20f90fc2f1a233
range: 9-10
```

This code uses `{!js} setDebugLabel` to set `accAlice` to the label of `Alice`, and `accBob` to `Bob`.
This makes debugging much simpler, especially when using a large number of accounts, because each account can be set to a different human-readable string.

---
@{ref("js", "tokenAccept")}
```js
acc.tokenAccept(token) => Promise<void>
```

Returns a Promise that completes when the Reach account abstraction is ready to accept non-network tokens specified by the `{!js} token`.
This does nothing on some consensus networks, but should always be used to ensure your frontend is blockchain agnostic.

Example:

```js
load: examples/atomic-swap/index.mjs
md5: 9e2dd8f8db4ef6e83cc5c95fab50fd80
range: 37-40
```

Here, `Alice` and `Bob` opt-in to the tokens `zorkmid` and `gil` using `{!js} tokenAccept` prior to receiving them.

---
@{ref("js", "tokenAccepted")}
```js
acc.tokenAccepted(token) => Promise<boolean>
```

Returns a Promise that returns if an account may accept a given token.
This does nothing on some consensus networks, but should always be used to ensure your frontend is blockchain agnostic.

---
@{ref("js", "acc.tokensAccepted")}@{ref("js", "stdlib.tokensAccepted")}
```js
stdlib.tokensAccepted(acc: Account | Address) => Promise<Array<Token>>
acc.tokensAccepted() => Promise<Array<Token>>
```

Returns a Promise for an array of tokens that are accepted by `acc`.
On networks which do not keep track of this information (e.g. Ethereum), this returns an empty array.

---
@{ref("js", "appOptIn")}
```js
ctc.appOptIn() => Promise<void>
```

This does nothing on some networks.
On others, it opts into the contract `ctc`.

---
@{ref("js", "appOptedIn")}@{ref("js", "stdlib.appOptedIn")}
```js
acc.appOptedIn(info:ContractInfo) => Promise<boolean>
stdlib.appOptedIn(acc: Account | Address, info: ContractInfo) => Promise<boolean>
```

On some networks this always returns `true`.
On others, it checks whether the account has opted into the contract specified by `info`.

---
@{ref("js", "tokenMetadata")}
```js
acc.tokenMetadata(token) => Promise<object>
```

Returns a Promise of the metadata for a non-network token specified by the `{!js} token`.

---
@{ref("js", "balanceOf")}@{ref("js", "stdlib.balanceOf")}@{ref("js", "acc.balanceOf")}
```js
acc.balanceOf(token?: Token) => Promise<BigNumber>
stdlib.balanceOf(acc: Account | Address, token?: Token) => Promise<BigNumber>
```

Promises the balance of network tokens (or non-network tokens if `{!js} token` is provided) held by given Reach account abstraction `{!js} acc`.

Example:

```js
load: examples/raffle/index.mjs
md5: 69477201beedc87b2616001290ae29cf
range: 10-10
```

This code obtains the balance of network tokens for a user, `{!js} who`, and sets it to the `getBalance` object.
This makes the account balance simpler to reference later in the code, such as displaying balances after transactions, proving that cancelled transactions did not transfer tokens, etc.

---
@{ref("js", "balancesOf")}@{ref("js", "stdlib.balancesOf")}@{ref("js", "acc.balancesOf")}
```js
acc.balancesOf(tokens: Array<Token | null>) => Promise<Array<BigNumber>>
stdlib.balancesOf(acc: Account | Address, tokens: Array<Token | null>) => Promise<Array<BigNumber>>
```
Promises an array of balances that corresponds with the provided array of tokens, `{!js} tokens`,
for a given Reach account `{!js} acc`.
If `{!js} tokens` contains a `{!js} null`,
the corresponding position in the output array will contain the account's balance of network tokens.
This function is more efficient for getting multiple token balances than repeated calls to `{!js} stdlib.balanceOf`.

Example:

```js
load: examples/account-balancesOf/index.mjs
md5: df8e6d557df389629e04f6eb544a7baf
range: 22-22
```

The `eggBal`, `bknBal`, and `tstBal` array is set equal to the array of `egg.id`, `bkn.id`, `tst.id` using an `{!js} await` and the `{!js} balancesOf` method.

---
@{ref("js", "minimumBalanceOf")}
```js
stdlib.minimumBalanceOf(acc) => Promise<BigNumber>
```

Promises the portion of `{!js} balanceOf(acc)` which may not be transferred by
the given account.
Some networks restrict the usage of an account's funds.
On networks that do not, this will always return zero.

---
@{ref("js", "transfer")}
```js
stdlib.transfer(from: Account, to: Account | Address, amount, token?: Token, opts?: TransferOpts) => Promise<void>
```

Performs a transfer of `{!js} amount` from `{!js} from` to `{!js} to`,
which are accounts, such as those returned by `{!js} connectAccount`.
If `{!js} token` is not provided, then the transfer is of network tokens;
otherwise, it is of the designated non-network token.
The returned `{!js} Promise` will only be resolved after the transfer completes.

Possible options to provide in `{!js} opts` include:

Algorand-only options:
+ `{!js} closeTo` An `{!js} Address` to which all remaining ALGO will be sent before the sender account is closed.
If `{!js} token` is specified, the sender account opts out of the token rather than closing.
See [this page](https://developer.algorand.org/docs/get-details/transactions/#close-an-account) for more information.
The default is no close address.
+ `{!js} note` A `{!js} Uint8Array` for the `Note` field of the asset creation transaction.
The default is a note containing the Reach version used to perform the transfer.


Example:

```js
load: /examples/transfer-from-zero/index.mjs
md5: e85c5362951c9e7c5d1c727d3f3e7dac
range: 24-25
```

This transfers one (1) unit of the network token from Alice's account `accAlice`, to Bob's account `accBob`.

## {#ref-frontends-js-acc-eth} EVM-specific (Ethereum and Conflux)

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

```js
load: /examples/atomic-swap/index.mjs
md5: 9e2dd8f8db4ef6e83cc5c95fab50fd80
range: 30-32
```

Here, there is a `{!js} myGasLimit` object created, which is set to `{!js} 5000000`.
This is then applied to both the `{!js} accAlice` and `{!js} accBob` by using the `{!js} setGasLimit` method so that the maximum gas cost is set for each account.

## {#ref-frontends-js-acc-cfx} Conflux-specific

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

# {#ref-frontends-js-ctc} Creating a Contract Handle

@{ref("js","ctc")}@{ref("js", "contract")}
The third thing you should do in a frontend is create a contract handle, so you can actually interact with your Reach program.

---

```js
acc.contract(bin: Backend, info?: Promise<ContractInfo>) => Contract
stdlib.contract(bin: Backend, info?: Promise<ContractInfo>) => Promise<Contract>
```

Returns a Reach contract handle based on the `{!js} bin` argument, and optional `ContractInfo` provided.
The `{!js} acc.contract` form will use the account `{!js} acc` to interact with the returned contract.
The `{!js} stdlib.contract` form is equivalent to `{!js} (await stdlib.createAccount()).contract`, generating an account on demand
(this means the contract can only be used for read-only operations).
This `{!js} bin` argument is the module produced by `{!cmd} reach compile`.

If `{!js} info` is provided, it must be a `{!js} ContractInfo` value, or a `{!js} Promise` that eventually yields a `{!js} ContractInfo` value.
When provided, Reach will verify that the contract given actually matches the bytecode produced by `{!cmd} reach compile` and will error if it is different in any way.

Typically, the deployer of a contract will not provide `{!js} info`, while users of a contract will.
In an automated, single instance program, `{!rsh} ctc.getInfo()` is typically used to acquire `{!js} info`;
while in non-automated programs, an application uses out-of-band communication, such as an external database or user input, to acquire the `{!js} info` argument.

The first publishing participant will attempt to deploy a contract for an application.
If `{!js} info` was provided, an error will be thrown.
This deployment can only happen one time, so subsequent attempts will fail with an error.

This function may emit warnings if there is any danger, risk, or subtlety to using this contract on your chosen consensus network.
You can omit this warning by setting `{!cmd} REACH_NO_WARN`, but we recommend that you do not.

The `{!js} acc.contract` form of the function does not block.

---

@{ref("js", "getInfo")}
```js
ctc.getInfo() => Promise<ContractInfo>
```

Returns a Promise for a `{!js} ContractInfo` value that may be given to `{!js} contract` to construct a Reach contract handle for this contract.
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

---
@{ref("js", "ctc.getEventTys")}
```js
ctc.getEventTys() => Record<string, ReachType[]>
```

Returns a mapping from event name to the Reach types of the data emitted for that event.
This can be used to determine the connector-specific event signature.
For example:

```js
const tys = ctc.getEventTys();
const e = 'Swap';
const sig = `${e}${tys[e].toString()}`;
```

---
@{ref("js", "getInternalState")}
```js
ctc.getInternalState() => Promise<{ [key: string], ReachType }>
```

Returns a mapping that associates the steps of a `{!js} contract` to the `{!js} ReachType` of its internal state variables.

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

# Running Participants: `ctc.participants`, `ctc.p`

Contract handles provide access to the `{!rsh} Participant`s of the compiled backend, `{!js} bin`, that they were constructed with.

---
@{ref("js", "ctc.participants")}@{ref("js", "ctc.p")}
```js
ctc.participants // {[name: string]: (interact:Object) => Promise}
ctc.p

ctc.p.Alice(interact)
```

An object where the keys are the participant names and the values are functions that accept an interact object and return a Promise that completes when the participant ends.

`{!js} acc.contract(backend).p.Alice(io)` is equivalent to `{!js} backend.Alice(acc.contract(backend), io)`, but does not require duplication of the `{!js} backend` component.

---

When you run a participant, you may want to run it up until a particular `{!rsh} interact` function and then disconnect.
If so, then you should use

@{ref("js", "withDisconnect")}@{ref("js", "stdlib.withDisconnect")}@{ref("js", "disconnect")}@{ref("js", "stdlib.disconnect")}
```js
stdlib.withDisconnect<T>(f: () => Promise<T>) => Promise<T>
stdlib.disconnect(t: any) => void
```
`withDisconnect` calls the given function `f` such that any calls to `disconnect` within `f` will cause `withDisconnect` to return immediately.
`withDisconnect` returns the value passed to `disconnect`.

`disconnect` causes the surrounding call to `withDisconnect` to immediately return `t`.
`disconnect` must be called from within a function passed to `withDisconnect`,
otherwise an exception will be thrown.

`withDisconnect` and `disconnect` are intended as a utility to exit participant frontends early, like such:
```js
await stdlib.withDisconnect(() => ctcAlice.participants.Alice({
  ready: () => {
    console.log("Ready!");
    stdlib.disconnect(null); // causes withDisconnect to immediately return null
  }
}));
```
Once an account disconnects from the contract, they cannot rejoin as the same participant.

# Calling API functions: `ctc.apis`, `ctc.a`

Contract handles provide access to the `{!rsh} API`s of the compiled backend, `{!js} bin`, that they were constructed with.

@{ref("js", "ctc.apis")}@{ref("js", "ctc.a")}
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

---

@{ref("js", "ctc.safeApis")}
```js
ctc.safeApis
ctc.safeApis.Voter.cast("Pedro")
```

This object is the same as `{!js} ctc.apis` except the API functions return a `{!rsh} Maybe` value.
If the call fails, then `{!js} ['None', null]` will be returned. If the call succeeds, the return value will
be wrapped with `{!js} Some`, e.g. `{!js} ['Some', 4]`.

# Calling View functions: `ctc.views`, `ctc.v`

Contract handles provide access to the `{!rsh} View`s of the compiled backend, `{!js} bin`, that they were constructed with.

:::note
Views are [defined in application initialization](##ref-programs-appinit-view) and then they are [set in consensus steps](##ref-programs-consensus-view). Both of these steps are in Reach. This section is about accessing them in JavaScript frontends.
:::

---

@{ref("js", "ctc.views")}@{ref("js", "ctc.v")}
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

If we add an alias for the above view using `{!rsh} View({ owner: Address }, { owner: ["holder"]})`, we could also access the alias:

```js
ctc.v.holder();
```

---

@{ref("js", "ctc.unsafeViews")}
```js
ctc.unsafeViews
ctc.unsafeViews.NFT.owner()
```

This object is the same as `{!js} ctc.views` except the value of the view is not wrapped in a `{!rsh} Maybe` type.
If a view is set, the value will be returned as is, without being wrapped in `{!rsh} Some`.
If a view is not set, an error will be thrown.

---
@{ref("js", "ctc.getViews")}@{ref("js", "getViews")}
```js
ctc.getViews() => Object
```

This deprecated function is an abbreviation of `{!js} ctc.views`.

# Observing Events: `ctc.events`, `ctc.e`

Contract handles provide access to the `{!rsh} Events` of the compiled backend, `{!js} bin`, that they were constructed with.

@{ref("js", "ctc.events")}@{ref("js", "ctc.e")}
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

 `{!js} next` will wait for the next `{!rsh} Event` to occur, returning the time the event occurred
and the arguments to the event.

 `{!js} seek` will set the internal time of the EventStream to the given argument.
The EventStream will use this time as the minimum bound when searching for `{!rsh} Event`s.

 `{!js} seekNow` will set the internal time of the EventStream to the latest network time.
The EventStream will use this time as the minimum bound when searching for `{!rsh} Event`s.

 `{!js} lastTime` will return the last network time that an `{!rsh} Event` was emitted.

 `{!js} monitor` accepts a function of type `{!js} Event<T> => void` as an argument. The provided function will be
called whenever the `{!rsh} Event` occurs.

# Launching tokens

The standard library provides a convenient way to launch new non-network tokens.

---
@{ref("js", "launchToken")}@{ref("js", "stdlib.launchToken")}
```js
stdlib.launchToken(accCreator: Account, name: string, sym: string, opts?: LaunchTokenOpts) => Promise<object>
```

Launches a non-network token with the given `{!js} name` and unit symbol `{!js} sym`.
Launched on the network by `{!js} accCreator`.

Example:

```js
load: /examples/atomic-swap/index.mjs
md5: 9e2dd8f8db4ef6e83cc5c95fab50fd80
range: 24-25
```

The tokens `gil` and `zorkmid` are created in this code snippet, and then launched using `{!js} stdlib.launchToken` by the `{!js} accCreator` account with the symbols `GIL` and `ZMD`.

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
+ `{!js} clawback` Address that can clawback (steal) holdings of the token.
The default is no clawback address.
+ `{!js} freeze` Address that can freeze holdings of the token.
The default is no freeze address.
+ `{!js} defaultFrozen` A boolean that indicates whether token holdings are frozen by default.
The default is `{!js} false`.
+ `{!js} reserve` Address that should hold reserves of the token.
The default is no reserve address.
+ `{!js} note` A `{!js} Uint8Array` for the `Note` field of the asset creation transaction.
The default is no note.

For more information on Algorand-only options, see
https://developer.algorand.org/docs/get-details/transactions/transactions/#asset-parameters.

# {#ref-frontends-js-network} Network Utilities

The standard library provides a number of utilities functions for interacting with the connected network.

---
@{ref("js", "stdlib.connector")}@{ref("js", "connector")}
```js
stdlib.connector : string
```

Represents the `{!js} Connector` the `{!js} stdlib` uses.

---
@{ref("js", "stdlib.getNetworkTime")}@{ref("js", "getNetworkTime")}
```js
stdlib.getNetworkTime() => Promise<time>
```

Returns a Promise for the current consensus network time, represented as a `BigNumber`.

```js
load: /examples/getTimeSecs-demo/index.mjs
md5: bd5f44e95dd7b28b6bd435703ef21596
range: 5-15
```

This example obtains the starting time of the DApp by using `{!js} getNetworkTime`.
The network time is then written to the console to provide the information to users.
Then, it is converted to seconds using `{!js} getTimeSecs`, which is also written to the console.

---
@{ref("js", "stdlib.getNetworkSecs")}@{ref("js", "getNetworkSecs")}
```js
stdlib.getNetworkSecs() => Promise<secs>
```

Like `{!js} stdlib.getNetworkTime`, but returns a network seconds Promise.

---
@{ref("js", "stdlib.getTimeSecs")}@{ref("js", "getTimeSecs")}
```js
stdlib.getTimeSecs(time) => Promise<secs>
```

Takes a network time, such as a value returned from `{!js} stdlib.getNetworkTime`, and converts it into network seconds.

---
@{ref("js", "stdlib.waitUntilTime")}@{ref("js", "waitUntilTime")}
```js
stdlib.waitUntilTime(time, onProgress?) => Promise<time>
```

Returns a Promise that will only be resolved after the specified consensus network time.
In isolated testing modes, this will also force time to pass on the network, usually by sending trivial transactions.
An @{defn("isolated testing mode")} is a `REACH_CONNECTOR_MODE` that matches
`$NET-devnet` for all valid `$NET`, or when `REACH_ISOLATED_NETWORK` is set.

You may provide an optional `{!js} onProgress` callback, used for reporting progress,
which may be called many times up until the specified network time.
It will receive an object with keys `{!js} current` and `{!js} target`,

Example:

```js
load: /examples/pr855/index.mjs
md5: 35b43976f21f4043f858cf344e210904
range: 20-22
```

The `{!js} while` statement in this code has an `{!js} await` that has the code wait the amount of time equal to `moment` by using `{!js} waitUntilTime`.

---
@{ref("js", "stdlib.waitUntilSecs")}@{ref("js", "waitUntilSecs")}
```js
stdlib.waitUntilSecs(secs, onProgress?) => Promise<secs>
```

Like `{!js} stdlib.waitUntilSecs`, but waits for a certain network seconds deadline.

---
@{ref("js", "stdlib.wait")}@{ref("js", "wait")}
```js
stdlib.wait(timedelta, onProgress?) => Promise<networkTime>
```

Returns a Promise that will only be resolved after the specified time delta has elapsed.
The expression `{!js} await stdlib.wait(delta, onProgress)` is the same as
`{!js} await stdlib.waitUntilTime(add(await stdlib.getNetworkTime(), delta), onProgress)`.
As with `{!js} stdlib.waitUntilTime`, the `{!js} onProgress` callback is optional.

One use case example of `{!js} stdlib.wait` is to emit an event and then call `stdlib.wait(delta, onProgress)` to ensure the event has time to complete.
(In the example below, `wt` is shorthand for "wait time".)

```js
waitToPay: async (price) => {
  const fmtPrice = stdlib.formatCurrency(price, 4);
  const wt = Math.floor(Math.random() * 60);
  console.log(`Alice waits ${wt} seconds.`);
  await stdlib.wait(wt);
  console.log(`Alice is ready to pay ${fmtPrice}.`);
  return true;
},
```

Although, it is possible to use `{!js} wait` to give an event time to execute,
it is a better practice to create an explicit synchronization with an [event](##ref-programs-appinit-events) or instruct the deployer to communicate with [API](##ref-programs-appinit-api) callers off-chain.

---
@{ref("js", "stdlib.setValidQueryWindow")}@{ref("js", "setValidQueryWindow")}
```js
stdlib.setValidQueryWindow(width: number|true) => void
```

Sets the maximum width of the query windows used to query the network for event logs.
The value `{!js} true` indicates that no window size should be used, and queries may span arbitrarily large window sizes.
While each connector has a default value that works for most common cases, tweaking this setting may be useful when dealing with layer two networks or custom endpoints that are more restrictive than normal nodes on the network.

# {#ref-frontends-js-utils} Utilities

The standard library provides a number of utilities functions for interacting with JavaScript representations of Reach values.

---
@{ref("js", "stdlib.protect")}@{ref("js", "protect")}
```js
stdlib.protect(t:ReachType, x:t) => x
```

Asserts that value `{!js} x` has Reach type `{!js} t`. An exception is thrown if this is not the case.

---
@{ref("js", "T_Null")}@{ref("js", "T_Bool")}@{ref("js", "T_UInt")}@{ref("js", "T_Bytes")}@{ref("js", "T_Address")}@{ref("js", "T_Array")}@{ref("js", "T_Tuple")}@{ref("js", "T_Object")}@{ref("js", "ReachType")}
```js
stdlib.T_Null // : ReachType
stdlib.T_Bool // : ReachType
stdlib.T_UInt // : ReachType
stdlib.T_Bytes(number) // : ReachType
stdlib.T_BytesDyn // : ReachType
stdlib.T_StringDyn // : ReachType
stdlib.T_Digest // : ReachType
stdlib.T_Address // : ReachType
stdlib.T_Array(ReachType, number) // : ReachType
stdlib.T_Tuple([ReachType ...]) // : ReachType
stdlib.T_Object({Key: ReachType ...}) // : ReachType
stdlib.T_Data({Variant: ReachType ...}) // : ReachType
```

Each of these represent the corresponding Reach type.

---
@{ref("js", "toString")}
```js
ReachType.toString() => string
```

Returns a string representation for a given `{!js} ReachType`.
This representation is consistent with the loaded connector's ABI.
For example, on ALGO, `{!js} stdlib.T_UInt.toString()` returns `'uint64'`, but `'uint256'` on ETH.

---
@{ref("js", "assert")}
```js
stdlib.assert(p)
```

Throws an exception if not given `{!js} true`.

Example:

```js
load: /examples/argz/index.mjs
md5: d98c02ebf76e5ce55b6dec475c82539e
range: 4 - 7
```

This code checks the arguments passed to the `{!cmd} ./reach run` command.
Lines 6 and 7 will not throw an exception if `{!cmd} ./reach run index hello "Mr. Postman"` is run in the terminal, because `{!js} process.argv` will return the following array:

```js
[ '/usr/local/bin/node', '/app/index.mjs', 'Hello', 'Mr. Postman' ]
```

Thereby making `{!js} process.argv[2] === 'Hello'` and `{!js} process.argv[3] === 'Mr. Postman'` to evaluate to `{!js} true`.

---
@{ref("js", "Array_set")}
```js
stdlib.Array_set(arr, idx, val)
```

Returns a new array identical to `{!js} arr`, except that index `{!js} idx` is `{!js} val`.

---
@{ref("js", "bigNumberify")}@{ref("js", "isBigNumber")}@{ref("js", "bigNumberToNumber")}@{ref("js", "bigNumberToBigInt")}
```js
stdlib.bigNumberify(x) => UInt
stdlib.isBigNumber(x) => bool
stdlib.bigNumberToNumber(x) => number
stdlib.bigNumberToBigInt(x) => bigint
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
stdlib.UInt_max // UInt
stdlib.isHex(x) => bool
stdlib.hexToBigNumber(bytes) => UInt
stdlib.stringToHex(string) => bytes
stdlib.bigNumberToHex(UInt) => bytes
stdlib.uintToBytes(UInt) => bytes
stdlib.bytesEq(bytes, bytes) => bool
stdlib.digestEq(Digest, Digest) => bool
stdlib.addressEq(Address, Address) => bool
stdlib.ctcAddrEq(Contract, Address) => bool
stdlib.digest_xor(Digest, Digest) => Digest
```

These are additional conversion and comparison utilities.

---
@{ref("js", "digest")}
```js
stdlib.digest(ty:ReachType, v:ty) => Digest
stdlib.digest(tys:ReachType[], vs:[tys]) => Digest
```

Hashes the values.
If given one type and one value, the result is the same as if the arguments were each wrapped in an array.

```js
load: /examples/digest-mod/index.mjs
md5: e53eb004657f03e541768cdbb6e39123
range: 23-25
```

The `ExampleeExampleeExampleeExamplee` is a string of the Reach type that `{!rsh} digest` turns into a hash.
The maximum string length that can be digested in this example is set to 32 bytes.

---
@{ref("js", "randomUInt")}
```js
stdlib.randomUInt() => UInt
```

Generates random bits as a `{!rsh} UInt`.
The number of bits generated depends on the particular consensus network.

Example:

```js
load: /examples/nft-auction/index.mjs
md5: e3dbe19654f83958921c964264c9d6b3
range: 91 - 94
```

This code is used to generate random bits as a `{!rsh} UInt` as the unique identifier of `Alice` stored as `id` on line 92.
Line 93 reveals the `id` in the console and line 94 returns the `id`.

---
@{ref("js", "hasRandom")}@{ref("js", "stdlib.hasRandom")}
```js
stdlib.hasRandom
```

@{defn("hasRandom (Frontend)")} A value suitable for use as a participant interact interface requiring a `random` function, such as `{!rsh} hasRandom`.
Reach does not natively support randomness and leaves random number generation to the frontend implementation.
This value is provided out of convenience; it is not mandatory to use this implementation.

---
@{ref("js", "hasConsoleLogger")}@{ref("js", "stdlib.hasConsoleLogger")}
```js
stdlib.hasConsoleLogger
```

@{defn("hasConsoleLogger (Frontend)")} A value suitable for use as a participant interact interface requiring a `log` function, such as `{!rsh} hasConsoleLogger`.
The `{!js} log` function provided takes an arbitrary amount of elements and prints them to stdout.
This value is provided out of convenience; it is not mandatory to use this implementation.

---
@{ref("js", "parseFixedPoint")}
```js
stdlib.parseFixedPoint(FixedPoint) => number
```

Parses a `{!rsh} FixedPoint` number into a JavaScript number.

---
@{ref("js", "numberToFixedPoint")}
```js
stdlib.numberToFixedPoint(number) => FixedPoint
```

Parses a JavaScript number into a `{!rsh} FixedPoint`.

---
@{ref("js", "parseInt")}
```js
stdlib.parseInt(Int) => number
```

Parses a signed `{!rsh} Int` into a JavaScript number.

---
@{ref("js", "numberToInt")}
```js
stdlib.numberToInt(number) => Int
```

Parses a JavaScript number into an `{!rsh} Int`.

---
@{ref("js", "add")}@{ref("js", "sub")}@{ref("js", "mod")}@{ref("js", "mul")}@{ref("js", "div")}
```js
stdlib.add(UInt, UInt) => UInt
stdlib.sub(UInt, UInt) => UInt
stdlib.mod(UInt, UInt) => UInt
stdlib.mul(UInt, UInt) => UInt
stdlib.div(UInt, UInt) => UInt
stdlib.band(UInt, UInt) => UInt
stdlib.bior(UInt, UInt) => UInt
stdlib.bxor(UInt, UInt) => UInt
stdlib.add256(UInt256, UInt256) => UInt256
stdlib.sub256(UInt256, UInt256) => UInt256
stdlib.mod256(UInt256, UInt256) => UInt256
stdlib.mul256(UInt256, UInt256) => UInt256
stdlib.div256(UInt256, UInt256) => UInt256
stdlib.band256(UInt256, UInt256) => UInt256
stdlib.bior256(UInt256, UInt256) => UInt256
stdlib.bxor256(UInt256, UInt256) => UInt256
stdlib.muldiv(UInt, UInt) => UInt
stdlib.cast(UIntTy, UIntTy, UInt, Bool) => UInt // Bool argument decides if cast should truncate
```

Integer arithmetic on `{!rsh} UInt`.

---
@{ref("js", "eq")}@{ref("js", "ge")}@{ref("js", "gt")}@{ref("js", "le")}@{ref("js", "lt")}
```js
stdlib.eq(UInt, UInt) => bool
stdlib.ge(UInt, UInt) => bool
stdlib.gt(UInt, UInt) => bool
stdlib.le(UInt, UInt) => bool
stdlib.lt(UInt, UInt) => bool
```

Integer comparisons on `{!rsh} UInt`.

---
@{ref("js", "btoiLast8")}
```js
stdlib.btoiLast8(Bytes) => UInt
```

Converts the last 8 bytes of a string to an `{!rsh} UInt`.

Example:

```js
load: /examples/bytes-mod/index.mjs
md5: be92a6fe101d3f22bf3dd463aa618f80
range: 14 -17
```

```js
load: /examples/bytes-mod/index.mjs
md5: be92a6fe101d3f22bf3dd463aa618f80
range: 22 -25
```

This code converts the last 8 bytes of `b1`, `b2`, `b3` and `b4` to an `{!rsh} UInt` on lines 22, 23, 24 and 25, respectively.
After which, the `{!js} mod` method is called on the respective results.

If the string is less than 8 bytes long, this function will convert the entire string to an `{!rsh} UInt`.

---
The following exports are for dealing with network tokens.

@{ref("js", "standardUnit")}@{ref("js", "atomicUnit")}@{ref("js", "minimumBalance")}@{ref("js", "parseCurrency")}@{ref("js", "formatCurrency")}@{ref("js", "formatWithDecimals")}
```js
stdlib.standardUnit // string
stdlib.atomicUnit // string
stdlib.minimumBalance // atomicUnitAmount
stdlib.parseCurrency(standardUnitAmount: int, decimals?: number) => atomicUnitAmount
stdlib.formatCurrency(atomicUnitAmount: int) => string  // display amount in standard unit
stdlib.formatWithDecimals(atomicUnitAmount: int, tokenDecimals: int) => string  // display amount of atomic unit with custom decimal place
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

`{!js} stdlib.bigNumberify` is transparently applied to `{!js} stdlib.formatCurrency`'s and `{!js} stdlib.formatWithDecimals`'s first arguments.

---
@{ref("js", "formatAddress")}@{ref("js", "stdlib.formatAddress")}
```js
stdlib.formatAddress(acc: Account | Address) => string
```

Formats the address in the way the user would expect to see it:

+ On Ethereum, it is a hex-encoded string starting with `{!js} '0x'`.
+ On Algorand, it is a base32-encoded string, ending with the checksum.

Example:

```js
load: /examples/getUntrackedFunds1/index.mjs
md5: d743dd6357719dfd1ba3fcd540ab4e49
range: 32 - 37
```

This code is a `{!rsh} function` that takes in `addr` on line 32.
Line 33 formats `addr` based on the consensus network's encoding.
On line 35, the formatted `addr` (now stored as `address`) is revealed to the user in the console.

There is no corresponding `{!js} parseAddress` function because
the user-friendly form is also accepted from the frontend
in all places that Reach expects an address.

# {#ref-frontends-js-ask} Reading User Input: `ask`

The Reach JavaScript standard library provides the `ask` object for constructing console interfaces to your frontends.

@{ref("js", "ask")}
```js
import { ask } from '@reach-sh/stdlib';
```

You do not need to use this module.
It is simply provided as a convenience for when you are starting out testing and building simple DApps.

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
const isAlice = await ask.ask(
    `Are you Alice?`,
    ask.yesno
  );

// Do something

ask.done();
```

Read the [Interaction and Independence](##tut-8) section the Rock, Paper, Scissors tutorial for a longer use case example of the `{!js} ask` object.

# {#ref-frontends-js-test} Writing tests: `test`

The Reach JavaScript standard library provides the `test` object for constructing automated tests.

@{ref("js", "test")}
```js
import { test } from '@reach-sh/stdlib';
```

You do not need to use this module.
We think it is a great way to write tests, but it doesn't do anything you couldn't do on your own or with any other of the excellent JavaScript testing frameworks.

---
On initialization, `{!js} test` will read the process's command-line arguments and interpret them as a list of tests to run.
If no arguments are given, then all tests will be run.

If you want to inspect this set of tests-to-run, then you can use the functions:

@{ref("js", "test.shouldRun")}
```js
test.shouldRun(x: string): boolean
```

Returns `{!js} true` if `x` is in the set of tests or if none were given.

@{ref("js", "test.shouldRunExac")}
```js
test.shouldRunExac(x: string): boolean
```

Returns `{!js} true` if `x` is in the set of tests.

---
The main way to write tests is to call these functions:

@{ref("js", "test.chk")}
```js
test.chk(id: string, actual:any, expected:any, xtra:object): void
```

Runs a check named `id` that does not fail if `actual` is the same as `expected`.
Records additional information in the log from `xtra`.

Example:

```js
load: /examples/map-tuple-key/index.mjs
md5: cd5c8b278ef9f5ee40c13627cb23889e
range: 34 - 36
```

In this code, the `{!js} chk` asserts that the unsigned integer `i[1]` is the same as `bn(val)`.

@{ref("js", "test.chkErr")}
```js
test.chkErr(id: string, expected:string, f:() => Promise): void
test.chkErr('overflows', 'add overflow', () => stdlib.safeAdd(stdlib.UInt_max, stdlib.UInt_max));
```

Runs a check named `id` that expects `f` to throw an exception that satisfies the regex `expected`.

---
@{ref("js", "test.one")}
```js
test.one(id:string, f:() => Promise): void
```

Schedules a test named `id` which is run by calling `f`.

This uses `{!js} test.shouldRun` to determine if the test should be run.

---
@{ref("js", "test.makeChkExport")}
```js
test.makeChkExport(stdlib:Stdlib, backend:Backend): [
  exports: Exports,
  chkExport: (fn:string, go:(
    chkf: (dom:any[], expected:any): void,
    chkfErr: (expected:string, dom:any[]),
  ): void,
]
```

Accepts a standard library object (`{!js} stdlib`) and a compiled backend (like given to `{!js} acc.contract`) and returns two objects:
  1. `exports` --- The object containing all of the `{!rsh} export`ed functions.
  2. `chkExport` --- A function to run tests on a particular function.

`chkExport` takes two arguments:
  1. `fn` --- The name of a function to test
  2. `go` --- A function that accepts a customized version of `{!js} test.chk` and `{!js} test.chkErr`.

These customized versions are referred to as:
  1. `chkf` --- A function that accepts a domain and an expected range and ensures that the actual function result matches it.
  2. `chkfErr` --- A function that accepts an expected exception value and a domain and ensures that the actual function throws the exception.

This function uses `{!js} test.one` internally.

---
@{ref("js", "test.run")}
```js
test.run(opts): Promise<void>
```

Runs any tests scheduled with `{!js} test.one`.

`opts.howManyAtOnce` may be a number that determines how many tests to run in parallel.
It defaults to `{!js} 1`.

`opts.exitOnFail` may be a boolean that determines whether testing should end whenever the first test fails.
It defaults to `{!js} true`.

When testing is finished, the process will be exited with an error code signifying whether any tests failed.

In addition, three lines will be outputed with the following format:
```
var RESULTS_B64='x'
var SUMMARY='y'
var STATUS='z'
```
where
- `x` is the base64 encoding of test results in the JUnit format as accepted by tools like [CircleCI](https://circleci.com/docs/2.0/collect-test-data/).
- `y` is a single line summary of how many tests failed
- `z` is a six character prefix suitable for using with `y`.

A convenient way to consume this output is with the following shell code:
```cmd
set -o pipefail +e
echo Running...
${REACH} run index --- "$@" | tee log
EXIT=$?
sed -i 's/\r//g' log
sed -n 's/^var //p' log > log.sh
source log.sh
echo "${RESULTS_B64}" | base64 -d - > test_results.xml
cat > message.sh <<END
STATUS="${STATUS}"
SUMMARY="${SUMMARY}"
END
grep -v '^var ' log
echo "${STATUS} ${SUMMARY}"
exit "${EXIT}"
```

We use this setup generate messages from CircleCI to Slack for continuous integration.

`opts.noVarOutput` may be a boolean that determines whether this print out will NOT be generated.
It defaults to `{!js} false`.

