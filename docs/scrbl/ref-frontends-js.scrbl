#lang scribble/manual
@(require "lib.rkt")
@(mint-scope 'js)

@title[#:version reach-vers #:tag "ref-frontends-js" #:style 'toc]{JavaScript}

The Reach JavaScript standard library, @(mint-define! '("stdlib")) @jsin{stdlib}, is provided by either
@itemlist[
 @item{the module @litchar{@"@"reach-sh/stdlib/ETH.mjs};}
 @item{the module @litchar{@"@"reach-sh/stdlib/ALGO.mjs};}
 @item{the @jsin{async} function @jsin{loadStdlib} from @litchar{@"@"reach-sh/stdlib/loader.mjs}.}
]

These modules are available in the @link["https://www.npmjs.com/package/@reach-sh/stdlib"]{@tt{@"@"reach-sh/stdlib}} @link["https://www.npmjs.com/"]{@tt{npm}} package, which you can install via:
@cmd{
npm install @"@"reach-sh/stdlib
}
Although, if you use @exec{reach run}, you don't need to install this package, because @exec{reach} automatically manages your standard library install behind the scenes.
You only need to install the package directly if you are running your frontend without @exec{reach} or using a tool like @link["https://webpack.js.org/"]{webpack} for deployment.

These libraries provide a standard interface that support developing @tech{frontends}.

@(local-table-of-contents)

@section[#:tag "ref-frontends-js-types"]{Types}

The table below shows the JavaScript representation of each of the Reach types:
@(mint-define! '("Contract"))
@js{
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
}

For example, the Reach type @reachin{MInt = Data({None: Null, Some: UInt})} inhabitant @reachin{MInt.Some(42)} is represented as @reachin{['Some', 42]} in JavaScript.

@(hrule)
@(mint-define! '("Connector"))
@js{
  type Connector = 'ETH' | 'ALGO' | 'CFX'
}

A @reachin{Connector} is the abbreviated name of the network
being connected to.


@section[#:tag "ref-frontends-js-loader.mjs"]{@tt{loader.mjs}}

The @tt{loader.mjs} module exports the following functions
that might help you write code that is portable to multiple consensus networks.

@(hrule)
@(mint-define! '("canonicalizeConnectorMode"))
@js{
  canonicalizeConnectorMode(string) => string
}

Expands a connector mode prefix
to its full, canonical name. The canonical connector modes are:

@itemize[

@item{@conmode{ETH-live}}
@item{@conmode{ETH-browser}}
@item{@conmode{ETH-devnet}, for @jsin{'ETH'}.}
@item{@conmode{ALGO-live}}
@item{@conmode{ALGO-browser}}
@item{@conmode{ALGO-devnet}, for @jsin{'ALGO'}.}

]

@(hrule)
@(mint-define! '("getConnectorMode"))
@js{
  getConnectorMode() => string
}

Returns the canonicalized connector mode, based on the
@jsin{process.env.REACH_CONNECTOR_MODE} environment variable.
If the variable is missing or empty, it will return the canonicalized form of @jsin{'ETH'}.

@(hrule)
@(mint-define! '("getConnector"))
@js{
  getConnector() => Connector
}

Returns the first piece of @jsin{getConnectorMode()}.

@(hrule)
@(mint-define! '("loadStdlib"))
@js{
  loadStdlib(env) => Promise<stdlib>
}

@index{loadStdlib} Returns a Promise for a standard library based on the provided @jsin{env} string or map.
In environments where the Reach standard library has implicit access to @jsin{process.env},
you may omit the @jsin{env} argument, in which case @jsin{process.env} will be used.

If the standard library is being used with JavaScript bundlers like Webpack
---as it is with React, for example---
then Reach does @bold{not} have implicit access to @jsin{process.env}.
In such scenarios, we recommend that you call this function like so:

@js{
  const reach = await loadStdlib(process.env);
}

Or construct a custom object that has all of the environment keys and fields you need.

As a special case, you may instead pass in the string @litchar{'ETH'} or the string @litchar{'ALGO'}
to select the desired connector directly.

@(hrule)

By default, this method allows a user to load a standard library for a single connector.
That is, this method may not be called multiple times with varying @tech{connectors}.
To bypass this restriction, use @jsin{unsafeAllowMultipleStdlibs}.

@(hrule)
@(mint-define! '("unsafeAllowMultipleStdlibs"))
@js{
  unsafeAllowMultipleStdlibs() => null
}

@index{unsafeAllowMultipleStdlibs} Calling this function will lift the restriction that
@jsin{loadStdlib} imposes on loading multiple standard libraries.

@section[#:tag "ref-frontends-js-acc"]{Account Handles}

These functions create and interact with @tech{account} representations.

@(hrule)
@(mint-define! '("getDefaultAccount"))
@js{
 getDefaultAccount() => Promise<acc> }

Returns a Promise for a Reach @tech{account} abstraction for a "default" @tech{account} on the @tech{consensus network}.
The meaning of "default account" varies between contexts.
When running in the browser, the default account will be connected to a wallet.
When running in Node while connected to one of Reach's standard devnets,
the default account will be connected to a faucet on the devnet.
This promise will be rejected with an exception if no sensible default account can be accessed for the current context.

@(hrule)
@(mint-define! '("newAccountFromSecret"))
@js{
 newAccountFromSecret(secret: string) => Promise<acc> }

Returns a Promise for a Reach @tech{account} abstraction for an @tech{account} on the @tech{consensus network} specified by the given secret.
The details of the secret encoding are specified uniquely to the @tech{consensus network}.


@(hrule)
@(mint-define! '("newAccountFromMnemonic"))
@js{
 newAccountFromMnemonic(phrase: string) => Promise<acc> }

Returns a Promise for a Reach @tech{account} abstraction for an @tech{account} on the @tech{consensus network} specified by the given mnemonic phrase.
The details of the mnemonic phrase encoding are specified uniquely to the @tech{consensus network}.


@(hrule)
@(mint-define! '("newTestAccount"))
@js{
 newTestAccount(balance) => Promise<acc> }

Returns a Promise for a Reach @tech{account} abstraction for a new @tech{account} on the @tech{consensus network} with a given balance of @tech{network tokens}. This can only be used in private testing scenarios, as it uses a private faucet to issue @tech{network tokens}.

@jsin{bigNumberify} is transparently applied to the @jsin{balance} argument.

@(hrule)
@(mint-define! '("newTestAccounts"))
@js{
 newTestAccounts(howMany, balance) => Promise<Array<acc>> }

Returns a Promise for an array of @jsin{howMany} test accounts, using @jsin{newTestAccount}.

@(hrule)
@(mint-define! '("createAccount"))
@js{
  createAccount() => Promise<acc> }

Returns a Promise for a Reach @tech{account} abstraction for a new @tech{account} on the @tech{consensus network}. The account will have an empty balance of @tech{network tokens}.

@(hrule)
@(mint-define! '("fundFromFaucet"))
@js{
  fundFromFaucet(account, balance) => Promise<void>}

Adds the given balance of @tech{network tokens} to a Reach @tech{account} abstraction.
This can only be used in private testing scenarios,
as it uses a private faucet to issue @tech{network tokens},
as well as certain public TestNet scenarios.
You can use @jsin{canFundFromFaucet} to check if @jsin{fundFromFaucet} can be used.

@jsin{bigNumberify} is transparently applied to the @jsin{balance} argument.

@(hrule)
@(mint-define! '("canFundFromFaucet"))
@js{
  canFundFromFaucet() => Promise<boolean>}

A Promise that resolves to @jsin{true} if @jsin{fundFromFaucet} can be used, @jsin{false} if not.

@(hrule)
@(mint-define! '("connectAccount"))
@js{
 connectAccount(networkAccount) => Promise<acc> }

Returns a Promise for a Reach @tech{account} abstraction for an existing @tech{account} for the @tech{consensus network} based on the @tech{connector}-specific @tech{account} specification provided by the @jsin{networkAccount} argument.

@js{
    // network => networkAccount type
    ETH        => ethers.Wallet
    ALGO       => {addr: string, sk: UInt8Array(64)}}

@(hrule)
@(mint-define! '("networkAccount"))
@js{
 acc.networkAccount => networkAccount }

@index{acc.networkAccount} Returns the @tech{connector}-specific @tech{account} specification of a Reach @tech{account} abstraction.

@(hrule)
@(mint-define! '("getAddress"))
@js{
  acc.getAddress() => string}

@index{acc.getAddress} Returns the @tech{account}'s address as a string. The format of this string varies across @tech{connectors}.

@(hrule)
@(mint-define! '("setDebugLabel"))
@js{
  acc.setDebugLabel(string) => acc }

@index{acc.setDebugLabel} An @tech{account} may set a distinguishing label to use in debug logs. If no label is provided, then the first four digits of the @tech{account} address will be used.

@(hrule)
@(mint-define! '("tokenAccept"))
@js{
  acc.tokenAccept(token) => Promise<void>}

@index{acc.tokenAccept} Returns a Promise that completes when the Reach @tech{account} abstraction is ready to accept @tech{non-network tokens} specified by the @jsin{token}.
This does nothing on some @tech{consensus networks}, but should always be used to ensure your frontend is blockchain agnostic.

@(hrule)
@(mint-define! '("tokenMetadata"))
@js{
  acc.tokenMetadata(token) => Promise<object>}

@index{acc.tokenMetadata} Returns a Promise of the metadata for a @tech{non-network token} specified by the @jsin{token}.

@(hrule)
@(mint-define! '("balanceOf"))
@js{
 balanceOf(acc, token?) => Promise<amount> }

Returns a Promise for the balance of @tech{network tokens} (or @tech{non-network tokens} if @jsin{token} is provided) held by the @tech{account} given by a Reach @tech{account} abstraction provided by the @jsin{acc} argument.

@(hrule)
@(mint-define! '("transfer"))
@js{
 transfer(from:acc, to:acc, amount, token?) => Promise<void> }

Performs a transfer of @jsin{amount} from @jsin{from} to @jsin{to},
which are @tech{account}s, such as those returned by @jsin{connectAccount}.
If @jsin{token} is not provided, then the transfer is of @tech{network tokens};
otherwise, it is of the designated @tech{non-network token}.
The returned @jsin{Promise} will only be resolved after the transfer completes.

@jsin{bigNumberify} is transparently applied to the @jsin{amount} argument.

@subsection[#:tag "ref-frontends-js-acc-eth"]{EVM-specific (Ethereum and Conflux)}

When connected to an EVM-based consensus network, the standard library provides additional functionality.

@(hrule)
@(mint-define! '("setGasLimit"))
@js{
 acc.setGasLimit(n) => void }

@index{acc.setGasLimit} Modifies the gas limit for each transaction originating from the given account for the rest of the program.
@jsin{n} must be a value that @jsin{bigNumberify} will accept.

On EVM-based consensus networks, the Reach standard library will automatically estimate the required gas necessary to execute transactions, i.e. make @tech{publications}.
However, sometimes this estimation process is inaccurate, especially when Reach programs interact with @tech{remote objects}.
In those cases, it is sometimes useful to specify a particular gas limit.
It is common on Ethereum to use gas limits like @jsin{5000000} in testing.
If you do this, you should inform your clients that they should pay attention to the gas stipend issued.

@subsection[#:tag "ref-frontends-js-acc-cfx"]{Conflux-specific}

When connected to the Conflux consensus network, the standard library provides additional functionality.

@(hrule)
@(mint-define! '("setStorageLimit"))
@js{
 acc.setStorageLimit(n) => void }

@index{acc.setStorageLimit} Modifies the storage limit for each transaction originating from the given account for the rest of the program.
@jsin{n} must be a value that @jsin{bigNumberify} will accept.

On the Conflux consensus networks, the Reach standard library will automatically use a storage limit of 2024 to execute transactions, i.e. make @tech{publications}.
Storage fees are refunded once the storage space is no longer used by the contract.
The @jsin{setStorageLimit} function allows you to choose a different storage limit, as you see fit.

@section[#:tag "ref-frontends-js-ctc"]{Contract Handles}

In order to interact with a deployed contract, you must construct a @tech{contract} handle from an account.

@(hrule)
@(mint-define! '("contract"))
@js{
 acc.contract(bin, ?info) => ctc }

@index{acc.contract} Returns a Reach @tech{contract} handle based on the @jsin{bin} argument provided with access to the account @jsin{acc}.
This @jsin{bin} argument is the @filepath{index.main.mjs} module produced by the JavaScript @tech{backend}.

If @jsin{info} is provided, it must be a @reachin{Contract} value, or a @jsin{Promise} that eventually yields a @reachin{Contract} value.
Typically, the deployer of a contract with not provide @jsin{info}, while users of a contract will.
In an automated, single instance program, @reachin{ctc.getInfo()} is typically used to acquire @jsin{info};
while in non-automated programs, an application uses out-of-band communication, such as an external database or user input, to acquire the @jsin{info} argument.

The first publishing participant will attempt to deploy a contract for an application.
If @jsin{info} was provided, an error will be thrown.
This deployment can only happen one time, so subsequent attempts will fail with an error.

This function does not block.

@(hrule)
@(mint-define! '("deploy"))
@js{acc.deploy(bin) => ctc}

@index{acc.deploy}
This deprecated function is an abbreviation of @jsin{acc.contract(bin)}.

@(mint-define! '("attach"))
@js{acc.attach(bin, info) => ctc }

@index{acc.attach}
This deprecated function is an abbreviation of @jsin{acc.contract(bin, info)}.

@(hrule)
@(mint-define! '("getInfo"))
@js{
 ctc.getInfo() => Promise<ctcInfo> }

@index{ctc.getInfo} Returns a Promise for a @reachin{Contract} value that may be given to @jsin{contract} to construct a Reach @tech{contract} handle for this contract.
This object may be stringified with @jsin{JSON.stringify} for printing and parsed again with @jsin{JSON.parse} without any loss of information.

If @jsin{ctc} will deploy the program, then the Promise will only be resolved after the contract is actually deployed on the network,
thus you cannot block on this Promise with @jsin{await} until after the first @reachin{publish} has occurred.
Awaiting @reachin{getInfo} too early may cause your program to enter a state of deadlock.
It is safer to make an @reachin{interact} function that receives @reachin{getContract()} from the Reach program.

@(hrule)
@(mint-define! '("getContractAddress"))
@js{
 ctc.getContractAddress() => Promise<Address> }

@index{ctc.getContractAddress} Returns a Promise for the @jsin{Address} of the connected Reach @tech{contract}.

@(hrule)

@subsection{@tt{ctc.participants}, @tt{ctc.p}}

Contract handles provide access to the interface of the compiled backend, @jsin{bin}, that they were constructed with.

@js{
 ctc.participants // {[name: string]: (interact:Object) => Promise}
 ctc.p

 ctc.p.Alice(interact)
}

@index{ctc.participants}
@index{ctc.p}
An object where the keys are the participant names and the values are functions that accept an interact object and return a Promise that completes when the participant ends.

@jsin{acc.contract(backend).p.Alice(io)} is equivalent to @jsin{backend.Alice(acc.contract(backend), io)}, but does not require duplication of the @jsin{backend} component.

@subsection{@tt{ctc.apis}, @tt{ctc.a}}

@js{
 ctc.apis // {[name: string]: {[fun:string]: (...args) => Promise<res>}}
 ctc.apis // {[name: string]: (...args) => Promise<result>}
 ctc.a

 ctc.a.Voter.cast("Pedro")
}

@index{ctc.apis}
@index{ctc.a}
An object that mirrors the @tech{API} hierarchy, so if @litchar{X.Y} is an @tech{API}, then @jsin{ctc.apis.X.Y} is an @deftech{API function}.
An @tech{API function} accepts the arguments of the @tech{API} and returns a @jsin{Promise} that results in the value of the @tech{API}.
This function may throw an error if the @tech{API} is not available.

If an @tech{API} was specified without an @reachin{apiName}, for example @reachin{API({ cast: Fun([String], Null)})}, it may be accessed by its property name:

@js{
  ctc.a.cast("Pedro");
}

@subsection{@tt{ctc.safeApis}}

@js{
  ctc.safeApis
  ctc.safeApis.Voter.cast("Pedro")
}

@index{ctc.safeApis}
This object is the same as @jsin{ctc.apis} except the API functions return a @reachin{Maybe} value.
If the call fails, then @jsin{['None', null]} will be returned. If the call succeeds, the return value will
be wrapped with @jsin{Some}, e.g. @jsin{['Some', 4]}.

@subsection{@tt{ctc.views}, @tt{ctc.v}}

@margin-note{@tech{Views} are @seclink["ref-programs-appinit-view"]{defined in application initialization} and then they are @seclink["ref-programs-consensus-view"]{set in consensus steps}. Both of these steps are in Reach. This section is about accessing them in JavaScript frontends.}

@js{
 ctc.views // {[name: string]: {[fun:string]: (...args) => Promise<res>}}
 ctc.views // {[name: string]: (...args) => Promise<res>}
 ctc.v

 ctc.v.NFT.owner()
}

@index{ctc.views}
@index{ctc.v}
An object that mirrors the @tech{view} hierarchy, so if @litchar{X.Y} is a @tech{view}, then @jsin{ctc.views.X.Y} is a @deftech{view function}.
A @tech{view function} accepts the arguments of the @tech{view} and returns a @jsin{Promise} that results in the value of the @tech{view} wrapped in a @reachin{Maybe} type (because the @tech{view} may not be bound).
For example, if @litchar{NFT.owner} is a @tech{view} with no arguments that represents the @reachin{Address} that owns an NFT, then @jsin{await ctc.v.NFT.owner()} is either @jsin{['Some', Owner]} or @jsin{['None', null]}.

If a @tech{View} was specified without a @reachin{viewName}, for example @reachin{View({ owner: Address })}, it may be accessed by its property name:

@js{
  ctc.v.owner();
}

@(hrule)

@(mint-define! '("getViews"))
@js{
 ctc.getViews() => Object }

@index{ctc.getViews}
This deprecated function is an abbreviation of @jsin{ctc.views}.

@subsection{@tt{ctc.unsafeViews}}

@jsin{
  ctc.unsafeViews
  ctc.unsafeViews.NFT.owner()
}

@index{ctc.unsafeViews}

This object is the same as @jsin{ctc.views} except the value of the @tech{view} is not wrapped in a @reachin{Maybe} type.
If a @tech{view} is set, the value will be returned as is, without being wrapped in @reachin{Some}.
If a @tech{view} is not set, an error will be thrown.

@section[#:tag "ref-frontends-js-network"]{Network Utilities}

These functions interact with the @tech{consensus network} itself.

@(hrule)
@(mint-define! '("getNetworkTime"))
@js{
 getNetworkTime() => Promise<time>
}

Returns a Promise for the current consensus @tech{network time},
represented as a @litchar{BigNumber}.

@(hrule)
@(mint-define! '("waitUntilTime"))
@js{
 waitUntilTime(time, onProgress?) => Promise<time>
}

Returns a Promise that will only be resolved after the specified consensus @tech{network time}.
In @tech{isolated testing modes}, this will also force time to pass on the network, usually by sending trivial transactions.
An @deftech{isolated testing mode} is a @envref{REACH_CONNECTOR_MODE} that matches
@litchar{$NET-devnet} for all valid @litchar{$NET}, or when @defenv{REACH_ISOLATED_NETWORK} is set.

You may provide an optional @jsin{onProgress} callback, used for reporting progress,
which may be called many times up until the specified @tech{network time}.
It will receive an object with keys @jsin{current} and @jsin{target},

@(hrule)
@(mint-define! '("getNetworkSecs"))
@js{
 getNetworkSecs() => Promise<secs>
}

Like @jsin{getNetworkTime}, but returns a @tech{network seconds} Promise.

@(hrule)
@(mint-define! '("waitUntilSecs"))
@js{
 waitUntilSecs(secs, onProgress?) => Promise<secs>
}

Like @jsin{waitUntilSecs}, but waits for a certain @tech{network seconds} deadline.

@(hrule)
@(mint-define! '("wait"))
@js{
 wait(timedelta, onProgress?) => Promise<networkTime>
}

Returns a Promise that will only be resolved after the specified @tech{time delta} has elapsed.
The expression @jsin{await wait(delta, onProgress)} is the same as
@jsin{await waitUntilTime(add(await getNetworkTime(), delta), onProgress)}.
As with @jsin{waitUntilTime}, the @jsin{onProgress} callback is optional.

@(hrule)
@(mint-define! '("connector"))
@js{
 connector : Connector }

Represents the @jsin{Connector} the @jsin{stdlib} uses.

@(hrule)

@(mint-define! '("setQueryLowerBound"))
@js{
  setQueryLowerBound(networkTime) => void
}

Sets the lower bound on querying the network for events.
The argument to this function is a @tech{network time}.
By default, Reach will query for events from time 0.
This default is usually fine, but on certain networks like Conflux,
it can be very slow.
You can use this function to tell Reach to only query from a given network time onwards,
which can speed up event log querying significantly on Conflux.
If you use the reach stdlib to @jsin{deploy} or @jsin{attach} to a contract,
the specified lower bound must be no later than the time at which the contract was deployed.

@(hrule)

@(mint-define! '("setValidQueryWindow"))

@js{
  setValidQueryWindow(width: number|true) => void
}

Sets the maximum width of the query windows used to query the network for event logs.
The value @jsin{true} indicates that no window size should be used, and queries may span arbitrarily large window sizes.
While each connector has a default value that works for most common cases, tweaking this setting may be useful when dealing with layer two networks or custom endpoints that are more restrictive than normal nodes on the network.

@section[#:tag "ref-frontends-js-provider"]{Provider Selection}

These functions allow you to choose which particular @tech{consensus network} API provider to connect to.

@(hrule)
@(mint-define! '("setProviderByName"))
@js{
  setProviderByName(string) => void }

Supported provider names are: @jsin{'MainNet'}, @jsin{'TestNet'}, and @jsin{'LocalHost'}.

On Ethereum, @jsin{'MainNet'} will connect to homestead, and @jsin{'TestNet'} to ropsten.
Multiple free API providers are used behind the scenes, @link["https://docs.ethers.io/v5/api/providers/#providers-getDefaultProvider"]{as implemented by ethers.js}.

On Algorand, @jsin{'MainNet'} will connect to MainNet, and @jsin{'TestNet'} to TestNet.
The free RandLabs API provider is used (@link["https://algoexplorerapi.io"]{https://algoexplorerapi.io}).

@(hrule)
@(mint-define! '("providerEnvByName"))
@js{
  providerEnvByName(string) => env }

Retrieve configuration information about providers by name.

@(hrule)
@(mint-define! '("setProviderByEnv"))
@js{
  setProviderByEnv(env) => void }

Select an API provider by supplying information about it.

This function's API is considered unstable.

@jsin{env} is a record with string keys and string values.

On Ethereum, @jsin{env} may include keys:
@jsin{'ETH_NODE_URI'}

On Algorand, @jsin{env} may include keys:
@jsin{'ALGO_SERVER'}, @jsin{'ALGO_PORT'}, @jsin{'ALGO_TOKEN'}, @jsin{'ALGO_INDEXER_SERVER'}, @jsin{'ALGO_INDEXER_PORT'}, @jsin{'ALGO_INDEXER_TOKEN'}.

@(hrule)
@(mint-define! '("setProvider"))
@js{
  setProvider(provider): void }

Select an API provider by providing an object satisfying its interface.

This function's API is considered unstable.

On Ethereum, @jsin{provider} is an instance of @jsin{ethers.provider}.
See: @link["https://docs.ethers.io/v5/api/providers/provider/"]{https://docs.ethers.io/v5/api/providers/provider/}

On Algorand, @jsin{provider} is an object:
@js{
interface Provider {
  algodClient: algosdk.Algodv2,
  indexer: algosdk.Indexer,
  getDefaultAddress: () => Promise<Address>,
  isIsolatedNetwork: boolean,
  signAndPostTxns: (txns:WalletTransaction[], opts?: any) => Promise<any>,
};
}

The @jsin{algodClient} and @jsin{indexer} values are as specified by the @link["https://algorand.github.io/js-algorand-sdk/"]{Algorand JS SDK}.
The @jsin{signAndPostTxns} function obeys @link["https://github.com/reach-sh/ARCs/blob/reach-wallet/ARCs/arc-0008.md"]{ARC-0008}.

@(hrule)
@(mint-define! '("setWalletFallback"))
@js{
  setWalletFallback(make: () => wallet): void
}

When you call this function, if no browser wallet is available, then @jsin{make} will be called to construct one.
The value that @jsin{make} should return differs between connectors.

On Ethereum, it must match the interface of MetaMask.
On Conflux, it must match the interface of ConfluxPortal.
On Algorand, it must match the @link["https://github.com/reach-sh/ARCs/blob/reach-wallet/ARCs/arc-0011.md"]{ARC-0011} standard.

@(hrule)
@(mint-define! '("walletFallback"))
@js{
  walletFallback(opts: object): () => wallet
}

This function returns a value that may be passed to @jsin{setWalletFallback} to synthesize a wallet for use in browsers that do not supply a compliant wallet.
Its customization options, @jsin{opts}, depend on the connector.

On Ethereum and Conflux, it always errors and cannot provide a wallet.

On Algorand, it can provide a wallet that directly connects to the Algorand network, like @jsin{setProviderByName} (& @jsin{setProviderByEnv}), but provide interactive signing.
The network connection is specified via the @litchar{providerEnv} key, which may be a string (which is used as an argument to @jsin{providerEnvByName}) or an environment (which is used as an argument to @jsin{setProviderByEnv}).
By default, signing is via an interactive browser window prompt, where the user repeatedly provides their mnemonic.
If the key @litchar{MyAlgoConnect} is provided, and bound to the export of @litchar{@"@"reach-sh/stdlib/ALGO_MyAlgoConnect}, then @link["https://wallet.myalgo.com/home"]{My Algo} will be used for signing.
For example, this sets the wallet fallback to be My Algo used with Algorand TestNet:
@js{
import MyAlgoConnect from '@"@"reach-sh/stdlib/ALGO_MyAlgoConnect';
stdlib.setWalletFallback(stdlib.walletFallback({
  providerEnv: 'TestNet', MyAlgoConnect }));
}

@section[#:tag "ref-frontends-js-utils"]{Utilities}

These functions operate on JavaScript representations of Reach values.

@(hrule)
@(mint-define! '("protect"))
@js{
 protect(t, x) => x }

Asserts that value @jsin{x} has Reach @tech{type} @jsin{t}. An exception is thrown if this is not the case.

@(hrule)
@(mint-define! '("T_Null") '("T_Bool") '("T_UInt") '("T_Bytes") '("T_Address") '("T_Array") '("T_Tuple") '("T_Object"))
@js{
 T_Null => ReachType
 T_Bool => ReachType
 T_UInt => ReachType
 T_Bytes(number) => ReachType
 T_Digest => ReachType
 T_Address => ReachType
 T_Array(ReachType, number) => ReachType
 T_Tuple([ReachType ...]) => ReachType
 T_Object({Key: ReachType ...}) => ReachType
 T_Data({Variant: ReachType ...}) => ReachType}

Each of these represent the corresponding Reach @tech{type}.

@(hrule)
@(mint-define! '("assert"))
@js{
 assert(p) }

Throws an exception if not given @jsin{true}.

@(hrule)
@(mint-define! '("Array_set"))
@js{
 Array_set(arr, idx, val) }

Returns a new array identical to @jsin{arr}, except that index @jsin{idx} is @jsin{val}.

@(hrule)
@(mint-define! '("bigNumberify") '("isBigNumber"))
@js{
 bigNumberify(x) => UInt
 isBigNumber(x) => bool
 bigNumberToNumber(x) => number}

@deftech{bigNumberify} converts a JavaScript number to a BigNumber,
the JavaScript representation of Reach's @reachin{UInt}.

@deftech{isBigNumber} checks if its input is a BigNumber.

@deftech{bigNumberToNumber} transparently applies @jsin{bigNumberify} to its
argument and returns a JavaScript number.

@(hrule)
@(mint-define! '("isHex") '("hexToBigNumber") '("stringToHex") '("bigNumberToHex") '("uintToBytes") '("bytesEq") '("digestEq") '("addressEq"))
@js{
 isHex(x) => bool
 hexToBigNumber(bytes) => UInt
 stringToHex(string) => bytes
 bigNumberToHex(UInt) => bytes
 uintToBytes(UInt) => bytes
 bytesEq(bytes, bytes) => bool
 digestEq(Digest, Digest) => bool
 addressEq(Address, Address) => bool}

These are additional conversion and comparison utilities.

@(hrule)
@(mint-define! '("digest"))
@js{
 digest(ty:Type, x:ty) => Digest}

Hashes the value.

@(hrule)
@(mint-define! '("randomUInt"))
@js{
 randomUInt() => UInt}

Generates random bits as a @reachin{UInt}.
The number of bits generated depends on the particular @tech{consensus network}.

@(hrule)
@js{
 hasRandom}

@deftech{hasRandom (Frontend)} A value suitable for use as a @tech{participant interact interface} requiring a @litchar{random} function, such as @reachin{hasRandom}.
Reach does not natively support randomness and leaves random number generation to the frontend implementation.
This value is provided out of convenience; it is not mandatory to use this implementation.

@(hrule)
@js{
 hasConsoleLogger}

@deftech{hasConsoleLogger (Frontend)} A value suitable for use as a @tech{participant interact interface} requiring a @litchar{log} function, such as @reachin{hasConsoleLogger}.
The @jsin{log} function provided takes an arbitrary amount of elements and prints them to stdout.
This value is provided out of convenience; it is not mandatory to use this implementation.

@(hrule)
@(mint-define! '("parseFixedPoint"))
@js{
 parseFixedPoint(FixedPoint) => number}

Parses a @reachin{FixedPoint} number into a JavaScript number.

@(hrule)
@(mint-define! '("numberToFixedPoint"))
@js{
 numberToFixedPoint(number) => FixedPoint}

Parses a JavaScript number into a @reachin{FixedPoint}.

@(hrule)
@(mint-define! '("parseInt"))
@js{
 parseInt(Int) => number}

Parses a signed @reachin{Int} into a JavaScript number.

@(hrule)
@(mint-define! '("numberToInt"))
@js{
 numberToInt(number) => Int}

Parses a JavaScript number into an @reachin{Int}.

@(hrule)
@(mint-define! '("add") '("sub") '("mod") '("mul") '("div"))
@js{
 add(UInt, UInt) => UInt
 sub(UInt, UInt) => UInt
 mod(UInt, UInt) => UInt
 mul(UInt, UInt) => UInt
 div(UInt, UInt) => UInt }

Integer arithmetic on @reachin{UInt}.

@(hrule)
@(mint-define! '("eq") '("ge") '("gt") '("le") '("lt"))
@js{
 eq(UInt, UInt) => bool
 ge(UInt, UInt) => bool
 gt(UInt, UInt) => bool
 le(UInt, UInt) => bool
 lt(UInt, UInt) => bool}

Integer comparisons on @reachin{UInt}.

@(hrule)
The following exports are for dealing with network tokens.

@(mint-define! '("standardUnit") '("atomicUnit") '("minimumBalance") '("parseCurrency") '("formatCurrency"))
@js{
 standardUnit // string
 atomicUnit // string
 minimumBalance // atomicUnitAmount
 parseCurrency(standardUnitAmount) => atomicUnitAmount
 formatCurrency(atomicUnitAmount, int) => string  // display amount in standard unit
}

These functions handle amounts in a network's @tech{standard unit} and its @tech{atomic unit}.
A @deftech{standard unit} is the @tech{network token} unit most commonly associated with a network.
For example, the @tech{standard unit} of Ethereum is ETH.
An @deftech{atomic unit} is the smallest unit of measure for the @tech{standard unit}.
For example, the atomic unit of Ethereum is WEI.
An @tech{atomic unit} is @deftech{atomic}, which means it cannot be divided into smaller units.

Some @tech{consensus networks}, typically those with proof-of-stake, have minimum balances on their accounts, so this is exposed as @jsin{minimumBalance}.

Because there are 1,000,000,000,000,000,000 WEI in 1 ETH,
BigNumber is used to represet values in WEI.

Quantities of a @tech{network token} should always be passed into Reach
in the token's @tech{atomic unit}.

@jsin{bigNumberify} is transparently applied to @jsin{formatCurrency}'s first argument.

@(hrule)
@(mint-define! '("formatAddress"))
@jsin{
  formatAddress(acc) => string }

Formats the address in the way the user would expect to see it.

@itemlist[
 @item{On Ethereum, it is a hex-encoded string starting with @jsin{'0x'}.}
 @item{On Algorand, it is a base32-encoded string, ending with the checksum.}
]

There is no corresponding @jsin{parseAddress} function because
the user-friendly form is also accepted from the frontend
in all places that Reach expects an address.

@section[#:tag "ref-frontends-js-ask.mjs"]{@tt{ask.mjs}}

The Reach JavaScript standard library also provides the helper module @litchar{@"@"reach-sh/stdlib/ask.mjs} for constructing console interfaces to your @tech{frontends}.

@(mint-define! '("ask"))
@js{
  import * as ask from '@"@"reach-sh/stdlib/ask.mjs';
}

It provides the following exports:

@(mint-define! '("ask") '("yesno") '("done"))
@js{
 ask(string, (string => result)) => Promise<result>
 yesno(string) => boolean
 done() => null
}

@jsin{ask} is an asynchronous function that asks a question on the console and returns a Promise for the first result that its second argument does not error on.

@jsin{yesno} is an argument appropriate to give as the second argument to @jsin{ask} that parses "Yes"/"No" answers.

@jsin{done} indicates that no more questions will be asked.
