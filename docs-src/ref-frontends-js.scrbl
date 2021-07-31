#lang scribble/manual
@(require "lib.rkt")

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
@js{
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
}

For example, the Reach type @reachin{MInt = Data({None: Null, Some: UInt})} inhabitant @reachin{MInt.Some(42)} is represented as @reachin{['Some', 42]} in JavaScript.

@(hrule)
@(mint-define! '("Connector"))
@js{
  type Connector = 'ETH' | 'ALGO'
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

@index{loadStdlib} Returns a Promise for a stlib based on the provided @jsin{env} string or map.
In environments where the reach stdlib has implicit access to @jsin{process.env},
you may omit the @jsin{env} argument, in which case @jsin{process.env} will be used.

If the reach stdlib is being used with JavaScript bundlers like webpack
-- as it is with React, for example --
then the reach stdlib does not have implicit access to @jsin{process.env}.
In such scenarios, we recommend that you call this function like so:

@js{
  const reach = await loadStdlib(process.env);
}

You may instead pass in the string @litchar{'ETH'} or the string @litchar{'ALGO'}
to select the desired stdlib directly.

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

@section[#:tag "ref-frontends-js-acc"]{Accounts}

These functions create and interact with @tech{account} representations.

@(hrule)
@(mint-define! '("getDefaultAccount"))
@js{
 getDefaultAccount() => Promise<acc> }

Returns a Promise for a Reach @tech{account} abstraction for a "default" @tech{account} on the @tech{consensus network}.
The meaning of "default account" varies between contexts.
When running in the browser,
the default account will be connected to a wallet such as MetaMask or AlgoSigner.
When running in node.js while connected to one of reach's standard devnets,
the default account will be connected to a faucet on the devnet.
This promise will be rejected with an exception
if no sensible default account can be accessed for the current context.

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

Adds the given balance of @tech{network tokens} to a Reach @tech{account} abstraction. This can only be used in private testing scenarios, as it uses a private faucet to issue @tech{network tokens}.

@jsin{bigNumberify} is transparently applied to the @jsin{balance} argument.

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

@index{acc.tokenMetadata} Returns a Promise of the metadata for a @tech{non-network tokens} specified by the @jsin{token}.

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

@subsection[#:tag "ref-frontends-js-acc-eth"]{Ethereum-specific}

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

@section[#:tag "ref-frontends-js-ctc"]{Contracts}

These functions create and interact with @tech{contract} representations.

@(hrule)
@(mint-define! '("deploy"))
@js{
 acc.deploy(bin) => ctc }

@index{acc.deploy} Returns a Reach @tech{contract} abstraction after starting the deployment of a Reach @DApp @tech{contract} based on the @jsin{bin} argument provided.
This @jsin{bin} argument is the @filepath{input.mjs} module produced by the JavaScript @tech{backend}.
This function does not block on the completion of deployment.
To wait for deployment, see @reachin{ctc.getInfo}.

@(hrule)
@(mint-define! '("getInfo"))
@js{
 ctc.getInfo() => Promise<ctcInfo> }

@index{ctc.getInfo} Returns a Promise for an object that may be given to @jsin{attach} to construct a Reach @tech{contract} abstraction representing this contract.
This object may be stringified with @jsin{JSON.stringify} for printing and parsed again with @jsin{JSON.parse} without any loss of information.
The Promise will only be resolved after the contract is actually deployed on the network.
If you are using @reachin{{deployMode: 'firstMsg'}},
avoid blocking on this Promise with @jsin{await} until after the first @reachin{publish} has occurred.
Awaiting @reachin{getInfo} too early may cause your program to enter a state of deadlock.

@(hrule)

@(mint-define! '("attach"))
@js{
 acc.attach(bin, ctcInfoP) => ctc }

@index{acc.attach} Returns a Reach @tech{contract} abstraction based on a deployed Reach @DApp @tech{contract} provided in the @jsin{ctcInfo} argument (or a Promise for ctcInfo) and the @jsin{bin} argument.
This @jsin{bin} argument is the @filepath{input.mjs} module produced by the JavaScript @tech{backend}.

@(hrule)

@subsection[#:tag "ref-frontends-js-view"]{View Access}
@(note-view-xref)

@(mint-define! '("getViews"))
@js{
 ctc.getViews() => Object }

@index{ctc.getViews} Returns an object representing the @tech{views} of the @tech{contract}.
This object mirrors the @tech{view} hierarchy, so if @litchar{X.Y} is a @tech{view}, then @jsin{ctc.getViews().X.Y} is a @deftech{view function}.
A @tech{view function} accepts the arguments of the @tech{view} and returns a @jsin{Promise} that results in the value of the @tech{view} wrapped in a @reachin{Maybe} type (because the @tech{view} may not be bound.)
For example, if @litchar{NFT.owner} is a @tech{view} with no arguments that represents the @reachin{Address} that owns an NFT, then @jsin{await ctc.getViews().NFT.owner()} is either @jsin{['Some', Owner]} or @jsin{['None', null]}.

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
 wait(timedelta, onProgress?) => Promise<time>
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

@section[#:tag "ref-frontends-js-provider"]{Provider Selection}

These functions allow you to choose which particular @tech{consensus network} API provider to connect to.

@margin-note{
On Ethereum, if you would like to use the wallet from the user's browser,
the @jsin{setProvider} functions are not needed.
The Reach standard library uses @jsin{window.ethereum} by default.

On Algorand, if you would like to use AlgoSigner,
the @jsin{setProvider} functions are necessary for all but @jsin{'LocalHost'}.
However, this will eventually become unnecessary.
Reach is working with Algorand wallets to develop standards
that will put provider selection in control of the end user,
instead of the DApp developer.
}

@(hrule)
@(mint-define! '("setProviderByName"))
@js{
  setProviderByName(string) => void }

Supported provider names are: @jsin{'MainNet'}, @jsin{'TestNet'}, and @jsin{'LocalHost'}.

On Ethereum, @jsin{'MainNet'} will connect to homestead, and @jsin{'TestNet'} to ropsten.
Multiple free API providers are used behind the scenes, @link["https://docs.ethers.io/v5/api/providers/#providers-getDefaultProvider"]{as implemented by ethers.js}.

On Algorand, @jsin{'MainNet'} will connect to MainNet, and @jsin{'TestNet'} to TestNet.
The free RandLabs API provider is used ( @link["https://algoexplorerapi.io"]{https://algoexplorerapi.io} ).

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

On Ethereum, env may include keys:
@jsin{'ETH_NODE_URI'}

On Algorand, env may include keys:
@jsin{'ALGO_LEDGER'}, @jsin{'ALGO_SERVER'}, @jsin{'ALGO_PORT'}, @jsin{'ALGO_TOKEN'}, @jsin{'ALGO_INDEXER_SERVER'}, @jsin{'ALGO_INDEXER_PORT'}, @jsin{'ALGO_INDEXER_TOKEN'}.

@jsin{'ALGO_LEDGER'} is used by AlgoSigner. Configuration for the indicated ledger should match the configuration present in AlgoSigner.

@(hrule)
@(mint-define! '("setProvider"))
@js{
  setProvider(provider): void }

Select an API provider by providing an object satisfying its interface.

This function's API is considered unstable.

On Ethereum, provider is an instance of ethers.provider.
See: @link["https://docs.ethers.io/v5/api/providers/provider/"]{https://docs.ethers.io/v5/api/providers/provider/}

On Algorand, provider is an object:
@js{
{
  ledger: string,
  algodClient: algosdk.Algodv2,
  indexer: algosdk.Indexer,
}
}

See: @link["https://algorand.github.io/js-algorand-sdk/"]{https://algorand.github.io/js-algorand-sdk/}

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
