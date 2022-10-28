# {#ref-programs-appinit} Application Initialization

The body of `{!rsh} Reach.app` is an @{defn("application initialization")}.
It defines the various participants and views of the DApp, as well as sets compilation options.
It is finalized with `{!rsh} init()` and then the application begins in a step.

## {#ref-programs-appinit-stmts} Statements

Any statements valid for a [computation](##ref-programs-compute-stmts) are valid for application initialization.
However, some additional statements are allowed.

### `init`

```reach
init();
```

@{ref("rsh", "init")}
A @{defn("init statement")}, written `{!rsh} init();`, finalizes all of the available participants, views, and compilation options.

@{ref("rsh", "deploy")}
In earlier versions of Reach, this was called `{!rsh} deploy()`, but it was
changed because that name was misleading.

Its continuation is a step, which means its content is specified by @{seclink("ref-programs-step")}.
It represents the body of the DApp to be compiled.

In the example below, see how `{!rsh} init();` is used to finalize the available `{!rsh} Participant` and `{!rsh} API`.
After which a local step is introduced:

```reach
load: /examples/api-twice/index.rsh
md5: 38ac4eabad2ced3f017a11fc24a72f6f
range: 4 - 15
```

### `setOptions`

@{ref("rsh", "setOptions")}
```reach
setOptions({ verifyArithmetic: true });
setOptions({});
```

The @{defn("compilation options")} for the DApp may be set by calling `{!rsh} setOptions(OBJ_EXPR);` where `{!rsh} OBJ_EXPR` is an object with the following keys and values:

+ @{ref("rsh", "untrustworthyMaps")} `{!rsh} untrustworthyMaps`

  `{!rsh} true` or `{!rsh} false` (default)

  Determines whether mappings are treated as trustworthy.
  A mapping is trustworthy if its values are guaranteed to be preserved across interactions.
  When this is `{!rsh} true`, the verifier will enforce that your program does not rely on values being preserved.

  See example below:
  ```reach
  load: /examples/map-simpl/index.rsh
  md5: 2d6c1487d7b96d41016e067c9cd265ef
  range: 4 - 6
  ```

  Reach cannot provide trustworthy mappings with some connectors; therefore it is dangerous to not set this to `{!rsh} true` on such connectors.
  Reach will emit a warning during compilation if you do such a dangerous thing.

+ @{ref("rsh", "verifyArithmetic")} `{!rsh} verifyArithmetic`

  `{!rsh} true` or `{!rsh} false` (default)

  Determines whether arithmetic operations automatically introduce static assertions that they do not overflow beyond `{!rsh} UInt.max`.

  See example below:
  ```reach
  load: /examples/uint256/index.rsh
  md5: 7d97bedae1816b162d61bfa307dad4ac
  range: 3 - 5
  ```

  This defaults to `{!rsh} false`, because it is onerous to verify.
  We recommend turning it on before final deployment, but leaving it off during development.
  When it is `{!rsh} false`, connectors will ensure that overflows do not actually occur on the network.

+ @{ref("rsh", "verifyPerConnector")} `{!rsh} verifyPerConnector`

  `{!rsh} true` or `{!rsh} false` (default)

  Determines whether verification is done per connector, or once for a generic connector.
  When this is `{!rsh} true`, then connector-specific constants, like `{!rsh} UInt.max`, will be instantiated to literal numbers.
  This concretization of these constants can induce performance degradation in the verifier.

  See example below:
  ```reach
  load: hs/t/y/overflow_con.rsh
  md5: d3f64764da8d0c2ae1e68f23ca943c0a
  range: 3 - 6
  ```

+ @{ref("rsh", "connectors")} `{!rsh} connectors`

  @{ref("rsh", "ETH")}@{ref("rsh", "ALGO")} `{!rsh} [ETH, ALGO]` (default)

  A tuple of the connectors that the application should be compiled for.
  By default, all available connectors are chosen.

  In the example below, only `ETH` and `ALGO` are chosen:
  ```reach
  load: /examples/popularity-contest/index.rsh
  md5: ea12cdcd4f262bc0b0a0e0cc74f01e63
  range: 10 - 15
  ```

+ @{ref("rsh", "autoTrackPublishedTokens")} `{!rsh} autoTrackPublishedTokens`

  `{!rsh} true` (default) or `{!rsh} false`

  Enable or disable automatic tracking of the contract's balance of non-network tokens introduced with `{!rsh} publish`.
  Tracking tokens takes some amount of extra space, and on some networks costs money.
  The Reach compiler tries to track only tokens which the contract may receive, but it sometimes fails and unnecessarily tracks a token.
  If you need to publish a token, but your contract will never receive any of that token, disabling this will remove the chance of unnecessarily tracking the token.
  If this is set to `{!rsh} false`, but you still need to track a published token, you can use `{!rsh} Token.track`.

  Example of manually tracking a published token when `{!rsh} autoTrackPublishedTokens` is turned off:
  ```reach
  load: hs/t/y/token_track_publish_2.rsh
  md5: bbc008ebb7dc3c5b757ce20eb95bbbe9
  range: 4 - 11
  ```

## {#ref-programs-appinit-exprs} Expressions

Any expressions valid for a [computation](##ref-programs-appinit-exprs) are valid for application initialization.
However, some additional expressions are allowed.

### Participant Definition

A participant and participant class may be declared with

@{ref("rsh", "Participant")}
```reach
Participant(participantName, participantInteractInterface)
```

and

@{ref("rsh", "ParticipantClass")}
```reach
ParticipantClass(participantName, participantInteractInterface)
```

respectively.

:::note
Since `{!rsh} ParticipantClass` is being deprecated, it is preferable to use `{!rsh} API`.
:::

`{!rsh} participantName` is a string which indicates the name of the participant function in the generated backend code.
Each `{!rsh} participantName` must be unique.

`{!rsh} participantInteractInterface` is a @{defn("participant interact interface")}, an object where each field indicates the type of a function or value which must be provided to the backend by the frontend for interacting with the participant.

In the [Rock, Paper, and Scissors](##tut-3) tutorial, Alice and Bob receive the `getHand` and `seeOutcome` interact interfaces from the construct `Player` in the following sample code:

```
load: /examples/rps-2-rps/index.rsh
md5: 3ea7718e88c86dd41e97b503d7aa3b67
range: 1-14
```

### {#ref-programs-appinit-api} API Definition

@{ref("rsh", "API")}
```reach
API('Voter', { vote: Fun([Address], UInt) })
// or
API({ vote: Fun([Address], UInt) })
// or
API('Voter', { vote: Fun([Address], UInt)}, { vote: "castVote" })
// or
API({ add2: Fun([UInt, UInt], UInt), add1: Fun([UInt], UInt) }, { add1: "add", add2: "add" })
```

APIs are functions that can be called by other contracts, as well as off-chain.

An API is defined with `{!rsh} API(apiName, apiInterface, ?apiAlias)` or `{!rsh} API(apiInterface, ?apiAlias)`, where `{!rsh} apiName` is a string that labels the API, `{!rsh} apiInterface` is an object where each field indicates the type of a function provided by the contract as an API, and `{!rsh} apiAlias` is an optional object that maps function names from the `{!rsh} apiInterface` to an alias.

The @{defn("function alias")} allows overloaded methods to be created.
Many functions may map to the same alias as long as each function domain is unique.

These APIs are available in frontends via the `{!js} ctc.apis` object.

These APIs are available on-chain by using the appropriate network's ABI and are named `apiName_f` or `f` is there is no `apiName`, where `f` is the name of the field or the alias (if there is one).

In backends, the `{!rsh} API` value is an object wherein fields are the members of `{!rsh} apiInterface` and may be used in `{!rsh} .api` components of `{!rsh} fork` and `{!rsh} parallelReduce` to specify the behavior of the corresponding call.
These are called @{defn("API member function")}s.
Each function must occur in the entire program and may only appear once in each consensus step.

```
load: /examples/dominant-assurance/index.rsh
md5: d327454b582bdfa6f03d71de5ce2dd97
range: 24-27
```

This example creates an API that has a name of `Investor`, and it has two functions.
The first function is to invest in the contract, and the second function is to collect the payment if the `Entrepreneur` fails to form a quorum.

### {#ref-programs-appinit-view} View Definition

:::note
This section is about defining views during application initialization. Views are [set in consensus steps](##ref-programs-consensus-view), in your Reach program. But, they are [accessed by frontends](##ref-frontends-js-ctc) by using the Reach standard library of the frontend language, such as JavaScript.
:::

@{ref("rsh", "View")}
```reach
View('NFT', { owner: Address })
// or
View({ owner: Address })
// or
View('NFT', { owner: Address }, { owner: ["currentHolder"] })
// or
View({ owner: Address }, { owner: ["currentHolder"] })
```

Views are read-only functions that can be called by other contracts, as well as off-chain.

A view is defined with `{!rsh} View(viewName, viewInterface, ?viewAlias)` or `{!rsh} View(viewInterface, ?viewAlias)`, where `{!rsh} viewName` is a string that labels the view, `{!rsh} viewInterface` is an object where each field indicates the type of a function or value provided by the contract associated with the specified DApp, and `{!rsh} viewAlias` is an optional object mapping fields from the `{!rsh} viewInterface` to a tuple of aliases.
An alias, which is a string, is another name that the view will be accessible from.

These views are available in frontends via the `{!js} ctc.views` object.

These views are available on-chain by using the appropriate network's ABI and are named `viewName_f` or `f` is there is no `viewName`, where `f` is the name of the field.
An alias uses the same naming convention; it is named `viewName_alias`, or `alias`, if there is no `viewName`.

The example below demonstrates the on-chain name of views:
```reach
  const V1 = View({ a: UInt }, { a: ['b', 'c'] });
  // Generates views: a, b, c
  const V2 = View('V', { a: UInt }, { a: ['b', 'c'] });
  // Generates views: V_a, V_b, V_c
```

In backends, the `{!rsh} View` object as a view object.

For example, `{!rsh} View` is used in the code below without a `{!rsh} viewName`:
```reach
load: /examples/remote-rsh/index.rsh
md5: c7a305584ec5c689a8c61e53b544240b
range: 16 - 19
```

While the `{!rsh} View` in the following code contains a `{!rsh} viewName`:
```reach
load: /examples/view-steps/index.rsh
md5: 78e1541e01ce0791b4b41d2bcd57aaa2
range: 11 - 12
```

### {#ref-programs-appinit-events} Events Definition

@{ref("rsh", "Events")}
```reach
Events('Logger', {
  log: [UInt, Byte(64)]
})
// or
Events({
  log: [UInt, Byte(64)]
})
```

An event is defined with `{!rsh} Events(eventName, eventInterface)` or `{!rsh} Events(eventInterface)`, where `{!rsh} eventName` is a string that labels the event and `{!rsh} eventInterface` is an object where each field is a `{!rsh} Tuple` of `{!rsh} Type`s, representing the type of values that an event will emit.

For example, the `{!rsh} Events` in the code below has no `{!rsh} eventName`:
```reach
load: /examples/event-monitor/index.rsh
md5: c40c1da17f2d66d2906192ccfb40f4b5
range: 10 - 13
```

While the `{!rsh} Events` in following example has an `{!rsh} eventName`:
```reach
load: /examples/dominant-assurance/index.rsh
md5: d327454b582bdfa6f03d71de5ce2dd97
range: 34 - 35
```

These events are available in the frontends via the `{!js} ctc.events` object.
In the DApp, the result of this application argument is referred to as an event object.

### {#ref-programs-appinit-contractcode} Contract code definition

@{ref("rsh", "ContractCode")}
Reach programs can create new child contracts based on predetermined, static code during compile time.
This code is specified with `{!rsh} ContractCode`, documented below, but is actually deployed with `{!rsh} new Contract` (`{!rsh} Contract.new`), which you can read about in @{seclink("ref-programs-consensus-new-contract")}.

As part of an application's specification, you can indicate some child contracts that may be created with:

```reach
const cc = Contract(connectors)
```

where `connectors` is an object with one field for each enabled connector.
Each connector expects the code for the child to be specified in a different way.

The `{!rsh} ETH` connector accepts:
+ A literal string of the shape `${BASE}.bin`, where `${BASE}.bin` is a file that contains the ASCII hexadecimal encoding of the EVM bytecode.
+ A literal string of the shape `${BASE}.json:${FILE}:${CONTRACT}`, where `${BASE}.json` is a file that contains the JSON output of the Solidity compiler and `${FILE}:${CONTRACT}` is a key in the `contracts` object therein.
+ A literal string of the shape `${BASE}.sol:${CONTRACT}`, where `${BASE}.sol` is a Solidity file that defines the contract `${CONTRACT}`.
  Reach will call the Solidity compiler it uses internally to compile the file.

The `{!rsh} ALGO` connector accepts an object with the keys `approval` and `clearState` that may be:
+ A literal string of the shape `${BASE}.tok`, where `${BASE}.tok` is a file that contains the binary encoding of the AVM bytecode (as produced by `{!cmd} goal clerk compile`.)
+ A literal string of the shape `${BASE}.teal`, where `${BASE}.teal` is a TEAL
file that defines the program.
  Reach will call the TEAL compiler it uses internally to compile the file.

The value returned (`cc`) can be used with `{!rsh} new Contract` to actually deploy the child contracts.
It embeds the actual bytecode of the child contracts, so it can (potentially drastically) expand the size (and thus cost) of deploying the parent contract.

