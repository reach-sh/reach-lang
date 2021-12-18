



# {#ref-programs-appinit} Application Initialization

The body of `{!rsh} Reach.app` is an @{defn("application initialization")}.
It defines the various participants and views of the DApp, as well as sets compilation options.
It is finalized with `{!rsh} deploy()` and then the application begins in a step.

## {#ref-programs-appinit-stmts} Statements

Any statements valid for a [computation](##ref-programs-compute-stmts) are valid for application initialization.
However, some additional statements are allowed.

### `deploy`

@{ref("rsh", "deploy")}
```reach
deploy(); 
```


A @{defn("deploy statement")}, written `{!rsh} deploy();`, deploys the DApp and finalizes all of the available participants, views, and compilation options.

Its continuation is a step, which means its content is specified by @{seclink("ref-programs-step")}.
It represents the body of the DApp to be compiled.

### `setOptions`

@{ref("rsh", "setOptions")}
```reach
setOptions({ verifyArithmetic: true });
setOptions({}); 
```


The @{defn("compilation options")} for the DApp may be set by calling `{!rsh} setOptions(OBJ_EXPR);` where `{!rsh} OBJ_EXPR` is an object with the following keys and values:

+ @{ref("rsh", "verifyArithmetic")} `{!rsh} verifyArithmetic`

`{!rsh} true` or `{!rsh} false` (default)

Determines whether arithmetic operations automatically introduce static assertions that they do not overflow beyond `{!rsh} UInt.max`.
This defaults to `{!rsh} false`, because it is onerous to verify.
We recommend turning it on before final deployment, but leaving it off during development.
When it is `{!rsh} false`, connectors will ensure that overflows do not actually occur on the network.
+ @{ref("rsh", "verifyPerConnector")} `{!rsh} verifyPerConnector`

`{!rsh} true` or `{!rsh} false` (default)

Determines whether verification is done per connector, or once for a generic connector.
When this is `{!rsh} true`, then connector-specific constants, like `{!rsh} UInt.max`, will be instantiated to literal numbers.
This concretization of these constants can induce performance degradation in the verifier.
+ @{ref("rsh", "connectors")} `{!rsh} connectors`

@{ref("rsh", "ETH")}@{ref("rsh", "ALGO")} `{!rsh} [ETH, ALGO]` (default)

A tuple of the connectors that the application should be compiled for.
By default, all available connectors are chosen.


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

`{!rsh} participantName` is a string which indicates the name of the participant function in the generated backend code.
Each `{!rsh} participantName` must be unique.

`{!rsh} participantInteractInterface` is a @{defn("participant interact interface")}, an object where each field indicates the type of a function or value which must be provided to the backend by the frontend for interacting with the participant.

### {#ref-programs-appinit-api} API Definition

@{ref("rsh", "API")}
```reach
API('Voter', { vote: Fun([Address], UInt) })
// or
API({ vote: Fun([Address], UInt) })
```


An API is defined with `{!rsh} API(apiName, apiInterface)` or `{!rsh} API(apiInterface)`, where `{!rsh} apiName` is a string that labels the API and `{!rsh} apiInterface` is an object where each field indicates the type of a function provided by the contract as an API.
These APIs are available in frontends via the `{!js} ctc.apis` object, wherein fields are the members of `{!rsh} apiInterface` and may be used in `{!rsh} .api` components of `{!rsh} fork` and `{!rsh} parallelReduce` to specify the behavior of the corresponding call.
These are called @{defn("API member function")}s.
Each function must occur exactly once in the entire program.

### {#ref-programs-appinit-view} View Definition

:::note
This section is about defining views during application initialization. Views are [set in consensus steps](##ref-programs-consensus-view), in your Reach program. But, they are [accessed by frontends](##ref-frontends-js-ctc) by using the Reach standard library of the frontend language, such as JavaScript.
:::


@{ref("rsh", "View")}
```reach
View('NFT', { owner: Address })
// or
View({ owner: Address })
```


A view is defined with `{!rsh} View(viewName, viewInterface)` or `{!rsh} View(viewInterface)`, where `{!rsh} viewName` is a string that labels the view and `{!rsh} viewInterface` is an object where each field indicates the type of a function or value provided by the contract associated with the specified DApp.
These views are available in frontends via the `{!js} ctc.views` object.
In the DApp, the result of this application argument is referred to as a view object.

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
These events are available in the frontends via the `{!js} ctc.events` object.
In the DApp, the result of this application argument is referred to as an event object.
