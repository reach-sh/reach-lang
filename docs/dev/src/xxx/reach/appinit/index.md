



# {#ref-programs-appinit} Application Initialization

The body of `{!reach} Reach.app` is an @{defn("application initialization")}.
It defines the various participants and views of the DApp, as well as sets compilation options.
It is finalized with `{!reach} deploy()` and then the application begins in a step.

## {#ref-programs-appinit-stmts} Statements

Any statements valid for a [computation](##ref-programs-compute-stmts) are valid for application initialization.
However, some additional statements are allowed.

### `deploy`

@{ref("rsh", "deploy")}
```reach
deploy(); 
```


A @{defn("deploy statement")}, written `{!reach} deploy();`, deploys the DApp and finalizes all of the available participants, views, and compilation options.

Its continuation is a step, which means its content is specified by @{seclink("ref-programs-step")}.
It represents the body of the DApp to be compiled.

### `setOptions`

@{ref("rsh", "setOptions")}
```reach
setOptions({ verifyArithmetic: true });
setOptions({}); 
```


The @{defn("compilation options")} for the DApp may be set by calling `{!reach} setOptions(OBJ_EXPR);` where `{!reach} OBJ_EXPR` is an object with the following keys and values:

+ @{ref("rsh", "verifyArithmetic")} `{!reach} verifyArithmetic`

`{!reach} true` or `{!reach} false` (default)

Determines whether arithmetic operations automatically introduce static assertions that they do not overflow beyond `{!reach} UInt.max`.
This defaults to `{!reach} false`, because it is onerous to verify.
We recommend turning it on before final deployment, but leaving it off during development.
When it is `{!reach} false`, connectors will ensure that overflows do not actually occur on the network.
+ @{ref("rsh", "verifyPerConnector")} `{!reach} verifyPerConnector`

`{!reach} true` or `{!reach} false` (default)

Determines whether verification is done per connector, or once for a generic connector.
When this is `{!reach} true`, then connector-specific constants, like `{!reach} UInt.max`, will be instantiated to literal numbers.
This concretization of these constants can induce performance degradation in the verifier.
+ @{ref("rsh", "connectors")} `{!reach} connectors`

@{ref("rsh", "ETH")}@{ref("rsh", "ALGO")} `{!reach} [ETH, ALGO]` (default)

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

`{!reach} participantName` is a string which indicates the name of the participant function in the generated backend code.
Each `{!reach} participantName` must be unique.

`{!reach} participantInteractInterface` is a @{defn("participant interact interface")}, an object where each field indicates the type of a function or value which must be provided to the backend by the frontend for interacting with the participant.

### {#ref-programs-appinit-api} API Definition

@{ref("rsh", "API")}
```reach
API('Voter', { vote: Fun([Address], UInt) })
// or
API({ vote: Fun([Address], UInt) })
```


An API is defined with `{!reach} API(apiName, apiInterface)` or `{!reach} API(apiInterface)`, where `{!reach} apiName` is a string that labels the API and `{!reach} apiInterface` is an object where each field indicates the type of a function provided by the contract as an API.
These APIs are available in frontends via the `{!js} ctc.apis` object, wherein fields are the members of `{!reach} apiInterface` and may be used in `{!reach} .api` components of `{!reach} fork` and `{!reach} parallelReduce` to specify the behavior of the corresponding call.
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


A view is defined with `{!reach} View(viewName, viewInterface)` or `{!reach} View(viewInterface)`, where `{!reach} viewName` is a string that labels the view and `{!reach} viewInterface` is an object where each field indicates the type of a function or value provided by the contract associated with the specified DApp.
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


An event is defined with `{!reach} Events(eventName, eventInterface)` or `{!reach} Events(eventInterface)`, where `{!reach} eventName` is a string that labels the event and `{!reach} eventInterface` is an object where each field is a `{!reach} Tuple` of `{!reach} Type`s, representing the type of values that an event will emit.
These events are available in the frontends via the `{!js} ctc.events` object.
In the DApp, the result of this application argument is referred to as an event object.
