



# {#ref-programs-appinit} Application Initialization

The body of `Reach.app` is an ${defn("application initialization")}.
It defines the various participants and views of the DApp, as well as sets compilation options.
It is finalized with `deploy()` and then the application begins in a step.

## {#ref-programs-appinit-stmts} Statements

Any statements valid for a [computation](##ref-programs-compute-stmts) are valid for application initialization.
However, some additional statements are allowed.

### `deploy`

${ref((quote rsh), "deploy")}
```reach
deploy(); 
```


A ${defn("deploy statement")}, written `deploy();`, deploys the DApp and finalizes all of the available participants, views, and compilation options.

Its continuation is a step, which means its content is specified by ${seclink("ref-programs-step")}.
It represents the body of the DApp to be compiled.

### `setOptions`

${ref((quote rsh), "setOptions")}
```reach
setOptions({ verifyArithmetic: true });
setOptions({}); 
```


The ${defn("compilation options")} for the DApp may be set by calling `setOptions(OBJ_EXPR);` where `OBJ_EXPR` is an object with the following keys and values:

+ ${ref((quote rsh), "verifyArithmetic")} `verifyArithmetic`

`true` or `false` (default)

Determines whether arithmetic operations automatically introduce static assertions that they do not overflow beyond `UInt.max`.
This defaults to `false`, because it is onerous to verify.
We recommend turning it on before final deployment, but leaving it off during development.
When it is `false`, connectors will ensure that overflows do not actually occur on the network.
+ ${ref((quote rsh), "verifyPerConnector")} `verifyPerConnector`

`true` or `false` (default)

Determines whether verification is done per connector, or once for a generic connector.
When this is `true`, then connector-specific constants, like `UInt.max`, will be instantiated to literal numbers.
This concretization of these constants can induce performance degradation in the verifier.
+ ${ref((quote rsh), "connectors")} `connectors`

${ref((quote rsh), "ETH")}${ref((quote rsh), "ALGO")} `[ETH, ALGO]` (default)

A tuple of the connectors that the application should be compiled for.
By default, all available connectors are chosen.


## {#ref-programs-appinit-exprs} Expressions

Any expressions valid for a [computation](##ref-programs-appinit-exprs) are valid for application initialization.
However, some additional expressions are allowed.

### Participant Definition

A participant and participant class may be declared with

${ref((quote rsh), "Participant")}
```reach
Participant(participantName, participantInteractInterface)
```


and

${ref((quote rsh), "ParticipantClass")}
```reach
ParticipantClass(participantName, participantInteractInterface)
```


respectively.

`participantName` is a string which indicates the name of the participant function in the generated backend code.
Each `participantName` must be unique.

`participantInteractInterface` is a ${defn("participant interact interface")}, an object where each field indicates the type of a function or value which must be provided to the backend by the frontend for interacting with the participant.

### {#ref-programs-appinit-api} API Definition

${ref((quote rsh), "API")}
```reach
API('Voter', { vote: Fun([Address], UInt) })
```


A API is defined with `API(apiName, apiInterface)`, where `apiName` is a string that labels the API and `apiInterface` is an object where each field indicates the type of a function provided by the contract as an API.
These APIs are available in frontends via the `ctc.apis` object.
The value returned by this function is an object where the fields are the members of `apiInterface` are may be used in `.api` components of `fork` and `parallelReduce` to specify the behavior of the corresponding call.
These are called ${defn("API member function")}s.
Each function must occur exactly once in the entire program.

### {#ref-programs-appinit-view} View Definition

::: note
This section is about defining views during in application initialization. Views are [set in consensus steps](##ref-programs-consensus-view), in your Reach program. But, they are [accessed by frontends](##ref-frontends-js-ctc) by using the Reach standard library of the frontend language, such as JavaScript.
:::

${ref((quote rsh), "View")}
```reach
View('NFT', { owner: Address })
```


A view is defined with `View(viewName, viewInterface)`, where `viewName` is a string that labels the view and `viewInterface` is an object where each field indicates the type of a function or value provided by the contract associated with the specified DApp.
These views are available in frontends via the `ctc.views` object.
In the DApp, the result of this application argument is referred to as a view object.

