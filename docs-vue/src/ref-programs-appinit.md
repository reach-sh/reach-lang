



# {#ref-programs-appinit} Application Initialization

The body of `Reach.app` is an <Defn :name="application initialization">application initialization</Defn>.
It defines the various participants and views of the DApp, as well as sets compilation options.
It is finalized with `deploy()` and then the application begins in a step.

## {#ref-programs-appinit-stmts} Statements

Any statements valid for a [computation](##ref-programs-compute-stmts) are valid for application initialization.
However, some additional statements are allowed.

### `deploy`

<Ref :name="(quote rsh):deploy" />
```reach
deploy(); 
```


A <Defn :name="deploy statement">deploy statement</Defn>, written `deploy();`, deploys the DApp and finalizes all of the available participants, views, and compilation options.

Its continuation is a step, which means its content is specified by XXX (Secref "ref-programs-step").
It represents the body of the DApp to be compiled.

### `setOptions`

<Ref :name="(quote rsh):setOptions" />
```reach
setOptions({ verifyArithmetic: true });
setOptions({}); 
```


The <Defn :name="compilation options">compilation options</Defn> for the DApp may be set by calling `setOptions(OBJ_EXPR);` where `OBJ_EXPR` is an object with the following keys and values:

XXX (tabular
 #:style
 'boxed
 (list
  (list
   (begin (mint-define! '("deployMode")) (reachin "deployMode"))
   ~
   (para (reachin "'constructor'") " (default) or " (reachin "'firstMsg'"))
   ~
   (para
    "Determines whether "
    (tech "contract")
    " should be "
    (tech "deploy")
    "ed independently ("
    (reachin "'constructor'")
    ") or as part of the first "
    (tech "publication")
    " ("
    (reachin "'firstMsg'")
    ")."
    "\n"
    "If deployed as part of the first publication, then the first publication must precede all uses of "
    (reachin "wait")
    " and "
    (reachin ".timeout")
    "."
    "\n"
    "See "
    (seclink "guide-deploymode" "the guide on deployment modes")
    " for a discussion of why to choose a particular mode."))
  (list ~ ~ ~ ~ ~)
  (list
   (begin (mint-define! '("verifyArithmetic")) (reachin "verifyArithmetic"))
   ~
   (para (reachin "true") " or " (reachin "false") " (default)")
   ~
   (para
    "Determines whether arithmetic operations automatically introduce static assertions that they do not overflow beyond "
    (reachin "UInt.max")
    "."
    "\n"
    "This defaults to "
    (reachin "false")
    ", because it is onerous to verify."
    "\n"
    "We recommend turning it on before final deployment, but leaving it off during development."
    "\n"
    "When it is "
    (reachin "false")
    ", "
    (tech "connectors")
    " will ensure that overflows do not actually occur on the network."))
  (list ~ ~ ~ ~ ~)
  (list
   (begin
     (mint-define! '("verifyPerConnector"))
     (reachin "verifyPerConnector"))
   ~
   (para (reachin "true") " or " (reachin "false") " (default)")
   ~
   (para
    "Determines whether verification is done per connector, or once for a generic connector."
    "\n"
    "When this is "
    (reachin "true")
    ", then connector-specific constants, like "
    (reachin "UInt.max")
    ", will be instantiated to literal numbers."
    "\n"
    "This concretization of these constants can induce performance degradation in the verifier."))
  (list ~ ~ ~ ~ ~)
  (list
   (begin (mint-define! '("connectors")) (reachin "connectors"))
   ~
   (para
    (mint-define! '("ETH") '("ALGO"))
    " "
    (reachin "[ETH, ALGO]")
    " (default)")
   ~
   (para
    "A tuple of the "
    (tech "connectors")
    " that the application should be compiled for."
    "\n"
    "By default, all available "
    (tech "connectors")
    " are chosen."))))

## {#ref-programs-appinit-exprs} Expressions

Any expressions valid for a [computation](##ref-programs-appinit-exprs) are valid for application initialization.
However, some additional expressions are allowed.

### Participant Definition

A participant and participant class may be declared with

<Ref :name="(quote rsh):Participant" />
```reach
Participant(participantName, participantInteractInterface)
```


and

<Ref :name="(quote rsh):ParticipantClass" />
```reach
ParticipantClass(participantName, participantInteractInterface)
```


respectively.

`participantName` is a string which indicates the name of the participant function in the generated backend code.
Each `participantName` must be unique.

`participantInteractInterface` is a <Defn :name="participant interact interface">participant interact interface</Defn>, an object where each field indicates the type of a function or value which must be provided to the backend by the frontend for interacting with the participant.

### {#ref-programs-appinit-view} View Definition
XXX (note-view-xref)

<Ref :name="(quote rsh):View" />
```reach
View('NFT', { owner: Address })
```


A view is defined with `View(viewName, viewInterface)`, where `viewName` is a string that labels the view and `viewInterface` is an object where each field indicates the type of a function or value provided by the contract associated with the specified DApp.
These views are available in frontends via the `ctc.getViews` function.
In the DApp, the result of this application argument is referred to as a view object.

