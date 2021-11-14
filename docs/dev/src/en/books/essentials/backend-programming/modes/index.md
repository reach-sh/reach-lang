---
menuItem: mi-docs
---

# Modes

Reach programs are organized into four modes: 

1. [Initialization](#initialization) defines Participants, Views, and APIs, and, optionally, overrides default compilation settings.
1. [Step](#step) defines actions taken by all participants.
1. [Local Step](#local-step) defines actions taken by a particular participant.
1. [Consensus Step](#consensus-step) defines actions taken by the contract.

## Transitions

The following diagram indicates mode transitions:

<div><img src="mode-transitions.png" class="img-fluid" width=500 height=390 loading="lazy"></div>

## Consensus Transfers

A consensus transfer is a statement or expression that (a) facilitates agreement among participants, (b) transitions to a consensus step, and (3) records the agreement as a transaction on the distributed ledger (i.e. blockchain) of the consensus network. These actions include `publish`, `pay`, `race`, `fork`, and `parallelReduce`. The developer chooses which consensus transfer to use depending on the number of participants, tasks, and iterations:

|# Participants|# Tasks|# Iterations|Consensus Transfer|
|-|-|-|-|
|One|One|One|`publish` or `pay`|
|Many|One|One|`race`|
|Many|Many|One|`fork`|
|Many|Many|Many|`parallelReduce`|

The following list describes when to use each type of consensus transfer:

* Use `publish` to cause one participant to make a value (e.g. the price of an item) available to all other participants.
* Use `pay` to cause one participant to pay an amount to the contract account.
* Use `race` when multiple participants are racing to `publish` or `pay`.
* Use `fork` when multiple participants are racing to do different actions. 
* Use `parallelReduce` when muliple participants are iteratively racing or forking.

`commit` transitions from a consensus step to a step.

## Computations

All four modes support a range of statements and expressions described in [Computations](/en/books/essentials/backend-programming/computations/). Each mode supports the additional statements/expressions as described below.

# Initialization

The Reach compiler recognizes `export const main = Reach.App(() => {}` as a Reach DApp, and the body of `Reach.App` represents Application Initialization mode:

``` js
export const main = Reach.App(() => {
  setOptions({ verifyArithmetic: true, connectors: [ETH, ALGO ] });
  const S = Participant('Seller', sellerInteract);
  const B = Participant('Buyer', buyerInteract);
  const V = View('Main', { price: UInt });
  deploy();
```

* Line 1: `Reach.App` accepts a no-argument function that specifies a DApp.
* Line 2: Optionally, `setOptions` overrides default compile options.
* Line 3-4: `Participant` specifies DApp participants.
* Line 5: `View` specifies DApp views.
* Line 6: The `deploy` function transitions from Initialization mode to Step mode.

In addition to the statements and expressions described in [Computations](/en/books/essentials/backend-programming/computations/), Initialization mode supports the following statements/expressions:

## API

An `API` expression defines an API in the contract.

### Declaration

``` js nonum
API(name, interface)
// or 
API(interface)
```

* `name` is a string that labels the API.
* `interface` is an object where each field indicates the type of a function provided by the contract as an API. These APIs are available in frontends via the `ctc.apis` object. The value returned by this function is an object where the fields are the members of `interface`. The object may be used in the `.api` components of `fork` and `parallelReduce` expressions. Each object method must occur exactly once in the entire program.

### Example

See [examples/api-full/index.rsh](https://github.com/reach-sh/reach-lang/blob/master/examples/api-full/index.rsh).

## deploy

The `deploy` statement defines Participants, Views, APIs, and compiler options, and transitions from Initialization mode to Step mode.

### Declaration

``` js nonum
deploy();
```

## Participant

A `Participant` expression defines a participant.

### Declaration

``` js nonum
Participant(name, interface)
```

* `name` is a string indicating the name of the participant function in the generated backend code. Each name must be unique.
* `interface` is a participant interact interface, an object where each field indicates the type of a function or value which must be provided to the backend by the frontend for interacting with the participant.

### Example

``` js nonum
const sellerInteract = {
  price: UInt,
  wisdom: Bytes(128),
  reportReady: Fun([UInt], Null)
};

const S = Participant('Seller', sellerInteract);
```

## ParticipantClass

A `ParticipantClass` expression defines a type of participant.

### Declaration

``` js nonum
ParticipantClass(name, interface)
```

* `name` is a string which indicates the name of the participant class function in the generated backend code. Each name must be unique.
* `interface` is a participant class interact interface, an object where each field indicates the type of a function or value which must be provided to the backend by the frontend for interacting with the participants.

### Example

``` js nonum
const sponsorApi = {
  donation: UInt,
  reportDonation: Fun([Address, UInt, UInt, UInt, UInt], Null),
};

const S = ParticipantClass('Sponsor', sponsorApi);
```

## setOptions

The `setOptions` statement overrides default application parameters.

### Declaration

``` js nonum
setOptions(options)
```

* `options` is an object containing options and values

The options object supports the following key:value pairs:

|Option|Type|Default|
|-|-|-|
|`connectors`|`Array`|All available connectors|
|`verifyArithmetic`|`Boolean`|`false`|
|`verifyPerConnector`|`Boolean`|`false`|

`connectors` is an array of tuples indicating for which consensus networks the compiler will generate contract bytecode. 

`verifyArithmetic` is a boolean value indicating whether arithmetic operations introduce static assertions that they do not overflow beyond UInt.max. This defaults to false because it is onerous to verify. We recommend turning it on before final deployment, but leaving it off during development. When it is false, connectors will ensure that overflows do not actually occur on the network.

`verifyPerConnector` is a boolean value that determines whether verification is done per connector, or once for a generic connector. When this is true, then connector-specific constants, like UInt.max, will be instantiated to literal numbers. This concretization of these constants can induce performance degradation in the verifier.

### Example

``` js nonum
setOptions({ verifyArithmetic: true, connectors: [ETH, ALGO ] });
```

## View

A View expression defines a view object that allows non-participants to see public variables in the contract.

### Declaration

``` js nonum
View(name, viewInterface)
// or
View(viewInterface)
```

* `name` is a string that labels the view.
* `interface` is an object where each field indicates the type of a function or value provided by the contract associated with the specified DApp. These views are available in frontends via the `ctc.views` object. In the DApp, the result of this argument is referred to as a `view` object.

### Example

The program instantiates the view in Line 4 and initializes the `price` property in Line 10:

``` js
export const main = Reach.App(() => {
  const S = Participant('Seller', sellerInteract);
  const B = Participant('Buyer', buyerInteract);
  const V = View('Main', { price: UInt });
  deploy();

  S.only(() => { const price = declassify(interact.price); });
  S.publish(price);
  S.interact.reportReady(price);
  V.price.set(price);
  commit();
```

# Step

A *Step* includes actions that apply to all participants. In addition to the statements and expressions described in [Computations](/en/books/essentials/backend-programming/computations/), Step mode supports the following statements/expressions:

## call

The `call` expression calls the specified API function, returns a pair `[DOMAIN, RET_FUN]`, and publishes the domain of the API member function.

### Declaration

``` js nonum
const [ DOMAIN, RET_FUN ] = call(API_EXPR)
  .pay(API_PAY_EXPR)
  .assume(API_ASSUME_EXPR)
  .throwTimeout(DELAY_EXPR, THROW_EXPR)
```

* `DOMAIN` is the domain of the API member function.
* `RET_FUN` is a function that returns a value to the API call. This function must be called.
* `API_EXPR` is an expression that evaluates to an API member function.
* `API_PAY_EXPR`, `API_ASSUME_EXPR`, and `throwTimeout` are optional. See [Fork](#fork).

### Example

``` js nonum
const A = API('A', { isGt: Fun([UInt, UInt], Bool); });
// ...
const [ dom, k ] = call(A.isGt).assume((x, y) => x != y).pay((x, y) => x);
const [x, y] = dom;
k(x > y);
commit();
```

## closeTo

The `closeTo` expression causes the specified participant to publish, receive a `transfer` of the contract balance and the [pay amount](/en/books/essentials/terminology/#pay-amount), and then exit. 

### Declaration

``` js nonum
closeTo( Who, after, nonNetPayAmt ) 
```

* `Who` is the participant.
* `after` is a no-argument function called before exit. It is optional.
* `nonNetPayAmt`. See [Pay Amount](/en/books/essentials/terminology/#pay-amount). It is optional.

### Example

``` js nonum
const aliceInteract = {
  informTimeout: Fun([], Null)
};

B.pay(wager).timeout(DEADLINE, () => closeTo(A, informTimeout));
```

## exit

### Declaration

### Example

## fork

### Declaration

### Example

## each and only

### Declaration

### Example

## publish, pay, when, timeout

### Declaration

### Example

## race

### Declaration

### Example

## unknowable

### Declaration

### Example

## wait

### Declaration

### Example

# Local Step

A *Local Step* occurs in the body of [each and only](#each-and-only) statements. It represents the actions taken by a single participant in an application. Inside a local step, `this` refers to the participant performing the step, useful when the local step was initiated by an `each` expression. In addition to the statements and expressions described in [Computations](/en/books/essentials/backend-programming/computations/), Local Step mode supports the following statements/expressions:

## assume

### Declaration

### Example

## declassify

### Declaration

### Example

## didPublish

### Declaration

### Example

## fail

### Declaration

### Example

## interact

### Declaration

### Example

## makeCommitment

### Declaration

### Example

# Consensus Step

A *Consensus Step* occurs in the continuation of a consensus transfer statement. It represents the actions taken by the consensus network contract of an application. Inside of a consensus step, `this` refers to the address of the participant that performed the consensus transfer. This is useful when the consensus transfer was initiated by a race expression. In addition to the statements and expressions described in [Computations](/en/books/essentials/backend-programming/computations/), Consensus Step mode supports the following statements/expressions:

## checkCommitment

### Declaration

### Example

## commit

### Declaration

### Example

## continue

### Declaration

### Example

## each and only (consensus)

[each and only](#each-and-only) are allowed in consensus steps and are executed by backends once they observe the completion of the consensus step (i.e., after the associated commit statement.)

## Map

### Declaration

### Example

## parallelReduce

### Declaration

### Example

## remote

### Declaration

### Example

## require

### Declaration

### Example

## Set

### Declaration

### Example

## .throughTimeout

### Declaration

### Example

## .timeRemaining

### Declaration

### Example

## Token

The `Token` expression mints a new non-network token. 

### Declaration

``` js nonum
new Token(PARAMS)
```

* `PARAMS` is an object with the keys in the table below.

    |Key|Type|Default|
    |-|-|-|
    |`name`|`Bytes(32)`|empty|
    |`symbol`|`Bytes(8)`|empty|
    |`url`|`Bytes(96)`|empty|
    |`metadata`|`Bytes(32)`|empty|
    |`supply`|`UInt`|`UInt.max`|
    |`decimals`|`UInt`|ALGO = 6; CFX and ETH = 18|

This returns a Token value and deposits a supply amount of the new non-network tokens into the contract account associated with the DApp. These tokens must be destroyed by the end of the DApp. A token has the following methods:

* `Token.burn(tok, amt)` or `tok.burn(amt)`, where tok is a Token value and amt is a UInt value, may be used to burn tokens in the contract account, meaning that they are utterly destroyed and can never be recovered.

* `Token.destroy(tok)` or `tok.destroy()`, where tok is a Token value, may be used to destroy the token so that it may never be used again by any users on the consensus network. This must be called before the application exits.

* `Token.destroyed(tok)` or `tok.destroyed()`, where tok is a Token value, returns whether destroy has been called on tok yet.

* `Token.supply(tok)` or `tok.supply()`, where tok is a Token value, may be used to query the current supply of tokens, i.e. the number of tokens which have not been burnt.

### Example

``` js nonum
require(supply >= 2 * amt);
const tok = new Token({ name, symbol, url, metadata, supply, decimals });
transfer(amt, tok).to(who);
tok.burn(amt);
assert(tok.supply() == supply - amt);
tok.burn();
assert(tok.destroyed() == false);
tok.destroy();
```

## transfer

A `transfer` expression transfers the specified number of tokens from the contract account to the specified participant account.

### Declaration

``` js nonum
transfer(AMOUNT_EXPR).to(ADDR_EXPR)
// or
transfer(AMOUNT_EXPR, TOKEN_EXPR).to(ADDR_EXPR)
```

* AMOUNT_EXPR is an unsigned integer specifying the number of tokens to transfer from the contract. This amount must evaluate to less than or equal to the balance of network tokens in the contract account.
* ADDR_EXPR is the target address for the transfer.
* TOKEN_EXPR is a Token type. This argument is optional.

### Example

``` js nonum
transfer(10).to(Alice);
```

## while

A `while` statement loops until the specified condition is `false`.

### Declaration

``` js nonum
var LHS = INIT_EXPR;
DEFINE_BLOCK; // optional
invariant(INVARIANT_EXPR);
while( COND_EXPR ) BLOCK
```

* `LHS` is a valid left-hand side of an identifier definition where the expression INIT_EXPR is the right-hand side.
* `DEFINE_BLOCK` is an optional block that may define bindings that use the LHS values which are bound inside the rest of the while and its tail.
* `INVARIANT_EXPR` is an expression, called the loop invariant, that must be true before and after every execution of the block BLOCK
* `COND_EXPR` is the condition. While it is true the block executes. When it is false the loop terminates and control transfers to the continuation of the while statement. The identifiers bound by `LHS` are bound within `DEFINE_BLOCK`, `INVARIANT_EXPR`, `COND_EXPR`, `BLOCK`, and the tail of the while statement.

### Example

``` js nonum
var [ heap1, heap2 ] = [ 21, 21 ];
{ const sum = () => heap1 + heap2; }
invariant(balance() == 2 * wagerAmount);
while ( sum() > 0 ) {
  ....
  [ heap1, heap2 ] = [ heap1 - 1, heap2 ];
  continue; }
```