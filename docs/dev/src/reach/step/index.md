



# {#ref-programs-step} Steps

A Reach step occurs in the continuation of a deploy statement or commit statement.
It represents the actions taken by each of the participants in an application.

## {#ref-programs-step-stmts} Statements

Any statements valid for a [computation](##ref-programs-compute-stmts) are valid for a step.
However, some additional statements are allowed.

### {#ref-programs-only-step} `only` and `each`

@{ref("rsh", "only")}
```reach
Alice.only(() => {
  const pretzel = interact.random(); }); 
```


A local step statement is written `{!reach} PART.only(() => BLOCK)`, where `{!reach} PART` is a participant identifier and `{!reach} BLOCK` is a block.
Within `{!reach} BLOCK`, `{!reach} PART` is bound to the address of the participant.
Any bindings defined within the block of a local step are available in the statement's tail as new local state.
For example,

```reach
Alice.only(() => {
  const x = 3; });
Alice.only(() => {
  const y = x + 1; }); 
```


is a valid program where `{!reach} Alice`'s local state includes the private values `{!reach} x` (bound to `{!reach} 3`) and `{!reach} y` (bound to `{!reach} 4`). However, such bindings are _not_ consensus state, so they are purely local state. For example,

```reach
Alice.only(() => {
  const x = 3; });
Bob.only(() => {
  const y = x + 1; }); 
```


is an invalid program, because `{!reach} Bob` does not know `{!reach} x`.

The @{defn("interact shorthand")}, written `{!reach} PART.interact.METHOD(EXPR_0, ..., EXPR_n)`, is available for calling an `{!reach} interact` function
from outside of an `{!reach} only` block. Such functions must return `{!reach} Null`; therefore, they are only useful
if they produce side-effects, such as logging on the frontend. For example, the
function `{!reach} log` in the participant interact interface of `{!reach} Alice` may be called via:

```reach
Alice.interact.log(x); 
```


---

@{ref("rsh", "each")}
```reach
each([Alice, Bob], () => {
  const pretzel = interact.random(); }); 
```


An @{defn("each")} local step statement can be written as `{!reach} each(PART_TUPLE () => BLOCK)`, where `{!reach} PART_TUPLE` is a tuple of participants and `{!reach} BLOCK` is a block.
It is an abbreviation of many local step statements that could have been written with `{!reach} only`.

### Pay Amounts

A @{defn("pay amount")} is either:
+ An integer, denoting an amount of network tokens; or,
+ A tuple of token amounts.


A @{defn("token amount")} is either:
+ An integer, denoting an amount of network tokens; or,
+ A tuple with two elements, where the first is an integer, denoting an amount of non-network tokens, and the second is `{!reach} Token`, specifying a particular non-network token.


For example, these are all pay amounts:
```reach
0
5
[ 5 ]
[ 5, [ 2, gil ] ]
[ [ 2, gil ], 5 ]
[ 5, [ 2, gil ], [ 8, zorkmids ] ]
```


It is invalid for a pay amount to specify an amount of tokens multiple times.
For example, these are invalid pay amounts:
```reach
[ 1, 2 ]
[ [2, gil], [1, gil] ]
```


The ordering of a pay amount is only significant when used within a fork statement or parallel reduce statement that specifies a `{!reach} paySpec`.
In this case, payments are expected to be a tuple where the first element is an integer pay amount, and the rest of the elements are token amount tuples. The ordering of the token amount elements should match the ordering in `{!reach} paySpec`. For example,
```reach
.paySpec([tokA, tokB])
```


will indicate that `{!reach} fork` payments should be of the format:

```reach
[ NETWORK_TOKEN_AMT, [ amtA, tokA ], [ amtB, tokB ] ]
```


:::note
Reach assumes that network tokens and non-network tokens behave identically, but often they do not; [this article](##guide-nntoks) discusses the causes and consequences of this.
:::


### `publish`, `pay`, `when`, and `timeout`

@{ref("rsh", "publish")}@{ref("rsh", "pay")}@{ref("rsh", "when")}@{ref("rsh", "timeout")}
```reach
Alice.publish(wagerAmount)
     .pay(wagerAmount)
     .timeout(DELAY, () => {
       Bob.publish();
       commit();
       return false; }); 
```

```reach
Alice.publish(wagerAmount)
     .pay(wagerAmount)
     .timeout(DELAY, () => closeTo(Bob, false)); 
```

```reach
Alice.publish(wagerAmount)
     .pay(wagerAmount)
     .timeout(false); 
```


:::note
If you're unsure of what kind of consensus transfer to use, you may want to read the [explanation of the differences](##guide-ctransfers) in the Guide.
:::


A consensus transfer is written
```reach
PART_EXPR.publish(ID_0, ..., ID_n)
 .pay(PAY_EXPR)
 .when(WHEN_EXPR)
 .timeout(DELAY_EXPR, () =>
   TIMEOUT_BLOCK)
 // or
 .throwTimeout(DELAY_EXPR, THROWN_EXPR)
```

where `{!reach} PART_EXPR` is an expression that evaluates to a participant or race expression,
`{!reach} ID_0` through `{!reach} ID_n` are identifiers for `{!reach} PART`'s public local state,
`{!reach} PAY_EXPR` is a public expression evaluating to a pay amount,
`{!reach} WHEN_EXPR` is a public expression evaluating to a boolean and determines if the consensus transfer takes place,
`{!reach} DELAY_EXPR` is a public expression that depends on only consensus state and evaluates to a time argument,
`{!reach} TIMEOUT_BLOCK` is a timeout block, which will be executed after the `{!reach} DELAY_EXPR` time argument passes without `{!reach} PART` executing this consensus transfer.

All of the expressions within a consensus transfer are evaluated in a @{defn("pure")} context, which may not alter the state of the
application.
The `{!reach} PAY_EXPR`, `{!reach} WHEN_EXPR`, and `{!reach} DELAY_EXPR` expressions must refer only to the consensus state, including the new data published via the `{!reach} .publish` component.

The continuation of a consensus transfer statement is a consensus step, which is finalized with a commit statement.
The continuation of a timeout block is the same as the continuation of the function the timeout occurs within.

:::note
See [the guide section on non-participation](##guide-timeout) to understand when to use timeouts and how to use them most effectively.
:::


The `{!reach} publish` component exclusive-or the `{!reach} pay` component may be omitted, if either there is no publication or no transfer of network tokens to accompany this consensus transfer.
The `{!reach} when` component may always be omitted, in which case it is assumed to be `{!reach} true`.
`{!reach} publish` or `{!reach} pay` must occur first, after which components may occur in any order.
For example, the following are all valid:

```reach
Alice.publish(coinFlip);

Alice.pay(penaltyAmount);

Alice.pay(penaltyAmount).publish(coinFlip);

Alice.publish(coinFlip)
     .timeout(DELAY, () => closeTo(Bob, () => exit()));

Alice.pay(penaltyAmount)
     .timeout(DELAY, () => {
       Bob.publish();
       commit();
       exit(); });

Alice.publish(bid).when(wantsToBid);

```


The `{!reach} timeout` component must be included if `{!reach} when` is not statically `{!reach} true`.
This ensures that your clients will eventually complete the program.
If a consensus transfer is a guaranteed race between non-class participants and a participant class that _may_ attempt to transfer (i.e. `{!reach} when` is not statically `{!reach} false`), then a `{!reach} timeout` may be explicitly omitted by writing `{!reach} .timeout(false)`.

`{!reach} .throwTimeout` may be used in place of `{!reach} .timeout`. It accepts a `{!reach} DELAY_EXPR` and an `{!reach} EXPR`, which will be thrown if a timeout should occur.
If an `{!reach} EXPR` is not provided, then `{!reach} null` will be thrown.
If a consensus transfer uses `{!reach} .throwTimeout`, it must be within a try statement.

If a consensus transfer specifies a single participant, which has not yet been fixed in the application and is not a participant class, then this statement does so; therefore, after it the `{!reach} PART` may be used as an address.

If a consensus transfer specificies a single participant class, then all members of that class will attempt to perform the transfer, but only one will succeed.

A consensus transfer binds the identifiers `{!reach} ID_0` through `{!reach} ID_n` for all participants to the values included in the consensus transfer, overwriting any bindings that already exist for those identifiers.
If an existing participant, not included in `{!reach} PART_EXPR`, has previously bound one of these identifiers, then the program is not valid. In other words, the following program is not valid:

```reach
Alice.only(() => {
 const x = 1; });
Bob.only(() => {
 const x = 2; });
Claire.only(() => {
 const x = 3; });
race(Alice, Bob).publish(x);
commit();
```


because `{!reach} Claire` is not included in the `{!reach} race`.
However, if we were to rename `{!reach} Claire`'s `{!reach} x` into `{!reach} y`, then it would be valid, because although `{!reach} Alice` and `{!reach} Bob` both bind `{!reach} x`, they participate in the `{!reach} race`, so it is allowed.
In the tail of this program, `{!reach} x` is bound to either `{!reach} 1` or `{!reach} 2`, i.e., either `{!reach} Alice` or `{!reach} Bob`'s value is overwritten.
This overwriting applies even if `{!reach} Alice` wins and `{!reach} Alice` is a participant class, i.e., the value of `{!reach} x` in the tail is guaranteed to be the single value that was agreed upon in the consensus.

### `fork`

@{ref("rsh", "fork")}@{ref("rsh", "paySpec")}
```reach
fork()
.case(Alice, (() => ({
  msg: 19,
  when: declassify(interact.keepGoing()) })),
  ((v) => v),
  (v) => {
    require(v == 19);
    transfer(wager + 19).to(this);
    commit();
    exit();
  })
.case(Bob, (() => ({
  when: declassify(interact.keepGoing()) })),
  ((_) => wager),
  (_) => {
    commit();

    Alice.only(() => interact.showOpponent(Bob));

    race(Alice, Bob).publish();
    transfer(2 * wager).to(this);
    commit();
    exit();
  })
.timeout(deadline, () => {
  race(Alice, Bob).publish();
  transfer(wager).to(this);
  commit();
  exit(); });
```


:::note
If you're unsure of what kind of consensus transfer to use, you may want to read the [explanation of the differences](##guide-ctransfers) in the Guide.
:::


A @{defn("fork statement")} is written:

```reach
fork()
.paySpec(TOKENS_EXPR)
.case(PART_EXPR,
  PUBLISH_EXPR,
  PAY_EXPR,
  CONSENSUS_EXPR)
.api(API_EXPR,
  API_ASSUME_EXPR,
  API_PAY_EXPR,
  API_CONSENSUS_EXPR)
.timeout(DELAY_EXPR, () =>
  TIMEOUT_BLOCK);
// or
.throwTimeout(DELAY_EXPR, THROW_EXPR)
```


where:
+ `{!reach} TOKENS_EXPR` is an expression that evaluates to a tuple of `{!reach} Token`s;
+ `{!reach} PART_EXPR` is an expression that evaluates to a participant;
+ `{!reach} PUBLISH_EXPR` is a syntactic arrow expression that is evaluated in a local step for the specified participant and must evaluate to an object that may contain a `msg` field, which may be of any type, and a `when` field, which must be a boolean;
+ (optional) `{!reach} PAY_EXPR` is an expression that evaluates to a function parameterized over the `msg` value and returns a pay amount; if this component is left-out, it is synthesized to zero;
+ `{!reach} CONSENSUS_EXPR` is a syntactic arrow expression parameterized over the `msg` value which is evaluated in a consensus step;
+ `{!reach} API_EXPR` is an expression that evaluates to an API member function;
+ (optional) `{!reach} API_ASSUME_EXPR` is a function parameterized over the input to the API member function which is evaluated for effect in a local step; thus it may be used to add `{!reach} assume` constraints on the values given by the API; if this is absent, then it is synthesized to an empty function; if it is present, then `{!reach} API_PAY_EXPR` must be included;
+ (optional) `{!reach} API_PAY_EXPR` is a function parameterized over the input to the API member function which is evaluated to determine the pay amount, like `{!reach} PAY_EXPR`;
+ `{!reach} API_CONSENSUS_EXPR` is a function parameterized over the input to the API member function and a function that returns a value to the API call; this function must be called;
+ the `{!reach} timeout` and `{!reach} throwTimeout` parameter are as in an consensus transfer.


If the discussion of `{!reach} .api` component, the phrase "parameterized over the input" means that if an API function has two arguments, such as `{!reach} Fun([UInt, UInt], Null)`, then the corresponding expression must receive two arguments.
For example, the `{!reach} API_PAY_EXPR` component would be a function that accepts two arguments, while the `{!reach} API_CONSENSUS_EXPR` would be a function that acccepts three arguments---the two for the API and the function used to return a value.

If the `msg` field is absent from the object returned from `{!reach} PUBLISH_EXPR`, then it is treated as if it were `{!reach} null`.

If the `when` field is absent from the object returned from `{!reach} PUBLISH_EXPR`, then it is treated as if it were `{!reach} true`.

If the `{!reach} PAY_EXPR` is absent, then it is treated as if it were `{!reach} (_) => 0`.

The `{!reach} TOKENS_EXPR` and `{!reach} PAY_EXPR` have the same restrictions as the `{!reach} .pay` component of a consensus transfer: i.e., they must be pure and can only refer to consensus state.

The `{!reach} .case` and `{!reach} .api` components may be repeated many times.

The same participant may specify multiple cases.
In this situation, the order of the cases is significant.
That is, a subsequent case will only be evaluated if the prior case's `when` field is `{!reach} false`.

If the participant specified by `{!reach} PART_EXPR` is not already fixed (in the sense of `{!reach} Participant.set`), then if it wins the `{!reach} race`, it is fixed, provided it is not a participant class.

#### `fork` intuition

A fork statement is an abbreviation of a common `{!reach} race` and `{!reach} switch` pattern you could write yourself.

The idea is that each of the participants in the `{!reach} case` components do an independent local step evaluation of a value they would like to `{!reach} publish` and then all `{!reach} race` to `{!reach} publish` their value.
The one that "wins" the `{!reach} race` then determines not only the value (& `{!reach} pay` expression), but also what consensus step code runs to consume the value.

The sample `{!reach} fork` statement linked to the `{!reach} fork` keyword is roughly equivalent to:
```reach
// We first define a Data instance so that each participant can publish a
// different kind of value
const ForkData = Data({Alice: UInt, Bob: Null});
// Then we bind these values for each participant
Alice.only(() => {
 const fork_msg = ForkData.Alice(19);
 const fork_when = declassify(interact.keepGoing()); });
Bob.only(() => {
 const fork_msg = ForkData.Bob(null);
 const fork_when = declassify(interact.keepGoing()); });
// They race
race(Alice, Bob)
 .publish(fork_msg)
 .when(fork_when)
 // The pay ammount depends on who is publishing
 .pay(fork_msg.match( {
   Alice: (v => v),
   Bob: ((_) => wager) } ))
 // The timeout is always the same
 .timeout(deadline, () => {
   race(Alice, Bob).publish();
   transfer(wager).to(this);
   commit();
   exit(); });

 // We ensure that the correct participant published the correct kind of value
 require(fork_msg.match( {
   // Alice had previously published
   Alice: (v => this == Alice),
   // But Bob had not.
   Bob: ((_) => true) } ));

 // Then we select the appropriate body to run
 switch (fork_msg) {
   case Alice: {
     assert (this == Alice);
     require(v == 19);
     transfer(wager + 19).to(this);
     commit();
     exit(); }
   case Bob: {
     Bob.set(this);
     commit();

     Alice.only(() => interact.showOpponent(Bob));

     race(Alice, Bob).publish();
     transfer(2 * wager).to(this);
     commit();
     exit(); }
 }
```


This pattern is tedious to write and error-prone, so the `{!reach} fork` statement abbreviates it for Reach programmers.
When a participant specifies multiple cases, the `msg` field of the participant will be wrapped with an additional
variant signifying what case was chosen.

### `wait`

@{ref("rsh", "wait")}
```reach
wait(TIME); 
```


A @{defn("wait statement")}, written `{!reach} wait(TIME);`, delays the computation until the `{!reach} TIME` time argument passes.
`{!reach} TIME` must be pure and only reference values known by the consensus state.
It may only occur in a step.

### `exit`

@{ref("rsh", "exit")}
```reach
exit(); 
```


An @{defn("exit statement")}, written `{!reach} exit();`, halts the computation.
It is a terminator statement, so it must have an empty tail.
It may only occur in a step.

## {#ref-programs-step-exprs} Expressions

Any expressions valid for a [computation](##ref-programs-compute-exprs) are valid for a step.
However, some additional expressions are allowed.

### `race`

@{ref("rsh", "race")}
```reach
race(Alice, Bob).publish(bet); 
```


:::note
If you're unsure of what kind of consensus transfer to use, you may want to read the [explanation of the differences](##guide-ctransfers) in the Guide.
:::


A @{defn("race expression")}, written `{!reach} race(PARTICIPANT_0, ..., PARTICIPANT_n);`, constructs a participant that may be used in a consensus transfer statement, such as `{!reach} publish` or `{!reach} pay`, where the various participants race to be the first one to perform the consensus transfer.

Reach provides a shorthand, `{!reach} Anybody`, which serves as a `{!reach} race` between all the participants.

:::note
See [the guide section on races](##guide-race) to understand the benefits and dangers of using `{!reach} race`.
:::


### `unknowable`

@{ref("rsh", "unknowable")}
```reach
unknowable( Notter, Knower(var_0, ..., var_N), [msg] ) 
```


 A knowledge assertion that the participant `{!reach} Notter` _does not_ know the results of the variables `{!reach} var_0` through `{!reach} var_N`, but that the participant `{!reach} Knower` _does_ know those values.
It accepts an optional bytes argument, which is included in any reported violation.

### `closeTo`

@{ref("rsh", "closeTo")}
```reach
closeTo( Who, after, nonNetPayAmt ) 
```


 Has participant `{!reach} Who` make a publication, then transfer the `{!reach} balance()` and the non-network pay amount to `{!reach} Who` and end the DApp after executing the function `{!reach} after` in a step.
The `{!reach} nonNetPayAmt` parameter should be a pay amount. For example, when closing a program that uses a `{!reach} Token` `{!reach} token`, the argument would be `{!reach} [ [balance(tok), tok] ]`.
The `{!reach} after` and `{!reach} nonNetPayAmt` arguments are optional.

### `call`

@{ref("rsh", "call")}
```reach
const A = API('A', {
  isGt: Fun([UInt, UInt], Bool);
});
// ...
const [ dom, k ] =
  call(A.isGt).assume((x, y) => x != y)
              .pay((x, y) => x);
const [x, y] = dom;
k(x > y);
commit();
```


A @{defn("call")} is written:

```reach
const [ DOMAIN, RET_FUN ] =
  call(API_EXPR)
    .pay(API_PAY_EXPR)
    .assume(API_ASSUME_EXPR)
    .throwTimeout(DELAY_EXPR, THROW_EXPR)
```


where:
+ `DOMAIN` is the the domain of the API member function.
+ `RET_FUN` is a function that returns a value to the API call. This function must be called.
+ `API_EXPR` is an expression that evaluates to an API member function.
+ `API_PAY_EXPR`, `API_ASSUME_EXPR`, and `{!reach} throwTimeout` are like the corresponding parts in a `{!reach} fork` statement.
They are optional.


 `{!reach} call` will call the given API member function, returning a pair, `{!reach} [DOMAIN, RET_FUN]`.
`{!reach} call` will publish the domain of the API member function, transferring the program from
a step to consensus step.
