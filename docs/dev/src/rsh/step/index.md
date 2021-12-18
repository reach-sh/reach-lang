



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


A local step statement is written `{!rsh} PART.only(() => BLOCK)`, where `{!rsh} PART` is a participant identifier and `{!rsh} BLOCK` is a block.
Within `{!rsh} BLOCK`, `{!rsh} PART` is bound to the address of the participant.
Any bindings defined within the block of a local step are available in the statement's tail as new local state.
For example,

```reach
Alice.only(() => {
  const x = 3; });
Alice.only(() => {
  const y = x + 1; }); 
```


is a valid program where `{!rsh} Alice`'s local state includes the private values `{!rsh} x` (bound to `{!rsh} 3`) and `{!rsh} y` (bound to `{!rsh} 4`). However, such bindings are _not_ consensus state, so they are purely local state. For example,

```reach
Alice.only(() => {
  const x = 3; });
Bob.only(() => {
  const y = x + 1; }); 
```


is an invalid program, because `{!rsh} Bob` does not know `{!rsh} x`.

The @{defn("interact shorthand")}, written `{!rsh} PART.interact.METHOD(EXPR_0, ..., EXPR_n)`, is available for calling an `{!rsh} interact` function
from outside of an `{!rsh} only` block. Such functions must return `{!rsh} Null`; therefore, they are only useful
if they produce side-effects, such as logging on the frontend. For example, the
function `{!rsh} log` in the participant interact interface of `{!rsh} Alice` may be called via:

```reach
Alice.interact.log(x); 
```


---

@{ref("rsh", "each")}
```reach
each([Alice, Bob], () => {
  const pretzel = interact.random(); }); 
```


An @{defn("each")} local step statement can be written as `{!rsh} each(PART_TUPLE () => BLOCK)`, where `{!rsh} PART_TUPLE` is a tuple of participants and `{!rsh} BLOCK` is a block.
It is an abbreviation of many local step statements that could have been written with `{!rsh} only`.

### Pay Amounts

A @{defn("pay amount")} is either:
+ An integer, denoting an amount of network tokens; or,
+ A tuple of token amounts.


A @{defn("token amount")} is either:
+ An integer, denoting an amount of network tokens; or,
+ A tuple with two elements, where the first is an integer, denoting an amount of non-network tokens, and the second is `{!rsh} Token`, specifying a particular non-network token.


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


The ordering of a pay amount is only significant when used within a fork statement or parallel reduce statement that specifies a `{!rsh} paySpec`.
In this case, payments are expected to be a tuple where the first element is an integer pay amount, and the rest of the elements are token amount tuples. The ordering of the token amount elements should match the ordering in `{!rsh} paySpec`. For example,
```reach
.paySpec([tokA, tokB])
```


will indicate that `{!rsh} fork` payments should be of the format:

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

where `{!rsh} PART_EXPR` is an expression that evaluates to a participant or race expression,
`{!rsh} ID_0` through `{!rsh} ID_n` are identifiers for `{!rsh} PART`'s public local state,
`{!rsh} PAY_EXPR` is a public expression evaluating to a pay amount,
`{!rsh} WHEN_EXPR` is a public expression evaluating to a boolean and determines if the consensus transfer takes place,
`{!rsh} DELAY_EXPR` is a public expression that depends on only consensus state and evaluates to a time argument,
`{!rsh} TIMEOUT_BLOCK` is a timeout block, which will be executed after the `{!rsh} DELAY_EXPR` time argument passes without `{!rsh} PART` executing this consensus transfer.

All of the expressions within a consensus transfer are evaluated in a @{defn("pure")} context, which may not alter the state of the
application.
The `{!rsh} PAY_EXPR`, `{!rsh} WHEN_EXPR`, and `{!rsh} DELAY_EXPR` expressions must refer only to the consensus state, including the new data published via the `{!rsh} .publish` component.

The continuation of a consensus transfer statement is a consensus step, which is finalized with a commit statement.
The continuation of a timeout block is the same as the continuation of the function the timeout occurs within.

:::note
See [the guide section on non-participation](##guide-timeout) to understand when to use timeouts and how to use them most effectively.
:::


The `{!rsh} publish` component exclusive-or the `{!rsh} pay` component may be omitted, if either there is no publication or no transfer of network tokens to accompany this consensus transfer.
The `{!rsh} when` component may always be omitted, in which case it is assumed to be `{!rsh} true`.
`{!rsh} publish` or `{!rsh} pay` must occur first, after which components may occur in any order.
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


The `{!rsh} timeout` component must be included if `{!rsh} when` is not statically `{!rsh} true`.
This ensures that your clients will eventually complete the program.
If a consensus transfer is a guaranteed race between non-class participants and a participant class that _may_ attempt to transfer (i.e. `{!rsh} when` is not statically `{!rsh} false`), then a `{!rsh} timeout` may be explicitly omitted by writing `{!rsh} .timeout(false)`.

`{!rsh} .throwTimeout` may be used in place of `{!rsh} .timeout`. It accepts a `{!rsh} DELAY_EXPR` and an `{!rsh} EXPR`, which will be thrown if a timeout should occur.
If an `{!rsh} EXPR` is not provided, then `{!rsh} null` will be thrown.
If a consensus transfer uses `{!rsh} .throwTimeout`, it must be within a try statement.

If a consensus transfer specifies a single participant, which has not yet been fixed in the application and is not a participant class, then this statement does so; therefore, after it the `{!rsh} PART` may be used as an address.

If a consensus transfer specificies a single participant class, then all members of that class will attempt to perform the transfer, but only one will succeed.

A consensus transfer binds the identifiers `{!rsh} ID_0` through `{!rsh} ID_n` for all participants to the values included in the consensus transfer, overwriting any bindings that already exist for those identifiers.
If an existing participant, not included in `{!rsh} PART_EXPR`, has previously bound one of these identifiers, then the program is not valid. In other words, the following program is not valid:

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


because `{!rsh} Claire` is not included in the `{!rsh} race`.
However, if we were to rename `{!rsh} Claire`'s `{!rsh} x` into `{!rsh} y`, then it would be valid, because although `{!rsh} Alice` and `{!rsh} Bob` both bind `{!rsh} x`, they participate in the `{!rsh} race`, so it is allowed.
In the tail of this program, `{!rsh} x` is bound to either `{!rsh} 1` or `{!rsh} 2`, i.e., either `{!rsh} Alice` or `{!rsh} Bob`'s value is overwritten.
This overwriting applies even if `{!rsh} Alice` wins and `{!rsh} Alice` is a participant class, i.e., the value of `{!rsh} x` in the tail is guaranteed to be the single value that was agreed upon in the consensus.

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
+ `{!rsh} TOKENS_EXPR` is an expression that evaluates to a tuple of `{!rsh} Token`s;
+ `{!rsh} PART_EXPR` is an expression that evaluates to a participant;
+ `{!rsh} PUBLISH_EXPR` is a syntactic arrow expression that is evaluated in a local step for the specified participant and must evaluate to an object that may contain a `msg` field, which may be of any type, and a `when` field, which must be a boolean;
+ (optional) `{!rsh} PAY_EXPR` is an expression that evaluates to a function parameterized over the `msg` value and returns a pay amount; if this component is left-out, it is synthesized to zero;
+ `{!rsh} CONSENSUS_EXPR` is a syntactic arrow expression parameterized over the `msg` value which is evaluated in a consensus step;
+ `{!rsh} API_EXPR` is an expression that evaluates to an API member function;
+ (optional) `{!rsh} API_ASSUME_EXPR` is a function parameterized over the input to the API member function which is evaluated for effect in a local step; thus it may be used to add `{!rsh} assume` constraints on the values given by the API; if this is absent, then it is synthesized to an empty function; if it is present, then `{!rsh} API_PAY_EXPR` must be included;
+ (optional) `{!rsh} API_PAY_EXPR` is a function parameterized over the input to the API member function which is evaluated to determine the pay amount, like `{!rsh} PAY_EXPR`;
+ `{!rsh} API_CONSENSUS_EXPR` is a function parameterized over the input to the API member function and a function that returns a value to the API call; this function must be called;
+ the `{!rsh} timeout` and `{!rsh} throwTimeout` parameter are as in an consensus transfer.

In the discussion of `{!rsh} .api` component, the phrase "parameterized over the input" means that if an API function has two arguments, such as `{!rsh} Fun([UInt, UInt], Null)`, then the corresponding expression must receive two arguments.
For example, the `{!rsh} API_PAY_EXPR` component would be a function that accepts two arguments, while the `{!rsh} API_CONSENSUS_EXPR` would be a function that acccepts three arguments---the two for the API and the function used to return a value.
All API functions must rely only on consensus state and the function domain.

If the `msg` field is absent from the object returned from `{!rsh} PUBLISH_EXPR`, then it is treated as if it were `{!rsh} null`.

If the `when` field is absent from the object returned from `{!rsh} PUBLISH_EXPR`, then it is treated as if it were `{!rsh} true`.

If the `{!rsh} PAY_EXPR` is absent, then it is treated as if it were `{!rsh} (_) => 0`.

The `{!rsh} TOKENS_EXPR` and `{!rsh} PAY_EXPR` have the same restrictions as the `{!rsh} .pay` component of a consensus transfer: i.e., they must be pure and can only refer to consensus state.

The `{!rsh} .case` and `{!rsh} .api` components may be repeated many times.

The same participant may specify multiple cases.
In this situation, the order of the cases is significant.
That is, a subsequent case will only be evaluated if the prior case's `when` field is `{!rsh} false`.

If the participant specified by `{!rsh} PART_EXPR` is not already fixed (in the sense of `{!rsh} Participant.set`), then if it wins the `{!rsh} race`, it is fixed, provided it is not a participant class.

#### `fork` intuition

A fork statement is an abbreviation of a common `{!rsh} race` and `{!rsh} switch` pattern you could write yourself.

The idea is that each of the participants in the `{!rsh} case` components do an independent local step evaluation of a value they would like to `{!rsh} publish` and then all `{!rsh} race` to `{!rsh} publish` their value.
The one that "wins" the `{!rsh} race` then determines not only the value (& `{!rsh} pay` expression), but also what consensus step code runs to consume the value.

The sample `{!rsh} fork` statement linked to the `{!rsh} fork` keyword is roughly equivalent to:
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


This pattern is tedious to write and error-prone, so the `{!rsh} fork` statement abbreviates it for Reach programmers.
When a participant specifies multiple cases, the `msg` field of the participant will be wrapped with an additional
variant signifying what case was chosen.

### `wait`

@{ref("rsh", "wait")}
```reach
wait(TIME); 
```


A @{defn("wait statement")}, written `{!rsh} wait(TIME);`, delays the computation until the `{!rsh} TIME` time argument passes.
`{!rsh} TIME` must be pure and only reference values known by the consensus state.
It may only occur in a step.

### `exit`

@{ref("rsh", "exit")}
```reach
exit(); 
```


An @{defn("exit statement")}, written `{!rsh} exit();`, halts the computation.
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


A @{defn("race expression")}, written `{!rsh} race(PARTICIPANT_0, ..., PARTICIPANT_n);`, constructs a participant that may be used in a consensus transfer statement, such as `{!rsh} publish` or `{!rsh} pay`, where the various participants race to be the first one to perform the consensus transfer.

Reach provides a shorthand, `{!rsh} Anybody`, which serves as a `{!rsh} race` between all the participants.

:::note
See [the guide section on races](##guide-race) to understand the benefits and dangers of using `{!rsh} race`.
:::


### `unknowable`

@{ref("rsh", "unknowable")}
```reach
unknowable( Notter, Knower(var_0, ..., var_N), [msg] ) 
```


 A knowledge assertion that the participant `{!rsh} Notter` _does not_ know the results of the variables `{!rsh} var_0` through `{!rsh} var_N`, but that the participant `{!rsh} Knower` _does_ know those values.
It accepts an optional bytes argument, which is included in any reported violation.

### `closeTo`

@{ref("rsh", "closeTo")}
```reach
closeTo( Who, after, nonNetPayAmt ) 
```


 Has participant `{!rsh} Who` make a publication, then transfer the `{!rsh} balance()` and the non-network pay amount to `{!rsh} Who` and end the DApp after executing the function `{!rsh} after` in a step.
The `{!rsh} nonNetPayAmt` parameter should be a pay amount. For example, when closing a program that uses a `{!rsh} Token` `{!rsh} token`, the argument would be `{!rsh} [ [balance(tok), tok] ]`.
The `{!rsh} after` and `{!rsh} nonNetPayAmt` arguments are optional.

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
+ `API_PAY_EXPR`, `API_ASSUME_EXPR`, and `{!rsh} throwTimeout` are like the corresponding parts in a `{!rsh} fork` statement.
They are optional.


 `{!rsh} call` will call the given API member function, returning a pair, `{!rsh} [DOMAIN, RET_FUN]`.
`{!rsh} call` will publish the domain of the API member function, transferring the program from
a step to consensus step.
