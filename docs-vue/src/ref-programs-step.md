



# {#ref-programs-step} Steps

A Reach step occurs in the continuation of a deploy statement or commit statement.
It represents the actions taken by each of the participants in an application.

## {#ref-programs-step-stmts} Statements

Any statements valid for a [computation](##ref-programs-compute-stmts) are valid for a step.
However, some additional statements are allowed.

### {#ref-programs-only-step} `only` and `each`

<Ref :name="(quote rsh):only" />
```reach
Alice.only(() => {
  const pretzel = interact.random(); }); 
```


A local step statement is written `PART.only(() => BLOCK)`, where `PART` is a participant identifier and `BLOCK` is a block.
Within `BLOCK`, `PART` is bound to the address of the participant.
Any bindings defined within the block of a local step are available in the statement's tail as new local state.
For example,

```reach
Alice.only(() => {
  const x = 3; });
Alice.only(() => {
  const y = x + 1; }); 
```


is a valid program where `Alice`'s local state includes the private values `x` (bound to `3`) and `y` (bound to `4`). However, such bindings are _not_ consensus state, so they are purely local state. For example,

```reach
Alice.only(() => {
  const x = 3; });
Bob.only(() => {
  const y = x + 1; }); 
```


is an invalid program, because `Bob` does not know `x`.

The <Defn :name="interact shorthand">interact shorthand</Defn>, written `PART.interact.METHOD(EXPR_0, ..., EXPR_n)`, is available for calling an `interact` function
from outside of an `only` block. Such functions must return `Null`; therefore, they are only useful
if they produce side-effects, such as logging on the frontend. For example, the
function `log` in the participant interact interface of `Alice` may be called via:

```reach
Alice.interact.log(x); 
```


---

<Ref :name="(quote rsh):each" />
```reach
each([Alice, Bob], () => {
  const pretzel = interact.random(); }); 
```


An <Defn :name="each">each</Defn> local step statement can be written as `each(PART_TUPLE () => BLOCK)`, where `PART_TUPLE` is a tuple of participants and `BLOCK` is a block.
It is an abbreviation of many local step statements that could have been written with `only`.

### Pay Amounts

A <Defn :name="pay amount">pay amount</Defn> is either:
+ An integer, denoting an amount of network tokens; or,
+ A tuple of token amounts.


A <Defn :name="token amount">token amount</Defn> is either:
+ An integer, denoting an amount of network tokens; or,
+ A tuple with two elements, where the first is an integer, denoting an amount of non-network tokens, and the second is `Token`, specifying a particular non-network token.


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
For examples, these are invalid pay amounts:
```reach
[ 1, 2 ]
[ [2, gil], [1, gil] ]
```


The ordering of a pay amount is only significant when used within a fork statement or parallel reduce statement that specifies a `paySpec`.
In this case, payments are expected to be a tuple where the first element is an integer pay amount, and the rest of the elements are token amount tuples. The ordering of the token amount elements should match the ordering in `paySpec`. For example,
```reach
.paySpec([tokA, tokB])
```


will indicate that `fork` payments should be of the format:

```reach
[ NETWORK_TOKEN_AMT, [ amtA, tokA ], [ amtB, tokB ] ]
```



### `publish`, `pay`, `when`, and `timeout`

<Ref :name="(quote rsh):publish" /><Ref :name="(quote rsh):pay" /><Ref :name="(quote rsh):when" /><Ref :name="(quote rsh):timeout" />
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


XXX (note-ctransfer)

A consensus transfer is written `PART_EXPR.publish(ID_0, ..., ID_n).pay(PAY_EXPR)..when(WHEN_EXPR).timeout(DELAY_EXPR, () => TIMEOUT_BLOCK)`,
where `PART_EXPR` is an expression that evaluates to a participant or race expression,
`ID_0` through `ID_n` are identifiers for `PART`'s public local state,
`PAY_EXPR` is a public expression evaluating to a pay amount,
`WHEN_EXPR` is a public expression evaluating to a boolean and determines if the consensus transfer takes place,
`DELAY_EXPR` is a public expression that depends on only consensus state and evaluates to a time argument,
`TIMEOUT_BLOCK` is a timeout block, which will be executed after the `DELAY_EXPR` time argument passes without `PART` executing this consensus transfer.

All of the expressions within a consensus transfer are evaluated in a <Defn :name="pure">pure</Defn> context, which may not alter the state of the
application.
The `PAY_EXPR`, `WHEN_EXPR`, and `DELAY_EXPR` expressions must refer only to the consensus state, including the new data published via the `.publish` component.

The continuation of a consensus transfer statement is a consensus step, which is finalized with a commit statement.
The continuation of a timeout block is the same as the continuation of the function the timeout occurs within.

::: note
See [the guide section on non-participation](##guide-timeout) to understand when to use timeouts and how to use them most effectively.
:::

The `publish` component exclusive-or the `pay` component may be omitted, if either there is no publication or no transfer of network tokens to accompany this consensus transfer.
The `when` component may always be omitted, in which case it is assumed to be `true`.
`publish` or `pay` must occur first, after which components may occur in any order.
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


The `timeout` component must be included if `when` is not statically `true`.
This ensures that your clients will eventually complete the program.
If a consensus transfer is a guaranteed race between non-class participants and a participant class that _may_ attempt to transfer (i.e. `when` is not statically `false`), then a `timeout` may be explicitly omitted by writing `.timeout(false)`.

`.throwTimeout` may be used in place of `.timeout`. It accepts a `DELAY_EXPR` and an `EXPR`, which will be thrown if a timeout should occur.
If an `EXPR` is not provided, then `null` will be thrown.
If a consensus transfer uses `.throwTimeout`, it must be within a try statement.

If a consensus transfer specifies a single participant, which has not yet been fixed in the application and is not a participant class, then this statement does so; therefore, after it the `PART` may be used as an address.

If a consensus transfer specificies a single participant class, then all members of that class will attempt to perform the transfer, but only one will succeed.

A consensus transfer binds the identifiers `ID_0` through `ID_n` for all participants to the values included in the consensus transfer, overwriting any bindings that already exist for those identifiers.
If an existing participant, not included in `PART_EXPR`, has previously bound one of these identifiers, then the program is not valid. In other words, the following program is not valid:

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


because `Claire` is not included in the `race`.
However, if we were to rename `Claire`'s `x` into `y`, then it would be valid, because although `Alice` and `Bob` both bind `x`, they participate in the `race`, so it is allowed.
In the tail of this program, `x` is bound to either `1` or `2`, i.e., either `Alice` or `Bob`'s value is overwritten.
This overwriting applies even if `Alice` wins and `Alice` is a participant class, i.e., the value of `x` in the tail is guaranteed to be the single value that was agreed upon in the consensus.

### `fork`

<Ref :name="(quote rsh):fork" /><Ref :name="(quote rsh):paySpec" />
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


XXX (note-ctransfer)

A <Defn :name="fork statement">fork statement</Defn> is written:

```reach
fork()
.paySpec(TOKENS_EXPR)
.case(PART_EXPR,
  PUBLISH_EXPR,
  PAY_EXPR,
  CONSENSUS_EXPR)
.timeout(DELAY_EXPR, () =>
  TIMEOUT_BLOCK);
```


where:
`TOKENS_EXPR` is an expression that evalues to a tuple of `Token`s.
`PART_EXPR` is an expression that evaluates to a participant;
`PUBLISH_EXPR` is a syntactic arrow expression that is evaluated in a local step for the specified participant and must evaluate to an object that may contain a `msg` field, which may be of any type, and a `when` field, which must be a boolean;
`PAY_EXPR` is an expression that evaluates to a function parameterized over the `msg` value and returns a pay amount;
`CONSENSUS_EXPR` is a syntactic arrow expression parameterized over the `msg` value which is evaluated in a consensus step; and,
the `timeout` and `throwTimeout` parameter are as in an consensus transfer.

If the `msg` field is absent from the object returned from `PUBLISH_EXPR`, then it is treated as if it were `null`.

If the `when` field is absent from the object returned from `PUBLISH_EXPR`, then it is treated as if it were `true`.

If the `PAY_EXPR` is absent, then it is treated as if it were `(_) => 0`.

The `TOKENS_EXPR` and `PAY_EXPR` have the same restrictions as the `.pay` component of a consensus transfer: i.e., they must be pure and can only refer to consensus state.

The `.case` component may be repeated many times.

The same participant may specify multiple cases. In this situation, the order of the cases is significant.
That is, a subsequent case will only be evaluated if the prior case's `when` field is `false`.

If the participant specified by `PART_EXPR` is not already fixed (in the sense of `Participant.set`), then if it wins the `race`, it is fixed, provided it is not a participant class.

#### `fork` intuition

A fork statement is an abbreviation of a common `race` and `switch` pattern you could write yourself.

The idea is that each of the participants in the `case` components do an independent local step evaluation of a value they would like to `publish` and then all `race` to `publish` their value.
The one that "wins" the `race` then determines not only the value (& `pay` expression), but also what consensus step code runs to consume the value.

The sample `fork` statement linked to the `fork` keyword is roughly equivalent to:
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


This pattern is tedious to write and error-prone, so the `fork` statement abbreviates it for Reach programmers.
When a participant specifies multiple cases, the `msg` field of the participant will be wrapped with an additional
variant signifying what case was chosen.

### `wait`

<Ref :name="(quote rsh):wait" />
```reach
wait(TIME); 
```


A <Defn :name="wait statement">wait statement</Defn>, written `wait(TIME);`, delays the computation until the `TIME` time argument passes.
`TIME` must be pure and only reference values known by the consensus state.
It may only occur in a step.

### `exit`

<Ref :name="(quote rsh):exit" />
```reach
exit(); 
```


An <Defn :name="exit statement">exit statement</Defn>, written `exit();`, halts the computation.
It is a terminator statement, so it must have an empty tail.
It may only occur in a step.

## {#ref-programs-step-exprs} Expressions

Any expressions valid for a [computation](##ref-programs-compute-exprs) are valid for a step.
However, some additional expressions are allowed.

### `race`

<Ref :name="(quote rsh):race" />
```reach
race(Alice, Bob).publish(bet); 
```


XXX (note-ctransfer)

A <Defn :name="race expression">race expression</Defn>, written `race(PARTICIPANT_0, ..., PARTICIPANT_n);`, constructs a participant that may be used in a consensus transfer statement, such as `publish` or `pay`, where the various participants race to be the first one to perform the consensus transfer.

Reach provides a shorthand, `Anybody`, which serves as a `race` between all the participants.

::: note
See [the guide section on races](##guide-race) to understand the benefits and dangers of using `race`.
:::

### `unknowable`

<Ref :name="(quote rsh):unknowable" />
```reach
unknowable( Notter, Knower(var_0, ..., var_N), [msg] ) 
```


 A knowledge assertion that the participant `Notter` _does not_ know the results of the variables `var_0` through `var_N`, but that the participant `Knower` _does_ know those values.
It accepts an optional bytes argument, which is included in any reported violation.

### `closeTo`

<Ref :name="(quote rsh):closeTo" />
```reach
closeTo( Who, after, nonNetPayAmt ) 
```


 Has participant `Who` make a publication, then transfer the `balance()` and the non-network pay amount to `Who` and end the DApp after executing the function `after` in a step.
The `nonNetPayAmt` parameter should be a pay amount. For example, when closing a program that uses a `Token` `token`, the argument would be `[ [balance(tok), tok] ]`.
The `after` and `nonNetPayAmt` argument are optional.


