---
menuItem: mi-docs
---

# Reach Statements

This page defines Reach statements, blocks, tails, continuations, block statements, and terminator statements, and lists and describes the statement identifiers supported by Reach. 

> Note that some statement identifiers are valid only in certain [Reach Modes](/en/essentials/backend-programming/reach-modes/). For example, `setOptions` and `deploy` are valid only during [Initialization](/en/essentials/backend-programming/reach-modes/#initialization), `fork` and `exit` are valid only in [Steps](/en/essentials/backend-programming/reach-modes/#step), and `commit` is valid only in [Consensus Steps](/en/essentials/backend-programming/reach-modes/#consensus-step).

### Statements

*Statements* are instructions composed of identifiers, operators, and expressions.  Statements often begin with an identifier (e.g. `const`) and end with a  semicolon:

``` js nonum
const wager = declassify(interact.wager);
```

Conditional statements, which begin with the identifier `if`, do not end with a semicolon: 

``` js nonum
if (!willBuy) {
  commit();
  each([S, B], () => interact.reportCancellation());
  exit();
}
```

### Blocks

*Blocks* are sequences of statements surrounded by curly braces. Below are five blocks. The third contains the fourth and fifth:

``` js nonum
{ return 42; }

{
  const x = 31;
  return x + 11;
}

{ 
  if ( x < y ) {
    return 'Why';
  } else {
    return 'Ecks';
  }
}
``` 

### Tails

*Tails* are statements that follow a prior statement within the same scope. Consider the following:

``` js nonum
{
  X;
  Y;
  Z;
}
```

In this block, the tail of statement `X` is `{ Y; Z; }`, and the tail of statement `Y` is `{ Z; }`. Tails are statically apparent from the structure of the source code.

### Continuations

*Continuations* are all the statements that follow a prior statement. Consider the following:

``` js nonum
{ 
  {
    X;
    Y;
  };
  Z;
}
```

In this block, the continuation of statement `X` is `{ Y; }; Z;`. Continuations are not statically apparent. On the contrary, continuations are influenced by function calls.

### Block Statements

*Block statements*, statements composed of blocks, establish a local scope for the identifier definitions within the curly braces, isolating the definitions from the statement tail. Consider this example that does *not* include a block statement:

``` js nonum
const x = 4;
return x; 
```

Above, `x` evaluates to `4`. The snippet below isolates `const x = 4` within a block statement:

``` js nonum
{ const x = 4; }
return x;
```

Now, `return x` is erroneous because `x` is unbound outside the block statement.

### Terminator Statements

*Terminator statements* are statements with no tail. Consider the following:

``` js nonum
{
  X;
  Y;
  Z;
}
```

In this block, `Z` is the terminator statement because it does not have a tail. Blocks that end with `return`, `continue`, or `exit` do not have terminator statements. The compiler treats these types of blocks as if they end with `return null`.

# commit

# const

`const` is an *identifier definition* that binds an identifier to a *value* definition. Here are examples:

``` js nonum
const DELAY = 10;
const [ Good, Bad ] = [ 42, 43 ];
const { x, y } = { x: 1, y: 2 };
const [ x, [ y ] ] = [ 1, [ 2 ] ];
const [ x, { y } ] = [ 1, { y: 2 } ];
const { x: [ a, b ] } = { x: [ 1, 2 ] };
```

You cannot re-bind a previously bound identifier. The following is invalid:

``` js nonum
const x = 3;
const x = 4; // invalid
```

The following is also invalid:

``` js nonum
Alice.only(() => { const x = 3; });
Bob.only(() => { const x = 4; }); // invalid
```

The special identifier `_` is an exception to this rule. The `_` binding is always considered to be unbound. This means that `_` is both an identifier that can never be read, as well as an identifier that may be bound many times which is useful for ignoring unwanted values. The following, for example, binds `x = 2` while ignoring `1` and `3`:

``` js nonum
const [_, x, _] = [1, 2, 3];
```

# continue

# deploy

The `deploy` statement transitions from Initialization Mode to Step Mode. See [Reach Modes](/en/essentials/backend-programming/reach-modes/).

# each

See [only](#only).

# exit

The `exit` statement halts the computation. It is a terminator statement, so it must have an empty tail:

``` js nonum
exit();
```

# export

# fork

A `fork` statement organizes a competition among several participants (each intent on accomplishing a potentially different task), determines which participant wins the competition, and executes the winner's task. Here is the declaration:

``` js nonum
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
  .timeout(DELAY_EXPR, () => TIMEOUT_BLOCK);
  // or
  .throwTimeout(DELAY_EXPR, THROW_EXPR)
```

* `TOKENS_EXPR` is an expression that evaluates to a tuple of Tokens;
* `PART_EXPR` is an expression that evaluates to a participant;
* `PUBLISH_EXPR` is a syntactic arrow expression that is evaluated in a local step for the specified participant and must evaluate to an object that may contain a msg field, which may be of any type, and a when field, which must be a boolean;
* (optional) `PAY_EXPR` is an expression that evaluates to a function parameterized over the msg value and returns a [pay amount](/en/books/essentials/terminology/#pay-amount); if this component is left-out, it is synthesized to zero;
* `CONSENSUS_EXPR` is a syntactic arrow expression parameterized over the msg value which is evaluated in a consensus step;
* `API_EXPR` is an expression that evaluates to an API member function;
* (optional) `API_ASSUME_EXPR` is a function parameterized over the input to the API member function which is evaluated for effect in a local step; thus it may be used to add assume constraints on the values given by the API; if this is absent, then it is synthesized to an empty function; if it is present, then API_PAY_EXPR must be included;
* (optional) `API_PAY_EXPR` is a function parameterized over the input to the API member function which is evaluated to determine the [pay amount](/en/books/essentials/terminology/#pay-amount), like PAY_EXPR;
* `API_CONSENSUS_EXPR` is a function parameterized over the input to the API member function and a function that returns a value to the API call; this function must be called;
* the timeout and throwTimeout parameter are as in an consensus transfer.

Here is an example:

``` js nonum
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

A fork statement is an abbreviation of a common race and switch pattern you could write yourself. The idea is that each of the participants in the case components do an independent local step evaluation of a value they would like to publish and then all race to publish their value. The one that "wins" the race then determines not only the value (& pay expression), but also what consensus step code runs to consume the value. The example above is roughly equivalent to the following:

``` js nonum
// We first define a Data instance so that each participant can publish a different kind of value.
const ForkData = Data({ Alice: UInt, Bob: Null });

// Then we bind these values for each participant
Alice.only(() => {
  const fork_msg = ForkData.Alice(19);
  const fork_when = declassify(interact.keepGoing());
});
Bob.only(() => {
  const fork_msg = ForkData.Bob(null);
  const fork_when = declassify(interact.keepGoing());
});

// They race
race(Alice, Bob)
  .publish(fork_msg)
  .when(fork_when)

  // The pay ammount depends on who is publishing
  .pay(fork_msg.match({
    Alice: (v => v),
    Bob: ((_) => wager)
  }))

  // The timeout is always the same
  .timeout(deadline, () => {
    race(Alice, Bob).publish();
    transfer(wager).to(this);
    commit();
    exit();
  });

// We ensure that the correct participant published the correct kind of value
require(fork_msg.match({

  // Alice had previously published
  Alice: (v => this == Alice),

  // But Bob had not.
  Bob: ((_) => true)
}));

// Then we select the appropriate body to run
switch (fork_msg) {
  case Alice: {
    assert(this == Alice);
    require(v == 19);
    transfer(wager + 19).to(this);
    commit();
    exit();
  }
  case Bob: {
    Bob.set(this);
    commit();

    Alice.only(() => interact.showOpponent(Bob));

    race(Alice, Bob).publish();
    transfer(2 * wager).to(this);
    commit();
    exit();
  }
}
```

This pattern is tedious to write and error-prone, so the fork statement abbreviates it for Reach programmers. When a participant specifies multiple cases, the msg field of the participant will be wrapped with an additional variant signifying what case was chosen.

# function

`function` is an *identifier definition* that binds an identifier to a *function* definition. Here is an example:

``` js nonum
function randomBool() { return (interact.random() % 2) == 0; };
```

Function parameters may specify default arguments, but they must appear last:

``` js nonum
function f(a, b, c = a + 1, d = b + c) => a + b + c + d;
```

Argument expressions like `d = b + c` have access to preceding arguments (e.g. `c`) and to any variables within the scope in which the function itself was defined.

The last parameter of a function may be a *rest* parameter (e.g. `...nums`) which allows the function to be called with an arbitrary number of arguments:

``` js nonum
function sum(x, ...nums) {
  const arr = array(UInt, nums);
  return x + arr.reduce(0, add);
}
```

You might invoke the `sum` function like this:

``` js nonum
A.interact.report(sum(2, 4, 6, 8));
```

You cannot re-bind a previously bound identifier to a new function definition. For more, see [const](#const).

# if else

Reach supports conditional statements. Here is an example:

``` js nonum
if (!willBuy) {
  commit();
  each([S, B], () => interact.reportCancellation());
  exit();
} else {
  commit();
}
```

If one branch of a conditional contains `commit` or `return`, then the other must, too.

Identifiers bound within the scopes of the `if` or `else` sets of curly braces are not bound outside the scopes:

``` js nonum
if ( x < y ) {
  const z = 3;
}
else {
  const z = 4;
}
return z; // Invalid
```

A conditional statement may include a consensus transfer only when the statement occurs within a consensus step.

# import

# only

`each` and `only` statements transition from *step* mode to *local step* mode and then back to *step* mode. In other words, local steps occur within the body of `each` and `only` statements. Here is the declaration:

``` js nonum
each(PART_TUPLE () => BLOCK)
// and
PART.only(() => BLOCK)
```

* `PART_TUPLE` is a tuple of participants (e.g. `[Alice, Bob]`).
* `PART` is a participant.
* `BLOCK` stands for one or more local statements.

Here is an example:

``` js nonum
Alice.only(() => { const x = 3; });
// and 
each([Alice, Bob], () => { const pretzel = interact.random(); });
```

A local step statement is written PART.only(() => BLOCK), where PART is a participant identifier and BLOCK is a block. Within BLOCK, PART is bound to the address of the participant. Any bindings defined within the block of a local step are available in the statement’s tail as new local state. For example,

``` js nonum
Alice.only(() => { const x = 3; });
Alice.only(() => { const y = x + 1; }); 
```

is a valid program where Alice’s local state includes the private values x (bound to 3) and y (bound to 4). However, such bindings are not consensus state, so they are purely local state. For example,

``` js nonum
Alice.only(() => { const x = 3; });
Bob.only(() => { const y = x + 1; }); 
```

is an invalid program, because Bob does not know x.

The interact shorthand, written PART.interact.METHOD(EXPR_0, ..., EXPR_n), is available for calling an interact function from outside of an only block. Such functions must return Null; therefore, they are only useful if they produce side-effects, such as logging on the frontend. For example, the function log in the participant interact interface of Alice may be called via:

``` js nonum
Alice.interact.log(x);
```

An each local step statement can be written as each(PART_TUPLE () => BLOCK), where PART_TUPLE is a tuple of participants and BLOCK is a block. It is an abbreviation of many local step statements that could have been written with only.

# parallelReduce

# pay

See [publish](#publish).

# publish

A consensus transfer often involves one or more of the following expressions: `publish`, `pay`, `when`, and `timeout`. Here is the declaration:

``` js nonum
PART_EXPR.publish(ID_0, ..., ID_n)
  .pay(PAY_EXPR)
  .when(WHEN_EXPR)
  .timeout(DELAY_EXPR, () => TIMEOUT_BLOCK)
  // or
  .throwTimeout(DELAY_EXPR, THROWN_EXPR)
```

* `PART_EXPR` is an expression that evaluates to a participant or race expression.
* `ID_0` through `ID_n` are identifiers for the participants’s public local state.
* `PAY_EXPR` is a public expression evaluating to a [pay amount](/en/books/essentials/terminology/#pay-amount).
* `WHEN_EXPR` is a public expression evaluating to a boolean and determines if the consensus transfer takes place. 
* `DELAY_EXPR` is a public expression that depends on only consensus state and evaluates to a time argument.
* `TIMEOUT_BLOCK` is a timeout block, which will be executed after the delay. 
* `DELAY_EXPR` time argument passes without the participant executing this consensus transfer.

Here are examples:

``` js nonum
Alice.publish(wagerAmount)
  .pay(wagerAmount)
  .timeout(DELAY, () => {
    Bob.publish();
    commit();
    return false; 
  }); 

Alice.publish(wagerAmount)
  .pay(wagerAmount)
  .timeout(DELAY, () => closeTo(Bob, false)); 

Alice.publish(wagerAmount)
  .pay(wagerAmount)
  .timeout(false);
```

All of the expressions within a consensus transfer are evaluated in a pure context, which may not alter the state of the application. The PAY_EXPR, WHEN_EXPR, and DELAY_EXPR expressions must refer only to the consensus state, including the new data published via the .publish component.

The continuation of a consensus transfer statement is a consensus step, which is finalized with a commit statement. The continuation of a timeout block is the same as the continuation of the function the timeout occurs within.

See the guide section on non-participation to understand when to use timeouts and how to use them most effectively.

The publish component exclusive-or the pay component may be omitted, if either there is no publication or no transfer of network tokens to accompany this consensus transfer. The when component may always be omitted, in which case it is assumed to be true. publish or pay must occur first, after which components may occur in any order. For example, the following are all valid:

``` js nonum
Alice.publish(coinFlip);

Alice.pay(penaltyAmount);

Alice.pay(penaltyAmount).publish(coinFlip);

Alice.publish(coinFlip)
  .timeout(DELAY, () => closeTo(Bob, () => exit()));

Alice.pay(penaltyAmount)
  .timeout(DELAY, () => {
    Bob.publish();
    commit();
    exit(); 
  });

Alice.publish(bid).when(wantsToBid);
```

The timeout component must be included if when is not statically true. This ensures that your clients will eventually complete the program. If a consensus transfer is a guaranteed race between non-class participants and a participant class that may attempt to transfer (i.e. when is not statically false), then a timeout may be explicitly omitted by writing .timeout(false).

.throwTimeout may be used in place of .timeout. It accepts a DELAY_EXPR and an EXPR, which will be thrown if a timeout should occur. If an EXPR is not provided, then null will be thrown. If a consensus transfer uses .throwTimeout, it must be within a try statement.

If a consensus transfer specifies a single participant, which has not yet been fixed in the application and is not a participant class, then this statement does so; therefore, after it the PART may be used as an address.

If a consensus transfer specificies a single participant class, then all members of that class will attempt to perform the transfer, but only one will succeed.

A consensus transfer binds the identifiers ID_0 through ID_n for all participants to the values included in the consensus transfer, overwriting any bindings that already exist for those identifiers. If an existing participant, not included in PART_EXPR, has previously bound one of these identifiers, then the program is not valid. In other words, the following program is not valid:

``` js nonum
Alice.only(() => {const x = 1; });
Bob.only(() => {const x = 2; });
Claire.only(() => {const x = 3; });
race(Alice, Bob).publish(x);
commit();
```

because Claire is not included in the race. However, if we were to rename Claire’s x into y, then it would be valid, because although Alice and Bob both bind x, they participate in the race, so it is allowed. In the tail of this program, x is bound to either 1 or 2, i.e., either Alice or Bob’s value is overwritten. This overwriting applies even if Alice wins and Alice is a participant class, i.e., the value of x in the tail is guaranteed to be the single value that was agreed upon in the consensus.

# return

Reach supports the `return` statement which returns a value to the surrounding scope:

``` js nonum
function whoWinsBestOfThree(winCountA, winCountB, lastOutcome) {
  if (winCountA > winCountB) { return A_WINS; } 
  else if (winCountB > winCountA) { return B_WINS; } 
  else if (lastOutcome == B_QUITS) { return B_QUITS; } 
  else if (lastOutcome == A_QUITS) { return A_QUITS; } 
  else { return A_WINS; }
}
```

A `return` statement is a terminator statement, so it must be the last statement in the scope:

``` js nonum
function whoWinsBestOfThree(winCountA, winCountB, lastOutcome) {
  return 3;
  const x = 5; // Invalid
}
```

# setOptions

The `setOptions` statement overrides default application parameters.

``` js nonum
setOptions({ verifyArithmetic: true, connectors: [ETH, ALGO ] });
```

The options object may include the following:

|Option|Type|Default|
|-|-|-|
|`connectors`|`Array`|All available connectors|
|`verifyArithmetic`|`Boolean`|`false`|
|`verifyPerConnector`|`Boolean`|`false`|

`connectors` is an array of tuples indicating for which consensus networks the compiler will generate contract bytecode. 

`verifyArithmetic` is a boolean value indicating whether arithmetic operations introduce static assertions that they do not overflow beyond `UInt.max`. This defaults to false because it is onerous to verify. We recommend turning it on before final deployment, but leaving it off during development. When it is false, connectors will ensure that overflows do not actually occur on the network.

`verifyPerConnector` is a boolean value that determines whether verification is done per connector, or once for a generic connector. When this is true, then connector-specific constants, like UInt.max, will be instantiated to literal numbers. This concretization of these constants can induce performance degradation in the verifier.

# switch

Reach supports the `switch` statement which selects one of many code blocks based on a given expression:

``` js nonum
switch (x) {
  case 0: return 0;
  case 1: return 1;
  default: return fib(x - 1) + fib(x - 2);
}
```

Note the following:

* If one case of a `switch` contains a `return` statement, then all must.
* A `case` must appear only once.
* A `case` must be the same type as the evaluated expression.
* `switch` statements do not support `break` statements because a previous `case` does not fall through to the next.


# try catch throw

# var

Reach supports `var` only immediately before a while loop and its invariant:

``` js nonum
'reach 0.1';

export const main = Reach.App(
  {}, [Participant('A', {})], (A) => {
    A.publish();
    var [x] = [1];
    invariant(true);
    while(x < 2) {
      [ x, y ] = [ x + 1, x ];
      continue;
    }
    commit();
  }
);
```

# wait

A `wait` statement delays computation for a duration specified in terms of seconds or in terms of increments specific to the consensus network. Here is the declaration:

``` js nonum
wait(TIME)
```

See [Time Argument](/en/essentials/terminology/#time-argument).

# while

A `while` statement loops until the specified condition is `false`. Here is the declaration:

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

Here is an example:

``` js nonum
var [ heap1, heap2 ] = [ 21, 21 ];
{ const sum = () => heap1 + heap2; }
invariant(balance() == 2 * wagerAmount);
while ( sum() > 0 ) {
  ....
  [ heap1, heap2 ] = [ heap1 - 1, heap2 ];
  continue; }
```
