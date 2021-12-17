---
menuItem: mi-docs
---

# Reach Expressions

See [Reach Types](/en/essentials/backend-programming/reach-types/) for expressions related to specific Reach types.

For arithmetic expressions, see [Array](/en/essentials/backend-programming/reach-types/#array), [FixedPoint](/en/essentials/backend-programming/reach-types/#fixedpoint), [Int](/en/essentials/backend-programming/reach-types/#int), [Interval](/en/essentials/backend-programming/reach-types/#interval), and [UInt](/en/essentials/backend-programming/reach-types/#uint). See also [Reach Operators](/en/essentials/backend-programming/reach-operators/).

# Anybody

# Arrow Expression

# Assertion Expressions

### assert

### assume

### fail

The `fail` expression is equivalent to `assume(false)`.

### false

### require

### unknowable

The `unknowable` expression asserts that one participant does not know certain values while another participant does. Here is the declaration:

``` js nonum
unknowable( Notter, Knower(var_0, ..., var_N), [msg] )
```

* `Notter` is the participant that is asserted not to know.
* `Knower` is the participant that is asserted to know.
* `var_0, ..., var_N` are the values not known and known.
* `msg` is a byte array included in any reported violation.

# call

A `call` expression invokes the specified API function. Below is the declaration:

``` js nonum
const [ DOMAIN, RET_FUN ] = call(API_EXPR)
  .pay(API_PAY_EXPR)
  .assume(API_ASSUME_EXPR)
  .throwTimeout(DELAY_EXPR, THROW_EXPR)
```

* `API_EXPR` is an expression that evaluates to an API member function.
* `pay`, `assume`, and `throwTimeout` are optional. See [Fork](#fork).
* The return value is `[ DOMAIN, RET_FUN ]` where `DOMAIN` is specific to the function, and `RET_FUN` is a function that *must be called* to return a value to the API function.

Here is an example:

``` js nonum
const A = API('A', { isGt: Fun([UInt, UInt], Bool); });
// ...
const [ dom, k ] = call(A.isGt).assume((x, y) => x != y).pay((x, y) => x);
const [x, y] = dom;
k(x > y);
commit();
```

# checkCommitment

# closeTo

The `closeTo` expression causes the specified participant to publish, receive a `transfer` of the contract balance and the [pay amount](/en/books/essentials/terminology/#pay-amount), and then exit. Below is the declaration:

``` js nonum
closeTo( Who, after, nonNetPayAmt ) 
```

* `Who` is the participant.
* `after` is a no-argument function called before exit. It is optional.
* `nonNetPayAmt`. See [Pay Amount](/en/books/essentials/terminology/#pay-amount). It is optional.

Here is an example:

``` js nonum
const aliceInteract = {
  informTimeout: Fun([], Null)
};

B.pay(wager).timeout(DEADLINE, () => closeTo(A, informTimeout));
```

# compose

# declassify

# didPublish

# ensure

# forall

# hasConsoleLogger

# hasRandom

# implies

# interact

# Literal Expressions

### unstrict

### use strict

# makeCommitment

# makeEnum

# match

# new

# possible

# race

A `race` expression organizes a competition among several participants (each intent on accomplishing the same task), determines which participant wins the competition, and executes task for the winner:

``` js nonum
race(Alice, Bob).publish(bet);
```

# this

Inside a local step, `this` refers to the participant performing the step:

``` js
// index.rsh
const commonInteract = { reportAddress: Fun([Address], Null) };
S.only(() => { 
  interact.reportAddress(this); 
  interact.reportAddress(getAddress());
});
```

* Line 4: `reportAddress` passes the account address of Participant `S` to the frontend.
* Line 5: `reportAddress` passes the account address of the contract to the frontend.

Inside a consensus step, `this` refers to the address of the participant that performed the consensus transfer, especially useful when the consensus transfer was initiated by a race expression:

``` js 
race(S, B).publish();
const winner = this;
```

* Line 1: `publish` transitions to a consensus step.
* Line 2: `this` is the address of the `race` winner (Participant `S` or Participant `B`).

# Time Expressions

### absoluteSecs

### absoluteTime

### baseWaitSecs

### baseWaitTime

### lastConsensusSecs

### lastConsensusTime

### makeDeadline

### relativeSecs

### relativeTime

### throughTimeout

### timeRemaining
