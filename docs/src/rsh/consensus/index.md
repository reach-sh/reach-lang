# {#ref-programs-consensus} Consensus Steps

A Reach consensus step occurs in the continuation of a consensus transfer statement.
It represents the actions taken by the consensus network contract of an application.

## {#ref-programs-consensus-stmts} Statements

Any statements valid for a [computation](##ref-programs-compute-stmts) are valid for a consensus step.
However, some additional statements are allowed.

### `commit`

@{ref("rsh", "commit")}
```reach
commit();
```

A @{defn("commit statement")}, written `{!rsh} commit();`, commits to statement's continuation as the next step of the DApp computation. In other words, it ends the current consensus step and allows more local steps.

For example, in the code below, the `{!rsh} commit();` on line 79 allows `Alice` to perform a local step after a consensus step:

```reach
load: /examples/rps-7-loops/index.rsh
md5: ee287e712cdfe8d91bbb038c383d25d3
range: 77 - 84
```

### {#ref-programs-only-consensus} `only` and `each`

@{seclink("ref-programs-only-step")} are allowed in consensus steps and are executed by backends once they observe the completion of the consensus step (i.e., after the associated commit statement.)

### {#ref-programs-consensus-view} View Objects

:::note
Views are [defined in application initialization](##ref-programs-appinit-view) in Reach.
They are [accessed by frontends](##ref-frontends-js-ctc) by using the Reach standard library of the frontend language, such as JavaScript.
This section is about defining the value of a view in your Reach program.
:::

```reach
vNFT.owner.set(creator);
```

If `{!rsh} VIEW` is a @{defn("view object")}, then its fields are the elements of the associated view.
Each of these fields are bound to an object with a `set` method that accepts the function or value to be bound to that view at the current step, and all steps dominated by the current step (unless otherwise overridden).
If this function is not provided with an argument, then the corresponding view is unset.

For example, consider the following program:

```
load: /examples/view-steps/index.rsh
md5: 78e1541e01ce0791b4b41d2bcd57aaa2
```

In this program, the Reach backend calls the frontend `{!rsh} interact` function, `{!rsh} checkView` with the expected value of the views at each point in the program.
The frontend compares that value with what is returned by
```js
[ await ctc.views().Main.last(),
  await ctc.views().Main.i() ]
```

When a view is bound to a function, it may inspect any values in its scope, including linear state.

### {#ref-programs-consensus-events} Event Objects

```reach
Logger.log(4, x);
```

If `{!rsh} EVENT` is an @{defn("event object")}, then its fields are the elements of the associated event.
Each of these fields are a function, whose domain is specified by the `{!rsh} Events` interface.

For example, consider the following program:

```reach
'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {
    getCtc: Fun([], Contract)
  });
  const E = Events('Announcer', {
    announce: [UInt, Contract],
  });
  init();

  A.publish();

  var [ i ] = [ 0 ];
  invariant(balance() == 0);
  while (true) {
    commit();
    A.only(() => {
      const ctc = declassify(interact.getCtc());
    });
    A.publish(ctc);

    E.announce(i, ctc);

    [ i ] = [ i + 1];
    continue;
  }

  commit();

});
```

In this program, there is an announcement made every loop; an event is emitted with the published
`{!rsh} ctc` and its corresponding index `{!rsh} i`.
A frontend may observe the values of these events with `{!js} await ctc.e.Announcer.announce.next()` or
`{!js} await ctc.e.Announcer.announce.monitor(announceHandler)` where `{!js} announceHandler` is a function.

### `Participant.set` and `.set`

@{ref("rsh", "Participant.set")}
```reach
Participant.set(PART, ADDR);
PART.set(ADDR);
```

 After execution, the given participant is fixed to the given address.
It is invalid to attempt to `{!rsh} .set` a participant class.
If a backend is running for this participant and its address does not match the given address, then it will abort.
This may only occur within a consensus step.

:::note
@{seclink("workshop-relay")} is a good introductory project that demonstrates how to use this feature of Reach.
:::

Example:

```reach
load: /examples/workshop-relay/index.rsh
md5: 56b6338c7902247ed5943735e96eed3b
range: 14 - 17
```

Alice first publishes the amount and the participant, `relay`, that will receive the amount, and then completes a `{!rsh} pay` action.
Then, the address of `Relay` is `{!rsh} set` in the consensus step, to make sure it is remembered by the consensus.
This code is also an assert that `relay` is the address of the participant `Relay`.

### `while`

@{ref("rsh", "while")}@{ref("rsh", "var")}@{ref("rsh", "invariant")}
```reach
var [ heap1, heap2 ] = [ 21, 21 ];
{ const sum = () => heap1 + heap2; }
invariant(balance() == 2 * wagerAmount);
while ( sum() > 0 ) {
  ....
  [ heap1, heap2 ] = [ heap1 - 1, heap2 ];
  continue; }
```

A @{defn("while statement")} may occur within a consensus step and is written:

```reach
var LHS = INIT_EXPR;
DEFINE_BLOCK; // optional
invariant(INVARIANT_EXPR, ?INVARIANT_MSG);
while( COND_EXPR ) BLOCK
```

where `{!rsh} LHS` is a valid left-hand side of an identifier definition where the expression `{!rsh} INIT_EXPR` is the right-hand side, and
`{!rsh} DEFINE_BLOCK` is an optional block that may define bindings that use the `{!rsh} LHS` values which are bound inside the rest of the `{!rsh} while` and its tail, and
`{!rsh} INVARIANT_EXPR` is an expression, called the @{defn("loop invariant")}, that must be true before and after every execution of the block `{!rsh} BLOCK`—it may be specified multiple times,
`INVARIANT_MSG` is an optional bytes argument, which is included in any reported violation, and
if `{!rsh} COND_EXPR` is true, then the block executes,
and if not, then the loop terminates and control transfers to the continuation of the while statement.
The identifiers bound by `{!rsh} LHS` are bound within `{!rsh} DEFINE_BLOCK`, `{!rsh} INVARIANT_EXPR`, `{!rsh} COND_EXPR`, `{!rsh} BLOCK`, and the tail of the while statement.

The identifier `{!rsh} this` is not bound within `{!rsh} DEFINE_BLOCK`, `{!rsh} INVARIANT_EXPR`, `{!rsh} COND_EXPR`, `{!rsh} BLOCK`, or after the `{!rsh} while`.
This is because it will almost never mean what you think it means: you may think it means the actor, but it actually means the previous actor.

:::note
Read about finding [loop invariants](##guide-loop-invs) in the Reach guide.
:::

```reach
load: /examples/chicken-race/index.rsh
md5: 2f62423e6b6ea82b9c1c89cba69104a1
range: 43-64
```

### `continue`

@{ref("rsh", "continue")}
```reach
[ heap1, heap2 ] = [ heap1 - 1, heap2 ];
continue;
```

A @{defn("continue statement")} may occur within a while statement's block and is written:

```reach
LHS = UPDATE_EXPR;
continue;
```

where the identifiers bound by `{!rsh} LHS` are a subset of the variables bound by the nearest enclosing while statement and `{!rsh} UPDATE_EXPR` is an expression which may be bound by `{!rsh} LHS`.

A continue statement is a terminator statement, so it must have an empty tail.

A continue statement may be written without the preceding identifier update, which is equivalent to writing

```reach
[] = [];
continue;
```

A continue statement must be dominated by a consensus transfer, which means that the body of a while statement must always `{!rsh} commit();` before calling `{!rsh} continue;`.
This restriction may be lifted in future versions of Reach, which will perform termination checking.

---

As a special case, a continue statement may occur in a step, if the `{!rsh} UPDATE_EXPR` transitions to a consensus step.
In other words, this is a valid program:
```reach
const f = () => {
 commit();
 A.publish();
 return 1;
};

var x = 0;
invariant(balance() == 0);
while ( x == 0 ) {
 x = f();
 continue;
}
```

The following example displays how a `{!rsh} continue` is used inside of a `{!rsh} while` loop that started in line 43.
Bob and Alice race to be the first to `{!rsh} publish` and therefore be the round winner.
When one of them publishes, the `keepGoing` function returns false, and the program hits `{!rsh} continue` where it exits the `{!rsh} race` and moves on with the rest of the program.

```reach
load: /examples/chicken-race/index.rsh
md5: 2f62423e6b6ea82b9c1c89cba69104a1
range: 53-58
```

### `parallelReduce`

@{ref("rsh", "parallelReduce")}
```reach
const [ keepGoing, as, bs ] =
  parallelReduce([ true, 0, 0 ])
  .invariant(balance() == 2 * wager)
  .while(keepGoing)
  .case(Alice, (() => ({
    when: declassify(interact.keepGoing()) })),
    (_) => {
      each([Alice, Bob], () => {
        interact.roundWinnerWas(true); });
      return [ true, 1 + as, bs ]; })
  .case(Bob, (() => ({
    when: declassify(interact.keepGoing()) })),
    (_) => {
      each([Alice, Bob], () => {
        interact.roundWinnerWas(false); });
      return [ true, as, 1 + bs ]; })
  .timeout(deadline, () => {
    showOutcome(TIMEOUT)();
    race(Alice, Bob).publish();
    return [ false, as, bs ]; });
```

:::note
If you're unsure of what kind of consensus transfer to use, you may want to read the [explanation of the differences](##guide-ctransfers) in the Guide.
:::

A @{defn("parallel reduce statement")} is written:

@{ref("rsh", "parallelReduce.define")}@{ref("rsh", "parallelReduce.invariant")}@{ref("rsh", "parallelReduce.while")}@{ref("rsh", "parallelReduce.paySpec")}@{ref("rsh", "parallelReduce.case")}@{ref("rsh", "parallelReduce.api")}@{ref("rsh", "parallelReduce.api_")}@{ref("rsh", "parallelReduce.timeout")}
```reach
const LHS =
  parallelReduce(INIT_EXPR)
  .define(() => DEFINE_BLOCK)
  .invariant(INVARIANT_EXPR, ?INVARIANT_MSG)
  .while(COND_EXPR)
  .paySpec(TOKENS_EXPR)
  .case(PART_EXPR,
    CHECK_EXPR,
    PUBLISH_EXPR,
    PAY_EXPR,
    CONSENSUS_EXPR)
  .api(API_EXPR,
    ASSUME_EXPR,
    PAY_EXPR,
    CONSENSUS_EXPR)
  .api_(API_EXPR,
    CHECKED_CONSENSUS_EXPR)
  .timeout(DELAY_EXPR, () =>
    TIMEOUT_BLOCK);
```

The `{!rsh} LHS` and `{!rsh} INIT_EXPR` are like the initialization component of a `{!rsh} while` loop; and,
the `{!rsh} .invariant` and `{!rsh} .while` components are like the invariant and condition of a `{!rsh} while` loop;
multiple `.invariant`s may be specified;
the `{!rsh} DEFINE_BLOCK` is like the `{!rsh} DEFINE_BLOCK` of a `{!rsh} while` loop. It may be specified multiple times;
while the `{!rsh} .case`, `{!rsh} .api`, `{!rsh} .api_`, `{!rsh} .timeout`, and `{!rsh} .paySpec` components are like the corresponding components of a `{!rsh} fork` statement.

The `{!rsh} .case` component may be repeated many times, just like in a `{!rsh} fork` statement.

The `{!rsh} .define` component may define bindings that reference the `{!rsh} LHS` values. These bindings are accessible
from every component of the `{!rsh} parallelReduce` statement, except for the `{!rsh} INIT_EXPR`.

Like a `{!rsh} while`, the `{!rsh} this` variable is not bound anywhere except for `{!rsh} INIT_EXPR`.

#### `.timeRemaining`

When dealing with absolute deadlines in `{!rsh} parallelReduce`, there is a common pattern in the
`{!rsh} TIMEOUT_BLOCK` to have participants `{!rsh} race` to `{!rsh} publish` and return the accumulator.
There is a shorthand, `{!rsh} .timeRemaining`, available for this situation:

@{ref("rsh", "parallelReduce.timeRemaining")}
```reach
const [ timeRemaining, keepGoing ] = makeDeadline(deadline);
const [ x, y, z ] =
  parallelReduce([ 1, 2, 3 ])
    .while(keepGoing())
    ...
    .timeRemaining(timeRemaining())
```

which will expand to:

```reach
.timeout(timeRemaining(), () => {
  race(...Participants).publish();
  return [ x, y, z ]; })
```

#### `.throwTimeout`

`{!rsh} .throwTimeout` is a shorthand that will throw the accumulator as an exception when a timeout occurs.
Therefore, a `{!rsh} parallelReduce` that uses this branch must be inside of a try statement. For example,

@{ref("rsh", "parallelReduce.throwTimeout")}
```reach
try {
  const [ x, y, z ] =
    parallelReduce([ 1, 2, 3 ])
    ...
    .throwTimeout(deadline)
} catch (e) { ... }
```

 will expand `{!rsh} throwTimeout` to:

```reach
.timeout(deadline, () => {
  throw [ x, y, z ]; })
```

#### `parallelReduce` intuition

A parallel reduce statement is essentially an abbreviation of pattern of a `{!rsh} while` loop combined with a `{!rsh} fork` statement that you could write yourself.
This is an extremely common pattern in decentralized applications.

The idea is that there are some values (the `{!rsh} LHS`) which after intialization will be repeatedly updated uniquely by each of the racing participants until the condition does not hold.

```reach
var LHS = INIT_EXPR;
DEFINE_BLOCK;
invariant(INVARIANT_EXPR);
while(COND_EXPR) {
  fork()
  .case(PART_EXPR,
    CHECK_EXPR,
    PUBLISH_EXPR,
    PAY_EXPR,
    (m) => {
      LHS = CONSENSUS_EXPR(m);
      continue; })
  .timeout(DELAY_EXPR, () =>
    TIMEOUT_BLOCK);
}
```

## {#ref-programs-consensus-exprs} Expressions

Any expressions valid for a [computation](##ref-programs-compute-exprs) are valid for a consensus step.
However, some additional expressions are allowed.

### {#ref-programs-consensus-this} `this`

Inside of a consensus step, `{!rsh} this` refers to the address of the participant that performed the consensus transfer.
This is useful when the consensus transfer was initiated by a `{!rsh} race` expression.

### `transfer`

```reach
transfer(10).to(Alice);
transfer(2, gil).to(Alice);
transfer([1, [2, gil]]).to(Alice);
```

@{ref("rsh", "transfer")}@{ref("rsh", "transfer.to")}
A @{defn("transfer expression")},
written `{!rsh} transfer(PAY_AMOUNT_EXPR).to(ADDR_EXPR)`,
where `{!rsh} PAY_AMOUNT_EXPR` is an expression that evaluates to a [pay amount](##payAmt), and
`{!rsh} ADDR_EXPR` evaluates to an address,
performs a transfer of network tokens or non-network tokens from the contract to the named participant.
The amount transfered must evaluate to less than or equal to the balance of the network and non-network tokens in the contract account.

A transfer expression may only occur within a consensus step.

```reach
load: /examples/simple-nft-auction/index.rsh
md5: 1f95425a9feb9f5bf1e677461a83df4e
range: 60-61
```

The first `{!rsh} transfer` example from the Simple NFT Auction pays the `lastPrice` to the NFT `Creator`.
The second `{!rsh} transfer` example gives the sale quantity of the NFT to the `{!rsh} Participant` that bid the highest price.

### `require`

@{ref("rsh", "require")}
```reach
require( claim, [msg] )
```

 A requirement where `{!rsh} claim` evaluates to `{!rsh} true` with honest participants.
This may only appear in a consensus step.
It accepts an optional bytes argument, which is included in any reported violation.

If a publication would violate the requirement, the consensus network rejects the transaction.

``` reach
load: /examples/abstract-tok/index.rsh
md5: aec14ab8610b29232a48b12841c09730
range: 20-22
```

The example above has Alice publish two tokens named `token1` and `token2` and the `{!rsh} require` statement checks if they are different tokens.
If they are not different tokens, then the transaction is rejected.

### `checkCommitment`

@{ref("rsh", "checkCommitment")}
```reach
checkCommitment( commitment, salt, x )
```

 Makes a requirement that `{!rsh} commitment` is the digest of `{!rsh} salt` and `{!rsh} x`.
This is used in a consensus step after `{!rsh} makeCommitment` was used in a local step.

The example below shows `{!rsh} checkCommitment` being used in a consensus step on line 87 after `{!rsh} makeCommitment` was used in a local step on line 66:

```reach
load: /examples/rps-7-loops/index.rsh
md5: ee287e712cdfe8d91bbb038c383d25d3
range: 64 - 68
```

```reach
load: /examples/rps-7-loops/index.rsh
md5: ee287e712cdfe8d91bbb038c383d25d3
range: 85 - 87
```

### {#ref-programs-consensus-token-minting} Token minting

@{ref("rsh", "burn")}@{ref("rsh", "Token.burn")}@{ref("rsh", "destroy")}@{ref("rsh", "Token.destroy")}@{ref("rsh", "supply")}@{ref("rsh", "Token.supply")}@{ref("rsh", "destroyed")}@{ref("rsh", "Token.destroyed")}@{ref("rsh", "Token.track")}@{ref("rsh", "Token.accepted")}
```reach
require(supply >= 2 * amt);
const tok = new Token({ name, symbol, url, metadata, supply, decimals });
transfer(amt, tok).to(who);
tok.burn(amt);
assert(tok.supply() == supply - amt);
tok.burn();
assert(tok.destroyed() == false);
tok.destroy();

const [ [tok2], k ] = call(API.getToken());
Token.track(tok2);
```

:::note
@{seclink("ref-networks")} discusses how Reach supports token minting on specific consensus networks.
:::

We refer to creation of a new non-network token as @{defn("token minting")}.
It is written with the expression `{!rsh} new Token(PARAMS)`, where `{!rsh} PARAMS` is an object with the following keys:
+ `name`: A value of type `{!rsh} Bytes(32)`; defaults to empty.
+ `symbol`: A value of type `{!rsh} Bytes(8)`; defaults to empty.
+ `url`: A value of type `{!rsh} Bytes(96)`; defaults to empty.
+ `metadata`: A value of type `{!rsh} Bytes(32)`; defaults to empty.
This value is intended to be a digest of a larger metadata document.
+ `supply`: A value of type `{!rsh} UInt`; defaults to `{!rsh} UInt.max`.
+ `decimals`: A value of type `{!rsh} UInt`; defaults to `{!rsh} 6` on Algorand, and `{!rsh} 18` on Ethereum and Conflux.

The following examples demonstrate how the details above may be used:

```reach
load: /examples/token-decimals/index.rsh
md5: 0b9ee900d1bd566dcc7383749d770ab1
range: 14 - 17
```

```reach
load: /examples/mint-basic/index.rsh
md5: 1ccb4872438f69d9c8af711b08d817a4
range: 28 - 33
```

This returns a `{!rsh} Token` value and deposits a `{!rsh} supply` amount of the new non-network tokens into the contract account associated with the DApp.
These tokens must be destroyed by the end of the DApp.

:::note
Reach assumes that network tokens and non-network tokens behave identically, but often they do not; [this article](##guide-nntoks) discusses the causes and consequences of this.
:::

---

`{!rsh} Token.burn(tok, amt?)`, or `{!rsh} tok.burn(amt?)`, where `{!rsh} tok` is a `{!rsh} Token` value and `{!rsh} amt` is a `{!rsh} UInt` value, may be used to @{defn("burn")} tokens in the contract account, meaning that they are utterly destroyed and can never be recovered. 
If `{!rsh} amt` is not given, the current balance of the token will be used.

---

`{!rsh} Token.destroy(tok)`, or `{!rsh} tok.destroy()`, where `{!rsh} tok` is a `{!rsh} Token` value, may be used to destroy the token so that it may never be used again by any users on the consensus network.
This must be called before the application exits.

---

`{!rsh} Token.destroyed(tok)`, or `{!rsh} tok.destroyed()`, where `{!rsh} tok` is a `{!rsh} Token` value, returns whether `{!rsh} destroy`
has been called on `{!rsh} tok` yet.

---

`{!rsh} Token.supply(tok)`, or `{!rsh} tok.supply()`, where `{!rsh} tok` is a `{!rsh} Token` value, may be used to query the current supply of tokens, i.e. the number of tokens which have not been burnt.

---

`{!rsh} Token.track(tok)`, or `{!rsh} tok.track()`, where `{!rsh} tok` is a `{!rsh} Token` value, may be used to explicitly start tracking the balance of a non-network token in an application.
This operation is useful for tracking a token that is received from an API call or published within a container, like an `{!rsh} Array` or `{!rsh} Data` object.

---

`{!rsh} Token.accepted(acc, tok)`, where `{!rsh} acc` is an `{!rsh} Account` value and `{!rsh} tok` is a `{!rsh} Token` value, returns true if `{!rsh} acc` is ready to accept non-network tokens specified by `{!rsh} tok`, and false otherwise.
This always returns true on some consensus networks, but could be useful on others where accounts need to explicitly agree to receive non-network tokens.

### Remote objects

@{ref("rsh", "remote")}
```reach
const randomOracle =
  remote( randomOracleCtcInfo, {
    getRandom: Fun([], UInt),
  });
const randomVal = randomOracle.getRandom.pay(randomFee)();
```

:::note
@{seclink("ref-networks")} discusses how Reach supports remote objects on specific consensus networks.
:::

A @{defn("remote object")} represents a foreign contract in a Reach application.
During a consensus step, a Reach computation may consensually communicate with such an object via a prescribed interface.

A remote object is constructed by calling the `{!rsh} remote` function with a `{!rsh} Contract`, an interface---an object where each key is bound to a function type, and an optional object of aliases.
The alias object maps function names from the interface to function names on the remote contract.
It allows users to bind specific instances of an overloaded remote function.
For example:
```reach
const randomOracle =
  remote( randomOracleCtcInfo, {
    getRandom: Fun([], UInt),
    getRandom1: Fun([UInt], UInt),
  }, {
    getRandom: "random",
    getRandom1: "random",
  });
const token =
  remote( tokenCtcInfo, {
    balanceOf: Fun([Address], UInt),
    transferTo: Fun([UInt, Address], Null),
  });
```

In this example, the random oracle contract has an overloaded method, `random`.
This `random` method accepts 0 or 1 parameters.
We explicitly specify that `getRandom` refers to calling the `random` function with no arguments, and `getRandom1` refers to calling the `random` function with 1 argument.

@{ref("rsh", "REMOTE_FUN")}
Once constructed, the fields of a remote object represent those remote contract interactions, referred to as @{defn("remote functions")}.
For example, `{!rsh} randomOracle.getRandom`, `{!rsh} token.balanceOf`, and `{!rsh} token.transferTo` are remote functions in the example.

A remote function may be invoked by calling it with the appropriate arguments, whereupon it returns the specified output.
In addition, a remote function may be augmented with one of the following operations:

+ `{!rsh} REMOTE_FUN.pay(AMT)` ---
  Returns a remote function that receives a pay amount, `{!rsh} AMT`, _from_ the caller when it is called.
+ @{ref("rsh", "bill")} `{!rsh} REMOTE_FUN.bill(AMT)` ---
  Returns a remote function that provides a pay amount, `{!rsh} AMT`, _to_ the caller when it returns.
+ @{ref("rsh", "withBill")} `{!rsh} REMOTE_FUN.withBill()` ---
  Returns a remote function that provides some number of network tokens and, possibly, non-network tokens _to_ the caller when it returns.
  The exact amount is returned from the invocation by wrapping the original result in a tuple.
+ @{ref("rsh", "remote.ALGO")} `{!rsh} REMOTE_FUN.ALGO(opts)` ---
  Returns a remote function that records the need for additional resources on Algorand.
  + `{!rsh} opts.fees` records extra fees.
    If this is needed, and not included, then the consensus transfer to the current consensus step will fail with an insufficient fee error.
  + `{!rsh} opts.accounts` records extra accounts.
    If this is needed, and not included, then the consensus transfer to the current consensus step will fail with an invalid account reference.
  + `{!rsh} opts.assets` records extra assets.
    If this is needed, and not included, then the consensus transfer to the current consensus step will fail with an invalid asset reference.
  + `{!rsh} opts.apps` records extra applications.
    If this is needed, and not included, then the consensus transfer to the current consensus step will fail with an invalid application reference.
  + `{!rsh} opts.addressToAccount` changes `{!rsh} Address` arguments to `Account` array references.
    If this is needed, and not included, then the consensus transfer to the current consensus step will fail because an incorrectly typed argument was provided to the remote object.
  + `{!rsh} opts.onCompletion` sets the `OnCompletion` field of the transaction.
    The value is a string for the field value; e.g., `{!rsh} 'DeleteApplication'`.
    It is dangerous to call this with `OptIn`, because the minimum balance of the calling contract will increase and Reach cannot track that, because it is not statically available.
    The minimum balance will decrease on a `CloseOut` or `ClearState` transaction.
  + `{!rsh} opts.strictPay` does not optimize away `pay` or `axfer` transactions that are statically zero.
    If this is needed, and not included, then `{!rsh} remote` calls that require a payment transaction to always be present, even if the amount is zero, could fail.
  + `{!rsh} opts.rawCall` is a boolean (default `{!rsh} false`) that when `{!rsh} true` omits the ABI method selector from the call.
  + `{!rsh} opts.simNetRecv` is an integer field (default `{!rsh} 0`).
    The field represents how many network tokens Reach will assume your contract received from the remote call, for the purposes of transaction simulation.
    This is useful when using `{!rsh} withBill` and `{!rsh} enforce` to enforce that your contract received a certain number network tokens from the remote call.
  + `{!rsh} opts.simTokensRecv` is a tuple-of-integers field (default all zeros).
    The field represents how many of each non-network token Reach will assume your contract received from the remote call, for the purposes of transaction simulation.
    Amounts must be specified in the same order as in the call to `{!rsh} bill` or `{!rsh} withBill`.
    This is useful when using `{!rsh} withBill` and `{!rsh} enforce` to enforce that your contract received a certain number non-network tokens from the remote call.
  + `{!rsh} opts.simReturnVal` is a field whose type must match the returned type of the remote function (default depends on return type).
    The field represents the return value of the remote function, for the purposes of transaction simulation.
    This is useful when using `{!rsh} enforce` on the return value of the remote function.

If the remote contract is not expected to return non-network tokens then a pair is returned, where the amount of network tokens received is the first element, and the original result is the second element.

If the remote contract is expected to return non-network tokens then a triple is returned, where the amount of network tokens received
is the first element, a tuple of the non-network tokens received is the second element, and the original result is the third element.
If the caller expects to receive non-network tokens, they must provide a tuple of tokens as an argument to `{!rsh} withBill`.
The ordering of tokens in the argument is preserved when returning the amounts received.
For example,

```reach
const [ netRecv, [gilRecv, zmdRecv], randomValue ] =
  randomOracle.getRandom.pay(stipend).withBill([gil, zmd])();
```

might be the way to communicate with a random oracle that receives a conservative approximation of its actual cost and returns what it does not use, along with some amount of network tokens, `GIL`, and `ZMD`.
This operation may not be used with `{!rsh} REMOTE_FUN.bill`.

### Contract.fromAddress
@{ref("rsh", "Contract.fromAddress")}

```reach
const ctcMaybe = Contract.fromAddress(address);
```

Takes an `{!rsh} Address}` and returns a `{!rsh} Maybe(Contract)`.

On Algorand this always returns `{!rsh} None`.
On Ethereum and Conflux, it returns `{!rsh} None` for addresses of externally owned accounts, and it returns `{!rsh} Some` for addresses of contracts under most circumstances.
However, it also returns `{!rsh} None` for addresses of contracts that are under construction, for addresses of contracts that have been destroyed, and for addresses of contracts that have not yet been created.


### Mappings: creation and modification

@{ref("rsh", "Map")}
```reach
const bidsM = new Map(Address, UInt);
       // or `new Map(UInt);`
bidsM[this] = 17;
delete bidsM[this];
```

A new mapping of linear state may be constructed in a consensus step by writing `{!rsh} new Map(VAL_TYPE_EXPR)` or `{!rsh} new Map(KEY_TYPE_EXPR, VAL_TYPE_EXPR)`, where `{!rsh} KEY_TYPE_EXPR` and `{!rsh} VAL_TYPE_EXPR` are types.
If `{!rsh} KEY_TYPE_EXPR` is not specified, it will default to `{!rsh} Address`.

For example the code below contains only the `{!rsh} VAL_TYPE_EXPR`:
```reach
load: /examples/splice1/index.rsh
md5: 944291e790336dbb2b87784588d60eeb
range: 7 - 7
```

While the following code contains both the `{!rsh} KEY_TYPE_EXPR` and `{!rsh} VAL_TYPE_EXPR`:
```reach
load: /examples/map-arbitrary-key/index.rsh
md5: defd89926eddaf8f3a24ec9d2d4c9839
range: 24 - 24
```

These examples return a value which may be used to dereference particular mappings via `{!rsh} map[EXPR]`.
Such dereferences return a value of type `{!rsh} Maybe(VAL_TYPE_EXPR)`, because the mapping may not contain a value for `{!rsh} EXPR`.

A mapping may be modified by writing `{!rsh} map[EXPR] = VALUE_EXPR` to install `{!rsh} VALUE_EXPR` (of type `{!rsh} VAL_TYPE_EXPR`) at `{!rsh} EXPR`, or by writing `{!rsh} delete map[EXPR]` to remove the mapping entry.
Such modifications may only occur in a consensus step.

N+2 relations can be created by using a `{!rsh} Tuple` as the Map key.
For example, a nested mapping: `{!rsh} Map(Address, Map(Address, UInt))`, can be simulated by coalescing the Map keys into a `{!rsh} Tuple`: `{!rsh} Map(Tuple(Address, Address), UInt)`.

### Sets: creation and modification

@{ref("rsh", "Set")}@{ref("rsh", "Set.insert")}@{ref("rsh", "Set.remove")}@{ref("rsh", "Set.member")}@{ref("rsh", "Set.Map")}
```reach
const bidders = new Set();
bidders.insert(Alice);
bidders.Map.size(); // 1
bidders.remove(Alice);
bidders.member(Alice); // false
bidders.Map.size(); // 0
```

A `{!rsh} Set` is another container for linear state. It is simply a type alias of `{!rsh} Map(Null)`;
it is only useful for tracking `{!rsh} Address`es. Because a `{!rsh} Set` is internally a `{!rsh} Map`, it may
only be constructed in a consensus step.

A `{!rsh} Set` may be modified by writing `{!rsh} s.insert(ADDRESS)` to install `{!rsh} ADDRESS` in the
set, `{!rsh} s`, or `{!rsh} s.remove(ADDRESS)` to remove the `{!rsh} ADDRESS` from the set.
Such modifications may only occur in a consensus step.

The following example shows the usage of `{!rsh} s.insert(ADDRESS)`:

```reach
load: /hs/t/y/merit-badge.rsh
md5: 2186c788d7b38a8b4f222960a77586d9
range: 46 - 48
```

While the example below shows the usage of  `{!rsh} s.remove(ADDRESS)`

```reach
load: /examples/dominant-assurance/index.rsh
md5: d327454b582bdfa6f03d71de5ce2dd97
range: 144 - 144
```

To check whether an address is in the set, `{!rsh} s.member(ADDRESS)` is used.

The following example shows the usage of `{!rsh} s.member(ADDRESS)`

```reach
load: /examples/dominant-assurance/index.rsh
md5: d327454b582bdfa6f03d71de5ce2dd97
range: 99 - 99
```

`{!rsh} s.Map` will return the underlying `{!rsh} Map`, so you can use foldable
instance methods.

### {#ref-programs-consensus-new-contract} Contract creation

@{ref("rsh", "new Contract")}@{ref("rsh", "Contract.new")}
Reach programs can create new child contracts based on predetermined, static code during compile time.
This code is specified with `{!rsh} ContractCode`, which you can read about in @{seclink("ref-programs-appinit-contractcode")}.

Given some child contract code, indicated by the variable `{!rsh} cc`, you can create a constructor function with:

```reach
const ctor = new Contract(cc, opts?)
```

where `opts` is an optional object.
Each enabled connector accepts its own options in a field with the name of the connector.

The `{!rsh} ETH` connector accepts no options.

The `{!rsh} ALGO` connector accepts the keys:
  + `{!rsh} opts.globalUints` --- The number of unsigned integers in the global storage of the contract.
  + `{!rsh} opts.globalBytes` --- The number of byte strings in the global storage of the contract.
  + `{!rsh} opts.localUints` --- The number of unsigned integers in the local storage of the contract.
  + `{!rsh} opts.localBytes` --- The number of byte strings in the local storage of the contract.

If these options are required, but not present, then the contract will behave incorrectly.

The constructor function must be called to actually create the contract.
It is like a `{!rsh} REMOTE_FUN}`, but it cannot be augmented, nor can it receive payment.
It returns a `{!rsh} Contract` value for the newly created contract.

:::note
Why can't you augment constructor calls?

Some constructor augmentations, like those specific for a particular network, could work, but are not supported by Reach presently.
Others, such as token payments, cannot work at all, because given that the contract doesn't exist yet, there's no way to know where to send the tokens to.
:::

On some connectors, like `{!rsh} ALGO`, it is necessary to delete child contracts before the parent exits.
Reach does not yet enforce this property during verification, so if you fail to obey it, then your program will not be able to finish.
