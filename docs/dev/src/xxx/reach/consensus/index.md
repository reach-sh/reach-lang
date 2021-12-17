



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


A @{defn("commit statement")}, written `{!reach} commit();`, commits to statement's continuation as the next step of the DApp computation. In other words, it ends the current consensus step and allows more local steps.

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


If `{!reach} VIEW` is a @{defn("view object")}, then its fields are the elements of the associated view.
Each of these fields are bound to an object with a `set` method that accepts the function or value to be bound to that view at the current step, and all steps dominated by the current step (unless otherwise overridden).
If this function is not provided with an argument, then the corresponding view is unset.

For example, consider the following program:

```
load: /examples/view-steps/index.rsh
```


In this program, the Reach backend calls the frontend `{!reach} interact` function, `{!reach} checkView` with the expected value of the views at each point in the program.
The frontend compares that value with what is returned by
```js
[ await ctc.getViews().Main.last(),
  await ctc.getViews().Main.i() ]
```


When a view is bound to a function, it may inspect any values in its scope, including linear state.

### {#ref-programs-consensus-events} Event Objects

```reach
Logger.log(4, x);
```


If `{!reach} EVENT` is an @{defn("event object")}, then its fields are the elements of the associated event.
Each of these fields are a function, whose domain is specified by the `{!reach} Events` interface.

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
  deploy();

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
`{!reach} ctc` and its corresponding index `{!reach} i`.
A frontend may observe the values of these events with `{!js} await ctc.e.Announcer.announce.next()` or
`{!js} await ctc.e.Announcer.announce.monitor(announceHandler)` where `{!js} announceHandler` is a function.


### `Participant.set` and `.set`

@{ref("rsh", "Participant.set")}
```reach
Participant.set(PART, ADDR);
PART.set(ADDR); 
```


 After execution, the given participant is fixed to the given address.
It is invalid to attempt to `{!reach} .set` a participant class.
If a backend is running for this participant and its address does not match the given address, then it will abort.
This may only occur within a consensus step.

:::note
@{seclink("workshop-relay")} is a good introductory project that demonstrates how to use this feature of Reach.
:::


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
invariant(INVARIANT_EXPR);
while( COND_EXPR ) BLOCK 
```


where `{!reach} LHS` is a valid left-hand side of an identifier definition where the expression `{!reach} INIT_EXPR` is the right-hand side, and
`{!reach} DEFINE_BLOCK` is an optional block that may define bindings that use the `{!reach} LHS` values which are bound inside the rest of the `{!reach} while` and its tail, and
`{!reach} INVARIANT_EXPR` is an expression, called the @{defn("loop invariant")}, that must be true before and after every execution of the block `{!reach} BLOCK`, and
if `{!reach} COND_EXPR` is true, then the block executes,
and if not, then the loop terminates and control transfers to the continuation of the while statement.
The identifiers bound by `{!reach} LHS` are bound within `{!reach} DEFINE_BLOCK`, `{!reach} INVARIANT_EXPR`, `{!reach} COND_EXPR`, `{!reach} BLOCK`, and the tail of the while statement.

:::note
Read about finding [loop invariants](##guide-loop-invs) in the Reach guide.
:::


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


where the identifiers bound by `{!reach} LHS` are a subset of the variables bound by the nearest enclosing while statement and `{!reach} UPDATE_EXPR` is an expression which may be bound by `{!reach} LHS`.

A continue statement is a terminator statement, so it must have an empty tail.

A continue statement may be written without the preceding identifier update, which is equivalent to writing

```reach
[] = [];
continue; 
```


A continue statement must be dominated by a consensus transfer, which means that the body of a while statement must always `{!reach} commit();` before calling `{!reach} continue;`.
This restriction may be lifted in future versions of Reach, which will perform termination checking.

---

As a special case, a continue statement may occur in a step, if the `{!reach} UPDATE_EXPR` transitions to a consensus step.
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

@{ref("rsh", "define")}
```reach
const LHS =
  parallelReduce(INIT_EXPR)
  .define(() => DEFINE_BLOCK)
  .invariant(INVARIANT_EXPR)
  .while(COND_EXPR)
  .paySpec(TOKENS_EXPR)
  .case(PART_EXPR,
    PUBLISH_EXPR,
    PAY_EXPR,
    CONSENSUS_EXPR)
  .api(API_EXPR,
    ASSUME_EXPR,
    PAY_EXPR,
    CONSENSUS_EXPR)
  .timeout(DELAY_EXPR, () =>
    TIMEOUT_BLOCK);
```


The `{!reach} LHS` and `{!reach} INIT_EXPR` are like the initialization component of a `{!reach} while` loop; and,
the `{!reach} .invariant` and `{!reach} .while` components are like the invariant and condition of a `{!reach} while` loop;
the `{!reach} DEFINE_BLOCK` is like the `{!reach} DEFINE_BLOCK` of a `{!reach} while` loop;
while the `{!reach} .case`, `{!reach} .api`, `{!reach} .timeout`, and `{!reach} .paySpec` components are like the corresponding components of a `{!reach} fork` statement.

The `{!reach} .case` component may be repeated many times, just like in a `{!reach} fork` statement.

The `{!reach} .define` component may define bindings that reference the `{!reach} LHS` values. These bindings are accessible
from every component of the `{!reach} parallelReduce` statement, except for the `{!reach} INIT_EXPR`.

#### `.timeRemaining`

When dealing with absolute deadlines in `{!reach} parallelReduce`, there is a common pattern in the
`{!reach} TIMEOUT_BLOCK` to have participants `{!reach} race` to `{!reach} publish` and return the accumulator.
There is a shorthand, `{!reach} .timeRemaining`, available for this situation:

@{ref("rsh", "timeRemaining")}
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

`{!reach} .throwTimeout` is a shorthand that will throw the accumulator as an exception when a timeout occurs.
Therefore, a `{!reach} parallelReduce` that uses this branch must be inside of a try statement. For example,

@{ref("rsh", "throwTimeout")}
```reach
try {
  const [ x, y, z ] =
    parallelReduce([ 1, 2, 3 ])
    ...
    .throwTimeout(deadline)
} catch (e) { ... } 
```


 will expand `{!reach} throwTimeout` to:

```reach
.timeout(deadline, () => {
  throw [ x, y, z ]; }) 
```



#### `parallelReduce` intuition

A parallel reduce statement is essentially an abbreviation of pattern of a `{!reach} while` loop combined with a `{!reach} fork` statement that you could write yourself.
This is an extremely common pattern in decentralized applications.

The idea is that there are some values (the `{!reach} LHS`) which after intialization will be repeatedly updated uniquely by each of the racing participants until the condition does not hold.

```reach
var LHS = INIT_EXPR;
DEFINE_BLOCK;
invariant(INVARIANT_EXPR);
while(COND_EXPR) {
  fork()
  .case(PART_EXPR,
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

Inside of a consensus step, `{!reach} this` refers to the address of the participant that performed the consensus transfer.
This is useful when the consensus transfer was initiated by a `{!reach} race` expression.

### `transfer`

@{ref("rsh", "transfer")}
```reach
transfer(10).to(Alice);
transfer(2, gil).to(Alice); 
```


A @{defn("transfer expression")},
written `{!reach} transfer(AMOUNT_EXPR).to(ADDR_EXPR)`,
where `{!reach} AMOUNT_EXPR` is an expression that evaluates to an unsigned integer, and
`{!reach} ADDR_EXPR` evaluates to an address,
performs a transfer of network tokens from the contract to the named participant.
`{!reach} AMOUNT_EXPR` must evaluate to less than or equal to the balance of network tokens in the contract account.

A transfer expression may also be written `{!reach} transfer(AMOUNT_EXPR, TOKEN_EXPR).to(ADDR_EXPR)`,
where `{!reach} TOKEN_EXPR` is a `{!reach} Token`,
which transfers non-network tokens of the specified type.

A transfer expression may only occur within a consensus step.

### `require`

@{ref("rsh", "require")}
```reach
require( claim, [msg] ) 
```


 A requirement where `{!reach} claim` evaluates to `{!reach} true` with honest participants.
This may only appear in a consensus step.
It accepts an optional bytes argument, which is included in any reported violation.

### `checkCommitment`

@{ref("rsh", "checkCommitment")}
```reach
checkCommitment( commitment, salt, x ) 
```


 Makes a requirement that `{!reach} commitment` is the digest of `{!reach} salt` and `{!reach} x`.
This is used in a consensus step after `{!reach} makeCommitment` was used in a local step.

### Token minting

@{ref("rsh", "burn")}@{ref("rsh", "destroy")}@{ref("rsh", "supply")}@{ref("rsh", "destroyed")}
```reach
require(supply >= 2 * amt);
const tok = new Token({ name, symbol, url, metadata, supply, decimals });
transfer(amt, tok).to(who);
tok.burn(amt);
assert(tok.supply() == supply - amt);
tok.burn();
assert(tok.destroyed() == false);
tok.destroy();
```


:::note
@{seclink("ref-networks")} discusses how Reach supports token minting on specific consensus networks.
:::


We refer to creation of a new non-network token as @{defn("token minting")}.
It is written with the expression `{!reach} new Token(PARAMS)`, where `{!reach} PARAMS` is an object with the following keys:
+ `name`: A value of type `{!reach} Bytes(32)`; defaults to empty.
+ `symbol`: A value of type `{!reach} Bytes(8)`; defaults to empty.
+ `url`: A value of type `{!reach} Bytes(96)`; defaults to empty.
+ `metadata`: A value of type `{!reach} Bytes(32)`; defaults to empty.
This value is intended to be a digest of a larger metadata document.
+ `supply`: A value of type `{!reach} UInt`; defaults to `{!reach} UInt.max`.
+ `decimals`: A value of type `{!reach} UInt`; defaults to `{!reach} 6` on Algorand, and `{!reach} 18` on Ethereum and Conflux.


This returns a `{!reach} Token` value and deposits a `{!reach} supply` amount of the new non-network tokens into the contract account associated with the DApp.
These tokens must be destroyed by the end of the DApp.

:::note
Reach assumes that network tokens and non-network tokens behave identically, but often they do not; [this article](##guide-nntoks) discusses the causes and consequences of this.
:::


---

`{!reach} Token.burn(tok, amt)`, or `{!reach} tok.burn(amt)`, where `{!reach} tok` is a `{!reach} Token` value and `{!reach} amt` is a `{!reach} UInt` value, may be used to @{defn("burn")} tokens in the contract account, meaning that they are utterly destroyed and can never be recovered.

---

`{!reach} Token.destroy(tok)`, or `{!reach} tok.destroy()`, where `{!reach} tok` is a `{!reach} Token` value, may be used to destroy the token so that it may never be used again by any users on the consensus network.
This must be called before the application exits.

---

`{!reach} Token.destroyed(tok)`, or `{!reach} tok.destroyed()`, where `{!reach} tok` is a `{!reach} Token` value, returns whether `{!reach} destroy`
has been called on `{!reach} tok` yet.

---

`{!reach} Token.supply(tok)`, or `{!reach} tok.supply()`, where `{!reach} tok` is a `{!reach} Token` value, may be used to query the current supply of tokens, i.e. the number of tokens which have not been burnt.

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

A remote object is constructed by calling the `{!reach} remote` function with a `{!reach} Contract` and an interface---an object where each key is bound to a function type. For example:
```reach
const randomOracle =
  remote( randomOracleCtcInfo, {
    getRandom: Fun([], UInt),
  });
const token =
  remote( tokenCtcInfo, {
    balanceOf: Fun([Address], UInt),
    transferTo: Fun([UInt, Address], Null),
  });
```


Once constructed, the fields of a remote object represent those remote contract interactions, referred to as @{defn("remote functions")}.
For example, `{!reach} randomOracle.getRandom`, `{!reach} token.balanceOf`, and `{!reach} token.transferTo` are remote functions in the example.

A remote function may be invoked by calling it with the appropriate arguments, whereupon it returns the specified output.
In addition, a remote function may be augmented with one of the following operations:

+ `{!reach} REMOTE_FUN.pay(AMT)` --- Returns a remote function that receives a pay amount, `{!reach} AMT`, _from_ the caller when it is called.
+ @{ref("rsh", "bill")} `{!reach} REMOTE_FUN.bill(AMT)` --- Returns a remote function that provides a pay amount, `{!reach} AMT`, _to_ the caller when it returns.
+ @{ref("rsh", "withBill")} `{!reach} REMOTE_FUN.withBill()` --- Returns a remote function that provides some number of network tokens and, possibly, non-network tokens _to_ the caller when it returns.
The exact amount is returned from the invocation by wrapping the original result in a tuple.

If the remote contract is not expected to return non-network tokens then a pair is returned, where the amount of network tokens received is the first element, and the original result is the second element.

If the remote contract is expected to return non-network tokens then a triple is returned, where the amount of network tokens received
is the first element, a tuple of the non-network tokens received is the second element, and the original result is the third element.
If the caller expects to receive non-network tokens, they must provide a tuple of tokens as an argument to `{!reach} withBill`. The ordering of
tokens in the argument is reserved when returning the amounts received.
For example,

```reach
const [ returned, [gilRecv, zmdRecv], randomValue ] =
  randomOracle.getRandom.pay(stipend).withBill([gil, zmd])();
```


might be the way to communicate with a random oracle that receives a conservative approximation of its actual cost and returns what it does not use, along with some amount of `GIL` and `ZMD`.
This operation may not be used with `{!reach} REMOTE_FUN.bill`.


### Mappings: creation and modification

@{ref("rsh", "Map")}
```reach
const bidsM = new Map(UInt);
bidsM[this] = 17;
delete bidsM[this];
```


A new mapping of linear state may be constructed in a consensus step by writing `{!reach} new Map(TYPE_EXPR)`, where `{!reach} TYPE_EXPR` is some type.

This returns a value which may be used to dereference particular mappings via `{!reach} map[ADDR_EXPR]`, where `{!reach} ADDR_EXPR` is an address.
Such dereferences return a value of type `{!reach} Maybe(TYPE_EXPR)`, because the mapping may not contain a value for `{!reach} ADDR_EXPR`.

A mapping may be modified by writing `{!reach} map[ADDR_EXPR] = VALUE_EXPR` to install `{!reach} VALUE_EXPR` (of type `{!reach} TYPE_EXPR`) at `{!reach} ADDR_EXPR`, or by writing `{!reach} delete map[ADDR_EXPR]` to remove the mapping entry.
Such modifications may only occur in a consensus step.

### Sets: creation and modification

@{ref("rsh", "Set")}@{ref("rsh", "insert")}@{ref("rsh", "remove")}@{ref("rsh", "member")}
```reach
const bidders = new Set();
bidders.insert(Alice);
bidders.remove(Alice);
bidders.member(Alice); // false
```


A `{!reach} Set` is another container for linear state. It is simply a type alias of `{!reach} Map(Null)`;
it is only useful for tracking `{!reach} Address`es. Because a `{!reach} Set` is internally a `{!reach} Map`, it may
only be constructed in a consensus step.

A `{!reach} Set` may be modified by writing `{!reach} s.insert(ADDRESS)` to install `{!reach} ADDRESS` in the
set, `{!reach} s`, or `{!reach} s.remove(ADDRESS)` to remove the `{!reach} ADDRESS` from the set.
Such modifications may only occur in a consensus step.

`{!reach} s.member(ADDRESS)` will return a `{!reach} Bool` representing whether the address is in the set.
