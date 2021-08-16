



# {#ref-programs-consensus} Consensus Steps

A Reach consensus step occurs in the continuation of a consensus transfer statement.
It represents the actions taken by the consensus network contract of an application.

## {#ref-programs-consensus-stmts} Statements

Any statements valid for a [computation](##ref-programs-compute-stmts) are valid for a consensus step.
However, some additional statements are allowed.

### `commit`

<Ref :name="(quote rsh):commit" />
```reach
commit(); 
```


A <Defn :name="commit statement">commit statement</Defn>, written `commit();`, commits to statement's continuation as the next step of the DApp computation. In other words, it ends the current consensus step and allows more local steps.

### {#ref-programs-only-consensus} `only` and `each`

XXX (secref "ref-programs-only-step") are allowed in consensus steps and are executed by backends once they observe the completion of the consensus step (i.e., after the associated commit statement.)

### {#ref-programs-consensus-view} View Objects
XXX (note-view-xref)

```reach
vNFT.owner.set(creator);
```


If `VIEW` is a <Defn :name="view object">view object</Defn>, then its fields are the elements of the associated view.
Each of these fields are bound to an object with an `set` method that accepts the function or value to be bound to that view at the current step, and all steps dominated by the current step (unless otherwise overridden.)
If this function is not provided with an argument, then the corresponding view is unset.

For example, consider the following program:

@[code](@reach-lang/examples/view-steps/index.rsh)

In this program, the Reach backend calls the frontend `interact` function, `checkView` with the expected value of the views at each point in the program.
The frontend compares that value with what is returned by
```js
[ await ctc.getViews().Main.last(),
  await ctc.getViews().Main.i() ]
```


When a view is bound to a function, it may inspect any values in its scope, including linear state.

### `Participant.set` and `.set`

<Ref :name="(quote rsh):Participant.set" />
```reach
Participant.set(PART, ADDR);
PART.set(ADDR); 
```


 After execution, the given participant is fixed to the given address.
It is invalid to attempt to `.set` a participant class.
If a backend is running for this participant and its address does not match the given address, then it will abort.
This may only occur within a consensus step.

::: note
XXX (secref "workshop-relay") is a good introductory project that demonstrates how to use this feature of Reach.
:::

### `while`

<Ref :name="(quote rsh):while" /><Ref :name="(quote rsh):var" /><Ref :name="(quote rsh):invariant" />
```reach
var [ heap1, heap2 ] = [ 21, 21 ];
{ const sum = () => heap1 + heap2; }
invariant(balance() == 2 * wagerAmount);
while ( sum() > 0 ) {
  ....
  [ heap1, heap2 ] = [ heap1 - 1, heap2 ];
  continue; } 
```


A <Defn :name="while statement">while statement</Defn> may occur within a consensus step and is written:

```reach
var LHS = INIT_EXPR;
DEFINE_BLOCK; // optional
invariant(INVARIANT_EXPR);
while( COND_EXPR ) BLOCK 
```


where `LHS` is a valid left-hand side of an identifier definition where the expression `INIT_EXPR` is the right-hand side, and
`DEFINE_BLOCK` is an optional block that may define bindings that use the `LHS` values which are bound inside the rest of the `while` and its tail, and
`INVARIANT_EXPR` is an expression, called the <Defn :name="loop invariant">loop invariant</Defn>, that must be true before and after every execution of the block `BLOCK`, and
if `COND_EXPR` is true, then the block executes,
and if not, then the loop terminates and control transfers to the continuation of the while statement.
The identifiers bound by `LHS` are bound within `DEFINE_BLOCK`, `INVARIANT_EXPR`, `COND_EXPR`, `BLOCK`, and the tail of the while statement.

::: note
Read about finding [loop invariants](##guide-loop-invs) in the Reach guide.
:::

### `continue`

<Ref :name="(quote rsh):continue" />
```reach
[ heap1, heap2 ] = [ heap1 - 1, heap2 ];
continue; 
```


A <Defn :name="continue statement">continue statement</Defn> may occur within a while statement's block and is written:

```reach
LHS = UPDATE_EXPR;
continue; 
```


where the identifiers bound by `LHS` are a subset of the variables bound by the nearest enclosing while statement and `UPDATE_EXPR` is an expression which may be bound by `LHS`.

A continue statement is a terminator statement, so it must have an empty tail.

A continue statement may be written without the preceding identifier update, which is equivalent to writing

```reach
[] = [];
continue; 
```


A continue statement must be dominated by a consensus transfer, which means that the body of a while statement must always `commit();` before calling `continue;`.
This restriction may be lifted in future versions of Reach, which will perform termination checking.

### `parallelReduce`

<Ref :name="(quote rsh):parallelReduce" />
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


XXX (note-ctransfer)

A <Defn :name="parallel reduce statement">parallel reduce statement</Defn> is written:

<Ref :name="(quote rsh):paySpec" /><Ref :name="(quote rsh):define" />
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
  .timeout(DELAY_EXPR, () =>
    TIMEOUT_BLOCK);
```


The `LHS` and `INIT_EXPR` are like the initialization component of a `while` loop; and,
the `.invariant` and `.while` components are like the invariant and condition of a `while` loop;
the `DEFINE_BLOCK` is like the `DEFINE_BLOCK` of a `while` loop;
while the `.case`, `.timeout`, and `.paySpec` components are like the corresponding components of a `fork` statement.

The `.case` component may be repeated many times, provided the `PART_EXPR`s each evaluate to a unique participant, just like in a `fork` statement.

The `.define` component may define bindings that reference the `LHS` values. These bindings are accessible
from every component of the `parallelReduce` statement, except for the `INIT_EXPR`.

#### `.timeRemaining`

When dealing with absolute deadlines in `parallelReduce`, there is a common pattern in the
`TIMEOUT_BLOCK` to have participants `race` to `publish` and return the accumulator.
There is a shorthand, `.timeRemaining`, available for this situation:

<Ref :name="(quote rsh):timeRemaining" />
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

`.throwTimeout` is a shorthand that will throw the accumulator as an exception when a timeout occurs.
Therefore, a `parallelReduce` that uses this branch must be inside of a try statement. For example,

<Ref :name="(quote rsh):throwTimeout" />
```reach
try {
  const [ x, y, z ] =
    parallelReduce([ 1, 2, 3 ])
    ...
    .throwTimeout(deadline)
} catch (e) { ... } 
```


 will expand `throwTimeout` to:

```reach
.timeout(deadline, () => {
  throw [ x, y, z ]; }) 
```



#### `parallelReduce` intuition

A parallel reduce statement is essentially an abbreviation of pattern of a `while` loop combined with a `fork` statement that you could write yourself.
This is an extremely common pattern in decentralized applications.

The idea is that there are some values (the `LHS`) which after intialization will be repeatedly updated uniquely by each of the racing participants until the condition does not hold.

```reach
var LHS = INIT_EXPR;
invariant(INVARIANT_EXPR)
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

Inside of a consensus step, `this` refers to the address of the participant that performed the consensus transfer.
This is useful when the consensus transfer was initiated by a `race` expression.

### `transfer`

<Ref :name="(quote rsh):transfer" />
```reach
transfer(10).to(Alice);
transfer(2, gil).to(Alice); 
```


A <Defn :name="transfer expression">transfer expression</Defn>,
written `transfer(AMOUNT_EXPR).to(ADDR_EXPR)`,
where `AMOUNT_EXPR` is an expression that evaluates to an unsigned integer, and
`ADDR_EXPR` evaluates to an address,
performs a transfer of network tokens from the contract to the named participant.
`AMOUNT_EXPR` must evaluate to less than or equal to the balance of network tokens in the contract account.

A transfer expression may also be written `transfer(AMOUNT_EXPR, TOKEN_EXPR).to(ADDR_EXPR)`,
where `TOKEN_EXPR` is a `Token`,
which transfers non-network tokens of the specified type.

A transfer expression may only occur within a consensus step.

### `require`

<Ref :name="(quote rsh):require" />
```reach
require( claim, [msg] ) 
```


 A requirement where `claim` evaluates to `true` with honest participants.
This may only appear in a consensus step.
It accepts an optional bytes argument, which is included in any reported violation.

### `checkCommitment`

<Ref :name="(quote rsh):checkCommitment" />
```reach
checkCommitment( commitment, salt, x ) 
```


 Makes a requirement that `commitment` is the digest of `salt` and `x`.
This is used in a consensus step after `makeCommitment` was used in a local step.

### Token minting

<Ref :name="(quote rsh):burn" /><Ref :name="(quote rsh):destroy" /><Ref :name="(quote rsh):supply" /><Ref :name="(quote rsh):destroyed" />
```reach
require(supply >= 2 * amt);
const tok = new Token({name, symbol, url, metadata, supply});
transfer(amt, tok).to(who);
tok.burn(amt);
assert(tok.supply() == supply - amt);
tok.burn();
assert(tok.destroyed() == false);
tok.destroy();
```


::: note
XXX (secref "ref-networks") discusses how Reach supports token minting on specific consensus networks.
:::

A non-network token may be XXX (deftech #:key "token minting" "minted") with the expression `new Token(PARAMS)`, where `PARAMS` is an object with the following keys:
+ `name`: A value of type `Bytes(32)`; defaults to empty.
+ `symbol`: A value of type `Bytes(8)`; defaults to empty.
+ `url`: A value of type `Bytes(96)`; defaults to empty.
+ `metadata`: A value of type `Bytes(32)`; defaults to empty.
This value is intended to be a digest of a larger metadata document.
+ `supply`: A value of type `UInt`; defaults to `UInt.max`.


This returns a `Token` value and deposits a `supply` amount of the new non-network tokens into the contract account associated with the DApp.
These tokens must be destroyed by the end of the DApp.

---

`Token.burn(tok, amt)`, or `tok.burn(amt)`, where `tok` is a `Token` value and `amt` is a `UInt` value, may be used to <Defn :name="burn">burn</Defn> tokens in the contract account, meaning that they are utterly destroyed and can never be recovered.

---

`Token.destroy(tok)`, or `tok.destroy()`, where `tok` is a `Token` value, may be used to destroy the token so that it may never be used again by any users on the consensus network.
This must be called before the application exits.

---

`Token.destroyed(tok)`, or `tok.destroyed()`, where `tok` is a `Token` value, returns whether `destroy`
has been called on `tok` yet.

---

`Token.supply(tok)`, or `tok.supply()`, where `tok` is a `Token` value, may be used to query the current supply of tokens, i.e. the number of tokens which have not been burnt.

### Remote objects

<Ref :name="(quote rsh):remote" />
```reach
const randomOracle =
  remote( randomOracleAddr, {
    getRandom: Fun([], UInt),
  });
const randomVal = randomOracle.getRandom.pay(randomFee)();
```


::: note
XXX (secref "ref-networks") discusses how Reach supports remote objects on specific consensus networks.
:::

A <Defn :name="remote object">remote object</Defn> is representation of a foreign contract in a Reach application.
During a consensus step, a Reach computation may consensually communicate with such an object via a prescribed interface.

A remote object is constructed by calling the `remote` function with an address and an interface---an object where each key is bound to a function type. For example:
```reach
const randomOracle =
  remote( randomOracleAddr, {
    getRandom: Fun([], UInt),
  });
const token =
  remote( tokenAddr, {
    balanceOf: Fun([Address], UInt),
    transferTo: Fun([UInt, Addres], Null),
  });
```


Once constructed, the fields of a remote object represent those remote contract interactions, referred to as <Defn :name="remote functions">remote functions</Defn>.
For example, `randomOracle.getRandom`, `token.balanceOf`, and `token.transferTo` are remote functions in the example.

A remote function may be invoked by calling it with the appropriate arguments, whereupon it returns the specified output.
In addition, a remote function may be augmented with one of the following operations:

+ `REMOTE_FUN.pay(AMT)` --- Returns a remote function that receives a pay amount, `AMT`, _from_ the caller when it is called.
+ <Ref :name="(quote rsh):bill" /> `REMOTE_FUN.bill(AMT)` --- Returns a remote function that provides a pay amount, `AMT`, _to_ the caller when it returns.
+ <Ref :name="(quote rsh):withBill" /> `REMOTE_FUN.withBill()` --- Returns a remote function that provides some number of network tokens and, possibly, non-network tokens _to_ the caller when it returns.
The exact amount is returned from the invocation by wrapping the original result in a tuple.

If the remote contract is not expected to return non-network tokens then a pair is returned, where the amount of network tokens received is the first element, and the original result is the second element.

If the remote contract is expected to return non-network tokens then a triple is returned, where the amount of network tokens received
is the first element, a tuple of the non-network tokens received is the second element, and the original result is the third element.
If the caller expects to receive non-network tokens, they must provide a tuple of tokens as an argument to `withBill`. The ordering of
tokens in the argument is reserved when returning the amounts received.
For example,

```reach
const [ returned, [gilRecv, zmdRecv], randomValue ] =
  randomOracle.getRandom.pay(stipend).withBill([gil, zmd])();
```


might be the way to communicate with a random oracle that receives a conservative approximation of its actual cost and returns what it does not use, along with some amount of `GIL` and `ZMD`.
This operation may not be used with `REMOTE_FUN.bill`.


### Mappings: creation and modification

<Ref :name="(quote rsh):Map" />
```reach
const bidsM = new Map(UInt);
bidsM[this] = 17;
delete bidsM[this];
```


A new mapping of linear state may be constructed in a consensus step by writing `new Map(TYPE_EXPR)`, where `TYPE_EXPR` is some type.

This returns a value which may be used to dereference particular mappings via `map[ADDR_EXPR]`, where `ADDR_EXPR` is an address.
Such dereferences return a value of type `Maybe(TYPE_EXPR)`, because the mapping may not contain a value for `ADDR_EXPR`.

A mapping may be modified by writing `map[ADDR_EXPR] = VALUE_EXPR` to install `VALUE_EXPR` (of type `TYPE_EXPR`) at `ADDR_EXPR`, or by writing `delete map[ADDR_EXPR]` to remove the mapping entry.
Such modifications may only occur in a consensus step.

### Sets: creation and modification

<Ref :name="(quote rsh):Set" /><Ref :name="(quote rsh):insert" /><Ref :name="(quote rsh):remove" /><Ref :name="(quote rsh):member" />
```reach
const bidders = new Set();
bidders.insert(Alice);
bidders.remove(Alice);
bidders.member(Alice); // false
```


A `Set` is another container for linear state. It is simply a type alias of `Map(Null)`;
it is only useful for tracking `Address`es. Because a `Set` is internally a `Map`, it may
only be constructed in a consensus step.

A `Set` may be modified by writing `s.insert(ADDRESS)` to install `ADDRESS` in the
set, `s`, or `s.remove(ADDRESS)` to remove the `ADDRESS` from the set.
Such modifications may only occur in a consensus step.

`s.member(ADDRESS)` will return a `Bool` representing whether the address is in the set.

