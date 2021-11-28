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

# each

# exit

# export

# fork

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

# parallelReduce

# pay

# publish

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

# while
