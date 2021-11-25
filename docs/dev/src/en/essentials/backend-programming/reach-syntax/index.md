---
menuItem: mi-docs
---

# Reach Syntax

This page demonstrates Reach syntax.

# Reach and JavaScript

Writing Reach code feels familiar because Reach syntax is a subset of modern JavaScript syntax. The following statement, for example, is valid in Reach and Javascript programs (assuming `getContract` is defined):

``` js nonum
const info = getContract();
```

Most of what you know about JavaScript statements, expressions, keywords, comments, declarations, operators, flow control, iterations, arrays, objects, functions, maps, sets, and error handling applies to Reach.

# Reach Types

Reach defines the following data types for use in Reach modules (e.g. *index.rsh* files):

|Type|Description|
|-|-|
|[Address](#address)||
|[Array](#array)||
|`Bool`|A boolean type can be `true` or `false`.|
|[Bytes](#bytes)||
|[Contract](#contract)||
|[Data](#data)||
|[Digest](#digest)||
|[FixedPoint](#fixedpoint)|A fixed-point type is used to represent numbers that have a fixed number of digits after the decimal. It is represented by the object `{ sign: Bool, i: { scale: UInt, i: UInt} }` where `scale` is `1`, `10`, `100`, `1000`, etc., and `i` is the underlying unsigned integer. The value of a fixed-point number is `(i / scale) * sign`.|
|[Fun](#fun)||
|[Int](#int)|A signed integer is represented by the object `{ sign: Bool, i: UInt }` rather than a scalar value because some consensus networks do not support signed integers.|
|[Interval](#interval)||
|`Null`||
|[Object](#object)||
|[Refine](#refine)||
|[Struct](#struct)||
|[Token](#token)||
|[Tuple](#tuple)||
|[UInt](#uint)||

Use the unary operator `typeof` or the function `typeOf()` to return the type of the argument:

``` js nonum
const XTy = typeof 0;
const YTy = typeOf(true);
```

You can use the returned type in declarations like this:

``` js nonum
{
  getX: Fun([], XTy),
  getY: Fun([], YTy)
}
```

Use the `is()` function to verify that a variable is of a certain type:

``` js nonum
const addOneImpl = (x) => x + 1;
export const addOne = is(addOneImpl, Fun([UInt], UInt));
```

If `true`, `is()` returns its first argument. Otherwise, the compiler outputs an error. Consider the following:


``` js nonum
const addOneImpl = (x) => x + 1;
export const addOne = is(addOneImpl, Fun([UInt], Null));
```

The compiler outputs `These types are mismatched: UInt vs Null` because `addOneImpl` returns a `UInt` not a `Null`.

# Reach Statements

*Statements* are instructions composed of values, operators, expressions, keywords, and/or comments.  Statements often end with semicolons. Here are two examples:

``` js nonum
const wager = declassify(interact.wager);
const deadline = declassify(interact.deadline);
```

But, *conditional statements* do not end with semicolons:

``` js nonum
if (!willBuy) {
  commit();
  each([S, B], () => interact.reportCancellation());
  each([S, B], () => interact.reportExit());
  exit();
}
```

Statements often include *statement identifiers* such as `break`, `const`, `continue`, `for`, `if`, `var`, and `switch`:

``` js nonum
switch ( mi ) {
  case None: return civ;
  case Some:
  return civ + mi;
}
```

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

*Tails* are statements that follow a prior statement. Consider the following:

``` js nonum
{
  X;
  Y;
  Z;
}
```

In this block, the tail of statement `X` is `{ Y; Z; }`, and the tail of statement `Y` is `{ Z; }`. Tails are statically apparent from the structure of the source code.

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

A *block statement*, a statement composed of a block, establishes a local scope for the identifier definitions within the curly braces, isolating the definitions from the statement tail. Consider this example that does *not* include a block statement:

``` js nonum
const x = 4;
return x; 
```

Above, `x` evaluates to `4`. The snippet below isolates `const x = 4` within a block statment:

``` js nonum
{ const x = 4; }
return x;
```

Now, `return x` is erroneous because `x` is unbound outside the block statement.

*Terminator Statements* are statements with no tail. Consider the following:

``` js nonum
{
  X;
  Y;
  Z;
}
```

In this block, `Z` is the terminator statement because it does not have a tail. Blocks that end with `return`, `continue`, or `exit` do not have terminator statements. The compiler treats these types of blocks as if they end with `return null`.

## const

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
const x = 4; 
```

The following is also invalid:

``` js nonum
Alice.only(() => { const x = 3; });
Bob.only(() => { const x = 4; });
```

The special identifier `_` is an exception to this rule. The `_` binding is always considered to be unbound. This means that `_` is both an identifier that can never be read, as well as an identifier that may be bound many times which is useful for ignoring unwanted values. The following, for example, binds `x = 2` while ignoring `1` and `3`:

``` js nonum
const [_, x, _] = [1, 2, 3];
```

## function

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

## if else

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

## return

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

## switch

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

## var

Reach supports `var` in two situations:

1. Outside `export const main = Reach.App(() => {}`:

    ``` js nonum
    'reach 0.1';
    var x = 0;
    export const main = Reach.App(() => {}
    ```

1. Immediately before a while loop and its invariant:

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

# Reach Expressions

An *expression* evaluates to a value (e.g. `add(4, 5)`). Reach includes the expressions described below.

## Address

### getAddress

## Anybody

## Arithmetic

See [Array](#array), [FixedPoint](#fixedpoint), [Int](#int), [Interval](#interval), and [UInt](#uint).

## Array

An *Array* is an order list of values. The size of an array is fixed, and the values must be of the same type. Values may be referenced using a zero-based index. The following creates an array of type `Array(UInt, 3)` from a tuple:

``` js nonum
const a = array(UInt, [4, 6, 8]);
const v = a[0]; // 4
```

The following creates an array of type `Array(Object({price: UInt, quantity: UInt}), 3)` from a tuple:

``` js nonum
const a = array(
  Object({price: UInt, quantity: UInt}),
  [
    {price:1000, quantity:10},
    {price:2000, quantity:20},
    {price:3000, quantity:30}
  ]
);
const p = a[0].price; // 1000
```

### Array.all

### Array.and

### Array.any

### Array.average

### Array.concat

### Array.count

### Array.elemType

### Array.empty

### Array.find

### Array.findIndex

### Array.forEach

### Array.includes

### Array.indexOf

### Array.iota

### Array.length

### Array.map

### Array.mapWithIndex

### Array.max

### Array.min

### Array.or

### Array.product

### Array.reduce

### Array.reduceWithIndex

### Array.replicate

### Array.set

### Array.size

### Array.slice

### Array.sum

### Array.withIndex

### Array.zip

## Arrow Expression

## assert

## balance

## bitwise

## Bytes

## comparison

## compose

## Contract

### getContract

## Data

A *data instance* is ...

## Digest

## Either

## ensure

## FixedPoint

## Fun

## forall

## hasConsoleLogger

## hasRandom

## implies

## Int

### iadd

``` js nonum
iadd(a, b) // iadd(Int, Int)
```

### isub

``` js nonum
isub(a, b) // isub(Int, Int)
```

### imul

``` js nonum
imul(a, b) // imul(Int, Int)
```

### idiv

``` js nonum
idiv(a, b) // idiv(Int, Int)
```

### imod

``` js nonum
imod(a, b) // imod(Int, Int)
```

## Interval

Constructors

Accessors

Relational Operations

Arithmetic Operations

Other Operations

## Literals

## makeEnum

## Maps

A *map* is ...

## match

## Maybe

## new

## Object

An *object* is ...

## Operators

*Operators* perform operations on operands. Most operators have equivalent JS Stdlib function equivalents.

| Operator | Function | Description |
|-|-|-|
| `-` | `sub(a, b)` | `a - b` |
| `!` | n/a | `not` |
| `!=` <br/> `!==` | `polyNeq(a, b)` | |
| `*` | `mul(a, b)` | `a * b` |
| `/` | `div(a, b)` | `a / b` |
| `&` | `band(a, b)` | |
| `&&` | `and(a, b)` | |
| `%` | `mod(a, b)` | |
| `^` | `bxor(a, b)` | |
| `+` | `add(a, b)` | `a + b` |
| `<` | `lt(a, b)` | |
| `<<` | `lsh(a, b)` | |
| `<=` | `le(a, b)` | |
| `==` <br/> `===`| `addrEq(a, b)` <br/> `boolEq(a, b)` <br/> `digestEq(a, b)` <br/> `fxeq(a, b)` <br/> `ieq(a, b)` <br/> `intEq(a, b)` <br/> `polyEq(a, b)` <br/> `typeEq(a, b)` | |
| `>` | `gt(a, b)` | |
| `>=` | `ge(a, b)` | |
| `>>` | `rsh(a, b)` | |
| `\|` | `bior(a, b)` | |
| `\|\|` | `or(a, b)` | |
| n/a | `xor(a, b)` | |

## pad

## possible

## Refine

## Sets

A *set* is ...

## Struct

A *struct* is ...

## Ternary Operator

## Time

lastConsensusTime

lastConsensusSecs

baseWaitTime

baseWaitSecs

relativeTime

absoluteTime

relativeSecs

absoluteSecs

makeDeadline

## this

## Token

## Tuple

A *Tuple* is an order list of values. The size of a tuple is fixed. The values may be different types. Values may be referenced using a zero-based index. The following creates a tuple of type `Tuple(UInt, UInt, UInt)`:

``` js nonum
const t = [5, 10, 15];
const v = t[2]; // 15
```

The following creates a tuple of type `Tuple(UInt, Object({price: UInt, quantity: UInt}), UInt)`:

``` js nonum
const t = [5, { price: 1000, quantity: 40 }, 15];
const p = t[1].price; // 1000
const v = t[2];       // 15
```

The following creates a tuple of type `Tuple(UInt, UInt, UInt, UInt, Object({price: UInt, quantity: UInt}), UInt)`:

``` js nonum
const t_temp = [5, { price: 1000, quantity: 40 }, 15];
const t = [5, 10, 15, ...t_temp];
const p = t[4].price; // 1000
const v = t[5];       // 15
```

### Tuple.length

`Tuple.length` returns the length of the tuple:

``` js nonum
const lenA = Tuple.length([5, 10, 15]); // 3
// or
const lenB = [5, 10, 15].length; // 3
```

### Tuple.set

`Tuple.set` returns a new tuple identical to the original except that the value at the specified index is the new value:

``` js nonum
const tA = Tuple.set([5, 10, 15], 1, 44);
const vA = tA[1]; // 44
// or
const tB = [5, 10, 15].set(1, 44);
const vB = tB[1]; // 44
```

## UInt

### add

``` js nonum
a + b      // UInt + UInt
add(a, b)  // add(UInt, UInt)
```

### sub

``` js nonum
a - b      // UInt - UInt
sub(a, b)  // sub(UInt, UInt)
```

### mul

``` js nonum
a * b      // UInt * UInt
mul(a, b)  // mul(UInt, UInt)
```

### muldiv

The `muldiv` function performs `(a * b) / c`:

``` js nonum
muldiv(a, b, c) // muldiv(UInt, UInt, UInt)
```

The product of `a * b` may exceed `UInt.max`, but the final quotient must be less than `UInt.max`.

### div

``` js nonum
a / b      // UInt * UInt
div(a, b)  // div(UInt, UInt)
```

### mod

``` js nonum
a % b      // UInt % UInt
mod(a, b)  // mod(UInt, UInt)
```

### sqrt

The `sqrt` function finds the square root of `value`, iterating `iterations` number of times.

``` js nonum
sqrt(value, iterations) // sqrt(UInt, UInt)
```

This function utilizes the [Babylonian Method](https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Babylonian_method) for computing the square root. `iterations` must be known at compile time. When performing 5 iterations, the algorithm can reliably calculate the square root up to 32 squared, or 1,024. When performing 10 iterations, the algorithm can reliably calculate the square root up to 580 squared, or 336,400.

### Power

The `pow` function calculates the approximate value of raising `base` to `power`, iterating `iterations` number of times.:

``` js nonum
pow(base, power, iterations) // pow(UInt, UInt, UInt)
```

`iterations` must be known at compile time. Six iterations provides enough accuracy to calculate up to <code>2<sup>63</sup></code>.

## unstrict

## use strict
