---
menuItem: mi-docs
---

# Reach Basics

This page decribes statements, types, and expressions valid in all Reach modes.

# Reach and JavaScript

Writing Reach code feels familiar because Reach syntax is a subset of modern JavaScript syntax. The following statement, for example, is valid in Reach and Javascript programs (assuming `getContract` is defined):

``` js nonum
const info = getContract();
```

Most of what you know about JavaScript statements, expressions, keywords, comments, declarations, operators, flow control, iterations, arrays, objects, functions, maps, sets, and error handling applies to Reach.

# Reach Statements

*Statements* are instructions composed of identifiers, operators, and expressions.  Statements often begin with an identifier (e.g. `const`) and end with a  semicolon:

``` js nonum
const wager = declassify(interact.wager);
```

Conditional statements, however, which begin with the identifier `if`, do not end with a semicolon: 

``` js nonum
if (!willBuy) {
  commit();
  each([S, B], () => interact.reportCancellation());
  exit();
}
```

## Blocks

A *block* is a sequence of statements surrounded by curly braces. Below are five blocks. The third contains the fourth and fifth:

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

## Tails

A *tail* consists of statements that follow a prior statement. Consider the following:

``` js nonum
{
  X;
  Y;
  Z;
}
```

In this block, the tail of statement `X` is `{ Y; Z; }`, and the tail of statement `Y` is `{ Z; }`. Tails are statically apparent from the structure of the source code.

## Continuations

A *continuation* consists of all the statements that follow a prior statement. Consider the following:

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

## Block Statements

A *block statement*, a statement composed of a block, establishes a local scope for the identifier definitions within the curly braces, isolating the definitions from the statement tail. Consider this example that does *not* include a block statement:

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

## Terminator Statements

A *terminator statement* is a statement with no tail. Consider the following:

``` js nonum
{
  X;
  Y;
  Z;
}
```

In this block, `Z` is the terminator statement because it does not have a tail. Blocks that end with `return`, `continue`, or `exit` do not have terminator statements. The compiler treats these types of blocks as if they end with `return null`.

# Reach Identifiers

All Reach modes support the following statement identifiers:

|Identifier|Description|
|-|-|
|[const](#const)|`const` binds an identifier to a value definition.|
|[function](#function)|`function` binds an identifier to a function definition.|
|[if else](#if-else)|`if else` supports conditional statements.|
|[return](#return)|`return` returns a value to the surrounding scope.|
|[switch](#switch)|`switch` selects one of many code blocks.|
|[var](#var)|`var` binds identifiers in one special case (preceding a `while` statement).|

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

# Reach Types

Reach defines the data types below for use in Reach modules (e.g. *index.rsh* files). Click the type name to learn more.

|Type|Description|
|-|-|
|[Address](#address)|`Address` resembles `Bytes`, and represents an account address.|
|[Array](#array)|`Array` is an immutable, ordered list of same-typed values.|
|`Bool`|`Bool` is a boolean which may be `true` or `false`.|
|[Bytes](#bytes)|`Bytes` is a specialized (immutable, fixed-length) array of characters.|
|[Contract](#contract)||
|[Data](#data)||
|[Digest](#digest)||
|[Either](#either)||
|[FixedPoint](#fixedpoint)|`FixedPoint` is the object `{ sign: Bool, i: { scale: UInt, i: UInt} }` where `scale` is `1`, `10`, `100`, `1000`, etc., and `i` is the underlying unsigned integer. The value of a fixed-point number is `(i / scale) * sign`. It is used to represent numbers that have a fixed number of digits after the decimal.|
|[Fun](#fun)||
|[Int](#int)|`Int` is the object `{ sign: Bool, i: UInt }`. It is an object rather than a scalar value because some consensus networks do not support signed integers.|
|[Interval](#interval)||
|[Map](#map)||
|[Maybe](#maybe)||
|`Null`|`Null` or `null` is the intentional absence of any value.|
|[Object](#object)||
|[Refine](#refine)||
|[Struct](#struct)||
|[Token](#token)||
|[Tuple](#tuple)|`Tuple` is an immutable, ordered list of potentially differently typed values.|
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

## Address

An `Address` (which resembles `Bytes`) represents an account address. Format differs slightly depending on the consensus network.

### addressEq

``` js nonum
addressEq(a, b)
==, ===, !=, and !==,
```

## Array

An *Array* is an immutable, ordered list of values. The size of an array is fixed, and the values must be of the same type. Values may be referenced using a zero-based index. The following creates an array of type `Array(UInt, 3)` from a tuple:

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

## Bytes

`Bytes` is a specialized (immutable, fixed-length) array of characters:

``` js nonum
// index.rsh
const ar = 'فراشة';          // Bytes(10)
const de = 'Schmetterling';  // Bytes(13)
const en = 'butterfly';      // Bytes(9)
const fr = 'papillon';       // Bytes(8)
const ga = 'féileacán';      // Bytes(11)
const he = 'פַּרְפַּר';           // Bytes(18)
const vi = 'Con bướm';       // Bytes(11)
const zh = '蝴蝶';            // Bytes(6)
```

### Matching Bytes

Two `Bytes` of different lengths are not interchangeable. Consider the following:

``` js nonum
// index.rsh
const myInteract = {
  reportBytes: Fun([Bytes(24)], Null),
}
M.interact.reportBytes('butterfly'); // Bytes(9)
```

Because `'butterfly'` is of type `Bytes(9)` and `reportBytes` requires an argument of type `Bytes(24)`, the compiler, encountering this code, will generate an error similar to the following:

``` nonum
reachc: error[RE0088]: These types are mismatched: Bytes(9) vs Bytes(24)
```

To match the two types, use the `pad` function:

``` js nonum
// index.rsh
const myInteract = {
  reportBytes: Fun([Bytes(24)], Null),
}
M.interact.reportBytes(Bytes(24).pad('butterfly'));
```

### Frontend Perspective

Here is one implementation of the corresponding frontend interact object:

``` js nonum
// index.mjs
const myInteract = {
  reportBytes: (v) => { console.log(`Length is ${v.length}. Value is ${v}.`); },
}
```

Note that the lengths vary:

``` nonum
Length is 19. Value is فراشة.
Length is 24. Value is Schmetterling.
Length is 24. Value is butterfly.
Length is 24. Value is papillon.
Length is 22. Value is féileacán.
Length is 15. Value is פַּרְפַּר.
Length is 21. Value is Con bướm.
Length is 20. Value is 蝴蝶.
```

Another `reportBytes` implementation enables you to inspect the strings as byte arrays:

``` js nonum
// index.mjs
const myInteract = {
  reportBytes: (v) => { 
    var myBuffer = [];
    var buffer = new Buffer(v, 'utf16le');
    for (var i = 0; i < buffer.length; i++) {
        myBuffer.push(buffer[i]);
    }
    console.log(...myBuffer);
  },
}
```

Here is the output:

``` nonum
65 6 49 6 39 6 52 6 41 6 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
83 0 99 0 104 0 109 0 101 0 116 0 116 0 101 0 114 0 108 0 105 0 110 0 103 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
98 0 117 0 116 0 116 0 101 0 114 0 102 0 108 0 121 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
112 0 97 0 112 0 105 0 108 0 108 0 111 0 110 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
102 0 233 0 105 0 108 0 101 0 97 0 99 0 225 0 110 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
228 5 188 5 183 5 232 5 176 5 228 5 188 5 183 5 232 5 0 0 0 0 0 0 0 0 0 0 0 0
67 0 111 0 110 0 32 0 98 0 176 1 219 30 109 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
116 135 118 135 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
```

## Contract

### getAddress

The `getAddress` function returns the `Address` of the contract:

``` js nonum
// index.rsh
const myInteract = {
  reportAddress: Fun([Address], Null)
}
M.interact.reportAddress(getAddress());
```

Here is one implementation of the corresponding frontend interact object:

``` js nonum
// index.mjs
const myInteract = {
  reportAddress: (v) => { console.log(v) }
}
```

And, here is the output:

``` nonum
# REACH_CONNECTOR_MODE=ALGO-devnet
0x5b26ec467ab3e54c793315b548923ec376624d3d29e70083bd2555ef24da8383
  
# REACH_CONNECTOR_MODE=CFX-devnet
NET999:ACD8UE5KYBVG62MB9Z8R20CFBPHJ632ESJTEKUZ31V
  
# REACH_CONNECTOR_MODE=ETH-devnet
0xD2651496526c4E93fc01A87F99bD38092A244FB5
```

### getContract

The `getContract` function returns the `Contract` information representing the contract:

``` js nonum
// index.rsh
const myInteract = {
  reportContract: Fun([Contract], Null)
}
M.interact.reportContract(getContract());
```

Here is one implementation of the corresponding frontend interact object:

``` js nonum
// index.mjs
const myInteract = {
  reportContract: (v) => { console.log(v) }
}
```

And, here is the output:

``` nonum
# REACH_CONNECTOR_MODE=ALGO-devnet
8
  
# REACH_CONNECTOR_MODE=CFX-devnet
NET999:ACC3MBPMGWWKET1Z405M7CT697775FMKZPBURP3CVZ
  
# REACH_CONNECTOR_MODE=ETH-devnet
0x6Da448EeE4A2B10aaE01307C81247cb39A4fea89
```

## Data

A *data instance* is ...

## Digest

## Either

## FixedPoint

## Fun

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

## Map

A *map* is ...

### Map.all

### Map.and

### Map.any

### Map.average

### Map.count

### Map.forEach

### Map.includes

### Map.max

### Map.min

### Map.or

### Map.product

### Map.reduce

### Map.size

### Map.sum

## Maybe

### Some

### None

## Object

An *object* is ...

### Object.has

### Object.set

### Object.setIfUnset

## Refine

## Struct

A *struct* is ...

## Token

## Tuple

A *Tuple* is an immutable, ordered list of values. The size of a tuple is fixed. The values may be different types. Values may be referenced using a zero-based index. The following creates a tuple of type `Tuple(UInt, UInt, UInt)`:

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

# Other Expressions

## Anybody

## compose

## ensure

## forall

## hasConsoleLogger

## hasRandom

## implies

## makeEnum

## match

## new

## possible

## this
