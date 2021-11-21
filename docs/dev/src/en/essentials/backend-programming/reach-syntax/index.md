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

# Statements

*Statements* are instructions composed of values, operators, expressions, keywords, and/or comments.  Statements often end with semicolons. Here are two examples:

``` js
const wager = declassify(interact.wager);
const deadline = declassify(interact.deadline);
```

But, *conditional statements* do not end with semicolons:

``` js
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

``` js
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

``` js
{
  X;
  Y;
  Z;
}
```

In this block, the tail of statement `X` is `{ Y; Z; }`, and the tail of statement `Y` is `{ Z; }`. Tails are statically apparent from the structure of the source code.

*Continuations* are all the statements that follow a prior statement. Consider the following:

``` js
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

``` js
const x = 4;
return x; 
```

Above, `x` evaluates to `4`. The snippet below isolates `const x = 4` within a block statment:

``` js
{ const x = 4; }
return x;
```

Now, `return x` is erroneous because `x` is unbound outside the block statement.

*Terminator Statements* are statements with no tail. Consider the following:

``` js
{
  X;
  Y;
  Z;
}
```

In this block, `Z` is the terminator statement because it does not have a tail. Blocks that end with `return`, `continue`, or `exit` do not have terminator statements. The compiler treats these types of blocks as if they end with `return null`.

## const

`const` is an *identifier definition* that binds an identifier to a *value* definition. Here are examples:

``` js
const DELAY = 10;
const [ Good, Bad ] = [ 42, 43 ];
const { x, y } = { x: 1, y: 2 };
const [ x, [ y ] ] = [ 1, [ 2 ] ];
const [ x, { y } ] = [ 1, { y: 2 } ];
const { x: [ a, b ] } = { x: [ 1, 2 ] };
```

## function

`function` is an *identifier definition* that binds an identifier to a *function* definition. Here are examples:

``` js
function randomBool() { return (interact.random() % 2) == 0; };
```

``` js
function f(a, b, c = a + 1, d = b + c) => a + b + c + d;
```

## if...else

## return

## switch

## var

# Expressions

An *expression* evaluates to a value. 

Expressions may include parentheses that enforce operational order:

``` js nonum
(a + b) - c 
```

Reach includes the expressions described below.

## Anybody

## Arithmetic

muldiv

sqrt

pow

## Arrays

An *array* is ...

`array` converts a tuple of homogeneous values of the specified type into an array.

An *element reference* ...

## Arrow Expression

## assert

## balance

## compose

## Data

A *data instance* is ...

## digest

## Either

## ensure

## Fixed-point Numbers

## forall

## getAddress

## getContract

## hasConsoleLogger

## hasRandom

## implies

## Intervals

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

## Objects

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

## Sets

A *set* is ...

## Signed Integers

## Structs

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

## Tuples

A *tuple* is ...

## Types

## unstrict

## use strict
