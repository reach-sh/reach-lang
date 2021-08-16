



# {#ref-programs-compute} Computations

This section describes the common features available in all Reach contexts.

## Comments

```reach
// single-line comment
/* multi-line
 * comment
 */ 
```


Comments are text that is ignored by the compiler.
Text starting with `//` up until the end of the line forms a <Defn :name="single-line comment">single-line comment</Defn>.
Text enclosed with `/*` and `*/` forms a <Defn :name="multi-line comment">multi-line comment</Defn>.
It is invalid to nest a multi-line comment within a multi-line comment.

## Blocks

```reach
{ return 42; }
{ const x = 31;
  return x + 11; }
{ if ( x < y ) {
    return "Why";
  } else {
    return "Ecks"; } } 
```


A <Defn :name="block">block</Defn> is a sequence of statements surrounded by braces, i.e. `{` and `}`.

## {#ref-programs-compute-stmts} Statements

This section describes the <Defn :name="statements">statements</Defn> which are allowed in any Reach context.

Each statement affects the meaning of the subsequent statements, which is called its <Defn :name="tail">tail</Defn>. For example, if `{X; Y; Z;}` is a block, then `X`'s tail is `{Y; Z;}` and `Y`'s tail is `{Z;}`.

Distinct from tails are <Defn :name="continuations">continuations</Defn> which include everything after the statement. For example, in `{ {X; Y;}; Z;}`, `X`'s tail is just `Y`, but its continuation is `{Y;}; Z;`.

Tails are statically apparent from the structure of the program source code, while continuations are influenced by function calls.

A sequence of statements that does not end in a <Defn :name="terminator statement">terminator statement</Defn> (a statement with no tail), such as a return statement, continue statement, or exit statement is treated as if it ended with `return null;`.

The remainder of this section enumerates each kind of statement.

### `const` and `function`

An <Defn :name="identifier definition">identifier definition</Defn> is either
a value definition
or a function definition.
Each of these introduces one or more <Defn :name="bound identifier">bound identifier</Defn>s.

---

<Ref :name="(quote rsh):const" />
```reach
const DELAY = 10;
const [ Good, Bad ] = [ 42, 43 ];
const { x, y } = { x: 1, y: 2 };
const [ x, [ y ] ] = [ 1, [ 2 ] ];
const [ x, { y } ] = [ 1, { y: 2 } ];
const { x: [ a, b ] } = { x: [ 1, 2 ] };
```

::: note
Valid <Defn :name="identifiers">identifiers</Defn> follow the same rules as JavaScript identifiers:
they may consist of Unicode alphanumeric characters,
or `_` or `$`,
but may not begin with a digit.
:::

A <Defn :name="value definition">value definition</Defn> is written `const LHS = RHS;`.

`LHS` must obey the grammar:

XXX (BNF
 (list
  (nonterm "LHS")
  (nonterm "id")
  (BNF-seq (litchar "[") (nonterm "LHS-tuple-seq") (litchar "]"))
  (BNF-seq (litchar "{") (nonterm "LHS-obj-seq") (litchar "}")))
 (list
  (nonterm "LHS-tuple-seq")
  (BNF-seq)
  (BNF-seq (litchar "...") (nonterm "LHS"))
  (BNF-seq (nonterm "LHS"))
  (BNF-seq (nonterm "LHS") (litchar ",") (nonterm "LHS-tuple-seq")))
 (list
  (nonterm "LHS-obj-seq")
  (BNF-seq)
  (BNF-seq (litchar "...") (nonterm "LHS"))
  (BNF-seq (nonterm "LHS-obj-elem"))
  (BNF-seq (nonterm "LHS-obj-elem") (litchar ",") (nonterm "LHS-obj-seq")))
 (list
  (nonterm "LHS-obj-elem")
  (BNF-seq (nonterm "id"))
  (BNF-seq (nonterm "propertyName") (litchar ":") (nonterm "LHS")))
 (list
  (nonterm "propertyName")
  (nonterm "id")
  (nonterm "string")
  (nonterm "number")
  (BNF-seq (litchar "[") (nonterm "expr") (litchar "]"))))

`RHS` must be compatible with the given `LHS`.
That is, if a `LHS` is an `LHS-tuple-seq`, then the corresponding `RHS` must be a tuple with the correct number of elements.
If a `LHS` is an `LHS-obj-seq`, then the corresponding `RHS` must be an object with the correct fields.

Those values are available as their corresponding bound identifiers in the statement's tail.

---

<Ref :name="(quote rsh):function" />
```reach
function randomBool() {
  return (interact.random() % 2) == 0; }; 
```


A <Defn :name="function definition">function definition</Defn>, written `function FUN(LHS_0, ..., LHS_n) BLOCK;`, defines `FUN` as a function which abstracts its <Defn :name="function body">function body</Defn>, the block `BLOCK`, over the left-hand sides `LHS_0` through `LHS_n`.

Function parameters may specify default arguments. The expressions used to instantiate these parameters
have access to any variables in the scope of which the function was defined. Additionally, these expressions
may reference previous arguments of the function definition.
Parameters with default arguments must come after all other parameters.

```reach
function f(a, b, c = a + 1, d = b + c) =>
  a + b + c + d;
```


The last parameter of a function may be a <Defn :name="rest parameter">rest parameter</Defn>, which allows the function to be called
with an arbitrary number of arguments. A rest parameter is specified via `...IDENT`, where
`IDENT` is bound to a `Tuple` containing all the remaining arguments.

---

All identifiers in Reach programs must be <Defn :name="unbound">unbound</Defn>
at the position of the program where they are bound,
i.e., it is invalid to shadow identifiers with new definitions.
For example,

```reach
const x = 3;
const x = 4; 
```


is invalid.
This restriction is independent of whether a binding is
only known to a single participant. For example,

```reach
Alice.only(() => {
  const x = 3; });
Bob.only(() => {
  const x = 3; }); 
```


is invalid.

The special identifier `_` is an exception to this rule.
The `_` binding is always considered to be unbound.
This means means that `_` is both
an identifier that can never be read,
as well as an identifier that may be bound many times.
This may be useful for ignoring unwanted values, for example:

```reach
const [_, x, _] = [1, 2, 3];
```


### `return`

<Ref :name="(quote rsh):return" />
```reach
return 17;
return 3 + 4;
return f(2, false);
return; 
```


A <Defn :name="return statement">return statement</Defn>, written `return EXPR;`, where `EXPR` is an expression, evaluates to the same value as `EXPR`.
As a special case, `return;` is interpreted the same as `return null;`.

A return statement returns its value to the surrounding function application.

A return statement is a terminator statement, so it must have an empty tail.
For example,

```reach
{ return 1;
  return 2; } 
```


is invalid, because the first `return`'s tail is not empty.

Furthermore, a `return` must have an empty continuation (i.e. it must be in <Defn :name="tail position">tail position</Defn>.)

### `if`

<Ref :name="(quote rsh):if" /><Ref :name="(quote rsh):else" />
```reach
if ( 1 + 2 < 3 ) {
  return "Yes!";
} else {
  return "No, waaah!"; } 
```


A <Defn :name="conditional statement">conditional statement</Defn>,
written `if (COND) NOT_FALSE else FALSE`,
where `COND` is an expression
and `NOT_FALSE` and `FALSE` as statements
(potentially block statements),
selects between the `NOT_FALSE` statement and `FALSE` statement based on whether `COND` evaluates to `false`.

Both `NOT_FALSE` and `FALSE` have empty tails, i.e. the tail of the conditional statement is not propagated. For example,

```reach
if ( x < y ) {
  const z = 3; }
else {
  const z = 4; }
return z; 
```


is erroneous, because the identifier `z` is not bound outside the conditional statement.

A conditional statement may only include a consensus transfer in `NOT_FALSE` or `FALSE` if it is within a consensus step, because its statements are in the same context as the conditional statement itself.

If one branch of a conditional contains a `return`, then both must.

### `switch`

<Ref :name="(quote rsh):switch" /><Ref :name="(quote rsh):case" /><Ref :name="(quote rsh):default" />
```reach
const mi = Maybe(UInt).Some(42);
switch ( mi ) {
 case None: return 8;
 case Some: return mi + 10; }
switch ( mi ) {
 case None: return 8;
 default: return 41; } 
```


A <Defn :name="switch statement">switch statement</Defn>,
written `switch (VAR) { CASE ... }`,
where `VAR` is a variable bound to a data instance
and `CASE` is either `case VARIANT: STMT ...`, where `VARIANT` is a variant, or `default: STMT ...`, and `STMT` is a sequence of statements,
selects the appropriate sequence of statements based on which variant `VAR` holds.
Within the body of a `switch` case, `VAR` has the type of variant; i.e. in a `Some` case of a `Maybe(UInt)` `switch`, the variable is bound to an integer.

All cases have empty tails, i.e. the tail of the switch statement is not propagated.

A switch statement may only include a consensus transfer in its cases if it is within a consensus step, because its statements are in the same context as the conditional statement itself.

It is invalid for a case to appear multiple times, or be missing, or to be superfluous (i.e. for a variant that does not exist in the `Data` type of `VAR`).

If one case of a `switch` contains a `return`, then all must.

### Block statements

A <Defn :name="block statement">block statement</Defn> is when a block occurs in a statement position, then it establishes a local, separate scope for the definitions of identifiers within that block. In other words, the block is evaluated for effect, but the tail of the statements within the block are isolated from the surrounding tail. For example,

```reach
const x = 4;
return x; 
```


evaluates to `4`, but

```reach
{ const x = 4; }
return x; 
```


is erroneous, because the identifier `x` is not bound outside the block statement.

### Try/Catch & Throw Statements

```reach
try {
  throw 10;
} catch (v) {
  transfer(v).to(A); }
```


A <Defn :name="try statement">try statement</Defn>, written `try BLOCK catch (VAR) BLOCK`, allows a block
of code to execute with a specified handler should an exception be thrown.

A <Defn :name="throw statement">throw statement</Defn>,
written `throw EXPR`, will transfer control flow to the exception handler, binding `EXPR`
to `VAR`.
Any value that is able to exist at runtime may be thrown.
For example, `Int`s
and `Array`s are valid values to throw, but a function is not.
A `throw` must have an empty tail.

### Expression statements

```reach
4;
f(2, true); 
```


An expression, `E`, in a statement position is equivalent to the block statement `{ return E; }`.

## {#ref-programs-compute-exprs} Expressions

This section describes the expressions which are allowed in any Reach context.
There are a large variety of different <Defn :name="expressions">expressions</Defn> in Reach programs.

The remainder of this section enumerates each kind of expression.

### 'use strict'

<Ref :name="(quote rsh):'use strict'" />
```reach
'use strict'; 
```


 `'use strict'` enables unused variables checks for all subsequent
declarations within the current scope. If a variable is declared, but never used, there will
be an error emitted at compile time.

<Defn :name="strict mode">strict mode</Defn> will reject some code that is normally valid and limit how dynamic Reach's type system is.
For example, normally Reach will permit expressions like the following to be evaluated:

```reach
const foo = (o) =>
  o ? o.b : false;

void foo({ b: true });
void foo(false); 
```


Reach allows `o` to be either an object with a `b` field or `false` because it
partially evaluates the program at compile time. So, without `'use strict'`, Reach will not evaluate
`o.b` when `o = false` and this code will compile successfully.

But, in strict mode, Reach will ensure that this program treats `o` as
having a single type and detect an error in the program as follows:

```
reachc: error: Invalid field access. Expected object, got: Bool 
```


The correct way to write a program like this in strict mode is to use `Maybe`. Like this:

```reach
const MObj = Maybe(Object({ b : Bool }));

const foo = (mo) =>
  mo.match({
    None: (() => false),
    Some: ((o) => o.b)
  });

void foo(MObj.Some({ b : true }));
void foo(MObj.None()); 
```


### Identifier reference

```reach
X
Y
Z 
```


An identifier, written `ID`, is an expression that evaluates to the value of the bound identifier.

<Ref :name="(quote rsh):this" />
The identifier `this` has a special meaning inside of a local step (i.e. the body of an `only` or `each` expression), as well as in a consensus step (i.e. the tail of `publish` or `pay` statement and before a `commit` statement). For details, see XXX (secref "ref-programs-local-this") and XXX (secref "ref-programs-consensus-this").

### Function application

```reach
assert( amount <= heap1 )
step( moveA )
digest( coinFlip )
interact.random()
declassify( _coinFlip ) 
```


A <Defn :name="function application">function application</Defn>, written `EXPR_rator(EXPR_rand_0, ..., EXPR_rand_n)`, is an expression where `EXPR_rator` and `EXPR_rand_0` through `EXPR_rand_n` are expressions that evaluate to one value.
`EXPR_rator` must evaluate to an abstraction over `n` values or a primitive of arity `n`.
A spread expression (`...expr`) may appear in the list of operands to a function application, in which case the elements of the expr are spliced in place.

<Ref :name="(quote rsh):new" />
`new f(a)` is equivalent to `f.new(a)` and is a convenient short-hand for writing class-oriented programs.

### {#ref-programs-types} Types

Reach's <Defn :name="type">type</Defn>s are represented in programs by the following identifiers and constructors:

+ <Ref :name="(quote rsh):Null" /> `Null`.
+ <Ref :name="(quote rsh):Bool" /> `Bool`, which denotes a boolean.
+ <Ref :name="(quote rsh):UInt" /> `UInt`, which denotes an unsigned integer.
`UInt.max` is the largest value that may be assigned to a `UInt`.
+ <Ref :name="(quote rsh):Bytes" /> `Bytes(length)`, which denotes a string of bytes of length at most `length`.
Bytes of different lengths are not compatible; however the shorter bytes may be padded.
+ <Ref :name="(quote rsh):Digest" /> `Digest`, which denotes a digest.
+ <Ref :name="(quote rsh):Address" /> `Address`, which denotes an account address.
+ <Ref :name="(quote rsh):Token" /> `Token`, which denotes a non-network token. XXX (secref "ref-networks") discusses how `Token`s are represented on specific networks.
+ <Ref :name="(quote rsh):Fun" /> `Fun([Domain_0, ..., Domain_N], Range)`, which denotes a <Defn :name="function type">function type</Defn>, when `Domain_i` and `Range` are types.
The domain of a function is negative position.
The range of a function is positive position.
+ `Fun(true, Range)`, which denotes an <Defn :name="unconstrained domain function type">unconstrained domain function type</Defn>, when `Range` is a type.
These functions may only appear in participant interact interfaces.
+ <Ref :name="(quote rsh):Tuple" /> `Tuple(Field_0, ..., FieldN)`, which denotes a tuple.
(Refer to XXX (secref "ref-programs-tuples") for constructing tuples.)
+ <Ref :name="(quote rsh):Object" /> `Object({key_0: Type_0, ..., key_N: Type_N})`, which denotes an object.
(Refer to XXX (secref "ref-programs-objects") for constructing objects.)
+ <Ref :name="(quote rsh):Struct" /> `Struct([[key_0, Type_0], ..., [key_N, Type_N]])`, which denotes a struct.
(Refer to XXX (secref "ref-programs-structs") for constructing structs.)
+ <Ref :name="(quote rsh):Array" /> `Array(Type_0, size)`, which denotes a statically-sized array.
`Type_0` must be a type that can exist at runtime (i.e., not a function type.)
(Refer to XXX (secref "ref-programs-arrays") for constructing arrays.)
+ <Ref :name="(quote rsh):Data" /> `Data({variant_0: Type_0, ..., variant_N: Type_N})`, which denotes a [tagged union](https://en.wikipedia.org/wiki/Tagged_union) (or _sum type_).
(Refer to XXX (secref "ref-programs-data") for constructing data instances.)
+ <Ref :name="(quote rsh):Refine" /> `Refine(Type_0, Predicate, ?Message)`, where `Predicate` is a unary function returning a boolean, which denotes a [refinement type](https://en.wikipedia.org/wiki/Refinement_type), that is instances of `Type_0` that satisfy `Predicate`.
When a refinement type appears in a <Defn :name="negative position">negative position</Defn> (such as in an `is` or in the domain of a `Fun` of a participant interact interface), it introduces an `assert`;
while when it is in a <Defn :name="positive position">positive position</Defn>, it introduces an `assume`.
`Message` is an optional string to display if the predicate fails verification.

For example, if `f` had type ```reach
Fun([Refine(UInt, (x => x < 5))], Refine(UInt, (x => x > 10)))
```


then `const z = f(y)` is equivalent to

```reach
assert(y < 5);
const z = f(y);
assume(z > 10);
```

+ `Refine(Type_0, PreCondition, PostCondition, ?Messages)`, where `Type_0` is a function type, `PreCondition` is a unary function that accepts a tuple of the domain and returns a boolean, and `PostCondition` is a binary function that accepts a tuple of the domain and the range and returns a boolean, denotes a function type with a [precondition](https://en.wikipedia.org/wiki/Precondition) and [postcondition](https://en.wikipedia.org/wiki/Postcondition).
Preconditions are enforced with `assert` and postconditions are enforced with `assume`.
`Messages` is an optional two-tuple of `Bytes`.
The first message will be displayed when the precondition fails verification and the second when the postcondition fails verification.

For example, `Refine(Fun([UInt, UInt], UInt), ([x, y] => x < y), (([x, y], z) => x + y < z))` is a function that requires its second argument to be larger than its first and its result to be larger than its input.


`Object` and `Data` are commonly used to implemented [algebraic data types](https://en.wikipedia.org/wiki/Algebraic_data_type) in Reach.

<Ref :name="(quote rsh):typeOf" /><Ref :name="(quote rsh):isType" /><Ref :name="(quote rsh):is" />
```reach
typeOf(x) // type
isType(t) // Bool
is(x, t) // t
```


The `typeOf` primitive function is the same as `typeof`:
it returns the type of its argument.

The `isType` function returns `true` if its argument is a type.
Any expression satisfying `isType` is compiled away and does not exist at runtime.

The `is` function returns its first argument if it satisfies the type specified by the second argument.
If it is not, then the program is invalid.
For example, `is(5, UInt)` returns `5`, while `is(5, Bool)` is an invalid program.
The value returned by `is` may not be identical to the input, because in some cases, such as for functions, it will record the applied to type and enforce it on future invocations.
These applications are considered negative positions for `Refine`.

### Literal values

<Ref :name="(quote rsh):true" /><Ref :name="(quote rsh):false" /><Ref :name="(quote rsh):null" />
```reach
10
0xdeadbeef
007
-10
34.5432
true
false
null
"reality bytes"
'it just does' 
```


A <Defn :name="literal value">literal value</Defn>,
written `VALUE`,
is an expression that evaluates to the given value.

The <Defn :name="null literal">null literal</Defn> may be written as `null`.

<Defn :name="Numeric literal">Numeric literal</Defn>s may be written in decimal, hexadecimal, or octal.
Numeric literals must obey the <Defn :name="bit width">bit width</Defn> of `UInt` if they are used as `UInt` values at runtime, but if they only appear at compile-time, then they may be any positive number.
Reach provides abstractions for working with `Int`s and signed `FixedPoint` numbers.
`Int`s may be defined by applying the unary `+` and `-` operators to values of type `UInt`.
Reach provides syntactic sugar for defining signed `FixedPoint` numbers, in base 10, with decimal syntax.

<Defn :name="Boolean literal">Boolean literal</Defn>s may be written as `true` or `false`.

<Defn :name="String literal">String literal</Defn>s (aka byte strings)
may be written between double or single quotes
(with no distinction between the different styles)
and use the same escaping rules as JavaScript.
Since `Bytes` types are specialized in their length, literals typically need to be padded to be useful.

### Operator expression

An <Defn :name="operator">operator</Defn> is a special identifier,
which is either a unary operator, or a binary operator.

---

<Ref :name="(quote rsh):!" /><Ref :name="(quote rsh):-" /><Ref :name="(quote rsh):+" /><Ref :name="(quote rsh):typeof" /><Ref :name="(quote rsh):not" /><Ref :name="(quote rsh):minus" /><Ref :name="(quote rsh):plus" /><Ref :name="(quote rsh):void" />
```reach
! a  // not
- a  // minus
+ a  // plus
typeof a
void a
```


A <Defn :name="unary expression">unary expression</Defn>, written `UNAOP EXPR_rhs`, where `EXPR_rhs` is an expression and `UNAOP` is one of the <Defn :name="unary operator">unary operator</Defn>s: `! - + typeof void`. All the unary operators, besides `typeof`, have a
corresponding named version in the standard library.

It is invalid to use unary operations on the wrong types of values.

When applied to values of type `UInt`, unary `-` and `+` operators will cast
their arguments to type `Int`. The unary `-` and `+` operations are defined for
values of type: `Int`, and `FixedPoint`.

`void a` evaluates to `null` for all arguments.

---

<Ref :name="(quote rsh):&&" /><Ref :name="(quote rsh):||" /><Ref :name="(quote rsh):+" /><Ref :name="(quote rsh):-" /><Ref :name="(quote rsh):*" /><Ref :name="(quote rsh):/" /><Ref :name="(quote rsh):%" /><Ref :name="(quote rsh):|" /><Ref :name="(quote rsh):&" /><Ref :name="(quote rsh):^" /><Ref :name="(quote rsh):<<" /><Ref :name="(quote rsh):>>" /><Ref :name="(quote rsh):==" /><Ref :name="(quote rsh):!=" /><Ref :name="(quote rsh):===" /><Ref :name="(quote rsh):!==" /><Ref :name="(quote rsh):>" /><Ref :name="(quote rsh):>=" /><Ref :name="(quote rsh):<=" /><Ref :name="(quote rsh):<" />
```reach
a && b
a || b
a + b
a - b
a * b
a / b
a % b
a | b
a & b
a ^ b
a << b
a >> b
a == b
a != b
a === b
a !== b
a > b
a >= b
a <= b
a < b 
```


::: note
Bitwise operations are not supported by all consensus networks and greatly decrease the efficiency of verification.
:::

A <Defn :name="binary expression">binary expression</Defn> is written `EXPR_lhs BINOP EXPR_rhs`, where `EXPR_lhs` and `EXPR_rhs` are expressions and `BINOP` is one of the <Defn :name="binary operator">binary operator</Defn>s: `&& || + - * / % | & ^ << >> == != === !== > >= <= <`.
Numeric operations, like `+` and `>`, only operate on numbers.
Since all numbers in Reach are integers, operations like `/` truncate their result.
Boolean operations, like `&&`, only operate on booleans.
It is invalid to use binary operations on the wrong types of values.

<Ref :name="(quote rsh):and" /><Ref :name="(quote rsh):or" /><Ref :name="(quote rsh):add" /><Ref :name="(quote rsh):sub" /><Ref :name="(quote rsh):mul" /><Ref :name="(quote rsh):div" /><Ref :name="(quote rsh):mod" /><Ref :name="(quote rsh):lt" /><Ref :name="(quote rsh):le" /><Ref :name="(quote rsh):ge" /><Ref :name="(quote rsh):gt" /><Ref :name="(quote rsh):lsh" /><Ref :name="(quote rsh):rsh" /><Ref :name="(quote rsh):band" /><Ref :name="(quote rsh):bior" /><Ref :name="(quote rsh):band" /><Ref :name="(quote rsh):bxor" /><Ref :name="(quote rsh):polyEq" /><Ref :name="(quote rsh):polyNeq" />
```reach
and(a, b)     // &&
or(a, b)      // ||
add(a, b)     // +
sub(a, b)     // -
mul(a, b)     // *
div(a, b)     // /
mod(a, b)     // %
lt(a, b)      // <
le(a, b)      // <=
ge(a, b)      // >=
gt(a, b)      // >
lsh(a, b)     // <<
rsh(a, b)     // >>
band(a, b)    // &
bior(a, b)    // |
bxor(a, b)    // ^
polyEq(a, b)  // ==, ===
polyNeq(a, b) // !=, !==
```


All binary expression operators have a corresponding named function in the standard library.
While `&&` and `||` may not evaluate their second argument,
their corresponding named functions `and` and `or`, always do.

<Ref :name="(quote rsh):boolEq" /><Ref :name="(quote rsh):typeEq" /><Ref :name="(quote rsh):intEq" /><Ref :name="(quote rsh):digestEq" /><Ref :name="(quote rsh):addressEq" /><Ref :name="(quote rsh):fxeq" /><Ref :name="(quote rsh):ieq" />
```reach
polyEq(a, b)    // eq on all types
boolEq(a, b)    // eq on Bool
typeEq(a, b)    // eq on types
intEq(a, b)     // eq on UInt
digestEq(a, b)  // eq on Digest
addressEq(a, b) // eq on Addresses
fxeq(a, b)      // eq on FixedPoint
ieq(a, b)       // eq on Int
```


Equality functions, like `==`, `===`, `!=`, and `!==`, operate on all types.
However, values with different types are always not equal.
Both arguments must be of the same type.
Specialized functions exist for equality checking on each supported type.

---

If `verifyArithmetic` is `true`, then arithmetic operations automatically make a static assertion that their arguments would not overflow the bit width of the enabled consensus networks.
If it is `false`, then the connector will ensure this dynamically.

### xor

```reach
xor(false, false); // false
xor(false, true);  // true
xor(true, false);  // true
xor(true, true);   // false 
```


 `xor(Bool, Bool)` returns `true` only when the inputs differ in value.

### Padding

<Ref :name="(quote rsh):pad" />
```reach
Bytes(16).pad('abc');
```


`Bytes` are like `Array`s in that they are fixed and exactly sized.
This means that two `Bytes` of different lengths are not interchangeable.

For example, `'You win!'` and `'You lose!'` cannot both be provided to an `interact` function, because the second is one character longer.
Most of the time this is good, because it is a signal that you should use a `Data` type instead, so that the formatting and display logic is entirely controlled by the frontend.

But, sometimes it is necessary and useful to extend one byte string into a larger size.
Each `Bytes` type has a `pad` field that is bound to a function that extends its argument to the needed size.
A byte string extended in this way is called <Defn :name="padded">padded</Defn>, because it is extended with additional `NUL` bytes.

### Parenthesized expression

```reach
(a + b) - c 
```


An expression may be parenthesized, as in `(EXPR)`.

### {#ref-programs-tuples} Tuples

```reach
[ ]
[ 1, 2 + 3, 4 * 5 ] 
```


A <Defn :name="tuple">tuple</Defn> literal, written `[ EXPR_0, ..., EXPR_n ]`, is an expression which evaluates to a tuple of `n` values, where `EXPR_0` through `EXPR_n` are expressions.

`...expr` may appear inside tuple expressions, in which case the spreaded expression must evaluate to a tuple or array, which is spliced in place.

### {#ref-programs-arrays} `array`

<Ref :name="(quote rsh):array" />
```reach
const x = array(UInt, [1, 2, 3]); 
```


Converts a tuple of homogeneous values of the specific type into an <Defn :name="array">array</Defn>.

### Element reference

```reach
arr[3] 
```


A <Defn :name="reference">reference</Defn>, written `REF_EXPR[IDX_EXPR]`,
where `REF_EXPR` is an expression that evaluates to an array, a tuple, or a struct
and `IDX_EXPR` is an expression that evaluates to a natural number which is less than the size of the array,
selects the element at the given index of the array.
Indices start at zero.

If `REF_EXPR` is a tuple, then `IDX_EXPR` must be a compile-time constant, because tuples do not support dynamic access, because each element may be a different type.

If `REF_EXPR` is a mapping and `IDX_EXPR` evaluates to an address, then this reference evaluates to a value of type `Maybe(TYPE)`, where `TYPE` is the type of the mapping.

### Array & tuple length: `Tuple.length`, `Array.length`, and `.length`

<Ref :name="(quote rsh):length" />
```reach
Tuple.length(tup);
tup.length;
Array.length(arr);
arr.length; 
```


 `Tuple.length` Returns the length of the given tuple.

 `Array.length` Returns the length of the given array.

Both may be abbreviated as `expr.length` where `expr` evaluates to a tuple or an array.

### Array & tuple update: `Tuple.set`, `Array.set`, and `.set`

<Ref :name="(quote rsh):set" />
```reach
Tuple.set(tup, idx, val);
tup.set(idx, val);
Array.set(arr, idx, val);
arr.set(idx, val); 
```


 `Tuple.set` Returns a new tuple identical to `tup`,
except that index `idx` is replaced with `val`.
The `idx` must be a compile-time constant, because tuples do not support dynamic access, because each element may be a different type.

 `Array.set` Returns a new array identical to `arr`, except that index `idx` is replaced with `val`.

Both may be abbreviated as `expr.set(idx, val)` where `expr` evaluates to a tuple or an array.

### Array element type: `Array.elemType` and `.elemType`

<Ref :name="(quote rsh):elemType" />
```reach
Array.elemType(arr)
arr.elemType 
```


 `Array.elemType` Returns the `Type` of elements that the array contains.

### Foldable operations

The following methods are available on any <Ref :name="(quote rsh):Foldable" />`Foldable` containers, such as: `Array`s and `Map`s.

####  `Foldable.forEach` && `.forEach`

<Ref :name="(quote rsh):forEach" />
```reach
c.forEach(f)
Foldable.forEach(c, f)
Array.forEach(c, f)
Map.forEach(c, f) 
```


 `Foldable.forEach(c, f)` iterates the function `f` over the elements of a container `c`, discarding the result.
This may be abbreviated as `c.forEach(f)`.

#### `Foldable.all` && `.all`

<Ref :name="(quote rsh):all" />
```reach
Foldable.all(c, f)
Array.all(c, f)
Map.all(c, f)
c.all(f) 
```


 `Foldable.all(c, f)` determines whether the predicate, `f`, is satisfied
by every element of the container, `c`.

#### `Foldable.any` && `.any`

<Ref :name="(quote rsh):any" />
```reach
Foldable.any(c, f)
Array.any(c, f)
Map.any(c, f)
c.any(f) 
```


 `Foldable.any(c, f)` determines whether the predicate, `f`, is satisfied
by at least one element of the container, `c`.

#### `Foldable.or` && `.or`

<Ref :name="(quote rsh):or" />
```reach
Foldable.or(c)
Array.or(c)
Map.or(c)
c.or() 
```


 `Foldable.or(c)` returns the disjunction of a container of `Bool`s.

#### `Foldable.and` && `.and`

<Ref :name="(quote rsh):and" />
```reach
Foldable.and(c)
Array.and(c)
Map.and(c)
c.and() 
```


 `Foldable.and(c)` returns the conjunction of a container of `Bool`s.

#### `Foldable.includes` && `.includes`

<Ref :name="(quote rsh):includes" />
```reach
Foldable.includes(c, x)
Array.includes(c, x)
Map.includes(c, x)
c.includes(x) 
```


 `Foldable.includes(c, x)` determines whether the container includes
the element, `x`.

#### `Foldable.count` && `.count`

<Ref :name="(quote rsh):count" />
```reach
Foldable.count(c, f)
Array.count(c, f)
Map.count(c, f)
c.count(f) 
```


 `Foldable.count(c, f)` returns the number of elements in `c` that
satisfy the predicate, `f`.

#### `Foldable.size` && `.size`

<Ref :name="(quote rsh):size" />
```reach
Foldable.size(c)
Array.size(c)
Map.size(c)
c.size() 
```


 `Foldable.size(c)` returns the number of elements in `c`.

#### `Foldable.min` && `.min`

<Ref :name="(quote rsh):min" />
```reach
Foldable.min(c)
Array.min(c)
Map.min(c)
c.min() 
```


 `Foldable.min(c)` returns the lowest number in a container of `UInt`s.

#### `Foldable.max` && `.max`

<Ref :name="(quote rsh):max" />
```reach
Foldable.max(c)
Array.max(c)
Map.max(c)
c.max() 
```


 `Foldable.max(c)` returns the largest number in a container of `UInt`s.

#### `Foldable.sum` && `.sum`

<Ref :name="(quote rsh):sum" />
```reach
Foldable.sum(c)
Array.sum(c)
Map.sum(c)
c.sum() 
```


 `Foldable.sum(c)` returns the sum of a container of `UInt`s.

#### `Foldable.product` && `.product`

<Ref :name="(quote rsh):product" />
```reach
Foldable.product(c)
Array.product(c)
Map.product(c)
c.product() 
```


 `Foldable.product(c)` returns the product of a container of `UInt`s.

#### `Foldable.average` && `.average`

<Ref :name="(quote rsh):average" />
```reach
Foldable.average(c)
Array.average(c)
Map.average(c)
c.average() 
```


 `Foldable.average(c)` returns the mean of a container of `UInt`s.

### Array group operations

`Array` is a `Foldable` container. Along with the methods of `Foldable`, the
following methods may be used with `Array`s.

#### `Array.iota`

<Ref :name="(quote rsh):iota" />
```reach
Array.iota(5) 
```


 `Array.iota(len)` returns an array of length `len`, where each element is the same as its index.
For example, `Array.iota(4)` returns `[0, 1, 2, 3]`.
The given `len` must evaluate to an integer at compile-time.

#### `Array.replicate` && `.replicate`

<Ref :name="(quote rsh):Array_replicate" /><Ref :name="(quote rsh):replicate" />
```reach
Array.replicate(5, "five")
Array_replicate(5, "five") 
```


 `Array.replicate(len, val)` returns an array of length `len`, where each element is `val`.
For example, `Array.replicate(4, "four")` returns `["four", "four", "four", "four"]`.
The given `len` must evaluate to an integer at compile-time.

#### `Array.concat` && `.concat`

<Ref :name="(quote rsh):concat" />
```reach
Array.concat(x, y)
x.concat(y) 
```


 `Array.concat(x, y)` concatenates the two arrays `x` and `y`.
This may be abbreviated as `x.concat(y)`.

#### `Array.empty`

<Ref :name="(quote rsh):Array_empty" /><Ref :name="(quote rsh):empty" />
```reach
Array_empty
Array.empty 
```


 `Array.empty` is an array with no elements.
It is the identity element of `Array.concat`.
It may also be written `Array_empty`.

#### `Array.zip` && `.zip`

<Ref :name="(quote rsh):zip" />
```reach
Array.zip(x, y)
x.zip(y) 
```


 `Array.zip(x, y)` returns a new array the same size as `x` and `y` (which must be the same size) whose elements are tuples of the elements of `x` and `y`.
This may be abbreviated as `x.zip(y)`.

#### `Array.map` && `.map`

<Ref :name="(quote rsh):map" />
```reach
Array.map(arr, f)
arr.map(f) 
```


 `Array.map(arr, f)` returns a new array, `arr_mapped`, the same size as `arr`, where `arr_mapped[i] = f(arr[i])` for all `i`.
For example, `Array.iota(4).map(x => x+1)` returns `[1, 2, 3, 4]`.
This may be abbreviated as `arr.map(f)`.

This function is generalized to an arbitrary number of arrays of the same size, which are provided before the `f` argument.
For example, `Array.iota(4).map(Array.iota(4), add)` returns `[0, 2, 4, 6]`.

#### `Array.mapWithIndex` && `.mapWithIndex`

<Ref :name="(quote rsh):mapWithIndex" />
```reach
Array.mapWithIndex(arr, f)
arr.mapWithIndex(f) 
```


 `Array.mapWithIndex(arr, f)` is similar to `Array.map`, except it
provides `f` with an additional argument, which is the index of the current element in `arr`.
Unlike `Array.map`, this function is not generalized to an arbitrary number of arrays; it only accepts one array.

#### `Array.reduce` && `.reduce`

<Ref :name="(quote rsh):reduce" />
```reach
Array.reduce(arr, z, f)
arr.reduce(z, f) 
```


 `Array.reduce(arr, z, f)` returns the [left fold](https://en.wikipedia.org/wiki/Fold_(higher-order_function)) of the function `f` over the given array with the initial value `z`.
For example, `Array.iota(4).reduce(0, add)` returns `((0 + 1) + 2) + 3 = 6`.
This may be abbreviated as `arr.reduce(z, f)`.

This function is generalized to an arbitrary number of arrays of the same size, which are provided before the `z` argument.
For example, `Array.iota(4).reduce(Array.iota(4), 0, (x, y, z) => (z + x + y))` returns `((((0 + 0 + 0) + 1 + 1) + 2 + 2) + 3 + 3)`.

#### `Array.reduceWithIndex` && `.reduceWithIndex`

<Ref :name="(quote rsh):reduceWithIndex" />
```reach
Array.reduceWithIndex(arr, z, f)
arr.reduceWithIndex(z, f) 
```


 `Array.reduceWithIndex(arr, z, f)` is similar to `Array.reduce`, except it
provides `f` with an additional argument, which is the index of the current element in `arr`.
Unlike `Array.reduce`, this function is not generalized to an arbitrary number of arrays; it only accepts one array.

#### `Array.indexOf` && `.indexOf`

<Ref :name="(quote rsh):indexOf" />
```reach
Array.indexOf(arr, x)
arr.indexOf(x) 
```


 `Array.indexOf(arr, x)` returns the index of the first element
in the given array that is equal to `x`. The return value is of type `Maybe(UInt)`. If
the value is not present in the array, `None` is returned.

#### `Array.findIndex` && `.findIndex`

<Ref :name="(quote rsh):findIndex" />
```reach
Array.findIndex(arr, f)
arr.findIndex(f) 
```


 `Array.findIndex(arr, f)` returns the index of the first element
in the given array that satisfies the predicate `f`. The return value is of type `Maybe(UInt)`. If
no value in the array satisfies the predicate, `None` is returned.

#### `Array.find` && `.find`

<Ref :name="(quote rsh):find" />
```reach
Array.find(arr, f)
arr.find(f) 
```


 `Array.find(arr, f)` returns the first element in the array, `arr`,
that satisfies the predicate `f`. The return value is of type `Maybe`. If no value in the
array satisfies the predicate, `None` is returned.

#### `Array.withIndex` && `.withIndex`

<Ref :name="(quote rsh):withIndex" />
```reach
Array.withIndex(arr)
arr.withIndex() 
```


 `Array.withIndex(arr)` returns an array where every element of `arr`
is paired with its index. For example, `array(Bool, [false, true]).withIndex()` returns
`array(Tuple(Bool, UInt), [[false, 0], [true, 1]])`.

#### `Array.slice` && `.slice`

<Ref :name="(quote rsh):slice" />
```reach
Array.slice(arr, start, length)
arr.slice(start, length)
```


 `Array.slice(arr, start, length)` returns a portion of `arr`, starting from
the `start` index, up to the `start + length` index.

### Mapping group operations

`Map` is a `Foldable` container. Mappings may be aggregated with the following
operations and those of `Foldable` within the `invariant` of a `while` loop.

#### `Map.reduce` && `.reduce`

```reach
Map.reduce(map, z, f)
map.reduce(z, f) 
```


 `Map.reduce(map, z, f)` returns the [left fold](https://en.wikipedia.org/wiki/Fold_(higher-order_function)) of the function `f` over the given mapping with the initial value `z`.
For example, `m.reduce(0, add)` sums the elements of the mapping.
This may be abbreviated as `map.reduce(z, f)`.

The function `f` must satisfy the property, for all `z`, `a`, `b`, `f(f(z, b), a) == f(f(z, a), b)`, because the order of evaluation is unpredictable.

### {#ref-programs-objects} Objects

```reach
{ }
{ x: 3, "yo-yo": 4 }
{ [1 < 2 ? "one" : "two"]: 5 }
```


An <Defn :name="object">object</Defn>,
typically written `{ KEY_0: EXPR_0, ..., KEY_n: EXPR_n }`,
where `KEY_0` through `KEY_n` are identifiers or string literals
and `EXPR_0` through `EXPR_n` are expressions,
is an expression which evaluates to an object
with fields `KEY_0` through `KEY_n`.

Additional object literal syntax exists for convenience, such as:

```reach
{ ...obj, z: 5 }
```


An <Defn :name="object splice">object splice</Defn>,
where all fields from `obj` are copied into the object;
these fields may be accompanied by additional fields specified afterwards.

```reach
{ x, z: 5 }
```


Shorthand for `{ x: x, z: 5}`, where `x` is any bound identifier.

### {#ref-programs-structs} Structs

```reach
const Posn = Struct([["x", UInt], ["y", UInt]]);
const p1 = Posn.fromObject({x: 1, y: 2});
const p2 = Posn.fromTuple([1, 2]);
```


A <Defn :name="struct">struct</Defn> is a combination of a tuple and an object.
It has named elements, like an object, but is ordered like a tuple, so its elements may be accessed by either name or position.
Structs exist for interfacing with non-Reach remote objects, where both parties must agree to the runtime representation of the values.


A struct instance may be constructed by calling the `fromTuple` method of a struct type instance (like `Posn`) with a tuple of the appropriate length.


A struct instance may be constructed by calling the `fromObject` method of a struct type instance (like `Posn`) with an object with the appropriate fields.



Structs may be converted into a corresponding tuple or object via the `toTuple` and `toObject` methods on the `Struct` value (as well as struct type instances, like `Posn` in the example above):

```reach
assert(Posn.toTuple(p1)[0] == 1);
assert(Struct.toObject(p2).y == 2);
```


The names of elements may be restricted to avoid conflicting with reserved words of the specified connectors.

### Field reference

```reach
obj.x
```


An <Defn :name="object reference">object reference</Defn>,
written `OBJ.FIELD`,
where `OBJ` is an expression that evaluates to an object or a struct,
and `FIELD` is a valid identifier,
accesses the `FIELD` <Defn :name="field">field</Defn> of object OBJ.

### `Object.set`

<Ref :name="(quote rsh):Object_set" />
```reach
Object.set(obj, fld, val);
Object_set(obj, fld, val);
{ ...obj, [fld]: val };
```


 Returns a new object identical to `obj`,
except that field `fld` is replaced with `val`.

### `Object.setIfUnset`

<Ref :name="(quote rsh):Object_setIfUnset" />
```reach
Object.setIfUnset(obj, fld, val);
Object_setIfUnset(obj, fld, val);
```


 Returns a new object identical to `obj`,
except that field `fld` is `val` if `fld` is not already present in `obj`.

### `Object.has`

```reach
Object.has(obj, fld);
```


 Returns a boolean indicating whether the object has the field `fld`.
This is statically known.

### {#ref-programs-data} Data

```reach
const Taste = Data({Salty: Null,
                    Spicy: Null,
                    Sweet: Null,
                    Umami: Null});
const burger = Taste.Umami();

const Shape = Data({ Circle: Object({r: UInt}),
                     Square: Object({s: UInt}),
                     Rect: Object({w: UInt, h: UInt}) });
const nice = Shape.Circle({r: 5}); 
```


A <Defn :name="data instance">data instance</Defn> is written `DATA.VARIANT(VALUE)`, where `DATA` is `Data` type, `VARIANT` is the name of one of `DATA`'s variants, and `VALUE` is a value matching the type of the variant.
As a special case, when the type of a variant is `Null`, the `VALUE` may be omitted, as shown in the definition of `burger` in the same above.

Data instances are consumed by `switch` statements and `match` expressions.

### `Maybe`

<Ref :name="(quote rsh):Maybe" /><Ref :name="(quote rsh):Some" /><Ref :name="(quote rsh):None" /><Ref :name="(quote rsh):fromMaybe" />
```reach
const MayInt = Maybe(UInt);
const bidA = MayInt.Some(42);
const bidB = MayInt.None(null);

const getBid = (m) => fromMaybe(m, (() => 0), ((x) => x));
const bidSum = getBid(bidA) + getBid(bidB);
assert(bidSum == 42); 
```


[Option types](https://en.wikipedia.org/wiki/Option_type) are represented in Reach through the built-in `Data` type, `Maybe`, which has two variants: `Some` and `None`.

`Maybe` is defined by
```reach
export const Maybe = (A) => Data({None: Null, Some: A}); 
```


This means it is a function that returns a `Data` type specialized to a particular type in the `Some` variant.

`Maybe` instances can be conveniently consumed by `fromMaybe(mValue, onNone, onSome)`, where `onNone` is a function of no arguments which is called when `mValue` is `None`, `onSome` is a function of one argument which is called with the value when `mValue` is `Some`, and `mValue` is a data instance of `Maybe`.

<Ref :name="(quote rsh):isNone" /><Ref :name="(quote rsh):isSome" />
```reach
const m = Maybe(UInt).Some(5);
isNone(m); // false
isSome(m); // true
```


 `isNone` is a convenience method that determines whether the variant is `None`.

 `isSome` is a convenience method that determines whether the variant is `Some`.


<Ref :name="(quote rsh):fromSome" />
```reach
fromSome(Maybe(UInt).Some(1), 0); // 1
fromSome(Maybe(UInt).None(), 0);  // 0
```


 `fromSome` receives a `Maybe` value and a default value as arguments and will return the value inside
of the `Some` variant or the default value otherwise.

<Ref :name="(quote rsh):maybe" />
```reach
const add1 = (x) => x + 1;
maybe(Maybe(UInt).Some(1), 0, add1); // 2
maybe(Maybe(UInt).None(), 0, add1);  // 0
```


 `maybe(m, defaultVal, f)` receives a `Maybe` value, a default value, and a unary function as arguments. The function will
either return the application of the function, `f`, to the `Some` value or return the default value provided.

### `Either`

`Either` is defined by
```reach
export const Either = (A, B) => Data({Left: A, Right: B}); 
```


`Either` can be used to represent values with two possible types.

Similar to `Maybe`, `Either` may be used to represent values that are correct or erroneous.
A successful result is stored, by convention, in `Right`. Unlike `None`, `Left` may
carry additional information about the error.

<Ref :name="(quote rsh):either" />
```reach
either(e, onLeft, onRight) 
```


 `either(e, onLeft, onRight)` will either apply the function `onLeft` or `onRight` depending on `e`.

<Ref :name="(quote rsh):isLeft" /><Ref :name="(quote rsh):isRight" /><Ref :name="(quote rsh):fromLeft" /><Ref :name="(quote rsh):fromRight" />
```reach
const e = Either(UInt, Bool);
const l = e.Left(1);
const r = e.Right(true);
isLeft(l);  // true
isRight(l); // false
const x = fromLeft(l, 0);      // x = 1
const y = fromRight(l, false); // y = false 
```


 `isLeft` is a convenience method that determines whether the variant is `Left`.

 `isRight` is a convenience method that determines whether the variant is `Right`.

 `fromLeft(e, default)` is a convenience method that returns the value in `Left`,
or `default` if the variant is `Right`.

 `fromRight(e, default)` is a convenience method that returns the value in `Right`,
or `default` if the variant is `Left`.

### `match`

<Ref :name="(quote rsh):match" />
```reach
const Value = Data({
   EBool: Bool,
   EInt: UInt,
   ENull: Null,
 });
 const v1 = Value.EBool(true);
 const v2 = Value.EInt(200);
 const isTruthy = (v) =>
   v.match({
     EBool: (b) => { return b },
     EInt : (i) => { return i != 0 },
     ENull: ()  => { return false }
   });

 assert(isTruthy(v1));
 assert(isTruthy(v2));
```


A <Defn :name="match expression">match expression</Defn>, written `VAR.match({ CASE ... })`, where `VAR` is a variable
bound to a data instance and `CASE` is `VARIANT: FUNCTION`, where `VARIANT` is a
variant or `default`, and `FUNCTION` is a function that takes the same arguments as the
variant constructor.
If the variant has a type of `Null`, then the function is allowed to take no arguments.
`default` functions must always take an argument, even if all defaulted variants have type `Null`.

`match` is similar to a switch statement, but since it is an expression, it
can be conveniently used in places like the right hand side of an assignment statement.

Similar to a switch statement, the cases are expected to be exhaustive and nonredundant,
all cases have empty tails, and it may only include a consensus transfer in
its cases if it is within a consensus step.

### Conditional expression

<Ref :name="(quote rsh):?" />
```reach
choosesFirst ? [ heap1 - amount, heap2 ] : [ heap1, heap2 - amount ] 
```


A <Defn :name="conditional expression">conditional expression</Defn>, written `COND_E ? NOT_FALSE_E : FALSE_E`, where `COND_E`, `NOT_FALSE_E`, and `FALSE_E` are expressions, selects between the values which `NOT_FALSE_E` and `FALSE_E` evaluate to based on whether `COND_E` evaluates to `false`.

<Ref :name="(quote rsh):ite" />
```reach
ite(choosesFirst, [heap1 - amount, heap2], [heap1, heap2 - amount])
```


Conditional expressions may also be written with the `ite` function,
however, note that this function always evaluates both of its branches,
while the regular conditional expression only evaluates one branch.

### Arrow expression

<Ref :name="(quote rsh):=>" />
```reach
(() => 4)
((x) => x + 1)
((x) => { const y = x + 1;
          return y + 1; })
((x, y) => { assert(x + y == 3); })(1, 2);
((x, y) => { assert(x + y == 3); })(...[1, 2]);
((x, y = 2) => { assert(x + y == 3); })(1);
((x, y = 2) => { assert(x + y == 2); })(1, 1);
(([x, y]) => { assert(x + y == 3); })([1, 2]);
(({x, y}) => { assert(x + y == 3); })({x: 1, y: 2});
(([x, [y]]) => { assert(x + y == 3); })([1,[2]]);
(([x, {y}]) => { assert(x + y == 3); })([1,{ y: 2 }]);
((...xs) => Foldable.sum(xs))(1, 2, 3)
```


An <Defn :name="arrow expression">arrow expression</Defn>, written `(LHS_0, ..., LHS_n) => EXPR`, where `LHS_0` through `LHS_n` are left-hand sides and `EXPR` is an expression, evaluates to a function which is an abstraction of `EXPR` over `n` values compatible with the respective left-hand side.
Like function definitions, arrow expressions may use default argument notation and rest parameters.

### `makeEnum`

<Ref :name="(quote rsh):makeEnum" />
```reach
const [ isHand, ROCK, PAPER, SCISSORS ] = makeEnum(3); 
```


An <Defn :name="enumeration">enumeration</Defn> (or <Defn :name="enum">enum</Defn>, for short),
can be created by calling the `makeEnum` function, as in `makeEnum(N)`,
where `N` is the number of distinct values in the enum.
This produces a tuple of `N+1` values,
where the first value is a `Fun([UInt], Bool)`
which tells you if its argument is one of the enum's values,
and the next N values are distinct `UInt`s.

### `assert`

<Ref :name="(quote rsh):assert" />
```reach
assert( claim, [msg] ) 
```


 A static assertion which is only valid if `claim` always evaluates to `true`.
::: note
The Reach compiler will produce a counter-example (i.e. an assignment of the identifiers in the program to falsify the `claim`) when an invalid `claim` is provided.
It is possible to write a `claim` that actually always evaluates to `true`, but for which our current approach cannot prove always evaluates to `true`; if this is the case, Reach will fail to compile the program, reporting that its analysis is incomplete.
Reach will never produce an erroneous counter-example.
:::
It accepts an optional bytes argument, which is included in any reported violation.

::: note
See [the guide section on verification](##guide-assert) to better understand how and what to verify in your program.
:::

### `forall`

<Ref :name="(quote rsh):forall" />
```reach
forall( Type )
forall( Type, (var) => BLOCK ) 
```


 The single argument version returns an abstract value of the given type.
It may only be referenced inside of assertions; any other reference is invalid.

The two argument version is an abbreviation of calling the second argument with the result of `forall(Type)`.
This is convenient for writing general claims about expressions, such as

```reach
forall(UInt, (x) => assert(x == x)); 
```


### `possible`

<Ref :name="(quote rsh):possible" />
```reach
possible( claim, [msg] ) 
```


 A possibility assertion which is only valid if it is possible for `claim` to evaluate to `true` with honest frontends and participants.
It accepts an optional bytes argument, which is included in any reported violation.

### `digest`

<Ref :name="(quote rsh):digest" />
```reach
digest( arg_0, ..., arg_n ) 
```


The digest primitive performs a [cryptographic hash](https://en.wikipedia.org/wiki/Cryptographic_hash_function) of the binary encoding of the given arguments.
This returns a `Digest` value.
The exact algorithm used depends on the connector.

### `balance`

<Ref :name="(quote rsh):balance" />
```reach
balance();
balance(gil); 
```


The <Defn :name="balance">balance</Defn> primitive returns the balance of the contract account for the DApp.
It takes an optional non-network token value, in which case it returns the balance of the given token.

### `lastConsensusTime` and `lastConsensusSecs`

<Ref :name="(quote rsh):lastConsensusTime" />
```reach
lastConsensusTime() 
```


The <Defn :name="lastConsensusTime">lastConsensusTime</Defn> primitive returns the network time of the last publication of the DApp.
This may not be available if there was no such previous publication, such as at the beginning of an application where `deployMode` is `'firstMsg'`.

::: note
Why is there no `thisConsensusTime`?
Some networks do not support observing the time of a consensus operation until after it has finalized.
This aides scalability, because it increases the number of times when an operation could be finalized.
:::

---

<Ref :name="(quote rsh):lastConsensusSecs" />
```reach
lastConsensusSecs() 
```


<Defn :name="lastConsensusSecs">lastConsensusSecs</Defn> is like lastConsensusTime, except it returns the network seconds.

### `baseWaitTime` and `baseWaitSecs`

<Ref :name="(quote rsh):baseWaitTime" /><Ref :name="(quote rsh):baseWaitSecs" />
```reach
baseWaitTime()
baseWaitSecs() 
```


These primitives return the network time (network seconds) that a relative time argument refers to.
This is either the same as `lastConsensusTime` (`lastConsensusSecs`) or the deadline of the previous `wait` or `.timeout`.

### Time arguments - `relativeTime`, `absoluteTime`, `relativeSecs`, `absoluteSecs`

<Ref :name="(quote rsh):relativeTime" /><Ref :name="(quote rsh):absoluteTime" /><Ref :name="(quote rsh):relativeSecs" /><Ref :name="(quote rsh):absoluteSecs" />
```reach
relativeTime(amt)
absoluteTime(time)
relativeSecs(amt)
absoluteSecs(secs)
```


These functions return <Defn :name="time arguments">time arguments</Defn>, which are instances of the type `Either(UInt, UInt)`, where `Left` variants refer to absolute network time and `Right` variants refer to absolute network seconds.

The `absoluteTime` and `absoluteSecs` are equivalent to `Left` and `Right` variant tags.

The `relativeTime` and `relativeSecs` functions add `baseWaitTime` and `baseWaitSecs` to their arguments before tagging with the appropriate variant.

If a time argument is required, an integer value is allowed and is interpreted as a `relativeTime`.

### `makeDeadline`

<Ref :name="(quote rsh):makeDeadline" />
```reach
const [ timeRemaining, keepGoing ] = makeDeadline(10); 
```


 `makeDeadline(deadline)` takes a `UInt` as an argument and returns a pair of functions
that can be used for dealing with absolute deadlines. It internally determines the end time based off of the deadline
and the last consensus time—at the time of calling `makeDeadline`. `timeRemaining` will calculate the difference
between the end time and the current last consensus time. `keepGoing` determines whether the current last consensus time
is less than the end time. It is typical to use the two fields for the `while` and `timeout` field of a `parallelReduce`
expression. For example:

```reach
const [ timeRemaining, keepGoing ] = makeDeadline(10);
const _ = parallelReduce(...)
  .invariant(...)
  .while( keepGoing() )
  .case(...)
  .timeout( timeRemaining(), () => { ... }) 
```


This pattern is so common that it can be abbreviated as `.timeRemaining`.


### `implies`

<Ref :name="(quote rsh):implies" />
```reach
implies( x, y ) 
```


 Returns `true` if `x` is `false` or `y` is `true`.

### `ensure`

<Ref :name="(quote rsh):ensure" />
```reach
ensure( pred, x ) 
```


 Makes a static assertion that `pred(x)` is `true` and returns `x`.

### `hasRandom`

<Ref :name="(quote rsh):hasRandom" />
```reach
hasRandom 
```


 A participant interact interface which specifies `random` as a function that takes no arguments and returns an unsigned integer of bit width bits. Reach provides a default frontend implementation via hasRandom (Frontend).

### `hasConsoleLogger`

<Ref :name="(quote rsh):hasConsoleLogger" />
```reach
hasConsoleLogger 
```


 A participant interact interface which specifies `log` with an unconstrained domain function type that returns `Null`. Reach provides a default frontend implementation via hasConsoleLogger (Frontend).

### `compose`

<Ref :name="(quote rsh):compose" />
```reach
compose(f, g) 
```


 Creates a new function that applies its argument to `g`, then pipes the result to the function `f`.
The argument type of `f` must be the return type of `g`.


### `sqrt`

<Ref :name="(quote rsh):sqrt" />
```reach
sqrt(81, 10) 
```


 Calculates an approximate square root of the first argument. This method utilizes
the [Babylonian Method](https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Babylonian_method) for computing
the square root. The second argument must be a `UInt` whose value is known at compile time, which represents the number
of iterations the algorithm should perform.

For reference, when performing `5` iterations, the algorithm can reliably calculate the square root
up to `32` squared, or `1,024`. When performing `10` iterations, the algorithm can reliably calculate the
square root up to `580` squared, or `336,400`.

### `pow`

<Ref :name="(quote rsh):pow" />
```reach
pow (2, 40, 10) // => 1,099,511,627,776 
```


 `pow(base, power, precision)` calculates the approximate value of raising base to power.
The third argument must be a `UInt` whose value is known at compile time, which represents the number
of iterations the algorithm should perform. For reference, `6` iterations provides enough accuracy to calculate
up to `2^64 - 1`, so the largest power it can compute is `63`.

### Signed Integers

The standard library provides abstractions for dealing with signed integers. The following definitions
are used to represent `Int`s:

::: note
`Int` is represented as an object, as opposed to a scalar value, because some platforms
that Reach targets do not provide native support for signed integers. 
:::

<Ref :name="(quote rsh):Int" /><Ref :name="(quote rsh):Pos" /><Ref :name="(quote rsh):Neg" />
```reach
const Int = { sign: Bool, i: UInt };
const Pos = true;
const Neg = false;  
```


 `int(Bool, UInt)` is shorthand for defining an `Int` record. You may also
use the `+` and `-` unary operators to declare integers instead of `UInt`s.

<Ref :name="(quote rsh):int" />
```reach
int(Pos, 4); // represents 4
int(Neg, 4); // represents -4
-4;          // represents -4
+4;          // represents 4 : Int
 4;          // represents 4 : UInt 
```


 `iadd(x, y)` adds the `Int` `x` and the `Int` `y`.

 `isub(x, y)` subtracts the `Int` `y` from the `Int` `x`.

 `imul(x, y)` multiplies the `Int` `x` and the `Int` `y`.

 `idiv(x, y)` divides the `Int` `x` by the `Int` `y`.

 `imod(x, y)` finds the remainder of dividing the `Int` `x` by the `Int` `y`.

 `ilt(x, y)` determines whether `x` is less than `y`.

 `ile(x, y)` determines whether `x` is less than or equal to `y`.

 `igt(x, y)` determines whether `x` is greather than `y`.

 `ige(x, y)` determines whether `x` is greater than or equal to `y`.

 `ieq(x, y)` determines whether `x` is equal to `y`.

 `ine(x, y)` determines whether `x` is not equal to `y`.

 `imax(x, y)` returns the larger of two `Int`s.

 `abs(i)` returns the absolute value of an `Int`. The return value is of type `UInt`.

### Fixed-Point Numbers

`FixedPoint` is defined by

<Ref :name="(quote rsh):FixedPoint" />
```reach
export const FixedPoint = Object({ sign: bool, i: Object({ scale: UInt, i: UInt }) }); 
```


`FixedPoint` can be used to represent numbers with a fixed number of digits after the decimal point.
They are handy for representing fractional values, especially in base 10. The value of a fixed point number is determined
by dividing the underlying integer value, `i`, by its scale factor, `scale`. For example, we could
represent the value `1.234` with `{ sign: Pos, i: { scale: 1000, i : 1234 } }` or `fx(1000)(Pos, 1234)`.
Alternatively, Reach provides syntactic sugar for defining `FixedPoint` numbers. One can simply write
`1.234`, which will assume the value is in base 10. A scale factor of `1000` correlates to 3 decimal
places of precision. Similarly, a scale factor of `100` would have 2 decimal places of precision.

<Ref :name="(quote rsh):fx" />
```reach
const scale = 10;
const i = 56;
fx(scale)(Neg, i); // represents - 5.6 
```


 `fx(scale)(i)` will return a function that can be used to
instantiate fixed point numbers with a particular scale factor.

<Ref :name="(quote rsh):fxint" />
```reach
const i = 4;
fxint(-i); // represents - 4.0 
```


 `fxint(Int)` will cast the `Int` arg as a `FixedPoint`
number with a `scale` of 1.

<Ref :name="(quote rsh):fxrescale" />
```reach
const x = fx(1000)(Pos, 1234); // x = 1.234
fxrescale(x, 100);    // => 1.23 
```


 `fxrescale(x, scale)` will convert a fixed point number from using
one scale to another. This operation can result in loss of precision, as demonstrated in the above example.

<Ref :name="(quote rsh):fxunify" />
```reach
const x = fx(1000)(Pos, 824345); // x = 824.345
const y = 45.67;
fxunify(x, y);    // => [ 1000, 824.345, 45.670 ] 
```


 `fxunify(x, y)` will convert the fixed point numbers
to use the same scale. The larger scale of the two arguments will be chosen. The function will return a `3-tuple` consisting
of the common scale and the newly scaled values.

 `fxadd(x, y)` adds two fixed point numbers.

 `fxsub(x, y)` subtracts two fixed point numbers.

 `fxmul(x, y)` multiplies two fixed point numbers.

<Ref :name="(quote rsh):fxdiv" />
```reach
fxdiv(34.56, 1.234, 10)     // => 28
fxdiv(34.56, 1.234, 100000) // => 28.0064 
```


 `fxdiv(x, y, scale_factor)` divides two fixed point numbers. The numerator, `x`,
will be multiplied by the scale factor to provide a more precise answer. For example,

 `fxmod(x, y)` finds the remainder of dividing `x` by `y`.

 `fxfloor(x)` returns the greatest integer not greater than `x`.

 `fxsqrt(x, k)` approximates the sqrt of the fixed number, `x`, using
`k` iterations of the `sqrt` algorithm.

<Ref :name="(quote rsh):fxpow" />
`const base  = 2.0;
const power = 0.33;
fxpow(base, power, 10, 1000);    // 1.260
fxpow(base, power, 10, 10000);   // 1.2599
fxpow(base, power, 10, 1000000); // 1.259921 `

 `fxpow(base, power, precision, scalePrecision)` approximates the power of the fixed number, `base`,
raised to the fixed point number, `power`. The third argument must be a `UInt` whose value is known
at compile time, which represents the number of iterations the algorithm should perform.
The `scalePrecision` argument must be a `UInt` and represents the scale of the return value. Choosing a larger
`scalePrecision` allows for more precision when approximating the power, as demonstrated in the example below:

 `fxpowi(base, power, precision)` approximates the power of the fixed number, `base`,
raised to the `Int`, `power`. The third argument must be a `UInt` whose value is known
at compile time, which represents the number of iterations the algorithm should perform. For reference, `6` iterations
provides enough accuracy to calculate up to `2^64 - 1`, so the largest power it can compute is `63`.

<Ref :name="(quote rsh):fxpowui" />
`fxpowui(5.8, 3, 10); // 195.112 `

 `fxpowui(base, power, precision)` approximates the power of
the fixed number, `base`, raised to the `UInt`, `power`. The third
argument must be a `UInt` whose value is known at compile time.

 `fxcmp(op, x, y)` applies the comparison
operator to the two fixed point numbers after unifying their scales.

There are convenience methods defined for comparing fixed point numbers:

 `fxlt(x, y)` tests whether `x` is less than `y`.

 `fxle(x, y)` tests whether `x` is less than or equal to `y`.

 `fxgt(x, y)` tests whether `x` is greater than `y`.

 `fxge(x, y)` tests whether `x` is greater than or equal to `y`.

 `fxeq(x, y)` tests whether `x` is equal to `y`.

 `fxne(x, y)` tests whether `x` is not equal to `y`.

### Anybody

<Ref :name="(quote rsh):Anybody" />
```reach
Anybody.publish(); // race(...Participants).publish()
```


 Reach provides a shorthand, `Anybody`, which serves as a
`race` between all participants.
This shorthand can be useful for situations where
it does not matter who `publish`es, such as in a `timeout`.

`Anybody` is strictly an abbreviation of a `race` involving all of the named participants of the application.
In an application with a participant class, this means any principal at all, because there is no restriction on which principals (i.e. addresses) may serve as a member of that class.
In an application without any participant classes, `Anybody` instead would mean only the actual previously-bound participants.

### Intervals

An `Interval` is defined by

<Ref :name="(quote rsh):Interval" />
```reach
export const Interval = Tuple(IntervalType, Int, Int, IntervalType); 
```


where `IntervalType` is defined by

<Ref :name="(quote rsh):IntervalType" />
```reach
export const [ isIntervalType, Closed, Open ] = mkEnum(2);
export const IntervalType = Refine(UInt, isIntervalType);  
```


#### Constructors

An interval may be constructed with its tuple notation or by function:

```reach
// Representing [-10, +10)
const i1 = [Closed, -10, +10, Open];
const i2 = interval(Closed, -10, +10, Open);
const i3 = intervalCO(-10, +10); 
```


For convenience, Reach provides a number of functions for constructing intervals:

 `interval(IntervalType, Int, Int, IntervalType)` constructs an interval where the first and second argument
represent the left endpoint and whether it's open or closed; the third and fourth argument represent the right endpoint and whether it's open or closed.

 `intervalCC(l, r)` constructs a closed interval from two endpoints of type `Int`.

 `intervalCO(l, r)` constructs a half-open interval from two endpoints of type `Int` where the left endpoint is closed and the right endpoint is open.

 `intervalOC(l, r)` constructs a half-open interval from two endpoints of type `Int` where the left endpoint is open and the right endpoint is closed.

 `intervalOO(l, r)` constructs an open interval from two endpoints of type `Int`.

#### Accessors

 `leftEndpoint(i)` will return the `Int` that represents the left endpoint of an interval.

 `rightEndpoint(i)` will return the `Int` that represents the right endpoint of an interval.

#### Relational Operations

Intervals may be compared with the following functions:

 `intervalEq(l, r)` tests whether the intervals are equal.

 `intervalNe(l, r)` tests whether the intervals are not equal.

 `intervalLt(l, r)` tests whether the left interval is less than the right interval.

 `intervalLte(l, r)` tests whether the left interval is less than or equal to the right interval.

 `intervalGt(l, r)` tests whether the left interval is greater than the right interval.

 `intervalGte(l, r)` tests whether the left interval is greater than or equal to the right interval.

#### Arithmetic Operations

 `intervalAdd(l, r)` adds the two intervals.

 `intervalSub(l, r)` subtracts the two intervals.

 `intervalMul(l, r)` multiplies the two intervals.

 `intervalDiv(l, r)` divides the two intervals.

#### Other Operations

<Ref :name="(quote rsh):intervalIntersection" />
```reach
const i1 = intervalOO(+3, +11); // (+3, +11)
const i2 = intervalCC(+7, +9);  // [+7, +9]
intervalIntersection(i1, i2);   // [+7, +11)  
```


 `intervalIntersection(x, y)` returns the intersection of two intervals.

<Ref :name="(quote rsh):intervalUnion" />
```reach
const i1 = intervalOO(+3, +9);  // (+3, +9)
const i2 = intervalCC(+7, +11); // [+7, +11]
intervalUnion(i1, i2);          // (+3, +11]  
```


 `intervalUnion(x, y)` returns the union of two intervals.

<Ref :name="(quote rsh):intervalWidth" />
```reach
intervalWidth(intervalCC(+4, +45)); // +41 
```


 `intervalWidth(i)` returns the width of an interval.

<Ref :name="(quote rsh):intervalAbs" />
```reach
intervalAbs(intervalCC(+1, +10)); // +10 
```


 `intervalAbs(i)` returns the absolute value of an interval.

