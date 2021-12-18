



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
Text starting with `//` up until the end of the line forms a @{defn("single-line comment")}.
Text enclosed with `/*` and `*/` forms a @{defn("multi-line comment")}.
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


A @{defn("block")} is a sequence of statements surrounded by braces, i.e. `{` and `}`.

## {#ref-programs-compute-stmts} Statements

This section describes the @{defn("statements")} which are allowed in any Reach context.

Each statement affects the meaning of the subsequent statements, which is called its @{defn("tail")}. For example, if `{!rsh} {X; Y; Z;}` is a block, then `{!rsh} X`'s tail is `{!rsh} {Y; Z;}` and `{!rsh} Y`'s tail is `{!rsh} {Z;}`.

Distinct from tails are @{defn("continuations")} which include everything after the statement. For example, in `{!rsh} { {X; Y;}; Z;}`, `{!rsh} X`'s tail is just `{!rsh} Y`, but its continuation is `{!rsh} {Y;}; Z;`.

Tails are statically apparent from the structure of the program source code, while continuations are influenced by function calls.

A sequence of statements that does not end in a @{defn("terminator statement")} (a statement with no tail), such as a return statement, continue statement, or exit statement is treated as if it ended with `{!rsh} return null;`.

The remainder of this section enumerates each kind of statement.

### `const` and `function`

An @{defn("identifier definition")} is either
a value definition
or a function definition.
Each of these introduces one or more @{defn("bound identifier")}s.

---

@{ref("rsh", "const")}
```reach
const DELAY = 10;
const [ Good, Bad ] = [ 42, 43 ];
const { x, y } = { x: 1, y: 2 };
const [ x, [ y ] ] = [ 1, [ 2 ] ];
const [ x, { y } ] = [ 1, { y: 2 } ];
const { x: [ a, b ] } = { x: [ 1, 2 ] };
```

:::note
Valid @{defn("identifiers")} follow the same rules as JavaScript identifiers:
they may consist of Unicode alphanumeric characters,
or `{!rsh} _` or `{!rsh} $`,
but may not begin with a digit.
:::


A @{defn("value definition")} is written `{!rsh} const LHS = RHS;`.

`{!rsh} LHS` must obey the grammar:

```
LHS =
  | id
  | "[" LHS-tuple-seq "]"
  | "{" LHS-obj-seq "}"
LHS-tuple-seq =
  |
  | "..." LHS
  | LHS
  | LHS "," LHS-tuple-seq
LHS-obj-seq =
  |
  | "..." LHS
  | LHS-obj-elem
  | LHS-obj-elem "," LHS-obj-seq
LHS-obj-elem =
  | id
  | propertyName ":" LHS
propertyName =
  | id
  | string
  | number
  | "[" expr "]"
```


`{!rsh} RHS` must be compatible with the given `{!rsh} LHS`.
That is, if a `{!rsh} LHS` is an `LHS-tuple-seq`, then the corresponding `{!rsh} RHS` must be a tuple with the correct number of elements.
If a `{!rsh} LHS` is an `LHS-obj-seq`, then the corresponding `{!rsh} RHS` must be an object with the correct fields.

Those values are available as their corresponding bound identifiers in the statement's tail.

---

@{ref("rsh", "function")}
```reach
function randomBool() {
  return (interact.random() % 2) == 0; }; 
```


A @{defn("function definition")}, written `{!rsh} function FUN(LHS_0, ..., LHS_n) BLOCK;`, defines `{!rsh} FUN` as a function which abstracts its @{defn("function body")}, the block `{!rsh} BLOCK`, over the left-hand sides `{!rsh} LHS_0` through `{!rsh} LHS_n`.

Function parameters may specify default arguments. The expressions used to instantiate these parameters
have access to any variables in the scope of which the function was defined. Additionally, these expressions
may reference previous arguments of the function definition.
Parameters with default arguments must come after all other parameters.

```reach
function f(a, b, c = a + 1, d = b + c) =>
  a + b + c + d;
```


The last parameter of a function may be a @{defn("rest parameter")}, which allows the function to be called
with an arbitrary number of arguments. A rest parameter is specified via `{!rsh} ...IDENT`, where
`{!rsh} IDENT` is bound to a `{!rsh} Tuple` containing all the remaining arguments.

---

All identifiers in Reach programs must be @{defn("unbound")}
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

The special identifier `{!rsh} _` is an exception to this rule.
The `{!rsh} _` binding is always considered to be unbound.
This means means that `{!rsh} _` is both
an identifier that can never be read,
as well as an identifier that may be bound many times.
This may be useful for ignoring unwanted values, for example:

```reach
const [_, x, _] = [1, 2, 3];
```


### `return`

@{ref("rsh", "return")}
```reach
return 17;
return 3 + 4;
return f(2, false);
return; 
```


A @{defn("return statement")}, written `{!rsh} return EXPR;`, where `{!rsh} EXPR` is an expression, evaluates to the same value as `{!rsh} EXPR`.
As a special case, `{!rsh} return;` is interpreted the same as `{!rsh} return null;`.

A return statement returns its value to the surrounding function application.

A return statement is a terminator statement, so it must have an empty tail.
For example,

```reach
{ return 1;
  return 2; } 
```


is invalid, because the first `{!rsh} return`'s tail is not empty.

Furthermore, a `{!rsh} return` must have an empty continuation (i.e. it must be in @{defn("tail position")}.)

### `if`

@{ref("rsh", "if")}@{ref("rsh", "else")}
```reach
if ( 1 + 2 < 3 ) {
  return "Yes!";
} else {
  return "No, waaah!"; } 
```


A @{defn("conditional statement")},
written `{!rsh} if (COND) NOT_FALSE else FALSE`,
where `{!rsh} COND` is an expression
and `{!rsh} NOT_FALSE` and `{!rsh} FALSE` as statements
(potentially block statements),
selects between the `{!rsh} NOT_FALSE` statement and `{!rsh} FALSE` statement based on whether `{!rsh} COND` evaluates to `{!rsh} false`.

Both `{!rsh} NOT_FALSE` and `{!rsh} FALSE` have empty tails, i.e. the tail of the conditional statement is not propagated. For example,

```reach
if ( x < y ) {
  const z = 3; }
else {
  const z = 4; }
return z; 
```


is erroneous, because the identifier `{!rsh} z` is not bound outside the conditional statement.

A conditional statement may only include a consensus transfer in `{!rsh} NOT_FALSE` or `{!rsh} FALSE` if it is within a consensus step, because its statements are in the same context as the conditional statement itself.

If one branch of a conditional contains a `{!rsh} return`, then both must.

### `switch`

@{ref("rsh", "switch")}@{ref("rsh", "case")}@{ref("rsh", "default")}
```reach
const mi = Maybe(UInt).Some(42);
switch ( mi ) {
 case None: return 8;
 case Some: return mi + 10; }
switch ( mi ) {
 case None: return 8;
 default: return 41; } 
```


A @{defn("switch statement")},
written `{!rsh} switch (VAR) { CASE ... }`,
where `{!rsh} VAR` is a variable bound to a data instance
and `{!rsh} CASE` is either `{!rsh} case VARIANT: STMT ...`, where `{!rsh} VARIANT` is a variant, or `{!rsh} default: STMT ...`, and `{!rsh} STMT` is a sequence of statements,
selects the appropriate sequence of statements based on which variant `{!rsh} VAR` holds.
Within the body of a `{!rsh} switch` case, `{!rsh} VAR` has the type of variant; i.e. in a `{!rsh} Some` case of a `{!rsh} Maybe(UInt)` `{!rsh} switch`, the variable is bound to an integer.

All cases have empty tails, i.e. the tail of the switch statement is not propagated.

A switch statement may only include a consensus transfer in its cases if it is within a consensus step, because its statements are in the same context as the conditional statement itself.

It is invalid for a case to appear multiple times, or be missing, or to be superfluous (i.e. for a variant that does not exist in the `{!rsh} Data` type of `{!rsh} VAR`).

If one case of a `{!rsh} switch` contains a `{!rsh} return`, then all must.

### Block statements

A @{defn("block statement")} is when a block occurs in a statement position, then it establishes a local, separate scope for the definitions of identifiers within that block. In other words, the block is evaluated for effect, but the tail of the statements within the block are isolated from the surrounding tail. For example,

```reach
const x = 4;
return x; 
```


evaluates to `{!rsh} 4`, but

```reach
{ const x = 4; }
return x; 
```


is erroneous, because the identifier `{!rsh} x` is not bound outside the block statement.

### Try/Catch & Throw Statements

```reach
try {
  throw 10;
} catch (v) {
  transfer(v).to(A); }
```


A @{defn("try statement")}, written `{!rsh} try BLOCK catch (VAR) BLOCK`, allows a block
of code to execute with a specified handler should an exception be thrown.

A @{defn("throw statement")},
written `{!rsh} throw EXPR`, will transfer control flow to the exception handler, binding `EXPR`
to `VAR`.
Any value that is able to exist at runtime may be thrown.
For example, `{!rsh} Int`s
and `{!rsh} Array`s are valid values to throw, but a function is not.
A `{!rsh} throw` must have an empty tail.

### Expression statements

```reach
4;
f(2, true); 
```


An expression, `{!rsh} E`, in a statement position is equivalent to the block statement `{!rsh} { return E; }`.

## {#ref-programs-compute-exprs} Expressions

This section describes the expressions which are allowed in any Reach context.
There are a large variety of different @{defn("expressions")} in Reach programs.

The remainder of this section enumerates each kind of expression.

### 'use strict'

@{ref("rsh", "'use strict'")}
```reach
'use strict'; 
```


 `{!rsh} 'use strict'` enables unused variables checks for all subsequent
declarations within the current scope. If a variable is declared, but never used, there will
be an error emitted at compile time.

@{defn("strict mode")} will reject some code that is normally valid and limit how dynamic Reach's type system is.
For example, normally Reach will permit expressions like the following to be evaluated:

```reach
const foo = (o) =>
  o ? o.b : false;

void foo({ b: true });
void foo(false); 
```


Reach allows `{!rsh} o` to be either an object with a `{!rsh} b` field or `{!rsh} false` because it
partially evaluates the program at compile time. So, without `{!rsh} 'use strict'`, Reach will not evaluate
`{!rsh} o.b` when `{!rsh} o = false` and this code will compile successfully.

But, in strict mode, Reach will ensure that this program treats `{!rsh} o` as
having a single type and detect an error in the program as follows:

```
reachc: error: Invalid field access. Expected object, got: Bool 
```


The correct way to write a program like this in strict mode is to use `{!rsh} Maybe`. Like this:

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


### `unstrict`

@{ref("rsh", "unstrict")}
```reach
assert(unstrict(() => {
  'use strict';
  // the following fails in strict mode due to a type mismatch
  return 1 != true;
}));
```


 `{!rsh} unstrict` applies a thunk, ignoring any usage of strict mode. This
can be useful when dealing with libraries that are written in strict mode.


### Identifier reference

```reach
X
Y
Z 
```


An identifier, written `{!rsh} ID`, is an expression that evaluates to the value of the bound identifier.

@{ref("rsh", "this")}
The identifier `{!rsh} this` has a special meaning inside of a local step (i.e. the body of an `{!rsh} only` or `{!rsh} each` expression), as well as in a consensus step (i.e. the tail of `{!rsh} publish` or `{!rsh} pay` statement and before a `{!rsh} commit` statement). For details, see @{seclink("ref-programs-local-this")} and @{seclink("ref-programs-consensus-this")}.

### Function application

```reach
assert( amount <= heap1 )
step( moveA )
digest( coinFlip )
interact.random()
declassify( _coinFlip ) 
```


A @{defn("function application")}, written `{!rsh} EXPR_rator(EXPR_rand_0, ..., EXPR_rand_n)`, is an expression where `{!rsh} EXPR_rator` and `{!rsh} EXPR_rand_0` through `{!rsh} EXPR_rand_n` are expressions that evaluate to one value.
`{!rsh} EXPR_rator` must evaluate to an abstraction over `{!rsh} n` values or a primitive of arity `{!rsh} n`.
A spread expression (`{!rsh} ...expr`) may appear in the list of operands to a function application, in which case the elements of the expr are spliced in place.

@{ref("rsh", "new")}
`{!rsh} new f(a)` is equivalent to `{!rsh} f.new(a)` and is a convenient short-hand for writing class-oriented programs.

### {#ref-programs-types} Types

Reach's @{defn("type")}s are represented in programs by the following identifiers and constructors:

+ @{ref("rsh", "Null")} `{!rsh} Null`.
+ @{ref("rsh", "Bool")} `{!rsh} Bool`, which denotes a boolean.
+ @{ref("rsh", "UInt")} `{!rsh} UInt`, which denotes an unsigned integer.
`{!rsh} UInt.max` is the largest value that may be assigned to a `{!rsh} UInt`.
+ @{ref("rsh", "Bytes")} `{!rsh} Bytes(length)`, which denotes a string of bytes of length at most `{!rsh} length`.
Bytes of different lengths are not compatible; however the shorter bytes may be padded.
+ @{ref("rsh", "Digest")} `{!rsh} Digest`, which denotes a digest.
+ @{ref("rsh", "Address")} `{!rsh} Address`, which denotes an account address.
+ @{ref("rsh", "Contract")} `{!rsh} Contract`, which denotes the identifying information of a contract.

:::note
Reach has different representations of contracts across connectors.
For example, on Algorand a `{!rsh} Contract` is an Application ID, but on Ethereum it is an Address.
:::

+ @{ref("rsh", "Token")} `{!rsh} Token`, which denotes a non-network token. @{seclink("ref-networks")} discusses how `{!rsh} Token`s are represented on specific networks.

:::note
Reach assumes that every `{!rsh} Token` in your program refers to a different non-network token.
It will automatically insert `{!rsh} require` statements that enforce this constraint.
:::

+ @{ref("rsh", "Fun")} `{!rsh} Fun([Domain_0, ..., Domain_N], Range)`, which denotes a @{defn("function type")}, when `{!rsh} Domain_i` and `{!rsh} Range` are types.
The domain of a function is negative position.
The range of a function is positive position.
+ `{!rsh} Fun(true, Range)`, which denotes an @{defn("unconstrained domain function type")}, when `{!rsh} Range` is a type.
These functions may only appear in participant interact interfaces.
+ @{ref("rsh", "Tuple")} `{!rsh} Tuple(Field_0, ..., FieldN)`, which denotes a tuple.
(Refer to @{seclink("ref-programs-tuples")} for constructing tuples.)
+ @{ref("rsh", "Object")} `{!rsh} Object({key_0: Type_0, ..., key_N: Type_N})`, which denotes an object.
(Refer to @{seclink("ref-programs-objects")} for constructing objects.)
+ @{ref("rsh", "Struct")} `{!rsh} Struct([[key_0, Type_0], ..., [key_N, Type_N]])`, which denotes a struct.
(Refer to @{seclink("ref-programs-structs")} for constructing structs.)
+ @{ref("rsh", "Array")} `{!rsh} Array(Type_0, size)`, which denotes a statically-sized array.
`{!rsh} Type_0` must be a type that can exist at runtime (i.e., not a function type.)
(Refer to @{seclink("ref-programs-arrays")} for constructing arrays.)
+ @{ref("rsh", "Data")} `{!rsh} Data({variant_0: Type_0, ..., variant_N: Type_N})`, which denotes a [tagged union](https://en.wikipedia.org/wiki/Tagged_union) (or _sum type_).
(Refer to @{seclink("ref-programs-data")} for constructing data instances.)
+ @{ref("rsh", "Refine")} `{!rsh} Refine(Type_0, Predicate, ?Message)`, where `{!rsh} Predicate` is a unary function returning a boolean, which denotes a [refinement type](https://en.wikipedia.org/wiki/Refinement_type), that is instances of `{!rsh} Type_0` that satisfy `{!rsh} Predicate`.
When a refinement type appears in a @{defn("negative position")} (such as in an `{!rsh} is` or in the domain of a `{!rsh} Fun` of a participant interact interface), it introduces an `{!rsh} assert`;
while when it is in a @{defn("positive position")}, it introduces an `{!rsh} assume`.
`{!rsh} Message` is an optional string to display if the predicate fails verification.

For example, if `{!rsh} f` had type
```reach
Fun([Refine(UInt, (x => x < 5))], Refine(UInt, (x => x > 10)))
```

then `{!rsh} const z = f(y)` is equivalent to

```reach
assert(y < 5);
const z = f(y);
assume(z > 10);
```

+ `{!rsh} Refine(Type_0, PreCondition, PostCondition, ?Messages)`, where `{!rsh} Type_0` is a function type, `{!rsh} PreCondition` is a unary function that accepts a tuple of the domain and returns a boolean, and `{!rsh} PostCondition` is a binary function that accepts a tuple of the domain and the range and returns a boolean, denotes a function type with a [precondition](https://en.wikipedia.org/wiki/Precondition) and [postcondition](https://en.wikipedia.org/wiki/Postcondition).
Preconditions are enforced with `{!rsh} assert` and postconditions are enforced with `{!rsh} assume`.
`{!rsh} Messages` is an optional two-tuple of `{!rsh} Bytes`.
The first message will be displayed when the precondition fails verification and the second when the postcondition fails verification.

For example, `{!rsh} Refine(Fun([UInt, UInt], UInt), ([x, y] => x < y), (([x, y], z) => x + y < z))` is a function that requires its second argument to be larger than its first and its result to be larger than its input.


`{!rsh} Object` and `{!rsh} Data` are commonly used to implemented [algebraic data types](https://en.wikipedia.org/wiki/Algebraic_data_type) in Reach.

@{ref("rsh", "typeOf")}@{ref("rsh", "isType")}@{ref("rsh", "is")}
```reach
typeOf(x) // type
isType(t) // Bool
is(x, t) // t
```


The `{!rsh} typeOf` primitive function is the same as `{!rsh} typeof`:
it returns the type of its argument.

The `{!rsh} isType` function returns `{!rsh} true` if its argument is a type.
Any expression satisfying `{!rsh} isType` is compiled away and does not exist at runtime.

The `{!rsh} is` function returns its first argument if it satisfies the type specified by the second argument.
If it is not, then the program is invalid.
For example, `{!rsh} is(5, UInt)` returns `{!rsh} 5`, while `{!rsh} is(5, Bool)` is an invalid program.
The value returned by `{!rsh} is` may not be identical to the input, because in some cases, such as for functions, it will record the applied to type and enforce it on future invocations.
These applications are considered negative positions for `{!rsh} Refine`.

### Literal values

@{ref("rsh", "true")}@{ref("rsh", "false")}@{ref("rsh", "null")}
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


A @{defn("literal value")},
written `{!rsh} VALUE`,
is an expression that evaluates to the given value.

The @{defn("null literal")} may be written as `{!rsh} null`.

@{defn("Numeric literal")}s may be written in decimal, hexadecimal, or octal.
Numeric literals must obey the @{defn("bit width")} of `{!rsh} UInt` if they are used as `{!rsh} UInt` values at runtime, but if they only appear at compile-time, then they may be any positive number.
Reach provides abstractions for working with `{!rsh} Int`s and signed `{!rsh} FixedPoint` numbers.
`{!rsh} Int`s may be defined by applying the unary `{!rsh} +` and `{!rsh} -` operators to values of type `{!rsh} UInt`.
Reach provides syntactic sugar for defining signed `{!rsh} FixedPoint` numbers, in base 10, with decimal syntax.

@{defn("Boolean literal")}s may be written as `{!rsh} true` or `{!rsh} false`.

@{defn("String literal")}s (aka byte strings)
may be written between double or single quotes
(with no distinction between the different styles)
and use the same escaping rules as JavaScript.
Since `{!rsh} Bytes` types are specialized in their length, literals typically need to be padded to be useful.

### Operator expression

An @{defn("operator")} is a special identifier,
which is either a unary operator, or a binary operator.

---

@{ref("rsh", "!")}@{ref("rsh", "-")}@{ref("rsh", "+")}@{ref("rsh", "typeof")}@{ref("rsh", "not")}@{ref("rsh", "minus")}@{ref("rsh", "plus")}@{ref("rsh", "void")}
```reach
! a  // not
- a  // minus
+ a  // plus
typeof a
void a
```


A @{defn("unary expression")}, written `{!rsh} UNAOP EXPR_rhs`, where `{!rsh} EXPR_rhs` is an expression and `{!rsh} UNAOP` is one of the @{defn("unary operator")}s: `! - + typeof void`. All the unary operators, besides `{!rsh} typeof`, have a
corresponding named version in the standard library.

It is invalid to use unary operations on the wrong types of values.

When applied to values of type `{!rsh} UInt`, unary `{!rsh} -` and `{!rsh} +` operators will cast
their arguments to type `{!rsh} Int`. The unary `{!rsh} -` and `{!rsh} +` operations are defined for
values of type: `{!rsh} Int`, and `{!rsh} FixedPoint`.

`{!rsh} void a` evaluates to `{!rsh} null` for all arguments.

---

@{ref("rsh", "&&")}@{ref("rsh", "||")}@{ref("rsh", "+")}@{ref("rsh", "-")}@{ref("rsh", "*")}@{ref("rsh", "/")}@{ref("rsh", "%")}@{ref("rsh", "|")}@{ref("rsh", "&")}@{ref("rsh", "^")}@{ref("rsh", "<<")}@{ref("rsh", ">>")}@{ref("rsh", "==")}@{ref("rsh", "!=")}@{ref("rsh", "===")}@{ref("rsh", "!==")}@{ref("rsh", ">")}@{ref("rsh", ">=")}@{ref("rsh", "<=")}@{ref("rsh", "<")}
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


:::note
Bitwise operations are not supported by all consensus networks and greatly decrease the efficiency of verification.
:::


A @{defn("binary expression")} is written `{!rsh} EXPR_lhs BINOP EXPR_rhs`, where `{!rsh} EXPR_lhs` and `{!rsh} EXPR_rhs` are expressions and `{!rsh} BINOP` is one of the @{defn("binary operator")}s: `&& || + - * / % | & ^ << >> == != === !== > >= <= <`.
Numeric operations, like `{!rsh} +` and `{!rsh} >`, only operate on numbers.
Since all numbers in Reach are integers, operations like `{!rsh} /` truncate their result.
Boolean operations, like `{!rsh} &&`, only operate on booleans.
It is invalid to use binary operations on the wrong types of values.

@{ref("rsh", "and")}@{ref("rsh", "or")}@{ref("rsh", "add")}@{ref("rsh", "sub")}@{ref("rsh", "mul")}@{ref("rsh", "div")}@{ref("rsh", "mod")}@{ref("rsh", "lt")}@{ref("rsh", "le")}@{ref("rsh", "ge")}@{ref("rsh", "gt")}@{ref("rsh", "lsh")}@{ref("rsh", "rsh")}@{ref("rsh", "band")}@{ref("rsh", "bior")}@{ref("rsh", "band")}@{ref("rsh", "bxor")}@{ref("rsh", "polyEq")}@{ref("rsh", "polyNeq")}
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
While `{!rsh} &&` and `{!rsh} ||` may not evaluate their second argument,
their corresponding named functions `{!rsh} and` and `{!rsh} or`, always do.

@{ref("rsh", "boolEq")}@{ref("rsh", "typeEq")}@{ref("rsh", "intEq")}@{ref("rsh", "digestEq")}@{ref("rsh", "addressEq")}@{ref("rsh", "fxeq")}@{ref("rsh", "ieq")}
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


Equality functions, like `{!rsh} ==`, `{!rsh} ===`, `{!rsh} !=`, and `{!rsh} !==`, operate on all types.
However, values with different types are always not equal.
Both arguments must be of the same type.
Specialized functions exist for equality checking on each supported type.

---

If `{!rsh} verifyArithmetic` is `{!rsh} true`, then arithmetic operations automatically make a static assertion that their arguments would not overflow the bit width of the enabled consensus networks.
If it is `{!rsh} false`, then the connector will ensure this dynamically.

### xor

```reach
xor(false, false); // false
xor(false, true);  // true
xor(true, false);  // true
xor(true, true);   // false 
```


 `{!rsh} xor(Bool, Bool)` returns `{!rsh} true` only when the inputs differ in value.

### Padding

@{ref("rsh", "pad")}
```reach
Bytes(16).pad('abc');
```


`{!rsh} Bytes` are like `{!rsh} Array`s in that they are fixed and exactly sized.
This means that two `{!rsh} Bytes` of different lengths are not interchangeable.

For example, `{!rsh} 'You win!'` and `{!rsh} 'You lose!'` cannot both be provided to an `{!rsh} interact` function, because the second is one character longer.
Most of the time this is good, because it is a signal that you should use a `{!rsh} Data` type instead, so that the formatting and display logic is entirely controlled by the frontend.

But, sometimes it is necessary and useful to extend one byte string into a larger size.
Each `{!rsh} Bytes` type has a `pad` field that is bound to a function that extends its argument to the needed size.
A byte string extended in this way is called @{defn("padded")}, because it is extended with additional `NUL` bytes.

### Parenthesized expression

```reach
(a + b) - c 
```


An expression may be parenthesized, as in `{!rsh} (EXPR)`.

### {#ref-programs-tuples} Tuples

```reach
[ ]
[ 1, 2 + 3, 4 * 5 ] 
```


A @{defn("tuple")} literal, written `{!rsh} [ EXPR_0, ..., EXPR_n ]`, is an expression which evaluates to a tuple of `{!rsh} n` values, where `{!rsh} EXPR_0` through `{!rsh} EXPR_n` are expressions.

`{!rsh} ...expr` may appear inside tuple expressions, in which case the spreaded expression must evaluate to a tuple or array, which is spliced in place.

### {#ref-programs-arrays} `array`

@{ref("rsh", "array")}
```reach
const x = array(UInt, [1, 2, 3]); 
```


Converts a tuple of homogeneous values of the specific type into an @{defn("array")}.

### Element reference

```reach
arr[3] 
```


A @{defn("reference")}, written `{!rsh} REF_EXPR[IDX_EXPR]`,
where `{!rsh} REF_EXPR` is an expression that evaluates to an array, a tuple, or a struct
and `{!rsh} IDX_EXPR` is an expression that evaluates to a natural number which is less than the size of the array,
selects the element at the given index of the array.
Indices start at zero.

If `{!rsh} REF_EXPR` is a tuple, then `{!rsh} IDX_EXPR` must be a compile-time constant, because tuples do not support dynamic access, because each element may be a different type.

If `{!rsh} REF_EXPR` is a mapping and `{!rsh} IDX_EXPR` evaluates to an address, then this reference evaluates to a value of type `{!rsh} Maybe(TYPE)`, where `{!rsh} TYPE` is the type of the mapping.

### Array & tuple length: `Tuple.length`, `Array.length`, and `.length`

@{ref("rsh", "length")}
```reach
Tuple.length(tup);
tup.length;
Array.length(arr);
arr.length; 
```


 `{!rsh} Tuple.length` Returns the length of the given tuple.

 `{!rsh} Array.length` Returns the length of the given array.

Both may be abbreviated as `{!rsh} expr.length` where `{!rsh} expr` evaluates to a tuple or an array.

### Array & tuple update: `Tuple.set`, `Array.set`, and `.set`

@{ref("rsh", "set")}
```reach
Tuple.set(tup, idx, val);
tup.set(idx, val);
Array.set(arr, idx, val);
arr.set(idx, val); 
```


 `{!rsh} Tuple.set` Returns a new tuple identical to `{!rsh} tup`,
except that index `{!rsh} idx` is replaced with `{!rsh} val`.
The `{!rsh} idx` must be a compile-time constant, because tuples do not support dynamic access, because each element may be a different type.

 `{!rsh} Array.set` Returns a new array identical to `{!rsh} arr`, except that index `{!rsh} idx` is replaced with `{!rsh} val`.

Both may be abbreviated as `{!rsh} expr.set(idx, val)` where `{!rsh} expr` evaluates to a tuple or an array.

### Array element type: `Array.elemType` and `.elemType`

@{ref("rsh", "elemType")}
```reach
Array.elemType(arr)
arr.elemType 
```


 `{!rsh} Array.elemType` Returns the `{!rsh} Type` of elements that the array contains.

### Foldable operations

The following methods are available on any @{ref("rsh", "Foldable")}`{!rsh} Foldable` containers, such as: `{!rsh} Array`s and `{!rsh} Map`s.

####  `Foldable.forEach` && `.forEach`

@{ref("rsh", "forEach")}
```reach
c.forEach(f)
Foldable.forEach(c, f)
Array.forEach(c, f)
Map.forEach(c, f) 
```


 `{!rsh} Foldable.forEach(c, f)` iterates the function `{!rsh} f` over the elements of a container `{!rsh} c`, discarding the result.
This may be abbreviated as `{!rsh} c.forEach(f)`.

#### `Foldable.all` && `.all`

@{ref("rsh", "all")}
```reach
Foldable.all(c, f)
Array.all(c, f)
Map.all(c, f)
c.all(f) 
```


 `{!rsh} Foldable.all(c, f)` determines whether the predicate, `f`, is satisfied
by every element of the container, `c`.

#### `Foldable.any` && `.any`

@{ref("rsh", "any")}
```reach
Foldable.any(c, f)
Array.any(c, f)
Map.any(c, f)
c.any(f) 
```


 `{!rsh} Foldable.any(c, f)` determines whether the predicate, `f`, is satisfied
by at least one element of the container, `c`.

#### `Foldable.or` && `.or`

@{ref("rsh", "or")}
```reach
Foldable.or(c)
Array.or(c)
Map.or(c)
c.or() 
```


 `{!rsh} Foldable.or(c)` returns the disjunction of a container of `{!rsh} Bool`s.

#### `Foldable.and` && `.and`

@{ref("rsh", "and")}
```reach
Foldable.and(c)
Array.and(c)
Map.and(c)
c.and() 
```


 `{!rsh} Foldable.and(c)` returns the conjunction of a container of `{!rsh} Bool`s.

#### `Foldable.includes` && `.includes`

@{ref("rsh", "includes")}
```reach
Foldable.includes(c, x)
Array.includes(c, x)
Map.includes(c, x)
c.includes(x) 
```


 `{!rsh} Foldable.includes(c, x)` determines whether the container includes
the element, `x`.

#### `Foldable.count` && `.count`

@{ref("rsh", "count")}
```reach
Foldable.count(c, f)
Array.count(c, f)
Map.count(c, f)
c.count(f) 
```


 `{!rsh} Foldable.count(c, f)` returns the number of elements in `c` that
satisfy the predicate, `f`.

#### `Foldable.size` && `.size`

@{ref("rsh", "size")}
```reach
Foldable.size(c)
Array.size(c)
Map.size(c)
c.size() 
```


 `{!rsh} Foldable.size(c)` returns the number of elements in `c`.

#### `Foldable.min` && `.min`

@{ref("rsh", "min")}
```reach
Foldable.min(c)
Array.min(c)
Map.min(c)
c.min() 
```


 `{!rsh} Foldable.min(c)` returns the lowest number in a container of `UInt`s.

#### `Foldable.max` && `.max`

@{ref("rsh", "max")}
```reach
Foldable.max(c)
Array.max(c)
Map.max(c)
c.max() 
```


 `{!rsh} Foldable.max(c)` returns the largest number in a container of `UInt`s.

#### `Foldable.sum` && `.sum`

@{ref("rsh", "sum")}
```reach
Foldable.sum(c)
Array.sum(c)
Map.sum(c)
c.sum() 
```


 `{!rsh} Foldable.sum(c)` returns the sum of a container of `UInt`s.

#### `Foldable.product` && `.product`

@{ref("rsh", "product")}
```reach
Foldable.product(c)
Array.product(c)
Map.product(c)
c.product() 
```


 `{!rsh} Foldable.product(c)` returns the product of a container of `UInt`s.

#### `Foldable.average` && `.average`

@{ref("rsh", "average")}
```reach
Foldable.average(c)
Array.average(c)
Map.average(c)
c.average() 
```


 `{!rsh} Foldable.average(c)` returns the mean of a container of `UInt`s.

### Array group operations

`{!rsh} Array` is a `{!rsh} Foldable` container. Along with the methods of `{!rsh} Foldable`, the
following methods may be used with `{!rsh} Array`s.

#### `Array.iota`

@{ref("rsh", "iota")}
```reach
Array.iota(5) 
```


 `{!rsh} Array.iota(len)` returns an array of length `{!rsh} len`, where each element is the same as its index.
For example, `{!rsh} Array.iota(4)` returns `{!rsh} [0, 1, 2, 3]`.
The given `{!rsh} len` must evaluate to an integer at compile-time.

#### `Array.replicate` && `.replicate`

@{ref("rsh", "Array_replicate")}@{ref("rsh", "replicate")}
```reach
Array.replicate(5, "five")
Array_replicate(5, "five") 
```


 `{!rsh} Array.replicate(len, val)` returns an array of length `{!rsh} len`, where each element is `{!rsh} val`.
For example, `{!rsh} Array.replicate(4, "four")` returns `{!rsh} ["four", "four", "four", "four"]`.
The given `{!rsh} len` must evaluate to an integer at compile-time.

#### `Array.concat` && `.concat`

@{ref("rsh", "concat")}
```reach
Array.concat(x, y)
x.concat(y) 
```


 `{!rsh} Array.concat(x, y)` concatenates the two arrays `{!rsh} x` and `{!rsh} y`.
This may be abbreviated as `{!rsh} x.concat(y)`.

#### `Array.empty`

@{ref("rsh", "Array_empty")}@{ref("rsh", "empty")}
```reach
Array_empty
Array.empty 
```


 `{!rsh} Array.empty` is an array with no elements.
It is the identity element of `{!rsh} Array.concat`.
It may also be written `{!rsh} Array_empty`.

#### `Array.zip` && `.zip`

@{ref("rsh", "zip")}
```reach
Array.zip(x, y)
x.zip(y) 
```


 `{!rsh} Array.zip(x, y)` returns a new array the same size as `{!rsh} x` and `{!rsh} y` (which must be the same size) whose elements are tuples of the elements of `{!rsh} x` and `{!rsh} y`.
This may be abbreviated as `{!rsh} x.zip(y)`.

#### `Array.map` && `.map`

@{ref("rsh", "map")}
```reach
Array.map(arr, f)
arr.map(f) 
```


 `{!rsh} Array.map(arr, f)` returns a new array, `{!rsh} arr_mapped`, the same size as `{!rsh} arr`, where `{!rsh} arr_mapped[i] = f(arr[i])` for all `{!rsh} i`.
For example, `{!rsh} Array.iota(4).map(x => x+1)` returns `{!rsh} [1, 2, 3, 4]`.
This may be abbreviated as `{!rsh} arr.map(f)`.

This function is generalized to an arbitrary number of arrays of the same size, which are provided before the `{!rsh} f` argument.
For example, `{!rsh} Array.iota(4).map(Array.iota(4), add)` returns `{!rsh} [0, 2, 4, 6]`.

#### `Array.mapWithIndex` && `.mapWithIndex`

@{ref("rsh", "mapWithIndex")}
```reach
Array.mapWithIndex(arr, f)
arr.mapWithIndex(f) 
```


 `{!rsh} Array.mapWithIndex(arr, f)` is similar to `{!rsh} Array.map`, except it
provides `{!rsh} f` with an additional argument, which is the index of the current element in `{!rsh} arr`.
Unlike `{!rsh} Array.map`, this function is not generalized to an arbitrary number of arrays; it only accepts one array.

#### `Array.reduce` && `.reduce`

@{ref("rsh", "reduce")}
```reach
Array.reduce(arr, z, f)
arr.reduce(z, f) 
```


 `{!rsh} Array.reduce(arr, z, f)` returns the [left fold](https://en.wikipedia.org/wiki/Fold_(higher-order_function)) of the function `{!rsh} f` over the given array with the initial value `{!rsh} z`.
For example, `{!rsh} Array.iota(4).reduce(0, add)` returns `{!rsh} ((0 + 1) + 2) + 3 = 6`.
This may be abbreviated as `{!rsh} arr.reduce(z, f)`.

This function is generalized to an arbitrary number of arrays of the same size, which are provided before the `{!rsh} z` argument.
For example, `{!rsh} Array.iota(4).reduce(Array.iota(4), 0, (x, y, z) => (z + x + y))` returns `{!rsh} ((((0 + 0 + 0) + 1 + 1) + 2 + 2) + 3 + 3)`.

#### `Array.reduceWithIndex` && `.reduceWithIndex`

@{ref("rsh", "reduceWithIndex")}
```reach
Array.reduceWithIndex(arr, z, f)
arr.reduceWithIndex(z, f) 
```


 `{!rsh} Array.reduceWithIndex(arr, z, f)` is similar to `{!rsh} Array.reduce`, except it
provides `{!rsh} f` with an additional argument, which is the index of the current element in `{!rsh} arr`.
Unlike `{!rsh} Array.reduce`, this function is not generalized to an arbitrary number of arrays; it only accepts one array.

#### `Array.indexOf` && `.indexOf`

@{ref("rsh", "indexOf")}
```reach
Array.indexOf(arr, x)
arr.indexOf(x) 
```


 `{!rsh} Array.indexOf(arr, x)` returns the index of the first element
in the given array that is equal to `x`. The return value is of type `{!rsh} Maybe(UInt)`. If
the value is not present in the array, `{!rsh} None` is returned.

#### `Array.findIndex` && `.findIndex`

@{ref("rsh", "findIndex")}
```reach
Array.findIndex(arr, f)
arr.findIndex(f) 
```


 `{!rsh} Array.findIndex(arr, f)` returns the index of the first element
in the given array that satisfies the predicate `f`. The return value is of type `{!rsh} Maybe(UInt)`. If
no value in the array satisfies the predicate, `{!rsh} None` is returned.

#### `Array.find` && `.find`

@{ref("rsh", "find")}
```reach
Array.find(arr, f)
arr.find(f) 
```


 `{!rsh} Array.find(arr, f)` returns the first element in the array, `{!rsh} arr`,
that satisfies the predicate `{!rsh} f`. The return value is of type `{!rsh} Maybe`. If no value in the
array satisfies the predicate, `{!rsh} None` is returned.

#### `Array.withIndex` && `.withIndex`

@{ref("rsh", "withIndex")}
```reach
Array.withIndex(arr)
arr.withIndex() 
```


 `{!rsh} Array.withIndex(arr)` returns an array where every element of `{!rsh} arr`
is paired with its index. For example, `{!rsh} array(Bool, [false, true]).withIndex()` returns
`{!rsh} array(Tuple(Bool, UInt), [[false, 0], [true, 1]])`.

#### `Array.slice` && `.slice`

@{ref("rsh", "slice")}
```reach
Array.slice(arr, start, length)
arr.slice(start, length)
```


 `{!rsh} Array.slice(arr, start, length)` returns a portion of `{!rsh} arr`, starting from
the `{!rsh} start` index, up to the `{!rsh} start + length` index.

### Mapping group operations

`{!rsh} Map` is a `{!rsh} Foldable` container. Mappings may be aggregated with the following
operations and those of `{!rsh} Foldable` within the `{!rsh} invariant` of a `{!rsh} while` loop.

#### `Map.reduce` && `.reduce`

```reach
Map.reduce(map, z, f)
map.reduce(z, f) 
```


 `{!rsh} Map.reduce(map, z, f)` returns the [left fold](https://en.wikipedia.org/wiki/Fold_(higher-order_function)) of the function `{!rsh} f` over the given mapping with the initial value `{!rsh} z`.
For example, `{!rsh} m.reduce(0, add)` sums the elements of the mapping.
This may be abbreviated as `{!rsh} map.reduce(z, f)`.

The function `{!rsh} f` must satisfy the property, for all `{!rsh} z`, `{!rsh} a`, `{!rsh} b`, `{!rsh} f(f(z, b), a) == f(f(z, a), b)`, because the order of evaluation is unpredictable.

### {#ref-programs-objects} Objects

```reach
{ }
{ x: 3, "yo-yo": 4 }
{ [1 < 2 ? "one" : "two"]: 5 }
```


An @{defn("object")},
typically written `{!rsh} { KEY_0: EXPR_0, ..., KEY_n: EXPR_n }`,
where `{!rsh} KEY_0` through `{!rsh} KEY_n` are identifiers or string literals
and `{!rsh} EXPR_0` through `{!rsh} EXPR_n` are expressions,
is an expression which evaluates to an object
with fields `{!rsh} KEY_0` through `{!rsh} KEY_n`.

Additional object literal syntax exists for convenience, such as:

```reach
{ ...obj, z: 5 }
```


An @{defn("object splice")},
where all fields from `{!rsh} obj` are copied into the object;
these fields may be accompanied by additional fields specified afterwards.

```reach
{ x, z: 5 }
```


Shorthand for `{!rsh} { x: x, z: 5}`, where `{!rsh} x` is any bound identifier.

### {#ref-programs-structs} Structs

```reach
const Posn = Struct([["x", UInt], ["y", UInt]]);
const p1 = Posn.fromObject({x: 1, y: 2});
const p2 = Posn.fromTuple([1, 2]);
```


A @{defn("struct")} is a combination of a tuple and an object.
It has named elements, like an object, but is ordered like a tuple, so its elements may be accessed by either name or position.
Structs exist for interfacing with non-Reach remote objects, where both parties must agree to the runtime representation of the values.


A struct instance may be constructed by calling the `{!rsh} fromTuple` method of a struct type instance (like `{!rsh} Posn`) with a tuple of the appropriate length.


A struct instance may be constructed by calling the `{!rsh} fromObject` method of a struct type instance (like `{!rsh} Posn`) with an object with the appropriate fields.



Structs may be converted into a corresponding tuple or object via the `{!rsh} toTuple` and `{!rsh} toObject` methods on the `{!rsh} Struct` value (as well as struct type instances, like `{!rsh} Posn` in the example above):

```reach
assert(Posn.toTuple(p1)[0] == 1);
assert(Struct.toObject(p2).y == 2);
```


The names of elements may be restricted to avoid conflicting with reserved words of the specified connectors.

### Field reference

```reach
obj.x
```


An @{defn("object reference")},
written `{!rsh} OBJ.FIELD`,
where `{!rsh} OBJ` is an expression that evaluates to an object or a struct,
and `{!rsh} FIELD` is a valid identifier,
accesses the `FIELD` @{defn("field")} of object OBJ.

### `Object.set`

@{ref("rsh", "Object_set")}
```reach
Object.set(obj, fld, val);
Object_set(obj, fld, val);
{ ...obj, [fld]: val };
```


 Returns a new object identical to `{!rsh} obj`,
except that field `{!rsh} fld` is replaced with `{!rsh} val`.

### `Object.setIfUnset`

@{ref("rsh", "Object_setIfUnset")}
```reach
Object.setIfUnset(obj, fld, val);
Object_setIfUnset(obj, fld, val);
```


 Returns a new object identical to `{!rsh} obj`,
except that field `{!rsh} fld` is `{!rsh} val` if `{!rsh} fld` is not already present in `{!rsh} obj`.

### `Object.has`

```reach
Object.has(obj, fld);
```


 Returns a boolean indicating whether the object has the field `{!rsh} fld`.
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


A @{defn("data instance")} is written `{!rsh} DATA.VARIANT(VALUE)`, where `{!rsh} DATA` is `{!rsh} Data` type, `{!rsh} VARIANT` is the name of one of `{!rsh} DATA`'s variants, and `{!rsh} VALUE` is a value matching the type of the variant.
As a special case, when the type of a variant is `{!rsh} Null`, the `{!rsh} VALUE` may be omitted, as shown in the definition of `{!rsh} burger` in the same above.

Data instances are consumed by `{!rsh} switch` statements and `{!rsh} match` expressions.

### `Maybe`

@{ref("rsh", "Maybe")}@{ref("rsh", "Some")}@{ref("rsh", "None")}@{ref("rsh", "fromMaybe")}
```reach
const MayInt = Maybe(UInt);
const bidA = MayInt.Some(42);
const bidB = MayInt.None(null);

const getBid = (m) => fromMaybe(m, (() => 0), ((x) => x));
const bidSum = getBid(bidA) + getBid(bidB);
assert(bidSum == 42); 
```


[Option types](https://en.wikipedia.org/wiki/Option_type) are represented in Reach through the built-in `{!rsh} Data` type, `{!rsh} Maybe`, which has two variants: `{!rsh} Some` and `{!rsh} None`.

`{!rsh} Maybe` is defined by
```reach
export const Maybe = (A) => Data({None: Null, Some: A}); 
```


This means it is a function that returns a `{!rsh} Data` type specialized to a particular type in the `{!rsh} Some` variant.

`{!rsh} Maybe` instances can be conveniently consumed by `{!rsh} fromMaybe(mValue, onNone, onSome)`, where `{!rsh} onNone` is a function of no arguments which is called when `{!rsh} mValue` is `{!rsh} None`, `{!rsh} onSome` is a function of one argument which is called with the value when `{!rsh} mValue` is `{!rsh} Some`, and `{!rsh} mValue` is a data instance of `{!rsh} Maybe`.

@{ref("rsh", "isNone")}@{ref("rsh", "isSome")}
```reach
const m = Maybe(UInt).Some(5);
isNone(m); // false
isSome(m); // true
```


 `{!rsh} isNone` is a convenience method that determines whether the variant is `{!rsh} None`.

 `{!rsh} isSome` is a convenience method that determines whether the variant is `{!rsh} Some`.


@{ref("rsh", "fromSome")}
```reach
fromSome(Maybe(UInt).Some(1), 0); // 1
fromSome(Maybe(UInt).None(), 0);  // 0
```


 `{!rsh} fromSome` receives a `{!rsh} Maybe` value and a default value as arguments and will return the value inside
of the `{!rsh} Some` variant or the default value otherwise.

@{ref("rsh", "maybe")}
```reach
const add1 = (x) => x + 1;
maybe(Maybe(UInt).Some(1), 0, add1); // 2
maybe(Maybe(UInt).None(), 0, add1);  // 0
```


 `{!rsh} maybe(m, defaultVal, f)` receives a `{!rsh} Maybe` value, a default value, and a unary function as arguments. The function will
either return the application of the function, `{!rsh} f`, to the `{!rsh} Some` value or return the default value provided.

### `Either`

`{!rsh} Either` is defined by
```reach
export const Either = (A, B) => Data({Left: A, Right: B}); 
```


`{!rsh} Either` can be used to represent values with two possible types.

Similar to `Maybe`, `Either` may be used to represent values that are correct or erroneous.
A successful result is stored, by convention, in `Right`. Unlike `None`, `Left` may
carry additional information about the error.

@{ref("rsh", "either")}
```reach
either(e, onLeft, onRight) 
```


 `{!rsh} either(e, onLeft, onRight)` will either apply the function `onLeft` or `onRight` depending on `e`.

@{ref("rsh", "isLeft")}@{ref("rsh", "isRight")}@{ref("rsh", "fromLeft")}@{ref("rsh", "fromRight")}
```reach
const e = Either(UInt, Bool);
const l = e.Left(1);
const r = e.Right(true);
isLeft(l);  // true
isRight(l); // false
const x = fromLeft(l, 0);      // x = 1
const y = fromRight(l, false); // y = false 
```


 `{!rsh} isLeft` is a convenience method that determines whether the variant is `Left`.

 `{!rsh} isRight` is a convenience method that determines whether the variant is `Right`.

 `{!rsh} fromLeft(e, default)` is a convenience method that returns the value in `Left`,
or `default` if the variant is `Right`.

 `{!rsh} fromRight(e, default)` is a convenience method that returns the value in `Right`,
or `default` if the variant is `Left`.

### `match`

@{ref("rsh", "match")}
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


A @{defn("match expression")}, written `{!rsh} VAR.match({ CASE ... })`, where `VAR` is a variable
bound to a data instance and `CASE` is `VARIANT: FUNCTION`, where `VARIANT` is a
variant or `{!rsh} default`, and `FUNCTION` is a function that takes the same arguments as the
variant constructor.
If the variant has a type of `{!rsh} Null`, then the function is allowed to take no arguments.
`{!rsh} default` functions must always take an argument, even if all defaulted variants have type `{!rsh} Null`.

`{!rsh} match` is similar to a switch statement, but since it is an expression, it
can be conveniently used in places like the right hand side of an assignment statement.

Similar to a switch statement, the cases are expected to be exhaustive and nonredundant,
all cases have empty tails, and it may only include a consensus transfer in
its cases if it is within a consensus step.

### Conditional expression

@{ref("rsh", "?")}
```reach
choosesFirst ? [ heap1 - amount, heap2 ] : [ heap1, heap2 - amount ] 
```


A @{defn("conditional expression")}, written `{!rsh} COND_E ? NOT_FALSE_E : FALSE_E`, where `{!rsh} COND_E`, `{!rsh} NOT_FALSE_E`, and `{!rsh} FALSE_E` are expressions, selects between the values which `{!rsh} NOT_FALSE_E` and `{!rsh} FALSE_E` evaluate to based on whether `{!rsh} COND_E` evaluates to `{!rsh} false`.

@{ref("rsh", "ite")}
```reach
ite(choosesFirst, [heap1 - amount, heap2], [heap1, heap2 - amount])
```


Conditional expressions may also be written with the `{!rsh} ite` function,
however, note that this function always evaluates both of its branches,
while the regular conditional expression only evaluates one branch.

### Arrow expression

@{ref("rsh", "=>")}
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


An @{defn("arrow expression")}, written `{!rsh} (LHS_0, ..., LHS_n) => EXPR`, where `{!rsh} LHS_0` through `{!rsh} LHS_n` are left-hand sides and `{!rsh} EXPR` is an expression, evaluates to a function which is an abstraction of `{!rsh} EXPR` over `{!rsh} n` values compatible with the respective left-hand side.
Like function definitions, arrow expressions may use default argument notation and rest parameters.

### `makeEnum`

@{ref("rsh", "makeEnum")}
```reach
const [ isHand, ROCK, PAPER, SCISSORS ] = makeEnum(3); 
```


An @{defn("enumeration")} (or @{defn("enum")}, for short),
can be created by calling the `{!rsh} makeEnum` function, as in `{!rsh} makeEnum(N)`,
where `{!rsh} N` is the number of distinct values in the enum.
This produces a tuple of `{!rsh} N+1` values,
where the first value is a `{!rsh} Fun([UInt], Bool)`
which tells you if its argument is one of the enum's values,
and the next N values are distinct `{!rsh} UInt`s.

### `assert`

@{ref("rsh", "assert")}
```reach
assert( claim, [msg] ) 
```


 A static assertion which is only valid if `{!rsh} claim` always evaluates to `{!rsh} true`.
:::note
The Reach compiler will produce a counter-example (i.e. an assignment of the identifiers in the program to falsify the `{!rsh} claim`) when an invalid `{!rsh} claim` is provided.
It is possible to write a `{!rsh} claim` that actually always evaluates to `{!rsh} true`, but for which our current approach cannot prove always evaluates to `{!rsh} true`; if this is the case, Reach will fail to compile the program, reporting that its analysis is incomplete.
Reach will never produce an erroneous counter-example.
:::

It accepts an optional bytes argument, which is included in any reported violation.

:::note
See [the guide section on verification](##guide-assert) to better understand how and what to verify in your program.
:::


### `forall`

@{ref("rsh", "forall")}
```reach
forall( Type )
forall( Type, (var) => BLOCK ) 
```


 The single argument version returns an abstract value of the given type.
It may only be referenced inside of assertions; any other reference is invalid.

The two argument version is an abbreviation of calling the second argument with the result of `{!rsh} forall(Type)`.
This is convenient for writing general claims about expressions, such as

```reach
forall(UInt, (x) => assert(x == x)); 
```


### `possible`

@{ref("rsh", "possible")}
```reach
possible( claim, [msg] ) 
```


 A possibility assertion which is only valid if it is possible for `{!rsh} claim` to evaluate to `{!rsh} true` with honest frontends and participants.
It accepts an optional bytes argument, which is included in any reported violation.

### `digest`

@{ref("rsh", "digest")}
```reach
digest( arg_0, ..., arg_n ) 
```


The digest primitive performs a [cryptographic hash](https://en.wikipedia.org/wiki/Cryptographic_hash_function) of the binary encoding of the given arguments.
This returns a `{!rsh} Digest` value.
The exact algorithm used depends on the connector.

### `balance`

@{ref("rsh", "balance")}
```reach
balance();
balance(gil); 
```


The @{defn("balance")} primitive returns the balance of the contract account for the DApp.
It takes an optional non-network token value, in which case it returns the balance of the given token.

### `getContract`

@{ref("rsh", "getContract")}
```reach
getContract() 
```


The @{defn("getContract")} primitive returns the `{!rsh} Contract` value for the deployed contract.
This function may not be called until after the first publication (which creates the contract).

### `getAddress`

@{ref("rsh", "getAddress")}
```reach
getAddress() 
```


The @{defn("getAddress")} primitive returns the `{!rsh} Address` value of the deployed contract's account.
This function may not be called until after the first publication (which creates the contract).

### `lastConsensusTime` and `lastConsensusSecs`

@{ref("rsh", "lastConsensusTime")}
```reach
lastConsensusTime() 
```


The @{defn("lastConsensusTime")} primitive returns the network time of the last publication of the DApp.
This may not be available if there was no such previous publication, such as at the beginning of an application before the first publication.

:::note
Why is there no `thisConsensusTime`?
Some networks do not support observing the time of a consensus operation until after it has finalized.
This aides scalability, because it increases the number of times when an operation could be finalized.
:::


---

@{ref("rsh", "lastConsensusSecs")}
```reach
lastConsensusSecs() 
```


@{defn("lastConsensusSecs")} is like lastConsensusTime, except it returns the network seconds.

### `baseWaitTime` and `baseWaitSecs`

@{ref("rsh", "baseWaitTime")}@{ref("rsh", "baseWaitSecs")}
```reach
baseWaitTime()
baseWaitSecs() 
```


These primitives return the network time (network seconds) that a relative time argument refers to.
This is either the same as `{!rsh} lastConsensusTime` (`{!rsh} lastConsensusSecs`) or the deadline of the previous `{!rsh} wait` or `{!rsh} .timeout`.

### Time arguments - `relativeTime`, `absoluteTime`, `relativeSecs`, `absoluteSecs`

@{ref("rsh", "relativeTime")}@{ref("rsh", "absoluteTime")}@{ref("rsh", "relativeSecs")}@{ref("rsh", "absoluteSecs")}
```reach
relativeTime(amt)
absoluteTime(time)
relativeSecs(amt)
absoluteSecs(secs)
```


These functions return @{defn("time arguments")}, which are instances of the type `{!rsh} Either(UInt, UInt)`, where `{!rsh} Left` variants refer to absolute network time and `{!rsh} Right` variants refer to absolute network seconds.

The `{!rsh} absoluteTime` and `{!rsh} absoluteSecs` are equivalent to `{!rsh} Left` and `{!rsh} Right` variant tags.

The `{!rsh} relativeTime` and `{!rsh} relativeSecs` functions add `{!rsh} baseWaitTime` and `{!rsh} baseWaitSecs` to their arguments before tagging with the appropriate variant.

If a time argument is required, an integer value is allowed and is interpreted as a `{!rsh} relativeTime`, but this behavior is deprecated and you will see a warning.

### `makeDeadline`

@{ref("rsh", "makeDeadline")}
```reach
const [ timeRemaining, keepGoing ] = makeDeadline(10); 
```


 `{!rsh} makeDeadline(deadline)` takes a `{!rsh} UInt` as an argument and returns a pair of functions
that can be used for dealing with absolute deadlines. It internally determines the end time based off of the deadline
and the last consensus time—at the time of calling `{!rsh} makeDeadline`. `timeRemaining` will calculate the difference
between the end time and the current last consensus time. `keepGoing` determines whether the current last consensus time
is less than the end time. It is typical to use the two fields for the `while` and `timeout` field of a `{!rsh} parallelReduce`
expression. For example:

```reach
const [ timeRemaining, keepGoing ] = makeDeadline(10);
const _ = parallelReduce(...)
  .invariant(...)
  .while( keepGoing() )
  .case(...)
  .timeout( timeRemaining(), () => { ... }) 
```


This pattern is so common that it can be abbreviated as `{!rsh} .timeRemaining`.


### `implies`

@{ref("rsh", "implies")}
```reach
implies( x, y ) 
```


 Returns `{!rsh} true` if `{!rsh} x` is `{!rsh} false` or `{!rsh} y` is `{!rsh} true`.

### `ensure`

@{ref("rsh", "ensure")}
```reach
ensure( pred, x ) 
```


 Makes a static assertion that `{!rsh} pred(x)` is `{!rsh} true` and returns `{!rsh} x`.

### `hasRandom`

@{ref("rsh", "hasRandom")}
```reach
hasRandom 
```


 A participant interact interface which specifies `random` as a function that takes no arguments and returns an unsigned integer of bit width bits. Reach provides a default frontend implementation via hasRandom (Frontend).

### `hasConsoleLogger`

@{ref("rsh", "hasConsoleLogger")}
```reach
hasConsoleLogger 
```


 A participant interact interface which specifies `log` with an unconstrained domain function type that returns `{!rsh} Null`. Reach provides a default frontend implementation via hasConsoleLogger (Frontend).

### `compose`

@{ref("rsh", "compose")}
```reach
compose(f, g) 
```


 Creates a new function that applies its argument to `g`, then pipes the result to the function `f`.
The argument type of `f` must be the return type of `g`.

### `muldiv`

:::note
Currently, wide arithmetic operations are only suported on Algorand.
:::


@{ref("rsh", "muldiv")}
```reach
muldiv(a, b, c) 
```


 Multiplies `{!rsh} a` by `{!rsh} b`, then immediately divides the product by `{!rsh} c`.
The intermediate value may be larger than `{!rsh} UInt.max` if the connector supports wide arithmetic operations.
The resulting quotient must be less than `{!rsh} UInt.max`.

### `sqrt`

@{ref("rsh", "sqrt")}
```reach
sqrt(81, 10) 
```


 Calculates an approximate square root of the first argument. This method utilizes
the [Babylonian Method](https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Babylonian_method) for computing
the square root. The second argument must be a `{!rsh} UInt` whose value is known at compile time, which represents the number
of iterations the algorithm should perform.

For reference, when performing `{!rsh} 5` iterations, the algorithm can reliably calculate the square root
up to `32` squared, or `1,024`. When performing `{!rsh} 10` iterations, the algorithm can reliably calculate the
square root up to `580` squared, or `336,400`.

### `pow`

@{ref("rsh", "pow")}
```reach
pow (2, 40, 10) // => 1,099,511,627,776 
```


 `{!rsh} pow(base, power, precision)` calculates the approximate value of raising base to power.
The third argument must be a `{!rsh} UInt` whose value is known at compile time, which represents the number
of iterations the algorithm should perform. For reference, `6` iterations provides enough accuracy to calculate
up to `2^64 - 1`, so the largest power it can compute is `63`.

### Signed Integers

The standard library provides abstractions for dealing with signed integers. The following definitions
are used to represent `{!rsh} Int`s:

:::note
`Int` is represented as an object, as opposed to a scalar value, because some platforms
that Reach targets do not provide native support for signed integers. 
:::


@{ref("rsh", "Int")}@{ref("rsh", "Pos")}@{ref("rsh", "Neg")}
```reach
const Int = { sign: Bool, i: UInt };
const Pos = true;
const Neg = false;  
```


 `{!rsh} int(Bool, UInt)` is shorthand for defining an `{!rsh} Int` record. You may also
use the `{!rsh} +` and `{!rsh} -` unary operators to declare integers instead of `{!rsh} UInt`s.

@{ref("rsh", "int")}
```reach
int(Pos, 4); // represents 4
int(Neg, 4); // represents -4
-4;          // represents -4
+4;          // represents 4 : Int
 4;          // represents 4 : UInt 
```


 `{!rsh} iadd(x, y)` adds the `{!rsh} Int` `x` and the `{!rsh} Int` `y`.

 `{!rsh} isub(x, y)` subtracts the `{!rsh} Int` `y` from the `{!rsh} Int` `x`.

 `{!rsh} imul(x, y)` multiplies the `{!rsh} Int` `x` and the `{!rsh} Int` `y`.

 `{!rsh} idiv(x, y)` divides the `{!rsh} Int` `x` by the `{!rsh} Int` `y`.

 `{!rsh} imod(x, y)` finds the remainder of dividing the `{!rsh} Int` `x` by the `{!rsh} Int` `y`.

 `{!rsh} ilt(x, y)` determines whether `x` is less than `y`.

 `{!rsh} ile(x, y)` determines whether `x` is less than or equal to `y`.

 `{!rsh} igt(x, y)` determines whether `x` is greather than `y`.

 `{!rsh} ige(x, y)` determines whether `x` is greater than or equal to `y`.

 `{!rsh} ieq(x, y)` determines whether `x` is equal to `y`.

 `{!rsh} ine(x, y)` determines whether `x` is not equal to `y`.

 `{!rsh} imax(x, y)` returns the larger of two `{!rsh} Int`s.

 `{!rsh} abs(i)` returns the absolute value of an `{!rsh} Int`. The return value is of type `{!rsh} UInt`.

### Fixed-Point Numbers

`{!rsh} FixedPoint` is defined by

@{ref("rsh", "FixedPoint")}
```reach
export const FixedPoint = Object({ sign: bool, i: Object({ scale: UInt, i: UInt }) }); 
```


`{!rsh} FixedPoint` can be used to represent numbers with a fixed number of digits after the decimal point.
They are handy for representing fractional values, especially in base 10. The value of a fixed point number is determined
by dividing the underlying integer value, `i`, by its scale factor, `scale`. For example, we could
represent the value `{!rsh} 1.234` with `{!rsh} { sign: Pos, i: { scale: 1000, i : 1234 } }` or `{!rsh} fx(1000)(Pos, 1234)`.
Alternatively, Reach provides syntactic sugar for defining `{!rsh} FixedPoint` numbers. One can simply write
`{!rsh} 1.234`, which will assume the value is in base 10. A scale factor of `1000` correlates to 3 decimal
places of precision. Similarly, a scale factor of `100` would have 2 decimal places of precision.

@{ref("rsh", "fx")}
```reach
const scale = 10;
const i = 56;
fx(scale)(Neg, i); // represents - 5.6 
```


 `{!rsh} fx(scale)(i)` will return a function that can be used to
instantiate fixed point numbers with a particular scale factor.

@{ref("rsh", "fxint")}
```reach
const i = 4;
fxint(-i); // represents - 4.0 
```


 `{!rsh} fxint(Int)` will cast the `{!rsh} Int` arg as a `{!rsh} FixedPoint`
number with a `scale` of 1.

@{ref("rsh", "fxrescale")}
```reach
const x = fx(1000)(Pos, 1234); // x = 1.234
fxrescale(x, 100);    // => 1.23 
```


 `{!rsh} fxrescale(x, scale)` will convert a fixed point number from using
one scale to another. This operation can result in loss of precision, as demonstrated in the above example.

@{ref("rsh", "fxunify")}
```reach
const x = fx(1000)(Pos, 824345); // x = 824.345
const y = 45.67;
fxunify(x, y);    // => [ 1000, 824.345, 45.670 ] 
```


 `{!rsh} fxunify(x, y)` will convert the fixed point numbers
to use the same scale. The larger scale of the two arguments will be chosen. The function will return a `3-tuple` consisting
of the common scale and the newly scaled values.

 `{!rsh} fxadd(x, y)` adds two fixed point numbers.

 `{!rsh} fxsub(x, y)` subtracts two fixed point numbers.

 `{!rsh} fxmul(x, y)` multiplies two fixed point numbers.

@{ref("rsh", "fxdiv")}
```reach
fxdiv(34.56, 1.234, 10)     // => 28
fxdiv(34.56, 1.234, 100000) // => 28.0064 
```


 `{!rsh} fxdiv(x, y, scale_factor)` divides two fixed point numbers. The numerator, `x`,
will be multiplied by the scale factor to provide a more precise answer. For example,

 `{!rsh} fxmod(x, y)` finds the remainder of dividing `x` by `y`.

 `{!rsh} fxfloor(x)` returns the greatest integer not greater than `x`.

 `{!rsh} fxsqrt(x, k)` approximates the sqrt of the fixed number, `x`, using
`k` iterations of the `{!rsh} sqrt` algorithm.

@{ref("rsh", "fxpow")}
`{!rsh} const base  = 2.0;
const power = 0.33;
fxpow(base, power, 10, 1000);    // 1.260
fxpow(base, power, 10, 10000);   // 1.2599
fxpow(base, power, 10, 1000000); // 1.259921 `

 `{!rsh} fxpow(base, power, precision, scalePrecision)` approximates the power of the fixed number, `base`,
raised to the fixed point number, `power`. The third argument must be a `{!rsh} UInt` whose value is known
at compile time, which represents the number of iterations the algorithm should perform.
The `scalePrecision` argument must be a `UInt` and represents the scale of the return value. Choosing a larger
`scalePrecision` allows for more precision when approximating the power, as demonstrated in the example below:

 `{!rsh} fxpowi(base, power, precision)` approximates the power of the fixed number, `base`,
raised to the `{!rsh} Int`, `power`. The third argument must be a `{!rsh} UInt` whose value is known
at compile time, which represents the number of iterations the algorithm should perform. For reference, `6` iterations
provides enough accuracy to calculate up to `2^64 - 1`, so the largest power it can compute is `63`.

@{ref("rsh", "fxpowui")}
`{!rsh} fxpowui(5.8, 3, 10); // 195.112 `

 `{!rsh} fxpowui(base, power, precision)` approximates the power of
the fixed number, `base`, raised to the `{!rsh} UInt`, `power`. The third
argument must be a `{!rsh} UInt` whose value is known at compile time.

 `{!rsh} fxcmp(op, x, y)` applies the comparison
operator to the two fixed point numbers after unifying their scales.

There are convenience methods defined for comparing fixed point numbers:

 `{!rsh} fxlt(x, y)` tests whether `x` is less than `y`.

 `{!rsh} fxle(x, y)` tests whether `x` is less than or equal to `y`.

 `{!rsh} fxgt(x, y)` tests whether `x` is greater than `y`.

 `{!rsh} fxge(x, y)` tests whether `x` is greater than or equal to `y`.

 `{!rsh} fxeq(x, y)` tests whether `x` is equal to `y`.

 `{!rsh} fxne(x, y)` tests whether `x` is not equal to `y`.

### Anybody

@{ref("rsh", "Anybody")}
```reach
Anybody.publish(); // race(...Participants).publish()
```


 Reach provides a shorthand, `{!rsh} Anybody`, which serves as a
`{!rsh} race` between all participants.
This shorthand can be useful for situations where
it does not matter who `{!rsh} publish`es, such as in a `{!rsh} timeout`.

`{!rsh} Anybody` is strictly an abbreviation of a `{!rsh} race` involving all of the named participants of the application.
In an application with a participant class, this means any principal at all, because there is no restriction on which principals (i.e. addresses) may serve as a member of that class.
In an application without any participant classes, `{!rsh} Anybody` instead would mean only the actual previously-bound participants.

### Intervals

An `{!rsh} Interval` is defined by

@{ref("rsh", "Interval")}
```reach
export const Interval = Tuple(IntervalType, Int, Int, IntervalType); 
```


where `{!rsh} IntervalType` is defined by

@{ref("rsh", "IntervalType")}
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

 `{!rsh} interval(IntervalType, Int, Int, IntervalType)` constructs an interval where the first and second argument
represent the left endpoint and whether it's open or closed; the third and fourth argument represent the right endpoint and whether it's open or closed.

 `{!rsh} intervalCC(l, r)` constructs a closed interval from two endpoints of type `{!rsh} Int`.

 `{!rsh} intervalCO(l, r)` constructs a half-open interval from two endpoints of type `{!rsh} Int` where the left endpoint is closed and the right endpoint is open.

 `{!rsh} intervalOC(l, r)` constructs a half-open interval from two endpoints of type `{!rsh} Int` where the left endpoint is open and the right endpoint is closed.

 `{!rsh} intervalOO(l, r)` constructs an open interval from two endpoints of type `{!rsh} Int`.

#### Accessors

 `{!rsh} leftEndpoint(i)` will return the `{!rsh} Int` that represents the left endpoint of an interval.

 `{!rsh} rightEndpoint(i)` will return the `{!rsh} Int` that represents the right endpoint of an interval.

#### Relational Operations

Intervals may be compared with the following functions:

 `{!rsh} intervalEq(l, r)` tests whether the intervals are equal.

 `{!rsh} intervalNe(l, r)` tests whether the intervals are not equal.

 `{!rsh} intervalLt(l, r)` tests whether the left interval is less than the right interval.

 `{!rsh} intervalLte(l, r)` tests whether the left interval is less than or equal to the right interval.

 `{!rsh} intervalGt(l, r)` tests whether the left interval is greater than the right interval.

 `{!rsh} intervalGte(l, r)` tests whether the left interval is greater than or equal to the right interval.

#### Arithmetic Operations

 `{!rsh} intervalAdd(l, r)` adds the two intervals.

 `{!rsh} intervalSub(l, r)` subtracts the two intervals.

 `{!rsh} intervalMul(l, r)` multiplies the two intervals.

 `{!rsh} intervalDiv(l, r)` divides the two intervals.

#### Other Operations

@{ref("rsh", "intervalIntersection")}
```reach
const i1 = intervalOO(+3, +11); // (+3, +11)
const i2 = intervalCC(+7, +9);  // [+7, +9]
intervalIntersection(i1, i2);   // [+7, +11)  
```


 `{!rsh} intervalIntersection(x, y)` returns the intersection of two intervals.

@{ref("rsh", "intervalUnion")}
```reach
const i1 = intervalOO(+3, +9);  // (+3, +9)
const i2 = intervalCC(+7, +11); // [+7, +11]
intervalUnion(i1, i2);          // (+3, +11]  
```


 `{!rsh} intervalUnion(x, y)` returns the union of two intervals.

@{ref("rsh", "intervalWidth")}
```reach
intervalWidth(intervalCC(+4, +45)); // +41 
```


 `{!rsh} intervalWidth(i)` returns the width of an interval.

@{ref("rsh", "intervalAbs")}
```reach
intervalAbs(intervalCC(+1, +10)); // +10 
```


 `{!rsh} intervalAbs(i)` returns the absolute value of an interval.

