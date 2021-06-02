#lang scribble/manual
@(require scribble/bnf
          "lib.rkt")

@title[#:version reach-vers #:tag "ref-programs-compute"]{Computations}

This section describes the common features available in all Reach contexts.

@section{Comments}

@reach{
 // single-line comment
 /* multi-line
  * comment
  */ }

Comments are text that is ignored by the compiler.
Text starting with @litchar{//} up until the end of the line forms a @deftech{single-line comment}.
Text enclosed with @litchar{/*} and @litchar{*/} forms a @deftech{multi-line comment}.
It is @tech{invalid} to nest a @tech{multi-line comment} within a @tech{multi-line comment}.

@section{Blocks}

@reach{
  { return 42; }
  { const x = 31;
    return x + 11; }
  { if ( x < y ) {
      return "Why";
    } else {
      return "Ecks"; } } }

A @deftech{block} is a sequence of @tech{statements} surrounded by braces, i.e. @litchar["{"] and @litchar["}"].

@section[#:tag "ref-programs-compute-stmts"]{Statements}

This section describes the @deftech{statements} which are allowed in any Reach context.

Each @tech{statement} affects the meaning of the subsequent @tech{statements}, which is called its @deftech{tail}. For example, if @reachin{{X; Y; Z;}} is a @tech{block}, then @reachin{X}'s @tech{tail} is @reachin{{Y; Z;}} and @reachin{Y}'s @tech{tail} is @reachin{{Z;}}.

Distinct from @tech{tails} are @deftech{continuations} which include everything after the @tech{statement}. For example, in @reachin{{{X; Y;}; Z;}}, @reachin{X}'s @tech{tail} is just @reachin{Y}, but its @tech{continuation} is @reachin{{Y;}; Z;}.

@tech{Tails} are statically apparent from the structure of the program source code, while @tech{continuations} are influenced by function calls.

A sequence of @tech{statements} that does not end in a @deftech{terminator statement} (a @tech{statement} with no @tech{tail}), such as a @tech{return statement}, @tech{continue statement}, or @tech{exit statement} is treated as if it ended with @reachin{return null;}.

The remainder of this section enumerates each kind of @tech{statement}.

@subsection{@tt{const} and @tt{function}}

An @deftech{identifier definition} is either
a @tech{value definition}
or a @tech{function definition}.
Each of these introduces one or more @deftech{bound identifier}s.

@(hrule)

@(mint-define! '("const"))
@reach{
  const DELAY = 10;
  const [ Good, Bad ] = [ 42, 43 ];
  const { x, y } = { x: 1, y: 2 };
  const [ x, [ y ] ] = [ 1, [ 2 ] ];
  const [ x, { y } ] = [ 1, { y: 2 } ];
  const { x: [ a, b ] } = { x: [ 1, 2 ] };
  }
@margin-note{@tech{Valid} @deftech{identifiers} follow the same rules as JavaScript identifiers:
they may consist of Unicode alphanumeric characters,
or @reachin{_} or @reachin{$},
but may not begin with a digit.}

A @deftech{value definition} is written @reachin{const LHS = RHS;}.

@reachin{LHS} must obey the grammar:

@BNF[
(list
 @nonterm{LHS}
 @nonterm{id}
 @BNF-seq[@litchar["["] @nonterm{LHS-tuple-seq} @litchar["]"]]
 @BNF-seq[@litchar["{"] @nonterm{LHS-obj-seq} @litchar["}"]])
(list
 @nonterm{LHS-tuple-seq}
 @BNF-seq[]
 @BNF-seq[@litchar["..."] @nonterm{LHS}]
 @BNF-seq[@nonterm{LHS}]
 @BNF-seq[@nonterm{LHS} @litchar[","] @nonterm{LHS-tuple-seq}])
(list
 @nonterm{LHS-obj-seq}
 @BNF-seq[]
 @BNF-seq[@litchar["..."] @nonterm{LHS}]
 @BNF-seq[@nonterm{LHS-obj-elem}]
 @BNF-seq[@nonterm{LHS-obj-elem} @litchar[","] @nonterm{LHS-obj-seq}])
(list
 @nonterm{LHS-obj-elem}
 @BNF-seq[@nonterm{id}]
 @BNF-seq[@nonterm{propertyName} @litchar[":"] @nonterm{LHS}])
(list
 @nonterm{propertyName}
 @nonterm{id}
 @nonterm{string}
 @nonterm{number}
 @BNF-seq[@litchar["["] @nonterm{expr} @litchar["]"]])
]

@reachin{RHS} must be compatible with the given @reachin{LHS}.
That is, if a @reachin{LHS} is an @nonterm{LHS-tuple-seq}, then the corresponding @reachin{RHS} must be a tuple with the correct number of elements.
If a @reachin{LHS} is an @nonterm{LHS-obj-seq}, then the corresponding @reachin{RHS} must be an object with the correct fields.

Those @tech{values} are available as their corresponding @tech{bound identifier}s in the statement's @tech{tail}.

@(hrule)

@(mint-define! '("function"))
@reach{
  function randomBool() {
    return (interact.random() % 2) == 0; }; }

A @deftech{function definition}, written @reachin{function FUN(LHS_0, ..., LHS_n) BLOCK;}, defines @reachin{FUN} as a function which abstracts its @deftech{function body}, the @tech{block} @reachin{BLOCK}, over the left-hand sides @reachin{LHS_0} through @reachin{LHS_n}.

Function parameters may specify default arguments. The expressions used to instantiate these parameters
have access to any variables in the scope of which the function was defined. Additionally, these expressions
may reference previous arguments of the @tech{function definition}.
Parameters with default arguments must come after all other parameters.

@reach{
  function f(a, b, c = a + 1, d = b + c) =>
    a + b + c + d;
}

The last parameter of a function may be a @deftech{rest parameter}, which allows the function to be called
with an arbitrary number of arguments. A @tech{rest parameter} is specified via @reachin{...IDENT}, where
@reachin{IDENT} is bound to a @reachin{Tuple} containing all the remaining arguments.

@(hrule)

All identifiers in Reach programs must be @deftech{unbound}
at the position of the program where they are bound,
i.e., it is @tech{invalid} to shadow identifiers with new definitions.
For example,

@reach{
 const x = 3;
 const x = 4; }

is @tech{invalid}.
This restriction is independent of whether a binding is
only known to a single @tech{participant}. For example,

@reach{
 Alice.only(() => {
   const x = 3; });
 Bob.only(() => {
   const x = 3; }); }

is @tech{invalid}.

The special identifier @reachin{_} is an exception to this rule.
The @reachin{_} binding is always considered to be unbound.
This means means that @reachin{_} is both
an identifier that can never be read,
as well as an identifier that may be bound many times.
This may be useful for ignoring unwanted values, for example:

@reach{
 const [_, x, _] = [1, 2, 3];
}

@subsection{@tt{return}}

@(mint-define! '("return"))
@reach{
 return 17;
 return 3 + 4;
 return f(2, false);
 return; }

A @deftech{return statement}, written @reachin{return EXPR;}, where @reachin{EXPR} is an @tech{expression} evaluates to the same @tech{value} as @reachin{EXPR}.
As a special case, @reachin{return;} is interpreted the same as @reachin{return null;}.

A @tech{return statement} returns its value to the surrounding function application.

A @tech{return statement} is a @tech{terminator statement}, so it must have an empty @tech{tail}. For example,

@reach{
 { return 1;
   return 2; } }

is @tech{invalid}, because the first @reachin{return}'s @tech{tail} is not empty.

@subsection{@tt{if}}

@(mint-define! '("if") '("else"))
@reach{
 if ( 1 + 2 < 3 ) {
   return "Yes!";
 } else {
   return "No, waaah!"; } }

A @deftech{conditional statement},
written @reachin{if (COND) NOT_FALSE else FALSE},
where @reachin{COND} is an @tech{expression}
and @reachin{NOT_FALSE} and @reachin{FALSE} as @tech{statements}
(potentially @tech{block statements}),
selects between the @reachin{NOT_FALSE} @tech{statement} and @reachin{FALSE} @tech{statement} based on whether @reachin{COND} evaluates to @reachin{false}.

Both @reachin{NOT_FALSE} and @reachin{FALSE} have empty @tech{tails}, i.e. the @tech{tail} of the @tech{conditional statement} is not propagated. For example,

@reach{
 if ( x < y ) {
   const z = 3; }
 else {
   const z = 4; }
 return z; }

is erroneous, because the identifier @reachin{z} is not bound outside the @tech{conditional statement}.

A @tech{conditional statement} may only include a @tech{consensus transfer} in @reachin{NOT_FALSE} or @reachin{FALSE} if it is within a @tech{consensus step}, because its statements are in the same context as the conditional statement itself.

@subsection{@tt{switch}}

@(mint-define! '("switch") '("case") '("default"))
@reach{
 const mi = Maybe(UInt).Some(42);
 switch ( mi ) {
  case None: return 8;
  case Some: return mi + 10; }
 switch ( mi ) {
  case None: return 8;
  default: return 41; } }

A @deftech{switch statement},
written @reachin{switch (VAR) { CASE ... }},
where @reachin{VAR} is a variable bound to a @tech{data instance}
and @reachin{CASE} is either @reachin{case VARIANT: STMT ...}, where @reachin{VARIANT} is a variant, or @reachin{default: STMT ...}, @reachin{STMT} is a sequence of statements,
selects the appropriate sequence of statements based on which variant @reachin{VAR} holds.
Within the body of a @reachin{switch} case, @reachin{VAR} has the type of variant; i.e. in a @reachin{Some} case of a @reachin{Maybe(UInt)} @reachin{switch}, the variable is bound to an integer.

All cases have empty @tech{tails}, i.e. the @tech{tail} of the @tech{switch statement} is not propagated.

A @tech{switch statement} may only include a @tech{consensus transfer} in its cases if it is within a @tech{consensus step}, because its statements are in the same context as the conditional statement itself.

It is @tech{invalid} for a case to appear multiple times, or be missing, or to be superfluous (i.e. for a variant that does not exist in the @reachin{Data} type of @reachin{VAR}).

@subsection{Block statements}

A @deftech{block statement} is when a @tech{block} occurs in a @tech{statement} position, then it establishes a local, separate scope for the definitions of identifiers within that @tech{block}. In other words, the @tech{block} is evaluated for effect, but the @tech{tail} of the @tech{statements} within the @tech{block} are isolated from the surrounding @tech{tail}. For example,

@reach{
 const x = 4;
 return x; }

evaluates to @reachin{4}, but

@reach{
 { const x = 4; }
 return x; }

is erroneous, because the identifier @reachin{x} is not bound outside the @tech{block statement}.

@subsection{Try/Catch & Throw Statements}

@reach{
  try {
    throw 10;
  } catch (v) {
    transfer(v).to(A); }}

A @deftech{try statement}, written @reachin{try BLOCK catch (VAR) BLOCK}, allows a block
of code to execute with a specified handler should an exception be thrown.

A @deftech{throw statement},
written @reachin{throw EXPR}, will transfer control flow to the exception handler, binding @tt{EXPR}
to @tt{VAR}. Any value that is able to exist at runtime may be thrown. For example, @reachin{Int}s
and @reachin{Array}s are valid values to throw, but a function is not.

@subsection{Expression statements}

@reach{
 4;
 f(2, true); }

An @tech{expression}, @reachin{E}, in a @tech{statement} position is equivalent to the @tech{block statement} @reachin{{ return E; }}.

@section[#:tag "ref-programs-compute-exprs"]{Expressions}

This section describes the expressions which are allowed in any Reach context.
There are a large variety of different @deftech{expressions} in Reach programs.

The remainder of this section enumerates each kind of @tech{expression}.

@subsection{'use strict'}

@(mint-define! '("'use strict'"))
@reach{
  'use strict'; }

@index{'use strict'} @reachin{'use strict'} enables unused variables checks for all subsequent
declarations within the current scope. If a variable is declared, but never used, there will
be an error emitted at compile time.

@deftech{strict mode} will reject some code that is normally valid and limit how dynamic Reach's type system is.
For example, normally Reach will permit expressions like the following to be evaluated:

@reach{
  const foo = (o) =>
    o ? o.b : false;

  void foo({ b: true });
  void foo(false); }

Reach allows @reachin{o} to be either an object with a @reachin{b} field or @reachin{false} because it
partially evaluates the program at compile time. So, without @reachin{'use strict'}, Reach will not evaluate
@reachin{o.b} when @reachin{o = false} and this code will compile successfully.

But, in @tech{strict mode}, Reach will ensure that this program treats @reachin{o} as
having a single type and detect an error in the program as follows:

@verbatim{
  reachc: error: Invalid field access. Expected object, got: Bool }

The correct way to write a program like this in @tech{strict mode} is to use @reachin{Maybe}. Like this:

@reach{
  const MObj = Maybe(Object({ b : Bool }));

  const foo = (mo) =>
    mo.match({
      None: (() => false),
      Some: ((o) => o.b)
    });

  void foo(MObj.Some({ b : true }));
  void foo(MObj.None()); }

@subsection{Identifier reference}

@reach{
 X
 Y
 Z }

An identifier, written @reachin{ID}, is an @tech{expression} that evaluates to the value of the @tech{bound identifier}.

@(mint-define! '("this"))
The identifier @reachin{this} has a special meaning inside of a @tech{local step} (i.e. the body of an @reachin{only} or @reachin{each} expression), as well as in a @tech{consensus step} (i.e. the tail of @reachin{publish} or @reachin{pay} statement and before a @reachin{commit} statement). For details, see @secref["ref-programs-local-this"] and @secref["ref-programs-consensus-this"].

@subsection{Function application}

@reach{
 assert( amount <= heap1 )
 step( moveA )
 digest( coinFlip )
 interact.random()
 declassify( _coinFlip ) }

A @deftech{function application}, written @reachin{EXPR_rator(EXPR_rand_0, ..., EXPR_rand_n)}, where @reachin{EXPR_rator} and @reachin{EXPR_rand_0} through @reachin{EXPR_rand_n} are @tech{expressions} that evaluate to one value.
@reachin{EXPR_rator} must evaluate to an abstraction over @reachin{n} values or a primitive of arity @reachin{n}.
A spread expression (@reachin{...expr}) may appear in the list of operands to a function application, in which case the elements of the expr are spliced in place.

@(mint-define! '("new"))
@reachin{new f(a)} is equivalent to @reachin{f(a).new()} and is a convenient short-hand for writing class-oriented programs.

@subsection{Types}

Reach's @deftech{type}s are represented with programs by the following identifiers and constructors:

@itemlist[
  @item{@(mint-define! '("Null")) @reachin{Null}.}
  @item{@(mint-define! '("Bool")) @reachin{Bool}, which denotes a boolean.}
  @item{@(mint-define! '("UInt")) @reachin{UInt}, which denotes an unsigned integer.
  @reachin{UInt.max} is the largest value that may be assigned to a @reachin{UInt}.}
  @item{@(mint-define! '("Bytes")) @reachin{Bytes(length)}, which denotes a string of bytes of length at most @reachin{length}.}
  @item{@(mint-define! '("Digest")) @reachin{Digest}, which denotes a @tech{digest}.}
  @item{@(mint-define! '("Address")) @reachin{Address}, which denotes an @tech{account} @tech{address}.}
  @item{@(mint-define! '("Token")) @reachin{Token}, which denotes a @tech{non-network token}.}
  @item{@(mint-define! '("Fun")) @reachin{Fun([Domain_0, ..., Domain_N], Range)}, which denotes a @deftech{function type}, when @reachin{Domain_i} and @reachin{Range} are types.
  The domain of a function is @tech{negative position}.
  The range of a function is @tech{positive position}.}
  @item{@reachin{Fun(true, Range)}, which denotes an @deftech{unconstrained domain function type}, when @reachin{Range} is a type.
  These functions may only appear in @tech{participant interact interfaces}.}
  @item{@(mint-define! '("Tuple")) @reachin{Tuple(Field_0, ..., FieldN)}, which denotes a tuple.
  (Refer to @secref["ref-programs-tuples"] for constructing tuples.)}
  @item{@(mint-define! '("Object")) @reachin{Object({key_0: Type_0, ..., key_N: Type_N})}, which denotes an @tech{object}.
  (Refer to @secref["ref-programs-objects"] for constructing objects.)}
  @item{@(mint-define! '("Struct")) @reachin{Struct([[key_0, Type_0], ..., [key_N, Type_N]])}, which denotes a @tech{struct}.
  (Refer to @secref["ref-programs-structs"] for constructing structs.)}
  @item{@(mint-define! '("Array")) @reachin{Array(Type_0, size)}, which denotes a statically-sized array.
  @reachin{Type_0} must be a type that can exist at runtime (i.e., not a @tech{function type}.)
  (Refer to @secref["ref-programs-arrays"] for constructing arrays.)}
  @item{@(mint-define! '("Data")) @reachin{Data({variant_0: Type_0, ..., variant_N: Type_N})}, which denotes a @link["https://en.wikipedia.org/wiki/Tagged_union"]{tagged union} (or @emph{sum type}).
  (Refer to @secref["ref-programs-data"] for constructing @tech{data instances}.)}
  @item{@(mint-define! '("Refine")) @reachin{Refine(Type_0, Predicate, ?Message)}, where @reachin{Predicate} is a unary function returning a boolean, which denotes a @link["https://en.wikipedia.org/wiki/Refinement_type"]{refinement type}, that is instances of @reachin{Type_0} that satisfy @reachin{Predicate}.
  When a refinement type appears in a @deftech{negative position} (such as in a @reachin{is} or in the domain of a @reachin{Fun} of a @tech{participant interact interface}), it introduces an @reachin{assert};
  while when it is in a @deftech{positive position}, it introduces an @reachin{assume}.
  @reachin{Message} is an optional string to display if the predicate fails verification.

  For example, if @reachin{f} had type @reach{Fun([Refine(UInt, (x => x < 5))], Refine(UInt, (x => x > 10)))}

  then @reachin{const z = f(y)} is equivalent to

  @reach{
    assert(y < 5);
    const z = f(y);
    assume(z > 10);}}
 @item{@reachin{Refine(Type_0, PreCondition, PostCondition, ?Messages)}, where @reachin{Type_0} is a @tech{function type}, @reachin{PreCondition} is a unary function that accepts a tuple of the domain and returns a boolean, and @reachin{PostCondition} is a binary function that accepts a tuple of the domain and the range and returns a boolean, denotes a @tech{function type} with a @link["https://en.wikipedia.org/wiki/Precondition"]{precondition} and @link["https://en.wikipedia.org/wiki/Postcondition"]{postcondition}.
 Preconditions are enforced with @reachin{assert} and postconditions are enforced with @reachin{assume}.
 @reachin{Messages} is an optional two-tuple of @reachin{Bytes}.
 The first message will be displayed when the precondition fails verification and the second when the postcondition fails verification.

 For example, @reachin{Refine(Fun([UInt, UInt], UInt), ([x, y] => x < y), (([x, y], z) => x + y < z))} is a function that requires its second argument to be larger than its first and its result to be larger than its input.}
]

@reachin{Object} and @reachin{Data} are commonly used to implemented @link["https://en.wikipedia.org/wiki/Algebraic_data_type"]{algebraic data types} in Reach.

@(mint-define! '("typeOf") '("isType") '("is"))
@reach{
 typeOf(x) // type
 isType(t) // Bool
 is(x, t) // t
}

The @reachin{typeOf} primitive function is the same as @reachin{typeof}:
it returns the type of its argument.

The @reachin{isType} function returns @reachin{true} if its argument is a type.
Any expression satisfying @reachin{isType} is compiled away and does not exist at runtime.

The @reachin{is} function returns its first argument if it satisfies the type specified by the second argument.
If it is not, then the program is @tech{invalid}.
For example, @reachin{is(5, UInt)} returns @reachin{5}, while @reachin{is(5, Bool)} is an @tech{invalid} program.
The value returned by @reachin{is} may not be identical to the input, because in some cases, such as for functions, it will record the applied to type and enforce it on future invocations.
These applications are considered @tech{negative positions} for @reachin{Refine}.

@subsection{Literal values}

@(mint-define! '("true") '("false") '("null"))
@reach{
 10
 0xdeadbeef
 007
 -10
 34.5432
 true
 false
 null
 "reality bytes"
 'it just does' }

A @deftech{literal value},
written @reachin{VALUE},
is an @tech{expression} that evaluates to the given @tech{value}.

The @deftech{null literal} may be written as @reachin{null}.

@deftech{Numeric literal}s may be written in decimal, hexadecimal, or octal.
Numeric literals must obey the @deftech{bit width} of @reachin{UInt} if they are used as @reachin{UInt} values at runtime, but if they only appear at compile-time, then they may be any positive number.
Reach provides abstractions for working with @reachin{Int}s and signed @reachin{FixedPoint} numbers.
@reachin{Int}s may be defined by applying the unary @reachin{+} and @reachin{-} operators to values of type @reachin{UInt}.
Reach provides syntactic sugar for defining signed @reachin{FixedPoint} numbers, in base 10, with decimal syntax.

@deftech{Boolean literal}s may be written as @reachin{true} or @reachin{false}.

@deftech{String literal}s (aka byte strings)
may be written between double or single quotes
(with no distinction between the different styles)
and use the same escaping rules as JavaScript.

@subsection{Operator expression}

An @deftech{operator} is a special identifier,
which is either a @tech{unary operator}, or a @tech{binary operator}.

@(hrule)

@(mint-define! '("!") '("-") '("+") '("typeof") '("not") '("minus") '("plus") '("void"))
@reach{
 ! a  // not
 - a  // minus
 + a  // plus
 typeof a
 void a}

A @deftech{unary expression}, written @reachin{UNAOP EXPR_rhs}, where @reachin{EXPR_rhs} is an @tech{expression} and @reachin{UNAOP} is one of the @deftech{unary operator}s: @litchar{! - + typeof void}. All the unary operators, besides @reachin{typeof}, have a
corresponding named version in the standard library.

It is @tech{invalid} to use unary operations on the wrong types of @tech{values}.

When applied to values of type @reachin{UInt}, unary @reachin{-} and @reachin{+} operators will cast
their arguments to type @reachin{Int}. The unary @reachin{-} and @reachin{+} operations are defined for
values of type: @reachin{Int}, and @reachin{FixedPoint}.

@reachin{void a} evaluates to @reachin{null} for all arguments.

@(hrule)

@(mint-define! '("&" "&") '("||") '("+") '("-") '("*") '("/") '("%") '("|") '("&") '("^") '("<" "<") '(">" ">") '("==") '("!=") '("===") '("!==") '(">") '(">" "=") '("<" "=") '("<"))
@reach{
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
 a < b }

@margin-note{Bitwise operations are not supported by all @tech{consensus networks} and greatly decrease the efficiency of verification.}

A @deftech{binary expression}, written @reachin{EXPR_lhs BINOP EXPR_rhs}, where @reachin{EXPR_lhs} and @reachin{EXPR_rhs} are @tech{expressions} and @reachin{BINOP} is one of the @deftech{binary operator}s: @litchar{&& || + - * / % | & ^ << >> == != === !== > >= <= <}.
The operators @reachin{==} (and @reachin{===}) and @reachin{!=} (and @reachin{!==}) operate on all atomic values.
Numeric operations, like @reachin{+} and @reachin{>}, only operate on numbers.
Since all numbers in Reach are integers, operations like @reachin{/} truncate their result.
Boolean operations, like @reachin{&&}, only operate on booleans.
It is @tech{invalid} to use binary operations on the wrong types of @tech{values}.

@(mint-define! '("and") '("or") '("add") '("sub") '("mul") '("div") '("mod") '("lt") '("le") '("ge") '("gt") '("lsh") '("rsh") '("band") '("bior") '("band") '("bxor") '("polyEq") '("polyNeq"))
@reach{
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
}

All @tech{binary expression} operators have a corresponding named function in the standard library.
While @reachin{&&} and @reachin{||} may not evaluate their second argument,
their corresponding named functions @reachin{and} and @reachin{or}, always do.

@(mint-define! '("boolEq") '("typeEq") '("intEq") '("digestEq") '("addressEq") '("fxeq") '("ieq"))
@reach{
 polyEq(a, b)    // eq on all types
 boolEq(a, b)    // eq on Bool
 typeEq(a, b)    // eq on types
 intEq(a, b)     // eq on UInt
 digestEq(a, b)  // eq on Digest
 addressEq(a, b) // eq on Addresses
 fxeq(a, b)      // eq on FixedPoint
 ieq(a, b)       // eq on Int
}

@reachin{==} is a function which operates on all types.
Both arguments must be of the same type.
Specialized functions exist for equality checking on each supported type.

@(hrule)

If @reachin{verifyArithmetic} is @reachin{true}, then arithmetic operations automatically make a @tech{static assertion} that their arguments would not overflow the @tech{bit width} of the enable @tech{consensus networks}.
If it is @reachin{false}, then the @tech{connector} will ensure this dynamically.

@subsection{xor}

@reach{
 xor(false, false); // false
 xor(false, true);  // true
 xor(true, false);  // true
 xor(true, true);   // false }

@index{xor} @reachin{xor(Bool, Bool)} returns @reachin{true} only when the inputs differ in value.

@subsection{Parenthesized expression}

@reach{
 (a + b) - c }

An @tech{expression} may be parenthesized, as in @reachin{(EXPR)}.

@subsection[#:tag "ref-programs-tuples"]{Tuples}

@reach{
 [ ]
 [ 1, 2 + 3, 4 * 5 ] }

A @deftech{tuple} literal, written @reachin{[ EXPR_0, ..., EXPR_n ]}, is an @tech{expression} which evaluates to a tuple of @reachin{n} values, where @reachin{EXPR_0} through @reachin{EXPR_n} are @tech{expressions}.

@reachin{...expr} may appear inside tuple expressions, in which case the spreaded expression must evaluate to a tuple or array, which is spliced in place.

@subsection[#:tag "ref-programs-arrays"]{@tt{array}}

@(mint-define! '("array"))
@reach{
  const x = array(UInt, [1, 2, 3]); }

Converts a @tech{tuple} of homogeneous values of the specific type into an @deftech{array}.

@subsection{Element reference}

@reach{
 arr[3] }

A @deftech{reference}, written @reachin{REF_EXPR[IDX_EXPR]},
where @reachin{REF_EXPR} is an @tech{expression} that evaluates to an @tech{array}, a @tech{tuple}, or a @tech{struct}
and @reachin{IDX_EXPR} is an @tech{expression} that evaluates to a natural number which is less than the size of the array,
selects the element at the given index of the array.
Indices start at zero.

If @reachin{REF_EXPR} is a @tech{tuple}, then @reachin{IDX_EXPR} must be a compile-time constant, because tuples do not support dynamic access, because each element may be a different type.

If @reachin{REF_EXPR} is a @tech{mapping} and @reachin{IDX_EXPR} evaluates to an @tech{address}, then this @tech{reference} evaluates to a value of type @reachin{Maybe(TYPE)}, where @reachin{TYPE} is the @tech{type} of the @tech{mapping}.

@subsection{Array & tuple length: @tt{Tuple.length}, @tt{Array.length}, and @tt{.length}}

@(mint-define! '("length"))
@reach{
 Tuple.length(tup);
 tup.length;
 Array.length(arr);
 arr.length; }

@index{Tuple.length} @reachin{Tuple.length} Returns the length of the given tuple.

@index{Array.length} @reachin{Array.length} Returns the length of the given array.

Both may be abbreviated as @reachin{expr.length} where @reachin{expr} evaluates to a tuple or an array.

@subsection{Array & tuple update: @tt{Tuple.set}, @tt{Array.set}, and @tt{.set}}

@(mint-define! '("set"))
@reach{
 Tuple.set(tup, idx, val);
 tup.set(idx, val);
 Array.set(arr, idx, val);
 arr.set(idx, val); }

@index{Tuple.set} @reachin{Tuple.set} Returns a new @tech{tuple} identical to @reachin{tup},
except that index @reachin{idx} is replaced with @reachin{val}.
The @reachin{idx} must be a compile-time constant, because tuples do not support dynamic access, because each element may be a different type.

@index{Array.set} @reachin{Array.set} Returns a new @tech{array} identical to @reachin{arr}, except that index @reachin{idx} is replaced with @reachin{val}.

Both may be abbreviated as @reachin{expr.set(idx, val)} where @reachin{expr} evaluates to a @tech{tuple} or an @tech{array}.

@subsection{Foldable operations}

The following methods are available on any @(mint-define! '("Foldable"))@reachin{Foldable} containers, such as: @reachin{Array}s and @reachin{Map}s.

@subsubsection{ @tt{Foldable.forEach} && @tt{.forEach}}

@(mint-define! '("forEach"))
@reach{
 c.forEach(f)
 Foldable.forEach(c, f)
 Array.forEach(c, f)
 Map.forEach(c, f) }

@index{Foldable.forEach} @reachin{Foldable.forEach(c, f)} iterates the function @reachin{f} over the elements of a container @reachin{c}, discarding the result.
This may be abbreviated as @reachin{c.forEach(f)}.

@subsubsection{@tt{Foldable.all} && @tt{.all}}

@(mint-define! '("all"))
@reach{
  Foldable.all(c, f)
  Array.all(c, f)
  Map.all(c, f)
  c.all(f) }

@index{Foldable.all} @reachin{Foldable.all(c, f)} determines whether the predicate, @tt{f}, is satisfied
by every element of the container, @tt{c}.

@subsubsection{@tt{Foldable.any} && @tt{.any}}

@(mint-define! '("any"))
@reach{
  Foldable.any(c, f)
  Array.any(c, f)
  Map.any(c, f)
  c.any(f) }

@index{Foldable.any} @reachin{Foldable.any(c, f)} determines whether the predicate, @tt{f}, is satisfied
by at least one element of the container, @tt{c}.

@subsubsection{@tt{Foldable.or} && @tt{.or}}

@(mint-define! '("or"))
@reach{
  Foldable.or(c)
  Array.or(c)
  Map.or(c)
  c.or() }

@index{Foldable.or} @reachin{Foldable.or(c)} returns the disjunction of a container of @reachin{Bool}s.

@subsubsection{@tt{Foldable.and} && @tt{.and}}

@(mint-define! '("and"))
@reach{
  Foldable.and(c)
  Array.and(c)
  Map.and(c)
  c.and() }

@index{Foldable.and} @reachin{Foldable.and(c)} returns the conjunction of a container of @reachin{Bool}s.

@subsubsection{@tt{Foldable.includes} && @tt{.includes}}

@(mint-define! '("includes"))
@reach{
  Foldable.includes(c, x)
  Array.includes(c, x)
  Map.includes(c, x)
  c.includes(x) }

@index{Foldable.includes} @reachin{Foldable.includes(c, x)} determines whether the container includes
the element, @tt{x}.

@subsubsection{@tt{Foldable.count} && @tt{.count}}

@(mint-define! '("count"))
@reach{
  Foldable.count(c, f)
  Array.count(c, f)
  Map.count(c, f)
  c.count(f) }

@index{Foldable.count} @reachin{Foldable.count(c, f)} returns the number of elements in @tt{c} that
satisfy the predicate, @tt{f}.

@subsubsection{@tt{Foldable.size} && @tt{.size}}

@(mint-define! '("size"))
@reach{
  Foldable.size(c)
  Array.size(c)
  Map.size(c)
  c.size() }

@index{Foldable.size} @reachin{Foldable.size(c)} returns the number of elements in @tt{c}.

@subsubsection{@tt{Foldable.min} && @tt{.min}}

@(mint-define! '("min"))
@reach{
  Foldable.min(c)
  Array.min(c)
  Map.min(c)
  c.min() }

@index{Foldable.min} @reachin{Foldable.min(arr)} returns the lowest number in a container of @tt{UInt}s.

@subsubsection{@tt{Foldable.max} && @tt{.max}}

@(mint-define! '("max"))
@reach{
  Foldable.max(c)
  Array.max(c)
  Map.max(c)
  c.max() }

@index{Foldable.max} @reachin{Foldable.max(c)} returns the largest number in a container of @tt{UInt}s.

@subsubsection{@tt{Foldable.sum} && @tt{.sum}}

@(mint-define! '("sum"))
@reach{
  Foldable.sum(c)
  Array.sum(c)
  Map.sum(c)
  c.sum() }

@index{Foldable.sum} @reachin{Foldable.sum(c)} returns the sum of a container of @tt{UInt}s.

@subsubsection{@tt{Foldable.product} && @tt{.product}}

@(mint-define! '("product"))
@reach{
  Foldable.product(c)
  Array.product(c)
  Map.product(c)
  c.product() }

@index{Foldable.product} @reachin{Foldable.product(c)} returns the product of a container of @tt{UInt}s.

@subsubsection{@tt{Foldable.average} && @tt{.average}}

@(mint-define! '("average"))
@reach{
  Foldable.average(c)
  Array.average(c)
  Map.average(c)
  c.average() }

@index{Foldable.average} @reachin{Foldable.average(c)} returns the mean of a container of @tt{UInt}s.

@subsection{Array group operations}

@reachin{Array} is a @reachin{Foldable} container. Along with the methods of @reachin{Foldable}, the
following methods may be used with @reachin{Array}s.

@subsubsection{@tt{Array.iota}}

@(mint-define! '("iota"))
@reach{
 Array.iota(5) }

@index{Array.iota} @reachin{Array.iota(len)} returns an array of length @reachin{len}, where each element is the same as its index.
For example, @reachin{Array.iota(4)} returns @reachin{[0, 1, 2, 3]}.
The given @reachin{len} must evaluate to an integer at compile-time.

@subsubsection{@tt{Array.replicate} && @tt{.replicate}}

@(mint-define! '("Array_replicate") '("replicate"))
@reach{
 Array.replicate(5, "five")
 Array_replicate(5, "five") }

@index{Array.replicate} @reachin{Array.replicate(len, val)} returns an array of length @reachin{len}, where each element is @reachin{val}.
For example, @reachin{Array.replicate(4, "four")} returns @reachin{["four", "four", "four", "four"]}.
The given @reachin{len} must evaluate to an integer at compile-time.

@subsubsection{@tt{Array.concat} && @tt{.concat}}

@(mint-define! '("concat"))
@reach{
 Array.concat(x, y)
 x.concat(y) }

@index{Array.concat} @reachin{Array.concat(x, y)} concatenates the two arrays @reachin{x} and @reachin{y}.
This may be abbreviated as @reachin{x.concat(y)}.

@subsubsection{@tt{Array.empty}}

@(mint-define! '("Array_empty") '("empty"))
@reach{
 Array_empty
 Array.empty }

@index{Array.empty} @reachin{Array.empty} is an array with no elements.
It is the identity element of @reachin{Array.concat}.
It may also be written @reachin{Array_empty}.

@subsubsection{@tt{Array.zip} && @tt{.zip}}

@(mint-define! '("zip"))
@reach{
 Array.zip(x, y)
 x.zip(y) }

@index{Array.zip} @reachin{Array.zip(x, y)} returns a new array the same size as @reachin{x} and @reachin{y} (which must be the same size) whose elements are tuples of the elements of @reachin{x} and @reachin{y}.
This may be abbreviated as @reachin{x.zip(y)}.

@subsubsection{@tt{Array.map} && @tt{.map}}

@(mint-define! '("map"))
@reach{
 Array.map(arr, f)
 arr.map(f) }

@index{Array.map} @reachin{Array.map(arr, f)} returns a new array, @reachin{arr_mapped}, the same size as @reachin{arr}, where @reachin{arr_mapped[i] = f(arr[i])} for all @reachin{i}.
For example, @reachin{Array.iota(4).map(x => x+1)} returns @reachin{[1, 2, 3, 4]}.
This may be abbreviated as @reachin{arr.map(f)}.

This function is generalized to an arbitrary number of arrays of the same size, which are provided before the @reachin{f} argument.
For example, @reachin{Array.iota(4).map(Array.iota(4), add)} returns @reachin{[0, 2, 4, 6]}.

@subsubsection{@tt{Array.reduce} && @tt{.reduce}}

@(mint-define! '("reduce"))
@reach{
 Array.reduce(arr, z, f)
 arr.reduce(z, f) }

@index{Array.reduce} @reachin{Array.reduce(arr, z, f)} returns the @link["https://en.wikipedia.org/wiki/Fold_(higher-order_function)"]{left fold} of the function @reachin{f} over the given array with the initial value @reachin{z}.
For example, @reachin{Array.iota(4).reduce(0, add)} returns @reachin{((0 + 1) + 2) + 3 = 6}.
This may be abbreviated as @reachin{arr.reduce(z, f)}.

This function is generalized to an arbitrary number of arrays of the same size, which are provided before the @reachin{z} argument.
For example, @reachin{Array.iota(4).reduce(Array.iota(4), 0, (x, y, z) => (z + x + y))} returns @reachin{((((0 + 0 + 0) + 1 + 1) + 2 + 2) + 3 + 3)}.

@subsubsection{@tt{Array.indexOf} && @tt{.indexOf}}

@(mint-define! '("indexOf"))
@reach{
  Array.indexOf(arr, x)
  arr.indexOf(x) }

@index{Array.indexOf} @reachin{Array.indexOf(arr, x)} returns the index of the first element
in the given array that is equal to @tt{x}. The return value is of type @reachin{Maybe(UInt)}. If
the value is not present in the array, @reachin{None} is returned.

@subsubsection{@tt{Array.findIndex} && @tt{.findIndex}}

@(mint-define! '("findIndex"))
@reach{
  Array.findIndex(arr, f)
  arr.findIndex(f) }

@index{Array.findIndex} @reachin{Array.findIndex(arr, f)} returns the index of the first element
in the given array that satisfies the predicate @tt{f}. The return value is of type @reachin{Maybe(UInt)}. If
no value in the array satisfies the predicate, @reachin{None} is returned.

@subsection{Mapping group operations}

@reachin{Map} is a @reachin{Foldable} container. @tech{Mappings} may be aggregated with the following
operations and those of @reachin{Foldable} within the @reachin{invariant} of a @reachin{while} loop.

@subsubsection{@tt{Map.reduce} && @tt{.reduce}}

@reach{
 Map.reduce(map, z, f)
 map.reduce(z, f) }

@index{Map.reduce} @reachin{Map.reduce(map, z, f)} returns the @link["https://en.wikipedia.org/wiki/Fold_(higher-order_function)"]{left fold} of the function @reachin{f} over the given @tech{mapping} with the initial value @reachin{z}.
For example, @reachin{m.reduce(0, add)} sums the elements of the @tech{mapping}.
This may be abbreviated as @reachin{map.reduce(z, f)}.

The function @reachin{f} must satisfy the property, for all @reachin{z}, @reachin{a}, @reachin{b}, @reachin{f(f(z, b), a) == f(f(z, a), b)}, because the order of evaluation is unpredictable.

@subsection[#:tag "ref-programs-objects"]{Objects}

@reach{
  { }
  { x: 3, "yo-yo": 4 }
  { [1 < 2 ? "one" : "two"]: 5 }
}

An @deftech{object},
typically written @reachin{{ KEY_0: EXPR_0, ..., KEY_n: EXPR_n }},
where @reachin{KEY_0} through @reachin{KEY_n} are @tech{identifiers} or @tech{string literal}s
and @reachin{EXPR_0} through @reachin{EXPR_n} are @tech{expressions},
is an @tech{expression} which evaluates to an @tech{object}
with fields @reachin{KEY_0} through @reachin{KEY_n}.

Additional object literal syntax exists for convenience, such as:

@reach{
  { ...obj, z: 5 }
}

An @deftech{object splice},
where all fields from @reachin{obj} are copied into the object;
these fields may be accompanied by additional fields specified afterwards.

@reach{
  { x, z: 5 }
}

Shorthand for @reachin{{ x: x, z: 5}}, where @reachin{x} is any @tech{bound identifier}.

@subsection[#:tag "ref-programs-structs"]{Structs}

@reach{
  const Posn = Struct([["x", UInt], ["y", UInt]]);
  const p1 = Posn.fromObject({x: 1, y: 2});
  const p2 = Posn.fromTuple([1, 2]);
}

A @deftech{struct} is a combination of a @tech{tuple} and an @tech{object}.
It has named elements, like an @tech{object}, but is ordered like a @tech{tuple}, so its elements may be accessed by either name or position.
@tech{Structs} exist for interfacing with non-Reach @tech{remote objects}, where both parties must agree to the runtime representation of the values.

@index{Struct.fromTuple}
A @tech{struct} instance may be constructed by calling the @reachin{fromTuple} method of a @tech{struct} type instance (like @reachin{Posn}) with a @tech{tuple} of the appropriate length.

@index{Struct.fromObject}
A @tech{struct} instance may be constructed by calling the @reachin{fromObject} method of a @tech{struct} type instance (like @reachin{Posn}) with an @tech{object} with the appropriate fields.

@index{Struct.toTuple}
@index{Struct.toObject}
Structs may be converted into a corresponding @tech{tuple} or @tech{object} via the @reachin{toTuple} and @reachin{toObject} methods on the @reachin{Struct} value (as well as @tech{struct} type instances, like @reachin{Posn} in the example above):

@reach{
  assert(Posn.toTuple(p1)[0] == 1);
  assert(Struct.toObject(p2).y == 2);
}

The names of elements may be restricted to avoid conflicting with reserved words of the specified @tech{connectors}.

@subsection{Field reference}

@reach{
  obj.x
}

An @deftech{object reference},
written @reachin{OBJ.FIELD},
where @reachin{OBJ} is an expression that evaluates to an @tech{object} or a @tech{struct},
and @reachin{FIELD} is a @tech{valid} @tech{identifier},
accesses the @litchar{FIELD} @deftech{field} of object OBJ.

@subsection{@tt{Object.set}}

@(mint-define! '("Object_set"))
@reach{
 Object.set(obj, fld, val);
 Object_set(obj, fld, val);
 { ...obj, [fld]: val };
}

@index{Object.set} Returns a new @tech{object} identical to @reachin{obj},
except that field @reachin{fld} is replaced with @reachin{val}.

@subsection{@tt{Object.setIfUnset}}

@(mint-define! '("Object_setIfUnset"))
@reach{
 Object.setIfUnset(obj, fld, val);
 Object_setIfUnset(obj, fld, val);
}

@index{Object.setIfUnset} Returns a new object identical to @reachin{obj},
except that field @reachin{fld} is @reachin{val} if @reachin{fld} is not already present in @reachin{obj}.

@subsection{@tt{Object.has}}

@reach{
 Object.has(obj, fld);
}

@index{Object.has} Returns a boolean indicating whether the @tech{object} has the field @reachin{fld}.
This is statically known.

@subsection[#:tag "ref-programs-data"]{Data}

@reach{
 const Taste = Data({Salty: Null,
                     Spicy: Null,
                     Sweet: Null,
                     Umami: Null});
 const burger = Taste.Umami();

 const Shape = Data({ Circle: Object({r: UInt}),
                      Square: Object({s: UInt}),
                      Rect: Object({w: UInt, h: UInt}) });
 const nice = Shape.Circle({r: 5}); }

A @deftech{data instance} is written @reachin{DATA.VARIANT(VALUE)}, where @reachin{DATA} is @reachin{Data} type, @reachin{VARIANT} is the name of one of @reachin{DATA}'s variants, and @reachin{VALUE} is a value matching the type of the variant.
As a special case, when the type of a variant is @reachin{Null}, the @reachin{VALUE} may be omitted, as shown in the definition of @reachin{burger} in the same above.

@tech{Data instances} are consumed by @reachin{switch} statements.

@subsection{@tt{Maybe}}

@(mint-define! '("Maybe") '("Some") '("None") '("fromMaybe"))
@reach{
 const MayInt = Maybe(UInt);
 const bidA = MayInt.Some(42);
 const bidB = MayInt.None(null);

 const getBid = (m) => fromMaybe(m, (() => 0), ((x) => x));
 const bidSum = getBid(bidA) + getBid(bidB);
 assert(bidSum == 42); }

@link["https://en.wikipedia.org/wiki/Option_type"]{Option types} are represented in Reach through the built-in @reachin{Data} type, @reachin{Maybe}, which has two variants: @reachin{Some} and @reachin{None}.

@reachin{Maybe} is defined by
@reach{
 export const Maybe = (A) => Data({None: Null, Some: A}); }

This means it is a function that returns a @reachin{Data} type specialized to a particular type in the @reachin{Some} variant.

@reachin{Maybe} instances can be conveniently consumed by @reachin{fromMaybe(mValue, onNone, onSome)}, where @reachin{onNone} is a function of no arguments which is called when @reachin{mValue} is @reachin{None}, @reachin{onSome} is a function of on argument which is called with the value when @reachin{mValue} is @reachin{Some}, and @reachin{mValue} is a @tech{data instance} of @reachin{Maybe}.

@(mint-define! '("isNone") '("isSome"))
@reach{
  const m = Maybe(UInt).Some(5);
  isNone(m); // false
  isSome(m); // true
}

@index{isNone} @reachin{isNone} is a convenience method that determines whether the variant is @tt{isNone}.

@index{isSome} @reachin{isSome} is a convenience method that determines whether the variant is @tt{isSome}.


@(mint-define! '("fromSome"))
@reach{
  fromSome(Maybe(UInt).Some(1), 0); // 1
  fromSome(Maybe(UInt).None(), 0);  // 0
}

@index{fromSome} @reachin{fromSome} receives a @reachin{Maybe} value and a default value as arguments and will return the value inside
of the @reachin{Some} variant or the default value otherwise.

@(mint-define! '("maybe"))
@reach{
  const add1 = (x) => x + 1;
  maybe(Maybe(UInt).Some(1), 0, add1); // 2
  maybe(Maybe(UInt).None(), 0, add1);  // 0
}

@index{maybe} @reachin{maybe(m, defaultVal, f)} receives a @reachin{Maybe} value, a default value, and a unary function as arguments. The function will
either return the application of the function, @reachin{f}, to the @reachin{Some} value or return the default value provided.

@subsection{@tt{Either}}

@reachin{Either} is defined by
@reach{
  export const Either = (A, B) => Data({Left: A, Right: B}); }

@reachin{Either} can be used to represent values with two possible types.

Similar to @tt{Maybe}, @tt{Either} may be used to represent values that are correct or erroneous.
A successful result is stored, by convention, in @tt{Right}. Unlike @tt{None}, @tt{Left} may
carry additional information about the error.

@(mint-define! '("either"))
@reach{
  either(e, onLeft, onRight) }

@index{either} @reachin{either(e, onLeft, onRight)} For an @tt{Either} value, @tt{e}, @tt{either}
will either apply the function @tt{onLeft} or @tt{onRight} to the appropriate variant value.

@(mint-define! '("isLeft") '("isRight") '("fromLeft") '("fromRight"))
@reach{
  const e = Either(UInt, Bool);
  const l = e.Left(1);
  const r = e.Right(true);
  isLeft(l);  // true
  isRight(l); // false
  const x = fromLeft(l, 0);      // x = 1
  const y = fromRight(l, false); // y = false }

@index{isLeft} @reachin{isLeft} is a convenience method that determines whether the variant is @tt{Left}.

@index{isRight} @reachin{isRight} is a convenience method that determines whether the variant is @tt{Right}.

@index{fromLeft} @reachin{fromLeft(e, default)} is a convenience method that returns the value in @tt{Left},
or @tt{default} if the variant is @tt{Right}.

@index{fromRight} @reachin{fromRight(e, default)} is a convenience method that returns the value in @tt{Right},
or @tt{default} if the variant is @tt{Left}.

@subsection{@tt{match}}

@(mint-define! '("match"))
@reach{
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
}

A @deftech{match expression}, written @reachin{VAR.match({ CASE ... })}, where @tt{VAR} is a variable
bound to a @tech{data instance} and @tt{CASE} is @tt{VARIANT: FUNCTION}, where @tt{VARIANT} is a
variant or @reachin{default}, and @tt{FUNCTION} is a function that takes the same arguments as the
variant constructor.
If the variant has a type of @reachin{Null}, then the function is allowed to take no arguments.
@reachin{default} functions must always take an argument, even if all defaulted variants have type @reachin{Null}.

@reachin{match} is similar to a @tech{switch statement}, but since it is an expression, it
can be conveniently used in places like the right hand side of an assignment statement.

Similar to a @tech{switch statement}, the cases are expected to be exhaustive and nonredundant,
all cases have empty @@tech{tails}, and it may only include a @tech{consensus transfer} in
its cases if it is within a @tech{consensus step}.

@subsection{Conditional expression}

@(mint-define! '("?"))
@reach{
 choosesFirst ? [ heap1 - amount, heap2 ] : [ heap1, heap2 - amount ] }

A @deftech{conditional expression}, written @reachin{COND_E ? NOT_FALSE_E : FALSE_E}, where @reachin{COND_E}, @reachin{NOT_FALSE_E}, and @reachin{FALSE_E} are @tech{expressions}, selects between the @tech{values} which @reachin{NOT_FALSE_E} and @reachin{FALSE_E} evaluate to based on whether @reachin{COND_E} evaluates to @reachin{false}.

@(mint-define! '("ite"))
@reach{
 ite(choosesFirst, [heap1 - amount, heap2], [heap1, heap2 - amount])
}

@tech{Conditional expression}s may also be written with the @reachin{ite} function,
however, note that this function always evaluates both of its branches,
while the regular conditional expression only evaluates one branch.

@subsection{Arrow expression}

@(mint-define! '("=" ">"))
@reach{
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
}

An @deftech{arrow expression}, written @reachin{(LHS_0, ..., LHS_n) => EXPR}, where @reachin{LHS_0} through @reachin{LHS_n} are left-hand sides and @reachin{EXPR} is an @tech{expression}, evaluates to an function which is an abstraction of @reachin{EXPR} over @reachin{n} values compatible with the respective left-hand side.
Like @tech{function definition}s, @tech{arrow expression}s may use default argument notation and @tech{rest parameter}s.

@subsection{@tt{makeEnum}}

@(mint-define! '("makeEnum"))
@reach{
  const [ isHand, ROCK, PAPER, SCISSORS ] = makeEnum(3); }

An @deftech{enumeration} (or @deftech{enum}, for short),
can be created by calling the @reachin{makeEnum} function, as in @reachin{makeEnum(N)},
where @reachin{N} is the number of distinct values in the enum.
This produces a tuple of @reachin{N+1} values,
where the first value is a @reachin{Fun([UInt], Bool)}
which tells you if its argument is one of the enum's values,
and the next N values are distinct @reachin{UInt}s.

@subsection{@tt{assert}}

@(mint-define! '("assert"))
@reach{
 assert( claim, [msg] ) }

@index{assert} A @tech{static assertion} which is only @tech{valid} if @reachin{claim} always evaluates to @reachin{true}.
@margin-note{The Reach compiler will produce a counter-example (i.e. an assignment of the identifiers in the program to falsify the @reachin{claim}) when an @tech{invalid} @reachin{claim} is provided.
It is possible to write a @reachin{claim} that actually always evaluates to @reachin{true}, but for which our current approach cannot prove always evaluates to @reachin{true}; if this is the case, Reach will fail to compile the program, reporting that its analysis is incomplete.
Reach will never produce an erroneous counter-example.}
It accepts an optional bytes argument, which is included in any reported violation.

@margin-note{See @seclink["guide-assert"]{the guide section on verification} to better understand how and what to verify in your program.}

@subsection{@tt{forall}}

@(mint-define! '("forall"))
@reach{
 forall( Type )
 forall( Type, (var) => BLOCK ) }

@index{forall} The single argument version returns an abstract value of the given type.
It may only be referenced inside of @tech{assert}ions; any other reference is invalid.

The two argument version is an abbreviation of calling the second argument with the result of @reachin{forall(Type)}.
This is convenient for writing general claims about expressions, such as

@reach{
 forall(UInt, (x) => assert(x == x)); }

@subsection{@tt{possible}}

@(mint-define! '("possible"))
@reach{
 possible( claim, [msg] ) }

@index{possible} A @tech{possibility assertion} which is only @tech{valid} if it is possible for @reachin{claim} to evaluate to @reachin{true} with @tech{honest} @tech{frontends} and @tech{participants}.
It accepts an optional bytes argument, which is included in any reported violation.

@subsection{@tt{digest}}

@(mint-define! '("digest"))
@reach{
 digest( arg_0, ..., arg_n ) }

The @tech{digest} primitive performs a @link["https://en.wikipedia.org/wiki/Cryptographic_hash_function"]{cryptographic hash} of the binary encoding of the given arguments.
This returns a @reachin{Digest} value.
The exact algorithm used depends on the @tech{connector}.

@subsection{@tt{balance}}

@(mint-define! '("balance"))
@reach{
 balance();
 balance(gil); }

The @deftech{balance} primitive returns the balance of the @tech{contract} @tech{account} for the @|DApp|.
It takes an optional @tech{non-network token} value, in which case it returns the balance of the given token.

@subsection{@tt{lastConsensusTime}}

@(mint-define! '("lastConsensusTime"))
@reach{
 lastConsensusTime() }

The @deftech{lastConsensusTime} primitive returns the @tech{time} of the last @tech{publication} of the @|DApp|.
This may not be available if there was no such previous publication, such as at the beginning of an application where @reachin{deployMode} is @reachin{'firstMsg'}.

@margin-note{Why is there no @tt{thisConsensusTime}?
Some networks do not support observing the time of a consensus operation until after it has finalized.
This aides scalability, because it increases the number of times when an operation could be finalized.}

@subsection{@tt{makeDeadline}}

@(mint-define! '("makeDeadline"))
@reach{
  const [ timeRemaining, keepGoing ] = makeDeadline(10); }

@index{makeDeadline} @reachin{makeDeadline(deadline)} takes an @reachin{UInt} as an argument and returns a pair of functions
that can be used for dealing with absolute deadlines. It internally determines the end time based off of the deadline
and the last consensus time—at the time of calling @reachin{makeDeadline}. @tt{timeRemaining} will calculate the difference
between the end time and the current last consensus time. @tt{keepGoing} determines whether the current last consensus time
is less than the end time. It is typical to use the two fields for the @tt{while} and @tt{timeout} field of a @reachin{parallelReduce}
expression. For example:

@reach{
  const [ timeRemaining, keepGoing ] = makeDeadline(10);
  const _ = parallelReduce(...)
    .invariant(...)
    .while( keepGoing() )
    .case(...)
    .timeout( timeRemaining(), () => { ... }) }

This pattern is so common that it can be abbreviated as @reachin{.timeRemaining}.


@subsection{@tt{implies}}

@(mint-define! '("implies"))
@reach{
 implies( x, y ) }

@index{implies} Returns @reachin{true} if @reachin{x} is @reachin{false} or @reachin{y} is @reachin{true}.

@subsection{@tt{ensure}}

@(mint-define! '("ensure"))
@reach{
 ensure( pred, x ) }

@index{ensure} Makes a @tech{static assertion} that @reachin{pred(x)} is @reachin{true} and returns @reachin{x}.

@subsection{@tt{hasRandom}}

@(mint-define! '("hasRandom"))
@reach{
 hasRandom }

@index{hasRandom} A @tech{participant interact interface} which specifies @litchar{random} as a function that takes no arguments and returns an unsigned integer of @tech{bit width} bits. Reach provides a default frontend implementation via @tech{hasRandom (Frontend)}.


@subsection{@tt{compose}}

@(mint-define! '("compose"))
@reach{
 compose(f, g) }

@index{compose} Creates a new function that applies it's argument to @tt{g}, then pipes the result to the function @tt{f}.
The argument type of @tt{f} must be the return type of @tt{g}.


@subsection{@tt{sqrt}}

@(mint-define! '("sqrt"))
@reach{
  sqrt(81, 10) }

@index{sqrt} Calculates an approximate square root of the first argument. This method utilizes
the @link["https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Babylonian_method"]{Babylonian Method} for computing
the square root. The second argument must be an @reachin{UInt} whose value is known at compile time, which represents the number
of iterations the algorithm should perform.

For reference, when performing @reachin{5} iterations, the algorithm can reliably calculate the square root
up to @tt{32} squared, or @tt{1,024}. When performing @reachin{10} iterations, the algorithm can reliably calculate the
square root up to @tt{580} squared, or @tt{336,400}.

@subsection{@tt{pow}}

@(mint-define! '("pow"))
@reach{
  pow (2, 40, 10) // => 1,099,511,627,776 }

@index{pow} @reachin{pow(base, power, precision)} Calculates the approximate value of raising base to power.
The third argument must be an @reachin{UInt} whose value is known at compile time, which represents the number
of iterations the algorithm should perform. For reference, @tt{6} iterations provides enough accuracy to calculate
up to @tt{2^64 - 1}, so the largest power it can compute is @tt{63}.

@subsection{Signed Integers}

The standard library provides abstractions for dealing with signed integers. The following definitions
are used to represent @reachin{Int}s:

@margin-note{
  @tt{Int} is represented as an object, as opposed to a scalar value, because some platforms
  that Reach targets do not provide native support for signed integers. }

@(mint-define! '("Int") '("Pos") '("Neg"))
@reach{
  const Int = { sign: Bool, i: UInt };
  const Pos = true;
  const Neg = false;  }

@index{int} @reachin{int(Bool, UInt)} is shorthand for defining an @reachin{Int} record. You may also
use the @reachin{+} and @reachin{-} unary operators to declare integers instead of @reachin{UInt}s.

@(mint-define! '("int"))
@reach{
  int(Pos, 4); // represents 4
  int(Neg, 4); // represents -4
  -4;          // represents -4
  +4;          // represents 4 : Int
   4;          // represents 4 : UInt }

@index{iadd} @reachin{iadd(x, y)} adds the @reachin{Int} @tt{x} and the @reachin{Int} @tt{y}.

@index{isub} @reachin{isub(x, y)} subtracts the @reachin{Int} @tt{y} from the @reachin{Int} @tt{x}.

@index{imul} @reachin{imul(x, y)} multiplies the @reachin{Int} @tt{x} and the @reachin{Int} @tt{y}.

@index{idiv} @reachin{idiv(x, y)} divides the @reachin{Int} @tt{x} by the @reachin{Int} @tt{y}.

@index{imod} @reachin{imod(x, y)} finds the remainder of dividing the @reachin{Int} @tt{x} by the @reachin{Int} @tt{y}.

@index{ilt} @reachin{ilt(x, y)} determines whether @tt{x} is less than @tt{y}.

@index{ile} @reachin{ile(x, y)} determines whether @tt{x} is less than or equal to @tt{y}.

@index{igt} @reachin{igt(x, y)} determines whether @tt{x} is greather than @tt{y}.

@index{ige} @reachin{ige(x, y)} determines whether @tt{x} is greater than or equal to @tt{y}.

@index{ieq} @reachin{ieq(x, y)} determines whether @tt{x} is equal to @tt{y}.

@index{ine} @reachin{ine(x, y)} determines whether @tt{x} is not equal to @tt{y}.

@index{imax} @reachin{imax(x, y)} returns the larger of two @reachin{Int}s.

@index{abs} @reachin{abs(i)} returns the absolute value of an @reachin{Int}. The return value is of type @reachin{UInt}.

@subsection{Fixed-Point Numbers}

@reachin{FixedPoint} is defined by

@(mint-define! '("FixedPoint"))
@reach{
  export const FixedPoint = Object({ sign: bool, i: Object({ scale: UInt, i: UInt }) }); }

@reachin{FixedPoint} can be used to represent numbers with a fixed number of digits after the decimal point.
They are handy for representing fractional values, especially in base 10. The value of a fixed point number is determined
by dividing the underlying integer value, @tt{i}, by its scale factor, @tt{scale}. For example, we could
represent the value @reachin{1.234} with @reachin{{ sign: Pos, i: { scale: 1000, i : 1234 } }} or @reachin{fx(1000)(Pos, 1234)}.
Alternatively, Reach provides syntactic sugar for defining @reachin{FixedPoint} numbers. One can simply write
@reachin{1.234}, which will assume the value is in base 10. A scale factor of @tt{1000} correlates to 3 decimal
places of precision. Similarly, a scale factor of @tt{100} would have 2 decimal places of precision.

@(mint-define! '("fx"))
@reach{
  const scale = 10;
  const i = 56;
  fx(scale)(Neg, i); // represents - 5.6 }

@index{fx} @reachin{fx(scale)(i)} will return a function that can be used to
instantiate fixed point numbers with a particular scale factor.

@(mint-define! '("fxint"))
@reach{
  const i = 4;
  fxint(-i); // represents - 4.0 }

@index{fxint} @reachin{fxint(Int)} will cast the @reachin{Int} arg as a @reachin{FixedPoint}
number with a @tt{scale} of 1.

@(mint-define! '("fxrescale"))
@reach{
  const x = fx(1000)(Pos, 1234); // x = 1.234
  fxrescale(x, 100);    // => 1.23 }

@index{fxrescale} @reachin{fxrescale(x, scale)} will convert a fixed point number from using
one scale to another. This operation can result in loss of precision, as demonstrated in the above example.

@(mint-define! '("fxunify"))
@reach{
  const x = fx(1000)(Pos, 824345); // x = 824.345
  const y = 45.67;
  fxunify(x, y);    // => [ 1000, 824.345, 45.670 ] }

@index{fxunify} @reachin{fxunify(x, y)} will convert the fixed point numbers
to use the same scale. The larger scale of the two arguments will be chosen. The function will return a @tt{3-tuple} consisting
of the common scale and the newly scaled values.

@index{fxadd} @reachin{fxadd(x, y)} adds two fixed point numbers.

@index{fxsub} @reachin{fxsub(x, y)} subtracts two fixed point numbers.

@index{fxmul} @reachin{fxmul(x, y)} multiplies two fixed point numbers.

@(mint-define! '("fxdiv"))
@reach{
  fxdiv(34.56, 1.234, 10)     // => 28
  fxdiv(34.56, 1.234, 100000) // => 28.0064 }

@index{fxdiv} @reachin{fxdiv(x, y, scale_factor)} divides two fixed point numbers. The numerator, @tt{x},
will be multiplied by the scale factor to provide a more precise answer. For example,

@index{fxmod} @reachin{fxmod(x, y)} finds the remainder of dividing @tt{x} by @tt{y}.

@index{fxfloor} @reachin{fxfloor(x)} returns the greatest integer not greater than @tt{x}.

@index{fxsqrt} @reachin{fxsqrt(x, k)} approximates the sqrt of the fixed number, @tt{x}, using
@tt{k} iterations of the @reachin{sqrt} algorithm.

@(mint-define! '("fxpow"))
@reachin{
  const base  = 2.0;
  const power = 0.33;
  fxpow(base, power, 10, 1000);    // 1.260
  fxpow(base, power, 10, 10000);   // 1.2599
  fxpow(base, power, 10, 1000000); // 1.259921 }

@index{fxpow} @reachin{fxpow(base, power, precision, scalePrecision)} approximates the power of the fixed number, @tt{base},
raised to the fixed point number, @tt{power}. The third argument must be an @reachin{UInt} whose value is known
at compile time, which represents the number of iterations the algorithm should perform.
The @tt{scalePrecision} argument must be a @tt{UInt} and represents the scale of the return value. Choosing a larger
@tt{scalePrecision} allows for more precision when approximating the power, as demonstrated in the example below:

@index{fxpowi} @reachin{fxpowi(base, power, precision)} approximates the power of the fixed number, @tt{base},
raised to the @reachin{Int}, @tt{power}. The third argument must be an @reachin{UInt} whose value is known
at compile time, which represents the number of iterations the algorithm should perform. For reference, @tt{6} iterations
provides enough accuracy to calculate up to @tt{2^64 - 1}, so the largest power it can compute is @tt{63}.

@(mint-define! '("fxpowui"))
@reachin{
  fxpowui(5.8, 3, 10); // 195.112 }

@index{fxpowui} @reachin{fxpowui(base, power, precision)} approximates the power of
the fixed number, @tt{base}, raised to the @reachin{UInt}, @tt{power}. The third
argument must be an @reachin{UInt} whose value is known at compile time.

@index{fxcmp} @reachin{fxcmp(op, x, y)} applies the comparison
operator to the two fixed point numbers after unifying their scales.

There are convenience methods defined for comparing fixed point numbers:

@index{fxlt} @reachin{fxlt(x, y)} tests whether @tt{x} is less than @tt{y}.

@index{fxle} @reachin{fxle(x, y)} tests whether @tt{x} is less than or equal to @tt{y}.

@index{fxgt} @reachin{fxgt(x, y)} tests whether @tt{x} is greater than @tt{y}.

@index{fxge} @reachin{fxge(x, y)} tests whether @tt{x} is greater than or equal to @tt{y}.

@index{fxeq} @reachin{fxeq(x, y)} tests whether @tt{x} is equal to @tt{y}.

@index{fxne} @reachin{fxne(x, y)} tests whether @tt{x} is not equal to @tt{y}.

@subsection{Anybody}

@(mint-define! '("Anybody"))
@reach{
  Anybody.publish(); // race(...Participants).publish()
}

@index{Anybody} Reach provides a shorthand, @reachin{Anybody}, which serves as a
@reachin{race} between all @tech{participant}s.
This shorthand can be useful for situations where
it does not matter who @reachin{publish}es, such as in a @reachin{timeout}.

@reachin{Anybody} is strictly an abbreviation of a @reachin{race} involving all of the named participants of the application.
In an application with a @tech{participant class}, this means any principal at all, because there is no restriction on which principals (i.e. addresses) may serve as a member of that class.
In an application without any @tech{participant class}es, @reachin{Anybody} instead would mean only the actual previously-bound @tech{participant}s.

@subsection{Intervals}

An @reachin{Interval} is defined by

@(mint-define! '("Interval"))
@reach{
  export const Interval = Tuple(IntervalType, Int, Int, IntervalType); }

where @reachin{IntervalType} is defined by

@(mint-define! '("IntervalType"))
@reach{
  export const [ isIntervalType, Closed, Open ] = mkEnum(2);
  export const IntervalType = Refine(UInt, isIntervalType);  }

@subsubsection{Constructors}

An interval may be constructed with its tuple notation or by function:

@reach{
  // Representing [-10, +10)
  const i1 = [Closed, -10, +10, Open];
  const i2 = interval(Closed, -10, +10, Open);
  const i3 = intervalCO(-10, +10); }

For convenience, Reach provides a number of functions for constructing intervals:

@index{interval} @reachin{interval(IntervalType, Int, Int, IntervalType)} constructs an interval where the first and second argument
represent the left endpoint and whether it's open or closed; the third and fourth argument represent the right endpoint and whether it's open or closed.

@index{intervalCC} @reachin{intervalCC(l, r)} constructs a closed interval from two endpoints of type @reachin{Int}.

@index{intervalCO} @reachin{intervalCO(l, r)} constructs a half-open interval from two endpoints of type @reachin{Int} where the left endpoint is closed and the right endpoint is open.

@index{intervalOC} @reachin{intervalOC(l, r)} constructs a half-open interval from two endpoints of type @reachin{Int} where the left endpoint is open and the right endpoint is closed.

@index{intervalOO} @reachin{intervalOO(l, r)} constructs an open interval from two endpoints of type @reachin{Int}.

@subsubsection{Accessors}

@index{leftEndpoint} @reachin{leftEndpoint(i)} will return the @reachin{Int} that represents the left endpoint of an interval.

@index{rightEndpoint} @reachin{rightEndpoint(i)} will return the @reachin{Int} that represents the right endpoint of an interval.

@subsubsection{Relational Operations}

Intervals may be compared with the following functions:

@index{intervalEq} @reachin{intervalEq(l, r)} tests whether the intervals are equal.

@index{intervalNe} @reachin{intervalNe(l, r)} tests whether the intervals are not equal.

@index{intervalLt} @reachin{intervalLt(l, r)} tests whether the left interval is less than the right interval.

@index{intervalLte} @reachin{intervalLte(l, r)} tests whether the left interval is less than or equal to the right interval.

@index{intervalGt} @reachin{intervalGt(l, r)} tests whether the left interval is greater than the right interval.

@index{intervalGte} @reachin{intervalGte(l, r)} tests whether the left interval is greater than or equal to the right interval.

@subsubsection{Arithmetic Operations}

@index{intervalAdd} @reachin{intervalAdd(l, r)} adds the two intervals.

@index{intervalSub} @reachin{intervalSub(l, r)} subtracts the two intervals.

@index{intervalMul} @reachin{intervalMul(l, r)} multiplies the two intervals.

@index{intervalDiv} @reachin{intervalDiv(l, r)} divides the two intervals.

@subsubsection{Other Operations}

@(mint-define! '("intervalIntersection"))
@reach{
  const i1 = intervalOO(+3, +11); // (+3, +11)
  const i2 = intervalCC(+7, +9);  // [+7, +9]
  intervalIntersection(i1, i2);   // [+7, +11)  }

@index{intervalIntersection} @reachin{intervalIntersection(x, y)} returns the intersection of two intervals.

@(mint-define! '("intervalUnion"))
@reach{
  const i1 = intervalOO(+3, +9);  // (+3, +9)
  const i2 = intervalCC(+7, +11); // [+7, +11]
  intervalUnion(i1, i2);          // (+3, +11]  }

@index{intervalUnion} @reachin{intervalUnion(x, y)} returns the union of two intervals.

@(mint-define! '("intervalWidth"))
@reach{
  intervalWidth(intervalCC(+4, +45)); // +41 }

@index{intervalWidth} @reachin{intervalWidth(i)} returns the width of an interval.

@(mint-define! '("intervalAbs"))
@reach{
  intervalAbs(intervalCC(+1, +10)); // +10 }

@index{intervalAbs} @reachin{intervalAbs(i)} returns the absolute value of an interval.

