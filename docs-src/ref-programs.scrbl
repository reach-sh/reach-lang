#lang scribble/manual
@(require "lib.rkt")

@title[#:version reach-vers #:tag "ref-programs"]{Reach Programs}

This document describes the structure and content of Reach programs,
including their syntactic forms, the Reach standard library, and the standards of @tech{valid} programs.

@section{Validity}

Reach imposes further restrictions on syntactically well-formed programs. These restrictions are described throughout this manual using the term @deftech{valid} to refer to constructions that obey the restrictions and @deftech{invalid} to refer to constructions that do not obey them.

It is always @tech{invalid} to use a @tech{value} with an operation for which it is undefined. For example, @reachin{1 + true} is @tech{invalid}. In other words, Reach enforces a static type discipline.

@section{Source Files}

Reach @deftech{source files} are either @tech{executables} or @tech{libraries}. They are traditionally given the file extension @litchar{rsh}, e.g. @filepath{dao.rsh}.

Reach @deftech{executables} start with @reachin{'reach @|reach-short-vers| exe';} and are followed by a sequence of @tech{imports}, @tech{participant definitions}, @tech{identifier definitions}, and a @tech{main function}. The evaluation of the execution is that of the @tech{main function}.

Reach @deftech{libraries} start with @reachin{'reach @|reach-short-vers| lib';} and are followed by a sequence of @tech{imports} and @tech{identifier definitions}.

@section{Imports}

@reach{import "games-of-chance.rsh";}

When a Reach @tech{source file}, @litchar{X}, contains an @deftech{import}, written @reachin{import "LIB.rsh";}, then the path @filepath{LIB.rsh} must resolve to a file which is a @tech{library}. The definitions located in @filepath{LIB.rsh} are included in the set of definitions associated with @litchar{X}.

@margin-note{The path given to an @tech{import} may include @litchar{..} to specify files outside the current directory.}

@section{Participant Definitions}

@reach{const Alice = participant({_hand: uint256});}

Reach @tech{executables} may contain @deftech{participant definitions}, which are written:

@reach{
 const PARTICIPANT =
   participant( { ID_0: TYPE_0
                , ...
                , ID_n: TYPE_n } ); }

Such a definition defines a @tech{participant} named @reachin{PARTICIPANT} with the initial @tech{local state} @reachin{ID_0} through @reachin{ID_n} (which are conventionally preceded by an underscore character, i.e. @litchar{_}) each having an associated @tech{type} @reachin{TYPE_0} through @reachin{TYPE_n}.

When an @tech{executable} is compiled by a @tech{backend}, each @tech{participant} will be provided as a function abstracted over the initial @tech{local state} values. For example,

@reach{const Bob = participant({_wagerLimit: uint256});}

will produce a function, @reachin{Bob}, which accepts an argument for the maximum wager any game it participates in may charge.

@section{Types}

A @deftech{base type} is either:
@itemlist[
  @item{@reachin{address}, which denotes an @tech{account} @tech{address}; or,}
  @item{@reachin{uint256}, which denotes an unsigned integer of 256 bits; or,}
  @item{@reachin{bool}, which denotes a boolean; or,}
  @item{@reachin{bytes}, which denotes a string of bytes.} ]

A @deftech{type} is either a @tech{base type} or a statically-sized homogeneous array of @tech{base type} values, written @reachin{BASE[EXPR]} where @reachin{BASE} is a @tech{base type} and @reachin{EXPR} is an @tech{expression} that statically evaluates to a natural number. For example, the following are all @tech{types}:

@reach{
  address
  bool
  bool[4]
  uint256[4 + 5]
  uint256[3 * 7 + 99] }

@section{Identifier Definitions}

An @deftech{identifier definition} is either a @tech{value definition}, @tech{enumeration}, or @tech{function definition}. Each of these bind identifiers.

@(hrule)
@reach{
  const DELAY = 10;
  const [ Good, Bad ] = [ 42, 43 ]; }

A @deftech{value definition} is written @reachin{const LHS = RHS;} where @reachin{LHS} is either a single identifier, e.g. @reachin{isDelicious}, or an array of identifiers, e.g. @reachin{[ bestSushi, mediumestSushi, worstSushi ]}, and @reachin{RHS} is an @tech{expression}. @reachin{RHS} must evaluate to as many @tech{values} as there are identifiers in @reachin{LHS}. Those @tech{values} are available as the corresponding identifiers in the rest of the program.

@(hrule)
@reach{
  const isHand = Enum([ROCK, PAPER, SCISSORS]); }

An @deftech{enumeration}, written @reachin{const ENUM = Enum([OPTION_0, ..., OPTION_n]);}, defines the identifiers @reachin{OPTION_0} through @reachin{OPTION_n} as unique natural numbers and @reachin{ENUM} as a function which abstracts over one numeric argument and returns @reachin{true} if and only if it is one of these natural numbers.

@(hrule)
@reach{
  function randomBool() {
    return (random() % 2) == 0; } }

A @deftech{function definition}, written @reachin{function FUN(ARG_0, ..., ARG_n) BLOCK;}, defines @reachin{FUN} as a function which abstracts its @deftech{function body}, the @tech{block} @reachin{BLOCK}, over the identifiers @reachin{ARG_0} through @reachin{ARG_n}.

@(hrule)
@reach{
  function main() {
    return 42; } }

A @deftech{main function} is a @tech{function definition} with the name @reachin{main} and no arguments. The evaluation of the @tech{main function} is the same as its @tech{function body}.

@(hrule)

All identifiers in Reach programs must be unbound at the position of the program where they are bound, i.e., it is @tech{invalid} to shadow identifiers with new definitions. For example,

@reach{
 const x = 3;
 const x = 4; }

is @tech{invalid}. This restriction is independent of whether a binding is only known to a single @tech{participant}. For example,

@reach{
 Alice.only(() => {
   const x = 3; });
 Bob.only(() => {
   const x = 3; }); }

is @tech{invalid}.

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

@section{Statements}

There are a large variety of different @deftech{statements} in Reach programs.

Each @tech{statement} affects the meaning of the subsequent @tech{statements}, which is called its @deftech{tail}. For example, if @reachin{{X; Y; Z;}} is a @tech{block}, then @reachin{X}'s @tech{tail} is @reachin{{Y; Z;}} and @reachin{Y}'s @tech{tail} is @reachin{{Z;}}.

Distinct from @tech{tails} are @deftech{continuations} which include everything after the @tech{statement}. For example, in @reachin{{{X; Y;}; Z;}}, @reachin{X}'s @tech{tail} is just @reachin{Y}, but its @tech{continuation} is @reachin{{Y;}; Z;}.

@tech{Tails} are statically apparent from the structure of the program source code, while @tech{continuations} are influenced by function calls.

The remainder of this section enumerates each kind of @tech{statement}.

@subsection{Return statements}

@reach{
 return 17;
 return 3 + 4;
 return f(2, false); }

A @deftech{return statement}, written @reachin{return EXPR;}, where @reachin{EXPR} is an @tech{expression} evaluates to the same @tech{value} as @reachin{EXPR}. It must have an empty @tech{tail}. For example,

@reach{
 { return 1;
   return 2; } }

is @tech{invalid}, because the first @reachin{return}'s @tech{tail} is not empty.

@subsection{Value definition statements}

If a @tech{value definition} occurs in a @tech{statement} position, then the identifiers are bound in the @tech{statement}'s @tech{tail}. For example,

@reach{
 const [ x, y ] = [ 3, 4 ];
 const z = x + y;
 return z; }

evaluates to @reachin{7}.

@subsection{Conditional statements}

@reach{
 if ( 1 + 2 < 3 ) {
   return "Yes!";
 } else {
   return "No, waaah!"; } }

A @deftech{conditional statement}, written @reachin{if COND TRUE else FALSE}, where @reachin{COND} is an @tech{expression} which evaluates to a boolean and @reachin{TRUE} and @reachin{FALSE} as @tech{statements} (potentially @tech{block statements}), selects between the @reachin{TRUE} @tech{statement} and @reachin{FALSE} @tech{statement} based on whether @reachin{COND} evaluates to @reachin{true}.

Both @reachin{TRUE} and @reachin{FALSE} have empty @tech{tails}, i.e. the @tech{tail} of the @tech{conditional statement} is not propagated. For example,

@reach{
 if ( x < y ) {
   const z = 3;
   return; }
 else {
   const z = 4;
   return; }
 return z; }

is erroneous, because the identifier @reachin{z} is not bound outside the @tech{conditional statement}.

A @tech{conditional statement} may only include a @tech{consensus transfer} in @reachin{TRUE} or @reachin{FALSE} if it is within a @tech{consensus step}.

@subsection{Block statements}

A @deftech{block statement} is when a @tech{block} occurs in a @tech{statement} position, then it establishes a local, separate scope for the definitions of identifiers within that @tech{block}. In other words, the @tech{block} is evaluated for effect, but the @tech{tail} of the @tech{statements} within the @tech{block} are isolated from the surrounding @tech{tail}. For example,

@reach{
 const x = 4;
 return x; }

evaluates to @reachin{4}, but

@reach{
 { const x = 4;
   return; }
 return x; }

is erroneous, because the identifier @reachin{x} is not bound outside the @tech{block statement}.

@subsection{Expression statements}

@reach{
 4;
 f(2, true); }

An @tech{expression}, @reachin{E}, in a @tech{statement} position is equivalent to the @tech{block statement} @reachin{{ return E; }}.

@subsection{Local step}

@reach{
 Alice.only(() => {
   const pretzel = random(); }); }

A @tech{local step} statement is written @reachin{PART.only(() => BLOCK)}, where @reachin{PART} is a @tech{participant} identifier and @reachin{BLOCK} is a @tech{block}. Any bindings defined within the @tech{block} of a @tech{local step} are available in the @tech{statement}'s @tech{tail} as new @tech{local state}. For example,

@reach{
 Alice.only(() => {
   const x = 3; });
 Alice.only(() => {
   const y = x + 1; }); }

is a @tech{valid} program where @reachin{Alice}'s @tech{local state} includes the @tech{private} values @reachin{x} (bound to @reachin{3}) and @reachin{y} (bound to @reachin{4}). However, such bindings are @emph{not} @tech{consensus state}, so they are purely @tech{local state}. For example,

@reach{
 Alice.only(() => {
   const x = 3; });
 Bob.only(() => {
   const y = x + 1; }); }

is an @tech{invalid} program, because @reachin{Bob} does not know @reachin{x}.

@subsection{Consensus transfers}

@reach{
 Alice.publish(wagerAmount)
      .pay(wagerAmount)
      .timeout(DELAY, () => {
        Bob.publish();
        commit();
        return false; }); }
@reach{
 Alice.publish(wagerAmount)
      .pay(wagerAmount)
      .timeout(DELAY, closeTo(Bob, false)); }

A @tech{consensus transfer}, written @reachin{PART.publish(ID_0, ..., ID_n).pay(PAY_EXPR).timeout(DELAY_EXPR, TIMEOUT_EXPR)}, where @reachin{PART} is a @tech{participant} identifier, @reachin{ID_0} through @reachin{ID_n} are identifiers for @tech{public} @tech{local state}, @reachin{PAY_EXPR} is a @tech{public} @tech{expression} evaluating to an amount of @tech{network tokens}, @reachin{DELAY_EXPR} is a @tech{expression} that depends on only @tech{consensus state} that evaluates to a natural number and @reachin{TIMEOUT_EXPR} is an @tech{expression} that evaluates to a @tech{timeout} represent by a zero-arity function, which will be executed after @reachin{DELAY_EXPR} units of @tech{time} have passed from the end of the last @tech{consensus step} without @reachin{PART} executing this @tech{consensus transfer}. The @tech{tail} of a @tech{consensus transfer} @tech{statement} is a @tech{consensus step}, which is finalized with a @tech{commit statement}.

The @reachin{publish} component exclusive-or the @reachin{pay} component may be omitted, if either there is no @tech{publication} or no @tech{transfer} of @tech{network tokens} to accompany this @tech{consensus transfer}. The @reachin{timeout} component may always be omitted. For example, the following are all @tech{valid}:

@reach{
 Alice.publish(coinFlip);

 Alice.pay(penaltyAmount);

 Alice.publish(coinFlip)
      .timeout(DELAY, closeTo(Bob, false));

 Alice.pay(penaltyAmount)
      .timeout(DELAY, () => {
        Bob.publish();
        commit();
        return false; }); }

@subsection{Commit statements}

@reach{
 commit(); }

A @deftech{commit statement}, written @reachin{commit();}, @tech{commits} to @tech{statement}'s @tech{continuation} as the next @tech{step} of the @DApp computation. In other words, it ends the current @tech{consensus step} and allows more @tech{local steps}.

@subsection{While statements}

@reach{
 var [ heap1, heap2 ] = [ 21, 21 ];
 invariant(balance() = 2 * wagerAmount);
 while ( heap1 + heap2 > 0 ) {
   ....
   [ heap1, heap2 ] = [ heap1 - 1, heap2 ];
   continue; } }

A @deftech{while statement} may occur within a @tech{consensus step} and is written:

@reach{
 var [ VAR_0, ..., VAR_n ] = INIT_EXPR;
 invariant(INVARIANT_EXPR);
 while( COND_EXPR ) BLOCK }

where the identifiers @reachin{VAR_0} through @reachin{VAR_n} are bound to the result of the @tech{expression} @reachin{INIT_EXPR}, which must evaluate to @reachin{n} values, and @reachin{INVARIANT_EXPR} is an @tech{expression}, called the @deftech{loop invariant}, that must be true before and after every execution of the @tech{block} @reachin{BLOCK}, and if @reachin{COND_EXPR} is true, then the @tech{block} executes, and if not, then the loop terminates and control transfers to the @tech{continuation} of the @tech{while statement}. The identifiers @reachin{VAR_0} through @reachin{VAR_n} are bound within @reachin{INVARIANT_EXPR}, @reachin{COND_EXPR}, @reachin{BLOCK}, and the @tech{tail} of the @tech{while statement}.

@subsection{Continue statements}

@reach{
 [ heap1, heap2 ] = [ heap1 - 1, heap2 ];
 continue; }

A @deftech{continue statement} may occur within a @tech{while statement}'s @tech{block} and is written:

@reach{
 [ VAR_0, ..., VAR_n ] = UPDATE_EXPR;
 continue; }

where the identifiers @reachin{VAR_0} through @reachin{VAR_n} are the variables bound by the nearest enclosing @tech{while statement} and @reachin{UPDATE_EXPR} is an @tech{expression} which evaluates to @reachin{n} values. The @tech{tail} of a @tech{continue statement} must be empty.

@section{Expressions}

There are a large variety of different @deftech{expressions} in Reach programs.

The remainder of this section enumerates each kind of @tech{expression}.

@(hrule)
@reach{
 X
 Y
 Z }

An identifier, written @reachin{ID}, is an @tech{expression} that evaluates to the value of the identifier is bound to.

@(hrule)
@reach{
 10
 0xdeadbeef
 007
 true
 false
 "reality bytes"
 'it just does' }

A @deftech{literal value}, written @reachin{VALUE}, is an @tech{expression} that evaluates to the given @tech{value}. Numbers may be written in decimal, hexadecimal, or octal. Booleans may be written as @reachin{true} or @reachin{false}. Byte strings may be written between double or single quotes (with no distinction between the different styles) and use the same escaping rules as JavaScript.

@(hrule)
@reach{
 ! a
 - a}

A @deftech{unary expression}, written @reachin{UNAOP EXPR_rhs}, where @reachin{EXPR_rhs} is an @tech{expression} and @reachin{UNAOP} is one of the @deftech{unary operators}: @litchar{! -}. @margin-note{Since all numbers are non-negative in Reach, the @reachin{-} iunary operator is useless.} It is @tech{invalid} to use unary operations on the wrong types of @tech{values}.

@(hrule)
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

A @deftech{binary expression}, written @reachin{EXPR_lhs BINOP EXPR_rhs}, where @reachin{EXPR_lhs} and @reachin{EXPR_rhs} are @tech{expressions} and @reachin{BINOP} is one of the @deftech{binary operators}: @litchar{&& || + - * / % | & ^ << >> == != === !== > >= <= <}. The operators @reachin{==} and @reachin{!=} operate on numbers, while the operators @reachin{===} and @reachin{!==} operate on byte strings. Numeric operations, like @reachin{+} and @reachin{>}, only operate on numbers. Since all numbers in Reach are integers, operations like @reachin{/} truncate their result. Boolean operations, like @reachin{&&}, only operate on booleans. It is @tech{invalid} to use binary operations on the wrong types of @tech{values}.

@(hrule)
@reach{
 (a + b) - c }

An @tech{expression} may be parenthesized, as in @reachin{(EXPR)}.

@(hrule)
@reach{
 [ ]
 [ 1, 2 + 3, 4 * 5 ] }

A @deftech{multiple value expression}, written @reachin{[ EXPR_0, ..., EXPR_n ]}, is an @tech{expression} which evaluates to @reachin{n} values, where @reachin{EXPR_0} through @reachin{EXPR_n} are @tech{expressions} that evaluate to one value.

@(hrule)
@reach{
 map[3] }

An @deftech{array reference}, written @reachin{ARRAY_EXPR[IDX_EXPR]}, where @reachin{ARRAY_EXPR} is an @tech{expression} that evaluates to statically sized array and @reachin{IDX_EXPR} is an @tech{expression} that evaluates to a natural number which is less than the size of the array, selects the given element of the array.

@(hrule)
@reach{
 choosesFirst ? [ heap1 - amount, heap2 ] : [ heap1, heap2 - amount ] }

A @deftech{conditional expression}, written @reachin{COND_E ? TRUE_E : FALSE_E}, where @reachin{COND_E}, @reachin{TRUE_E}, and @reachin{FALSE_E} are @tech{expressions}, selects between the @tech{values} which @reachin{TRUE_E} and @reachin{FALSE_E} evaluate to based on whether @reachin{COND_E} evaluates to @reachin{true}.

@(hrule)
@reach{
 (() => 4)
 ((x) => x + 1)
 ((x) => { const y = x + 1;
           return y + 1; }) }

A @deftech{lambda expression}, written @reachin{(ID_0, ..., ID_n) => EXPR}, where @reachin{ID_0} through @reachin{ID_n} are identifiers and @reachin{EXPR} is an @tech{expression}, evaluates to an function which is an abstraction of @reachin{EXPR} over @reachin{n} values.

@(hrule)
@reach{
 transfer(10).to(Alice) }

A @deftech{transfer expression}, written @reachin{transfer(AMOUNT_EXPR).to(PART)}, where @reachin{AMOUNT_EXPR} is an @tech{expression} that evaluates to a natural number and @reachin{PART} is a @tech{participant} identifier, performs a @tech{transfer} of @tech{network tokens} from the @tech{contract} to the named @tech{participant}. @reachin{AMOUNT_EXPR} must evaluate to less than or equal to the balance of @tech{network tokens} in the @tech{contract} @tech{account}. A @tech{transfer expression} may only occur within a @tech{consensus step}.

@(hrule)
@reach{
 interact.notify(handA, handB);
 is(uint256, interact.chooseAmount(heap1, heap2)) }

An @deftech{interaction expression}, written @reachin{is(TYPE, interact.METHOD(EXPR_0, ..., EXPR_n))}, where @reachin{TYPE} is a @tech{type}, @reachin{METHOD} is an identifier, and @reachin{EXPR_0} through @reachin{EXPR_n} are @tech{expressions} that evaluate to one value, evaluates to the result of an @tech{interact}ion with a @tech{frontend} that receives the evaluation of the @reachin{n} @tech{expressions} and sends a @tech{value} of @tech{type} @reachin{TYPE} if it is @tech{honest}. The @reachin{is} component may be omitted, in which case it is treated as though it were @reachin{is(boolean, ....)}.

@(hrule)
@reach{
 assert( amount <= heap1 )
 step( moveA )
 digest( coinFlip )
 random()
 declassify( _coinFlip ) }

A @deftech{function application}, written @reachin{EXPR_rator(EXPR_rand_0, ..., EXPR_rand_n)}, where @reachin{EXPR_rator} and @reachin{EXPR_rand_0} through @reachin{EXPR_rand_n} are @tech{expressions} that evaluate to one value. @reachin{EXPR_rator} must evaluate to an abstraction over @reachin{n} values or a primitive of arity @reachin{n}.

@section{Standard Library}

Reach's standard library includes a few built-in primitives documented in this section.

@(hrule)
@reach{
 assert( claim ) }

@index{assert} A @tech{static assertion} which is only @tech{valid} if @reachin{claim} always evaluates to @reachin{true}. @margin-note{The Reach compiler will produce a counter-example (i.e. an assignment of the identifiers in the program to falsify the @reachin{claim}) when an @tech{invalid} @reachin{claim} is provided. It is possible to write a @reachin{claim} that actually always evaluates to @reachin{true}, but for which our current approach cannot prove always evaluates to @reachin{true}; if this is the case, Reach will fail to compile the program, reporting that its analysis is incomplete. Reach will never produce an erroneous counter-example.}

@(hrule)
@reach{
 assume( claim ) }

@index{assume} An @tech{assumption} where @reachin{claim} evaluates to @reachin{true} with @tech{honest} @tech{frontends}.

@(hrule)
@reach{
 require( claim ) }

@index{require} An @tech{requirement} where @reachin{claim} evaluates to @reachin{true} with @tech{honest} @tech{participants}.

@(hrule)
@reach{
 possible( claim ) }

@index{possible} A @tech{possibility assertion} which is only @tech{valid} if it is possible for @reachin{claim} to evaluate to @reachin{true} with @tech{honest} @tech{frontends} and @tech{participants}.

@(hrule)
@reach{
 digest( arg_0, ..., arg_n ) }

The @deftech{digest} primitive performs a @link["https://en.wikipedia.org/wiki/Cryptographic_hash_function"]{cryptographic hash} of the binary encoding of the given arguments, using the Keccak256 algorithm.

@(hrule)
@reach{
 balance() }

The @deftech{balance} primitive returns the balance of the @tech{contract} @tech{account} for the @|DApp|.

@(hrule)
@reach{
 random() }

The @deftech{random} primitive returns a random unsigned integer of 256 bits. This primitive may not be called in @tech{consensus steps}.

@(hrule)
@reach{
 declassify( arg ) }

The @deftech{declassify} primitive performs a @tech{declassification} of the given argument.

@(hrule)
@reach{
 implies( x, y ) }

@index{implies} Returns @reachin{true} if @reachin{x} is @reachin{false} or @reachin{y} is @reachin{true}.

@(hrule)
@reach{
 ensure( pred, x ) }

@index{ensure} Makes a @tech{static assertion} that @reachin{pred(x)} is @reachin{true} and returns @reachin{x}.

@(hrule)
@reach{
 precommit( x ) }

@index{precommit} Returns two values, @reachin{[ commitment, salt ]}, where @reachin{salt} is a random @reachin{uint256}, and
@reachin{commitment} is the @tech{digest} of @reachin{salt} and @reachin{x}.

@(hrule)
@reach{
 check_commit( commitment, salt, x ) }

@index{check_commit} Makes a @tech{requirement} that @reachin{commitment} is the @tech{digest} of @reachin{salt} and @reachin{x}.

@(hrule)
@reach{
 closeTo( Who, value ) }

@index{closeTo} Returns a function which accepts no arguments, has @tech{participant} @reachin{Who} make a @tech{publication}, then @tech{transfer} the @reachin{balance()} to @reachin{Who} and end the @|DApp| with the result @reachin{value}.
