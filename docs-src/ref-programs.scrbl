#lang scribble/manual
@(require "lib.rkt")

@title[#:version reach-vers #:tag "ref-programs"]{Reach Programs}

This document describes the structure and content of Reach programs,
including their syntactic forms and the Reach standard library.

@section{Source Files}

Reach @deftech{source files} are either @tech{executables} or @tech{libraries}. They are traditionally given the file extension @litchar{rsh}, e.g. @filepath{dao.rsh}.

Reach @deftech{executables} start with @reachin{'reach @|reach-short-vers| exe';} and are followed by a sequence of @tech{imports}, @tech{participant definitions}, @tech{identifier definitions}, and a @tech{main function}. The evaluation of the execution is that of the @tech{main function}.

Reach @deftech{libraries} start with @reachin{'reach @|reach-short-vers| lib';} and are followed by a sequence of @tech{imports} and @tech{identifier definitions}.

@section{Imports}

@reach{import "games-of-chance.rsh";}

When a Reach @tech{source file}, @litchar{X}, contains an @deftech{import}, written @reachin{import "LIB.rsh";}, then the path @filepath{LIB.rsh} must resolve to a file which is a valid @tech{library}. The definitions located in @filepath{LIB.rsh} are included in the set of definitions associated with @litchar{X}.

@margin-note{The path given to an @tech{import} may include @litchar{..} to specify files outside the current directory.}

@section{Participant Definitions}

@reach{const Alice = participant({_hand: uint256});}

Reach @tech{executables} may contain @deftech{participant definitions}, which are written:

@reach{
 const PARTICIPANT =
   participant( { ID_0: TYPE_0
                , ...
                , ID_n: TYPE_n} ); }

Such a definition defines a @tech{participant} named @reachin{PARTICIPANT} with the initial @tech{local state} @reachin{ID_0} through @reachin{ID_n} (which are conventionally preceded by an underscore character, i.e. @litchar{_}) each having an associated @tech{type} @reachin{TYPE_0} through @reachin{TYPE_n}.

When an @tech{executable} is compiled by a @tech{backend}, each @tech{participant} will be provided as a function parameterized over the initial @tech{local state} values. For example,

@reach{const Bob = participant({_wagerLimit: uint256});}

will produce a function, @reachin{Bob}, which accepts an argument for the maximum wager any game it participates in may charge.

@section{Types}

A @deftech{base type} is either:
@itemlist[
  @item{@reachin{address}, which denotes an @tech{account} @tech{address}; or,}
  @item{@reachin{uint256}, which denotes an unsigned integer of 256 bits; or,}
  @item{@reachin{bool}, which denotes a boolean; or,}
  @item{@reachin{bytes}, which denotes a string of bytes.} ]

A @deftech{type} is either a @tech{base type} or a statically-sized array of @tech{base type} values, written @reachin{BASE[EXPR]} where @reachin{BASE} is a @tech{base type} and @reachin{EXPR} is an @tech{expression} that statically evaluates to a natural number. For example, the following are all @tech{types}:

@reach{
  address
  bool
  bool[4]
  uint256[4 + 5]
  uint256[3 * 7 + 99] }

@section{Identifier Definitions}

An @deftech{identifier definition} is either a @tech{value definition}, @tech{enumeration}, or @tech{function definition}.

@reach{
  const DELAY = 10;
  const [ Good, Bad ] = [ 42, 43 ]; }

A @deftech{value definition} is written @reachin{const LHS = RHS;} where @reachin{LHS} is either a single identifier, e.g. @reachin{isDelicious}, or an array of identifiers, e.g. @reachin{[ bestSushi, mediumestSushi, worstSushi ]}, and @reachin{RHS} is an @tech{expression}. @reachin{RHS} must evaluate to as many @tech{values} as there are identifiers in @reachin{LHS}. Those @tech{values} are available as the corresponding identifiers in the rest of the program.

@reach{
  const isHand = Enum([ROCK, PAPER, SCISSORS]); }

An @deftech{enumeration}, written @reachin{const ENUM = Enum([OPTION_0, ..., OPTION_n]);}, defines the identifiers @reachin{OPTION_0} through @reachin{OPTION_n} as unique natural numbers and @reachin{ENUM} as a function which accepts one numeric argument and returns @reachin{true} if and only if it is one of these natural numbers.

@reach{
  function randomBool() {
    return (random() % 2) == 0; } }

A @deftech{function definition}, written @reachin{function FUN(ARG_0, ..., ARG_n) BLOCK;}, defines @reachin{FUN} as a function which parameterizes its @deftech{function body}, the @tech{block} @reachin{BLOCK}, over the identifiers @reachin{ARG_0} through @reachin{ARG_n}.

@reach{
  function main() {
    return 42; } }

A @deftech{main function} is a @tech{function definition} with the name @reachin{main} and no arguments. The evaluation of the @tech{main function} is the same as its @tech{function body}.

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

There are a large variety of different @deftech{statements} in Reach programs. Each affects the meaning of the subsequent @tech{statements}, which is called its @deftech{tail}. For example, if @reachin{{X; Y; Z;}} is a @tech{block}, then @reachin{X}'s @tech{tail} is @reachin{{Y; Z;}} and @reachin{Y}'s @tech{tail} is @reachin{{Z;}}.

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

is invalid, because the first @reachin{return}'s @tech{tail} is not empty.

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

XXX only

XXX publish + pay + timeout

XXX commit

XXX while

XXX continue

@section{Expressions}

XXX

@section{Standard Library}

XXX

