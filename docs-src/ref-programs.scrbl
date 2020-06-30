#lang scribble/manual
@(require "lib.rkt")

@title[#:version reach-vers #:tag "ref-programs"]{Reach Programs}

This document describes the structure and content of Reach programs,
including their syntactic forms and the Reach standard library.

@section{Source Files}

Reach @deftech{source files} are either @tech{executables} or @tech{libraries}. They are traditionally given the file extension @litchar{rsh}, e.g. @filepath{dao.rsh}.

Reach @deftech{executables} start with @reachin{'reach @|reach-short-vers| exe';} and are followed by a sequence of @tech{imports}, @tech{participant definitions}, @tech{identifier definitions}, and a @tech{main function}.

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

XXX

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
    return (random() % 2) == 0; }; }

A @deftech{function definition}, written @reachin{function FUN(ARG_0, ..., ARG_n) BLOCK;}, defines @reachin{FUN} as a function which parameterizes the @tech{block} @reachin{BLOCK} over the identifiers @reachin{ARG_0} through @reachin{ARG_n}.

@reach{
  function main() {
    return 42; } }

A @deftech{main function} is a @tech{function definition} with the name @reachin{main} and no arguments.

@section{Blocks}

XXX

@section{Statements}

XXX

@section{Expressions}

XXX

@section{Standard Library}

XXX

