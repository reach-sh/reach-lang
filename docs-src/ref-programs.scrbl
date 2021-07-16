#lang scribble/manual
@(require scribble/bnf
          "lib.rkt")

@title[#:version reach-vers #:tag "ref-programs" #:style 'toc]{Programs}

This document describes the structure and content of Reach @deftech{programs}, including
their syntactic forms,
the standard library,
and the standards of @tech{valid} programs.

@margin-note{Get language support for Reach in your editor by visiting @seclink["guide-editor-support"].}

The rest of this section is structured according to the contexts of the different parts of a Reach program, as follows:

@itemlist[

@item{@Secref["ref-programs-valid"] describes what is meant by the term @tech{valid} in Reach.}

@item{@Secref["ref-programs-module"] describes the top-level structure of Reach @tech{module}.}

@item{@Secref["ref-programs-appinit"] describes the structure of Reach @tech{application initialization}.}

@item{@Secref["ref-programs-step"] describes the structure of Reach @tech{steps}.}

@item{@Secref["ref-programs-local"] describes the structure of Reach @tech{local steps}.}

@item{@Secref["ref-programs-consensus"] describes the structure of Reach @tech{consensus steps}.}

@item{@Secref["ref-programs-compute"] describes the common structure of Reach computations shared by all contexts.}

]

@Figure-ref["fig:app-steps"] shows the relationship between the modes of a Reach application.

@figure["fig:app-steps" @elem{The modes of a Reach application}
  ]{@image["images/reference/StepDiagram.png" #:style "fig"]}

@section[#:tag "ref-programs-valid"]{Validity and other concepts}

Reach imposes further restrictions on syntactically well-formed programs.
These restrictions are described throughout this manual using the term @deftech{valid} to refer to constructions that obey the restrictions,
and the term @deftech{invalid} to refer to constructions that do not obey them.

It is always @tech{invalid} to use a @tech{value} with an operation for which it is undefined.
For example, @reachin{1 + true} is @tech{invalid}.
In other words, Reach enforces a static type discipline.

@subsection{Security levels and scope}

The text of Reach program is @tech{public} knowledge to all @tech{participants}.
However, any value that comes from an @tech{interaction expression} is a @deftech{secret} which only that participant knows.
Furthermore, any values derived from @tech{secret} values are also @tech{secret}.
A value, X, is considered derived from another, Y, if the value of Y is provided to a primitive operation to arrive at X, or if Y is used as part of a conditional that influences the definition of X.
@tech{Secrets} can only be made @tech{public} by using the @tech{declassify} primitive.

When @tech{secret} values are bound to an @tech{identifier}
within a @tech{local step},
the identifier name MUST be prefixed by an underscore (@reachin{_}).

When @tech{public} values are bound to an @tech{identifier},
regardless of context,
the identifier name MUST NOT be prefixed by an underscore (@reachin{_}).

Consequently, identifiers which appear inside of a
@tech{function definition} or @tech{arrow expression}
MAY be prefixed by an underscore.
This will cause a compiler error if any value bound to that
identifier is public.

@subsection{Domination}

A term Y is said to be "@deftech{dominated}" by a term X if all paths in the control-flow graph of the application from the root to Y pass through X.
In most cases, this corresponds to "X appears above Y at the same or lower level of indentation" in the program source code.

For example, in the following program:

@reach{
 f();
 if ( p() ) {
  g();
 } else {
  h();
 }
 m();
}

@reachin{f} dominates @reachin{p}, @reachin{g}, @reachin{h}, and @reachin{m}.
But no other term dominates any other term.
In particular, @reachin{g} does not dominate @reachin{m} because it is possible to reach @reachin{m} without going through @reachin{g}, such as when @reachin{p()} is false.

@include-section["ref-programs-module.scrbl"]
@include-section["ref-programs-appinit.scrbl"]
@include-section["ref-programs-step.scrbl"]
@include-section["ref-programs-local.scrbl"]
@include-section["ref-programs-consensus.scrbl"]
@include-section["ref-programs-compute.scrbl"]
