#lang scribble/manual
@(require scribble/bnf
          "lib.rkt")
@(mint-scope 'rsh)

@title[#:version reach-vers #:tag "ref-programs-local"]{Local Steps}

A Reach @tech{local step} occurs in the body of @reachin{only} or @reachin{each} statements.
It represents the actions taken by a single participant in an application.

@section[#:tag "ref-programs-local-stmts"]{Statements}

Any statements valid for a @seclink["ref-programs-compute-stmts"]{computation} are valid for a local step.

@section[#:tag "ref-programs-local-exprs"]{Expressions}

Any expressions valid for a @seclink["ref-programs-compute-exprs"]{computation} are valid for a local step.
However, some additional expressions are allowed.

@subsection[#:tag "ref-programs-local-this"]{@tt{this}}

Inside of a @tech{local step}, @reachin{this} refers to the participant performing the step.
This is useful when the @tech{local step} was initiated by an @reachin{each} expression.

@subsection{@tt{interact}}

@(mint-define! '("interact"))
@reach{
 interact.amount
 interact.notify(handA, handB)
 interact.chooseAmount(heap1, heap2) }

An @deftech{interaction expression}, written @reachin{interact.METHOD(EXPR_0, ..., EXPR_n)}, where @reachin{METHOD} is an identifier bound in the @tech{participant interact interface} to a function type, and @reachin{EXPR_0} through @reachin{EXPR_n} are @tech{expressions} that evaluates to the result of an @tech{interact}ion with a @tech{frontend} that receives the evaluation of the @reachin{n} @tech{expressions} and sends a @tech{value}.

An @tech{interaction expression} may also be written @reachin{interact.KEY}, where @reachin{KEY} is bound in the @tech{participant interact interface} to a non-function type.

An @tech{interaction expression} may only occur in a @tech{local step}.

@subsection{@tt{assume}}

@(mint-define! '("assume"))
@reach{
 assume( claim, [msg] ) }

@index{assume} An @tech{assumption} where @reachin{claim} evaluates to @reachin{true} with @tech{honest} @tech{frontends}.
This may only appear in a @tech{local step}.
It accepts an optional bytes argument, which is included in any reported violation.

@subsection{@tt{fail}}

@(mint-define! '("fail"))
@reach{
 fail() }

@index{fail} is a convenience method equivalent to @reachin{assume(false)}. This may only appear in a @tech{local step}.

@subsection{@tt{declassify}}

@(mint-define! '("declassify"))
@reach{
 declassify( arg ) }

The @deftech{declassify} primitive performs a @tech{declassification} of the given argument.

@subsection{@tt{makeCommitment}}

@(mint-define! '("makeCommitment"))
@reach{
 makeCommitment( interact, x ) }

@index{makeCommitment} Returns two values, @reachin{[ commitment, salt ]}, where @reachin{salt} is the result of calling @reachin{interact.random()}, and
@reachin{commitment} is the @tech{digest} of @reachin{salt} and @reachin{x}.
This is used in a @tech{local step} before @reachin{checkCommitment} is used in a @tech{consensus step}.


