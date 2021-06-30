#lang scribble/manual
@(require "lib.rkt")

@title[#:version reach-vers #:tag "ref-error-codes"]{Error Codes}

This section provides an in depth explanation of the error codes produced from
the Reach compiler.

@;{
  What is a "good" error description?
  * Intuitive explanation
  * Example errorneous program
  * How to fix to fix that program
  * Any other advice
}

@section{RE0001}

This error indicates that a program uses an invalid assignment operator.
Reach only supports assignment using the @reachin{=} operator. Any other operator,
such as @reachin{+=, *=, ...} is not allowed.

For example, the code below erroneously tries to re-assign @reachin{x} at the end of
a @reachin{while} loop:

@reach{
  x *= 2;
  continue;
}

To fix this code, you should explicitly write out the operation on the right hand
side of @reachin{=}:

@reach{
  x = x * 2;
  continue;
}

Keep in mind, that the assignment operator is a form of mutation and is only allowed
immediately before a @reachin{continue}.

@section{RE0002}

This error indicates that a program uses an invalid statement.
Reach is a strict subset of Javascript and does not accept every statement
that is valid Javascript. It may be necessary to express your program
with different constructs than you would Javascript.

For example, the code below erroneously uses a @jsin{for} loop, which is not
supported in Reach:

@reach{
  for (let i = 0; i < arr.length; i++) {
    // ...
  }
}

To fix this code, either use a @reachin{while} loop or a combination of
@reachin{Array.iota} and @reachin{Array.map/Array.forEach}:

@reach{
  Array.iota(arr.length).map((i) => {
    // ...
  });
}
