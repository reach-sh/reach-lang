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

@section{RE0003}

This error indicates that a statement block returns a non-null value although a @reachin{null} value is expected.
The block should either use a @reachin{return null;} statement or no return statement at all.

@section{RE0004}

This error indicates that the program uses @reachin{var} incorrectly.
In Reach, @reachin{var} is only allowed immediately before a while loop and its @reachin{invariant}.

For example, this code erroneously tries to declare @reachin{x} as a mutable variable then
re-assign it at some point:

@reach{
  var x = 0;
  if (iAmLegend) {
    x = 5;
  }
}

To fix this code, you should use @reachin{const} and either create fresh variables or collapse the logic if simple enough:

@reach{
  const x = 0;
  const xPrime = iAmLegend ? 5 : x
  // or
  const x = iAmLegend ? 5 : 0;
}

@section{RE0005}

This error indicates an incorrect use of @reachin{while}. @reachin{while} must be immediately
prefaced by a @reachin{var} and @reachin{invariant} declaration.

For example, this code erroneously tries to run a continuous loop where Alice pays @reachin{1} network token
per loop:

@reach{
  while (true) {
    commit();
    Alice.pay(1);
    continue;
  }
}

Reach requires the @reachin{invariant} to reason about the @reachin{while} loop during verification. To fix
this code, add a @reachin{var} and @reachin{invariant} declaration before the loop:

@reach{
  var rounds = 0;
  invariant(balance() == rounds);
  while (true) {
    commit();
    Alice.pay(1);
    rounds = rounds + 1;
    continue;
  }
}

@section{RE0006}

@error-version["v0.0" "v0.1"]

This error indicates that @reachin{return} may not be used within the current statement block.

@section{RE0007}

This error indicates that the @reachin{timeout} branch of a statement such
as @reachin{publish, pay, fork} has been given the wrong arguments.

For example, the following code erroneously attempts to @reachin{closeTo(Bob)} in the event that
@reachin{Alice} does not publish in time:

@reach{
  Alice
    .publish()
    .timeout(5, closeTo(Bob));
}

However, the second argument of the @reachin{timeout} branch must be a thunk. To fix this code,
we wrap @reachin{closeTo(Bob)} in an @tech{arrow expression}:

@reach{
  Alice
    .publish()
    .timeout(5, () => closeTo(Bob));
}

@section{RE0008}

This error indicates that a @reachin{timeout} branch of a statement such as
@reachin{publish, pay, fork} has not been given a block of code to execute in the
event of a @reachin{timeout}.

For example, the code below erroneously provides a @reachin{timeout} delay, but does
not specify a function to run if the timeout occurs:

@reach{
  A.pay(0)
   .timeout(1);
}

To fix this code, we provide a function as a second argument to @reachin{timeout}:

@reach{
  A.pay(0)
   .timeout(1, () => {
      // ...
    });
}

@section{RE0009}

This error indicates that the @tech{pay amount} provided states the amount of
network tokens more than once.

For example, the code below erroneously provides two atomic values in the @tech{pay amount},
which are both interpreted as the amount of network tokens to pay:

@reach{
  A.pay([ amt, amt, [ amt, tok ]]);
}

To fix this code, delete one of the atomic values:

@reach{
  A.pay([ amt, [ amt, tok ]]);
}

@section{RE0010}

This error indicates that the @tech{pay amount} provided states the amount of
a specific non-network token more than once.

For example, the code below erroneously provides two tuples in the @tech{pay amount},
both of which specify the amount of the same token:

@reach{
  A.pay([ amt, [amt, tok ], [ amt, tok ] ]);
}

To fix this code, delete one of the tuples:

@reach{
  A.pay([ amt, [amt, tok ] ]);
}

@section{RE0011}

This error indicates that the @tech{pay amount} provided does not provide values of the correct
type.

For example, the code below erroneously provides a @tech{pay amount} that consists of a one element
tuple:

@reach{
  A.pay([ amt, [ amt ] ];)
}

However, a tuple in a @tech{pay amount} must specify the amount and the @reachin{Token}. To fix this code,
we add the @reachin{Token} to the tuple:

@reach{
  // tok : Token
  A.pay([ amt, [ amt, tok ] ];)
}

@section{RE0012}

This error indicates that a @tech{participant interact interface} field has a type that is not first-order.
That is, an interact function has a return type of a function, which is not allowed in Reach.

For example, the code below erroneously provides an interact function that returns another function:

@reach{
  const A = Participant('A', {
    'curriedAdd': Fun([UInt], Fun([UInt], UInt));
  });
}

the frontend may look like this:

@js{
  await Promise.all([
    backend.A(ctcA, {
      curriedAdd: (x) => (y) => x + y;
    })
  ]);
}

To fix this code, we can decouple the functions and call them sequentially. This technique requires
changes to the frontend as well since we are changing the signature:

@reach{
  const A = Participant('A', {
    'curriedAdd1': Fun([UInt], Null);
    'curriedAdd2': Fun([UInt], UInt);
  });
  // ...
  A.only(() => {
    interact.curriedAdd1(1);
    const result = declassify(interact.curriedAdd2(1));
  });
}

the frontend may look like this:

@js{
  let x_;
  await Promise.all([
    backend.A(ctcA, {
      curriedAdd1: (x) => { x_ = x; },
      curriedAdd2: (y) => { return x_ + y; }
    })
  ]);
}

@section{RE0013}

This error indicates that you provided an unrecognized option to @reachin{setOptions}.
There is most likely a typo in your code.

@section{RE0014}

This error indicates that you did not provide an acceptable value for a specific option
in @reachin{setOptions}. Please review the eligible values listed in the documentation
for @reachin{setOptions}.

@section{RE0015}

This error indicates that your @tech{participant interact interface} does not provide a @reachin{Type}
for a given field.

For example, in the erroneous code below, @reachin{x} is assigned @reachin{3} in the @tech{participant interact interface}
of @reachin{Alice}:

@reach{
  const Alice = Participant('Alice', {
    'x': 3,
  });
}

However, the interact interface specifies the type of values that will be provided at runtime. To fix this code,
either make @reachin{x} a variable within Alice's scope inside the program:

@reach{
  const Alice = Participant('Alice', {});
  deploy();
  Alice.only(() => {
    const x = 3;
  });
}

or put @reachin{3} as the value of @reachin{x} in your frontend and adjust the @tech{participant interact interface}
to list the type:

@reach{
  const Alice = Participant('Alice', {
    'x': UInt,
  });
}


@section{RE0016}
@section{RE0017}
@section{RE0018}
@section{RE0019}
@section{RE0020}
