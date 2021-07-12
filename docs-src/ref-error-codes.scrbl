#lang scribble/manual
@(require "lib.rkt")

@(define (error x) @section[#:tag x]{@|x|})

@title[#:version reach-vers #:tag "ref-error-codes" #:style 'toc]{Error Codes}

This section provides an in depth explanation of the error codes produced from
the Reach compiler.

@(local-table-of-contents)

@;{
  What is a "good" error description?
  * Intuitive explanation
  * Example erroneous program
  * How to fix to fix that program (use active language.)
  * Any other advice
}

@error{RC0000}

This error indicates that the program uses a number that is beyond the range of acceptable numbers for the given
@tech{connector}.

For example, the code below uses a value too large for the @reachin{ALGO} connector:

@reach{
  const y = 18446744073709551616;
}

You can fix this error by having your frontend provide the value and accessing it
via the @tech{participant interact interface}:

@reach{
  const A = Participant('A', {
    // extend participant interact interface
    y: UInt,
  });
  A.only(() => {
    const y = declassify(interact.y);
  });
}

Alternatively, you can fix this by not compiling to the given @tech{connector}, in which
case, your application will no longer be blockchain agnostic.

@error{RE0000}

This error indicates that you provided an incorrect number of arguments to a function.

For example, the code below applies one value to @reachin{f}:

@reach{
  const f = () => { return 3 };
  const x = f(5);
}

You can fix this by providing the same amount of arguments expected:

@reach{
  const f = () => { return 3 };
  const x = f();
}

@error{RE0001}

This error indicates that a program uses an invalid assignment operator.
Reach only supports assignment using the @reachin{=} operator. Any other operator,
such as @reachin{+=, *=, ...} is not allowed.

For example, the code below erroneously tries to re-assign @reachin{x} at the end of
a @reachin{while} loop:

@reach{
  x *= 2;
  continue;
}

You can fix this by explicitly writing out the operation on the right hand
side of @reachin{=}:

@reach{
  x = x * 2;
  continue;
}

Keep in mind, that the assignment operator is a form of mutation and is only allowed
immediately before a @reachin{continue}.

@error{RE0002}

This error indicates that a program uses an invalid statement.
Reach is a strict subset of JavaScript and does not accept every statement
that is valid JavaScript. It may be necessary to express your program
with different constructs than you would JavaScript.

For example, the code below erroneously uses a @jsin{for} loop, which is not
supported in Reach:

@reach{
  for (let i = 0; i < arr.length; i++) {
    // ...
  }
}

You can fix this by either using a @reachin{while} loop or a combination of
@reachin{Array.iota} and @reachin{Array.map/Array.forEach}:

@reach{
  Array.iota(arr.length).map((i) => {
    // ...
  });
}

@error{RE0003}

This error indicates that a statement block returns a non-null value although a @reachin{null} value is expected.
The block should either use a @reachin{return null;} statement or no return statement at all.

@error{RE0004}

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

You can fix this by using @reachin{const} and either creating fresh variables or collapsing the logic if simple enough:

@reach{
  const x = 0;
  const xPrime = iAmLegend ? 5 : x
  // or
  const x = iAmLegend ? 5 : 0;
}

@error{RE0005}

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

Reach requires the @reachin{invariant} to reason about the @reachin{while} loop during verification. You
can fix this by adding a @reachin{var} and @reachin{invariant} declaration before the loop:

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

@error{RE0006}

@error-version[#:to "v0.1"]

This error indicates that @reachin{return} may not be used within the current statement block.

@error{RE0007}

This error indicates that the @reachin{timeout} branch of a statement such
as @reachin{publish, pay, fork} has been given the wrong arguments.

For example, the following code erroneously attempts to @reachin{closeTo(Bob)} in the event that
@reachin{Alice} does not publish in time:

@reach{
  Alice
    .publish()
    .timeout(5, closeTo(Bob));
}

However, the second argument of the @reachin{timeout} branch must be a thunk. You can fix this by
wrapping @reachin{closeTo(Bob)} in an @tech{arrow expression}:

@reach{
  Alice
    .publish()
    .timeout(5, () => closeTo(Bob));
}

@error{RE0008}

This error indicates that a @reachin{timeout} branch of a statement such as
@reachin{publish, pay, fork} has not been given a block of code to execute in the
event of a @reachin{timeout}.

For example, the code below erroneously provides a @reachin{timeout} delay, but does
not specify a function to run if the timeout occurs:

@reach{
  A.pay(0)
   .timeout(1);
}

You can fix this by providing a function as a second argument to @reachin{timeout}:

@reach{
  A.pay(0)
   .timeout(1, () => {
      // ...
    });
}

@error{RE0009}

This error indicates that the @tech{pay amount} provided states the amount of
network tokens more than once.

For example, the code below erroneously provides two atomic values in the @tech{pay amount},
which are both interpreted as the amount of network tokens to pay:

@reach{
  A.pay([ amt, amt, [ amt, tok ]]);
}

You can fix this by deleting one of the atomic values:

@reach{
  A.pay([ amt, [ amt, tok ]]);
}

@error{RE0010}

This error indicates that the @tech{pay amount} provided states the amount of
a specific non-network token more than once.

For example, the code below erroneously provides two tuples in the @tech{pay amount},
both of which specify the amount of the same token:

@reach{
  A.pay([ amt, [amt, tok ], [ amt, tok ] ]);
}

You can fix this by deleting one of the tuples:

@reach{
  A.pay([ amt, [amt, tok ] ]);
}

@error{RE0011}

This error indicates that the @tech{pay amount} provided does not provide values of the correct
type.

For example, the code below erroneously provides a @tech{pay amount} that consists of a one element
tuple:

@reach{
  A.pay([ amt, [ amt ] ];)
}

However, a tuple in a @tech{pay amount} must specify the amount and the @reachin{Token}. You can fix this
by adding the @reachin{Token} to the tuple:

@reach{
  // tok : Token
  A.pay([ amt, [ amt, tok ] ];)
}

@error{RE0012}

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

You can fix this by decoupling the functions and calling them sequentially. This technique requires
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

@error{RE0013}

This error indicates that you provided an unrecognized option to @reachin{setOptions}.
There is most likely a typo in your code.

Please review the recognized options in the documentation for @reachin{setOptions}.

@error{RE0014}

This error indicates that you did not provide an acceptable value for a specific option
in @reachin{setOptions}.

Please review the eligible values listed in the documentation for @reachin{setOptions}.

@error{RE0015}

This error indicates that your @tech{participant interact interface} does not provide a @reachin{Type}
for a given field.

For example, in the erroneous code below, @reachin{x} is assigned @reachin{3} in the @tech{participant interact interface}
of @reachin{Alice}:

@reach{
  const Alice = Participant('Alice', {
    'x': 3,
  });
}

However, the interact interface specifies the type of values that will be provided at runtime. You can fix this by
either making @reachin{x} a variable within Alice's scope inside the program:

@reach{
  const Alice = Participant('Alice', {});
  deploy();
  Alice.only(() => {
    const x = 3;
  });
}

or by putting @reachin{3} as the value of @reachin{x} in your frontend and adjusting the
@tech{participant interact interface} to list the type:

@reach{
  const Alice = Participant('Alice', {
    'x': UInt,
  });
}


@error{RE0016}

This error indicates that the arguments passed to @reachin{Reach.App} are incorrect.
@reachin{Reach.App} accepts a single thunk as its argument.

For example, the code below erroneously declares a @reachin{Reach.App} without too
many arguments:

@reach{
  export const main = Reach.App({}, () => {
  });
}

You can fix this by ensuring only one argument is passed to @reachin{Reach.App},
which is a function with no arguments:

@reach{
  export const main = Reach.App(() => {
  });
}

@error{RE0017}

This error indicates that the name of the @reachin{Participant} or @reachin{View} provided
is invalid. These names must satisfy the regex @reachin{[a-zA-Z][_a-zA-Z0-9]*}.

For example, the code below provides a Participant name that is unsatisfactory:

@reach{
  const P = Participant('Part 4!', {});
}

You can fix this by removing any illegal characters and replacing spaces with underscores:

@reach{
  const P = Participant('Part_4', {});
}

@error{RE0018}

This error indicates that an invalid expression is used on the left hand side of an
assignment.

For example, the code below erroneously puts an arithmetic expression on the left hand
side of an assignment:

@reach{
  const (f + 1) = 10;
}

You can fix this by moving all the arithmetic to the right hand side, leaving only the variable
on the left:

@reach{
  const f = 10 - 1;
}

@error{RE0019}

This error indicates that an object spread is occurring before the last position in a
destructuring assignment. It must come last due to the fact it binds the remaining
elements to the given variable.

For example, the code below erroneously attempts to destructure one element @reachin{y}
and the remaining elements into @reachin{x}:

@reach{
  const {...x, y} = {x: 1, y: 2, z: 3};
}

You can fix this by moving @reachin{...x} to the last position:

@reach{
  const {y, ...x} = {x: 1, y: 2, z: 3};
}

@error{RE0020}

This error indicates that an array spread is occurring before the last position in a
destructuring assignment. It must come last due to the fact it binds the remaining
elements to the given variable.

For example, the code below erroneously attempts to destructure one element @reachin{y}
and the remaining elements into @reachin{x}:

@reach{
  const [...x, y] = [1, 2, 3];
}

You can fix this by moving @reachin{...x} to the last position:

@reach{
  const [y, ...x] = [1, 2, 3];
}

@error{RE0021}

This error indicates that the compiler expected to receive a closure, but
it was given a different value.

For example, the code below erroneously attempts to provide a value for
the @reachin{match} case of a nullary @reachin{Data} constructor:

@reach{
  Maybe(UInt).None().match({
    None: 0,
    Some: (x) => x
  });
}

You can fix this by wrapping the value @reachin{0} in an arrow expression because
@reachin{match} expects all cases to be bound to closures:

@reach{
  Maybe(UInt).None().match({
    None: () => 0,
    Some: (x) => x
  });
}

@error{RE0022}

This error indicates that there was an invalid declaration. This error will
occur when attempting to bind multiple variables within a single @reachin{const}.

For example, the code below erroneously attempts to bind @reachin{x} and @reachin{y}
within one @reachin{const} assignment:

@reach{
  const x = 1, y = 2;
}

You can fix this by breaking apart the declarations into two @reachin{const} statements:

@reach{
  const x = 1;
  const y = 2;
}

@error{RE0023}

@error-version[#:to "v0.1"]

This error indicates that there was an invalid declaration.

@error{RE0024}

This error indicates that there is an attempt to unpack an @reachin{array}, but the binding
does not expect the same amount of values that the @reachin{array} contains.

For example, the code below erroneously tries to unpack an @reachin{array} of 3 values into
2 variables:

@reach{
  const [ x, y ] = [ 1, 2, 3 ];
}

You can fix this by either binding or ignoring, via @reachin{_}, the last element of the @reachin{array}:

@reach{
  const [ x, y, _ ] = [ 1, 2, 3 ];
}


@error{RE0025}

This error indicates that there is an attempt to access a field of an @reachin{object}
that does not exist. Ensure that you are referring to the correct name, or
add the needed field to the @reachin{object} if necessary.

@error{RE0026}

This error indicates that the @reachin{continue} statement is used outside of a
@reachin{while} loop. To fix this issue, delete the erroneous @reachin{continue},
or move it to the end of your @reachin{while} loop.

@error{RE0027}

This error indicates that there is an attempt to @reachin{wait} or @reachin{timeout} before the first publication
in the @reachin{firstMsg} deploy mode. This situation is not allowed because the @reachin{firstMsg} deploy mode,
waits to deploy the contract along with the first publication.

@reach{
  export const main = Reach.App(() => {
    setOptions({ deployMode: 'firstMsg' });
    const A = Participant('Alice', {});
    deploy();
    wait(1);
  });
}

You can fix this by having a @reachin{Participant} @reachin{publish} first:

@reach{
  export const main = Reach.App(() => {
    setOptions({ deployMode: 'firstMsg' });
    const A = Participant('Alice', {});
    deploy();
    A.publish();
    wait(1);
  });
}

@error{RE0028}

This error indicates that a the variable update inside of a loop, e.g. @reachin{while},
is attempting to mutate variables that are not mutable. For a @reachin{while} loop, this
means the variable was not declared with @reachin{var} prior to the loop.

For example, the code below erroneously attempts to mutate @reachin{y} which has not been
defined via @reachin{var}:

@reach{
  var [x] = [1];
  invariant(true);
  while(x < 2) {
    [ x, y ] = [ x + 1, x ];
    continue;
  }
}

You can fix this by either deleting @reachin{y} or adding it to the variable list:

@reach{
  var [ x, y ] = [ 1, 1 ];
  invariant(true);
  while(x < 2) {
    [ x, y ] = [ x + 1, x ];
    continue;
  }
}


@error{RE0029}

This error indicates an attempt to bind a @reachin{ParticipantClass} to a
specific @reachin{Address}.

For example, the example code below erroneously tries to @reachin{set} a @reachin{ParticipantClass}
to a specific address:

@reach{
  const C = ParticipantClass('C', {});
  // ...
  C.set(addr);
}

You can fix this by using a @reachin{Participant}, which may be associated with a single address:

@reach{
  const C = Participant('C', {});
  // ...
  C.set(addr);
}

@error{RE0030}

This error indicates an attempt to re-bind a @reachin{Participant} to another @reachin{Address}.
Once a @reachin{Participant} is bound to an @reachin{Address}, either by making a @tech{publication}
or explicitly via @reachin{Participant.set}, they may not be re-bound.

For example, the code below erroneously has @reachin{Bob} make a @tech{publication}, then later,
attempts to bind him to a specific address:

@reach{
  Bob.publish();
  // ...
  Bob.set(addr);
}

You can fix this by deleting one of the statements (depending on the logic of your application).

@error{RE0031}

This error indicates that you are attempting to use a specific statement or expression in the wrong
mode. Consult the documentation for the specific keyword to learn more about what mode is
expected. Additionally, see the figure on @secref["ref-programs"] for a diagram regarding the modes
of a Reach application.

@error{RE0032}

This error indicates that you are attempting to mutate a variable in an inappropriate place.
Variable mutation is only allowed to occur on variables declared via @reachin{var} and immediately
before a @reachin{continue} statement of a loop.

For example, the code below attempts to mutate a loop variable improperly:

@reach{
  var [ x ] = [ 0 ];
  invariant(balance() == 0);
  while (true) {
    commit();
    x = 1;
    // ...
    continue;
  }
}

You can fix this issue by moving the mutation directly before the @reachin{continue}:

@reach{
  var [ x ] = [ 0 ];
  invariant(balance() == 0);
  while (true) {
    commit();
    // ...
    x = 1;
    continue;
  }
}

@error{RE0033}

This error indicates you are using an illegal JavaScript expression in Reach. Not all JavaScript
expressions are valid Reach, as they are not applicable to the language.

@error{RE0034}

This error indicates that there is nowhere to return to in the current statement block.
This may occur if you write a @reachin{return} statement at the top level of a file
or if you've already wrote a @reachin{return} statement.

For example, the code below has two @reachin{return} statements, the first of which will
always occur, since it is not within a conditional:

@reach{
  const f = () => {
    return 0;
    return 1;
  };
}

You can fix this by removing the second @reachin{return} which is dead code:

@reach{
  const f = () => {
    return 0;
  }
}

@error{RE0035}

This error indicates that a value, which is not a function, is being
applied as if it were a function. Ensure you are writing the correct name
of the function you intend to use.

For example, the code below has two variables: @reachin{f} and @reachin{g}:

@reach{
  const f = () => 2;
  const g = 2;
  const h = g();
}

@reachin{g} is being applied as if it were a function, although we really intended
on calling @reachin{f}. This can be fixed by ensuring we call a function:

@reach{
  const f = () => 2;
  const g = 2;
  const h = f();
}

@error{RE0036}

This error indicates that a value, which is not a function, is being
applied as if it were a function.

For example, the code erroneously tries to create an @reachin{array} the same
size as @reachin{arr}, but filled with @reachin{1}:

@reach{
  const a = arr.map(1);
}

You can fix this code by providing a function to @reachin{Array.map}:

@reach{
  const a = arr.map((_) => 1);
}

@error{RE0037}

This error indicates that a value, which is not an @reachin{Object}, is being
treated as if it were an @reachin{Object}. This error occurs when you try to access
a field of an erroneous value. This issue is most likely caused by a typo in your
program.

@error{RE0038}

This error indicates that a value, which is not an @reachin{Array} or @reachin{Tuple}, is being
treated as if it were. This error occurs when you try to access
an element of an erroneous value. This issue is most likely caused by a typo in your
program.

@error{RE0039}

This error indicates that there is an attempt to dereference an @reachin{Array} or @reachin{Tuple}
with a non-numerical value. You must use a value of type @reachin{UInt} to dereference
an @reachin{Array}.

@error{RE0040}

This error indicates that you are using a dynamic value to dereference a value which is not an @reachin{Array}.
This issue is most likely caused by a typo. Please ensure you are dereferencing an @reachin{Array}.

@error{RE0041}

This error indicates that there is an attempt to statically dereference an @reachin{Array} beyond it's bounds.
Ensure you are using an index that is between @tt{0} and @tt{1} less than the length of the @reachin{Array}.

@error{RE0042}

This error indicates that there is an attempt to reference an identifier that is not in scope. This issue may be
caused by a typo, a scoping issue, or a missing @reachin{import}.

For example, the code below declares a function with a variable @reachin{x} declared within it. Attempting to reference
@reachin{x} outside of the function will result in an error:

@reach{
  const f = () => {
    const x = 5;
  }
  const y = x;
}

You can fix this issue by returning the value of @reachin{x} from the function:

@reach{
  const f = () => {
    const x = 5;
    return x;
  }
  const y = f();
}

If you are attempting to use a value from a library, simply add the necessary @reachin{import} to the top
of the Reach file.

@error{RE0043}

This error indicates that there is a mismatch between the expected security levels of a variable
and the actual one provided. This may happen if you use a @tech{public} variable where a @tech{secret}
is expected, or vice versa.

For example, the code below erroneously declassifies the variable @reachin{x}, which is not @tech{secret}:

@reach{
  const x = 0;
  A.only(() => {
    const y = declassify(x);
  });
}

You can fix this issue by simply assigning @reachin{y} to @reachin{x}.

@error{RE0044}

This error indicates that you provided an incorrect number of arguments to a function. You can fix this
by providing the same amount of arguments expected.

@error{RE0045}

This error indicates that an anonymous function was provided a name, which is not allowed.

For example, the code below names the anonymous function @reachin{m}:

@reach{
  const x = array(UInt, [0, 1, 2]);
  const y = x.map(function m(i){ return i + 1; });
}

You can fix this by removing the function name:

@reach{
  const x = array(UInt, [0, 1, 2]);
  const y = x.map(function (i){ return i + 1; });
}

@error{RE0046}

This error indicates that there was an invalid syntax used for an @reachin{import}.
The acceptable @reachin{import} formats are defined in the documentation for the keyword.

For example, the code below erroneously performs a default @reachin{import}:

@reach{
  import blah from 'sample_lib.rsh';
}

You can fix this code by explicitly importing the bindings you want:

@reach{
  import {a,b,c} from 'sample_lib.rsh';
}

or by binding all the exports to an identifier:

@reach{
  import * as lib from 'sample_lib.rsh';
}

@error{RE0047}

@error-version[#:to "v0.1"]

This error indicates that there was a @reachin{return} statement
at the top level.

@error{RE0048}

This error indicates that the Reach file does not have a header at
the top of the file. The first top level statement of a Reach module
must indicate what version of Reach the file uses.

For example, the code below erroneously exports an application
without specifying what version of Reach it uses:

@reach{
  export const main = Reach.App(() => {});
}

Fix this by adding a header to the file:

@reach{
  "reach 0.1";

  export const main = Reach.App(() => {});
}

@error{RE0049}

This error indicates that an @reachin{object} has been given a field
that is not an identifier or a @reachin{string}.

For example, the code below erroneously uses a dynamic string as an object key:

@reach{
  A.only(() => {
    const x = declassify(interact.x);
  });
  A.publish(x);
  const o = {
    [x]: 4,
  };
}

You can fix this by using a static string as the key.

@error{RE0050}

@error-version[#:to "v0.1"]

This error indicates an @reachin{Object} has an incorrect number of values
associated with a field.

@error{RE0051}

This error indicates that the field of an @reachin{Object} uses the incorrect
syntax for defining a function.

For example, the code below declares a field as a function with the following syntax:

@reach{
  const o = {
    f() {
      return 1;
    }
  };
}

You can fix this by using the following @tech{arrow expression} syntax:

@reach{
  const o = {
    f: () => { return 1; }
  }
}


@error{RE0052}

This error indicates that a @reachin{UInt} has been used as the key of an
@reachin{Object}. However, only identifiers and values of type @reachin{Bytes}
are valid object keys.

You can fix this issue by replacing the erroneous key with a static string.

@error{RE0053}

This error indicates that you are attempting to spread a value that is not
an object. This issue is most likely caused by a typo in your program.


@error{RE0054}

This error indicates that the argument provided to @reachin{Array.iota} is
not static. @reachin{Array.iota} requires its argument to be computable at
compile time.

You can fix this issue by providing a static @reachin{UInt} to the function.

@error{RE0055}

This error occurs when you provide a primitive operation with the incorrect
number of arguments or arguments of the wrong type. Please review the documentation
for the function you are attempting to use and provide it with the correct arguments.

@error{RE0056}

This error indicates that you are attempting to create a variable, although another
variable in the scope uses the same name. In Reach, identifier shadowing is
not allowed. You can fix this issue by renaming your variable or moving one of the
variable declarations to another scope where it does not conflict with the other.

@error{RE0057}

This error indicates that the compiler expected the tail of a statement block
to be empty, but it wasn't. This issue may arise if there are statements beyond
a @reachin{return} or @reachin{exit} statement. These statements are dead code
and you can fix this issue by deleting them.


@error{RE0058}

@error-version[#:to "v0.1"]

This error indicates that you tried to use the @reachin{publish} keyword twice
in a publication.

@error{RE0059}

@error-version[#:to "v0.1"]

This error indicates that there is a function at the top level without a name.
You can fix this by naming your function.

@error{RE0060}

@error-version[#:to "v0.1"]

This error indicates that there is an illegal @reachin{while} loop @reachin{invariant}.
You can fix this issue by providing only one expression to @reachin{invariant}.

@error{RE0061}

@error-version[#:to "v0.1"]

This error indicates that @reachin{Participant.only} was not supplied a single thunk
as its argument.

You can fix this by providing the expected value to the function.

@error{RE0062}

@error-version[#:to "v0.1"]

This error indicates that @reachin{each} was not given a @reachin{Tuple} of @reachin{Participant}s
as its first argument.

You can fix this by providing the expected value to the function.

@error{RE0063}

This error indicates that a given function expects a @reachin{Participant} or @reachin{ParticipantClass}
as an argument, but it was given something else.

For example, the code below erroneously provides @reachin{false} instead of a @reachin{Participant} to
@reachin{unknowable}:

@reach{
  A.only(() => {
    const _x = interact.x;
  });
  unknowable(false, A(_x));
}

You can fix this by passing a @reachin{Participant} as the first argument to @reachin{unknowable}:

@reach{
  A.only(() => {
    const _x = interact.x;
  });
  unknowable(B, A(_x));
}

@error{RE0064}

This error indicates that the program is attempting to transfer funds to a @reachin{Participant}
that is not yet bound to an @reachin{Address}.

For example, the code below transfers funds to @reachin{Bob} before he has a set @reachin{Address}:

@reach{
  Alice.publish().pay(100);
  transfer(100).to(Bob);
}

You can fix this by using @reachin{Participant.set} first or having @reachin{Bob} publish before the
@reachin{transfer}:

@reach{
  Bob.publish();
  commit();
  Alice.publish().pay(100);
  transfer(100).to(Bob);
}

@error{RE0065}

This error indicates that you are attempting to @reachin{transfer} funds to a @reachin{ParticipantClass}.
This is not possible because @reachin{transfer} expects a single @reachin{Address} to transfer to.

For example, the code below erroneously attempts to transfer the @reachin{balance} of the @tech{contract} to a class:

@reach{
  const Alice = Participant('Alice', {});
  const Bob   = ParticipantClass('Bob', {});
  deploy();
  Alice.publish().pay(100);
  transfer(100).to(Bob);
}

You can fix this code by specifying a specific @reachin{Address} to use. For example, the
class could @reachin{race} to specify their own address:

@reach{
  const Alice = Participant('Alice', {});
  const Bob   = ParticipantClass('Bob', {});
  deploy();
  Alice.publish().pay(100);
  commit();
  Bob.only(() => {
    const b = this;
  });
  Bob.publish(b);
  transfer(100).to(b);
  commit();
}

@error{RE0066}

This error indicates that the state of the program differs in the @tech{continuation} of a
branching statement. That is, if a Reach program may execute multiple different code paths at
runtime, the @tech{continuation} of those branches must make the same assumption about state.

For example, this error may be caused by having one branch end in @tech{consensus step} and
the other in a @tech{step}. You can fix this by ensuring both branches end in the same mode.

Another example is a @reachin{Participant} makes their first publication in the branch
of a conditional. You can fix this by having the @reachin{Participant} make their first
publication before the conditional statement.

@error{RE0067}

This error indicates that you are attempting to bind a @tech{secret} value to an identifier
of the wrong format. @tech{secret} identifiers must be prefixed with @reachin{_}.

For example, the code below erroneously assigns a @tech{secret} value to a @tech{public} identifier, @reachin{x}:

@reach{
  A.only(() => {
    const x = interact.x;
  });
}

You can fix this by either changing the identifier to start with @reachin{_} or using @reachin{declassify}
to make the value @tech{public}:

@reach{
  A.only(() => {
    const _x = interact.x;
  });
  // or
  A.only(() => {
    const x = declassify(interact.x);
  });
}

@error{RE0068}

This error indicates that you are attempting to bind a @tech{public} value to an identifier
of the wrong format. @tech{public} identifiers cannot be prefixed with @reachin{_}.

For example, the code below erroneously assigns a @tech{public} value to a @tech{secret} identifier, @reachin{_x}:

@reach{
  const _x = 1;
}

You can fix this by removing the @reachin{_} prefix:

@reach{
  const x = 1;
}

@error{RE0069}

This error indicates that you are attempting to read the value of @reachin{_}. Any binding to @reachin{_} is
ignored and therefore cannot be read from.

You can fix this by using another identifier and referencing it as usual.

@error{RE0070}

This error indicates that you are attempting to spread a value as if it were
@reachin{Tuple}, @reachin{Array}, or @reachin{Struct}, but it is not. This issue
is likely caused by a typo in your code.

For example, the code below erroneously spreads the wrong values:

@reach{
  const xi = 1;
  const xa = [2, 3];
  add(1, ...xi);
}

You can fix this code by spreading a tuple-like value for the second argument of @reachin{add}:

@reach{
  const xi = 1;
  const xa = [2, 3];
  add(1, ...xa);
}

@error{RE0071}

This error indicates that the two @reachin{Array}s given to @reachin{Array.zip} are not of
equal length. You can fix this error by providing two @reachin{Array}s of equal length to
the function.

@error{RE0072}

This error indicates that a @reachin{switch} statement was supplied with a value that is not a
@reachin{Data} instance.

For example, the code below expects a @reachin{Maybe} type, but is erroneously provided with an @reachin{UInt}:

@reach{
  const f = (mx) => {
    switch (mx) {
      case Some: { return mx; }
      case None: { return 0; }
    };
  };

  f(1);
}

You can fix this code by providing a value with the correct @reachin{Type} to the @reachin{switch} statement:

@reach{
  const f = (mx) => {
    switch (mx) {
      case Some: { return mx; }
      case None: { return 0; }
    };
  };

  f(Maybe(UInt).Some(1));
}

@error{RE0073}

This error indicates that there are multiple cases for the same variant in a @reachin{switch} statement or
@reachin{match} expression.

You can fix this error by deleting one of the branches, ensuring there are only
one branch per variant.

@error{RE0074}

This error indicates that a @reachin{switch} statement or @reachin{match} expression does not have a case
for every variant of a @reachin{Data} instance.

You can fix this issue by adding the missing cases listed
in the error message.

@error{RE0075}

This error indicates that a @reachin{switch} statement or @reachin{match} expression contains cases
for unknown variants. These erroneous variants are not listed in the @reachin{Data} definition.

You can fix this issue by adding the unknown variant to the @reachin{Data} definition or removing the case.

@error{RE0076}

This error indicates that the @reachin{Type} of a value you provided to a function or operation does
not match the expected @reachin{Type}.

For example, the code below erroneously provides a number as the second argument to @reachin{assert}:

@reach{
  assert(2 == 2, 5);
}

However, the second argument of @reachin{assert} is expected to be of type @reach{Bytes}. You can fix this
issue by providing a value of the correct type:

@reach{
  assert(2 == 2, "5th assertion")
}

@error{RE0077}

This error indicates that the depth of recursion for a function call exceeded the limit allowed.
This issue may indicate that the recursive function does not have a base case.

You can fix this issue by re-writing your recursive function into an iterative set of
statements, e.g. @reachin{while} loop.

@error{RE0078}

This error indicates that the program is no longer live by the time it reaches a publication.
That is, the program will have already @reachin{exit}ed before the given point.

For example, the code below will always @reachin{exit} before calling @reachin{publish}:

@reach{
  const f = () => { exit(); };
  f();
  Alice.publish();
  commit();
}

You can fix this code by wrapping the @reachin{exit} in a conditional:

@reach{
  const f = () => { exit(); };
  if (/* ... */) {
    f();
  }
  Alice.publish();
  commit();
}

@error{RE0079}

@error-version[#:to "v0.1"]

This error indicates that a statement is being used in place of an expression.
Refer to the documentation for the statement you are attempting to use for more
information on how to use it.

@error{RE0080}

This error indicates that there is an attempt to conditionally transition to consensus
without a @reachin{timeout}. When making a conditional publication, such as @reachin{A.publish(x).when(shouldPublish)},
there needs to be a timeout associated with the publication if @reachin{shouldPublish} is not statically @reachin{true}.

In @reachin{parallelReduce} or @reachin{fork}, a @reachin{timeout} is required unless one @reachin{Participant}
always races, the @reachin{when} field in their @tt{PUBLISH_EXPR} is statically @reachin{true}, or if one
@reachin{ParticipantClass} will attempt to race.

For example, the code below erroneously attempts to publish a value if a certain condition holds:

@reach{
  A.only(() => {
    const { x, shouldPublish } = declassify(interact.getParams());
  });
  A.publish(x)
   .when(shouldPublish);
  commit();
}

You can fix this issue by providing a @reachin{timeout} case for

@reach{
  A.only(() => {
    const { x, shouldPublish, deadline } = declassify(interact.getParams());
  });
  A.publish(x)
   .when(shouldPublish)
   .timeout(deadline, () => closeTo(Bob));
  commit();
}

@error{RE0081}

This error indicates that the result of @tt{PUBLISH_EXPR} for a @reachin{fork} or @reachin{parallelReduce}
is not of the right @reachin{Type}. It is expected to be an @reachin{Object} with a @reachin{when} field,
and optionally a @reachin{msg} field.

For example, the code below erroneously tries to publish a value in a @reachin{parallelReduce} case:

@reach{
  parallelReduce(/* ... */)
    // ...
    .case(Alice,
      (() => {
        const x = declassify(interact.x);
        return x;
      })
    // ...
    )
}

This code can be fixed by using an @reachin{Object} and assigning the value to be published to
the @reachin{msg} field of the object:

@reach{
  parallelReduce(/* ... */)
    // ...
    .case(Alice,
      (() => {
        const x = declassify(interact.x);
        return {
          msg: x;
        }
      })
    // ...
    )
}

@error{RE0082}

This error indicates that the parameters of a @tt{CONSENSUS_EXPR} in a @reachin{fork} or
@reachin{parallelReduce} are incorrect. The function provided should either accept zero
parameters or one parameter, which represents the @reachin{msg} of the @reachin{PUBLISH_EXPR}.

For example, the code below erroneously tries to publish multiple values and bind them in
the function provided to @tt{CONSENSUS_EXPR}:

@reach{
  parallelReduce(/* ... */)
    // ...
    .case(Alice,
      (() => ({
        msg: [declassify(interact.x), declassify(interact.y)];
        when: declassify(interact.shouldGo())
      })),
      ((x, y) => {
        // ...
      })
    )
}

You can fix this code by changing the arrow expression to accept one parameter. You can
either destructure the argument with a @reachin{const} assignment or as part of the function
syntax:

@reach{
  parallelReduce(/* ... */)
    // ...
    .case(Alice,
      (() => ({
        msg: [declassify(interact.x), declassify(interact.y)];
        when: declassify(interact.shouldGo())
      })),
      (([ x, y ]) => {
        // ...
      })
    )
}

@error{RE0083}

This error indicates that not all the components of the @reachin{parallelReduce} statement are provided.
Please refer to the documentation of @reachin{parallelReduce} to see the required components.

You can fix this error by adding any components the compiler has listed.

@error{RE0084}

This error indicates that you have provided the wrong number of arguments to a component of
@reachin{parallelReduce}. Please refer to the documentation for the specific component you
are trying to use.

For example, the code below erroneously supplies a closure as the second argument to @reachin{timeRemaining}.

@reach{
  parallelReduce([ 0 ])
    .invariant(balance() == balance())
    .while(true)
    .case(A,
      (() => { when: true }),
      (() => {
        return [ x + 1]
      })
    )
    .timeRemaining(1, () => {});
}

However, @reachin{timeRemaining} is a shorthand for a timeout which automatically publishes and returns the
@reachin{parallelReduce} accumulator. The component only expects one argument. You can fix this code by removing
the second argument supplied.

@error{RE0085}

This error indicates that your program would contain a value at runtime which would not be allowed. This
error usually stems from not fully applying a primitive function or using a value incorrectly, such
as the @tech{participant interact interface} of a @reachin{Participant}.

For example, the code below erroneously tries to publish @reachin{Alice}'s interact interface:

@reach{
  Alice.only(() => {
    const aInteract = declassify(interact);
  });
  Alice.publish(aInteract);
}

You can fix this code by specifying a specific field of @reachin{Alice}'s interact interface to @reachin{publish}:

@reach{
  Alice.only(() => {
    const aX = declassify(interact.x);
  });
  Alice.publish(aX);
}

@error{RE0086}

This error indicates that the @reachin{Type} of a value cannot exist at runtime. This error
may be caused by a @reachin{Fun} in a @tech{participant interact interface} having a return
type of another @reachin{Fun}.

For examples of this error and how to fix it, see @secref["RE0012"].

@error{RE0087}

@error-version[#:to "v0.1"]

This error indicates that you are attempting to apply a non-function value as if it were a function. This
issue is most likely caused by a typo with an identifier.

@error{RE0088}

This error indicates that there is a mismatch between the actual @reachin{Type} of a value and the expected
@reachin{Type} of a value.

For example, the code below erroneously returns a @reachin{Bool} when the type annotation states that
the function should return an @reachin{UInt}.

@reach{
export const f =
  is(((x) => true),
     Fun([UInt], UInt));
}

You can fix this code by returning an @reachin{UInt} from the function or changing the return type of the function.

This error may be caused by using a value of the incorrect type in an operation. The code below erroneously uses
a @reachin{Maybe} value in an @reachin{+} expression:

@reach{
  A.only(() => {
    const mi = declassify(interact.get1());
    const i = (() => {
      switch (mi) {
      case None: return 42;
      default: return mi+1; } })(); });
}

In this code, @reachin{mi} is still of @reachin{Maybe} type. You can fix this code by changing @reachin{default}
to @reachin{case Some}, which will re-bind @reachin{mi} to the value contained within @reachin{Some}:

@reach{
  A.only(() => {
    const mi = declassify(interact.get1());
    const i = (() => {
      switch (mi) {
      case None: return 42;
      case Some: return mi+1; } })(); });
}

@error{RE0089}

This error indicates that a @reachin{Map.reduce} is being performed outside of an @reachin{invariant}, which
is the only place map reductions are allowed to occur.

For example, the code below erroneously attempts to keep the sum of the @reachin{Map} as a loop variable:

@reach{
  var [keepGoing, sum] = [true, m.sum()];
  invariant(balance() == sum);
  while (keepGoing) {
    commit();

    Alice.pay(1);
    m[Alice] = fromSome(m[Alice], 0) + 1;

    [keepGoing, sum ] = [true,  m.sum()];
    continue;
  }
}

You can fix this code by moving any @reachin{Map} reductions to inside the @reachin{invariant}:

@reach{
  var keepGoing = true;
  invariant(balance() == m.sum());
  while (keepGoing) {
    commit();

    Alice.pay(1);
    m[Alice] = fromSome(m[Alice], 0) + 1;

    keepGoing = true;
    continue;
  }
}

@error{RE0090}

This error indicates that a @reachin{Map} was expected in an expression, but a value
of a different type was provided. This issue is most likely caused by a typo in an
identifier.

@error{RE0091}

This error indicates that you are attempting to create a @reachin{Foldable} value, which is
not possible. @reachin{Foldable} is an interface that @reachin{Array} and @reachin{Map} implement.

For example, the code below erroneously tries to create a @reachin{Foldable} value:

@reach{
  const container = Foldable();
}

You can fix this code by instead creating a @reachin{Map} or an @reachin{Array}.

@error{RE0092}

This error indicates that there are normal parameters listed after parameters with default arguments
in a function definition. Parameters with default arguments must come after all other arguments.

For example, the code below erroneously lists its parameters:

@reach{
  const f = (name = "Reach", msg) => {
    // ...
  }
}

You can fix this error by rearranging the parameters so that the ones with default arguments are last:

@reach{
  const f = (msg, name = "Reach") => {
    // ...
  }
}

@error{RE0093}

This error indicates that you are attempting to bind an effect or statement, which is not allowed.

For example, the code below supplies a statement as an argument to a function:

@reach{
  closeTo(Bob,
    each([Alice, Bob], () => {
      interact.showResult(5); }));
}

The result of @reachin{each} cannot be bound as a function argument. You can fix this code by
wrapping the statement in an @tech{arrow expression}:

@reach{
  closeTo(Bob, () => {
    each([Alice, Bob], () => {
      interact.showResult(5); })});
}

@error{RE0094}

This error indicates that there are unused variables in your program. This error will
only occur with @reachin{'use strict'}.

You can fix this error by either replacing the unused variable names with @reachin{_} or
subsequently using @reachin{void(x)}.

@error{RE0095}

This error indicates that a field in a @reachin{Remote} object is not a function. You
can fix this by ensuring your @reachin{Remote} object only contains fields that are functions.
This fix may require changes to the foreign contract you are attempting to connect to.

@error{RE0096}

This error indicates that the key supplied to a @reachin{Struct} does not match the required regex.
@reachin{Struct} keys must satisfy the regex: @tt{[_a-zA-Z][_a-zA-Z0-9]*}.

For example, the code below provides an erroneous key value:

@reach{
  const s = Struct([["$x ", UInt]]);
}

You can fix this by removing any illegal characters:

@reach{
  const s = Struct([["x", UInt]]);
}

@error{RE0097}

This error indicates that a key in a @reachin{Struct} has been used more than once.
Every key must be unique in a @reachin{Struct}.

For example, the code below erroneously uses the same key twice:

@reach{
  const s = Struct([["x", UInt], ["y", UInt], ["x", UInt]]);
}

You can fix this by renaming one of the @reachin{"x"} fields:

@reach{
  const s = Struct([["x", UInt], ["y", UInt], ["x2", UInt]]);
}

@error{RE0098}

This error indicates that you are attempting to export a name that the
Reach backend already produces. For example, the names provided in @reachin{Participant},
@reachin{ParticipantClass}, and @reachin{View} will be exported by the Reach backend.

Reach exports a few names from the backend automatically, such as @reachin{getExports}.
Therefore, you cannot export a @reachin{Participant} named @reachin{getExports} as such:

@reach{
  const P = Participant('getExports', {});
}

You can fix this error by choosing a different name.

@error{RE0099}

This error indicates that you are attempting to use a value that is not a @reachin{Bool}
in a condition, while using @tech{strict mode}.

For example, the code below erroneously uses a number as the condition to @reachin{if}:

@reach{
  const y = declassify(interact.getInt());
  const x = y ? 2 : 3;
}

You can fix this code by using a @reachin{Bool} instead. The following code will consider
any number that is not @reachin{0} @reachin{true}:

@reach{
  const y = declassify(interact.getInt());
  const x = (y != 0) ? 2 : 3;
}

@error{RE0100}

This error indicates that there are multiple @reachin{throw} statements inside a @reachin{try}
block and the values thrown are of different @reachin{Type}s.

You can fix this error by ensuring that every value thrown is of the same type. It may be necessary
to create a new @reachin{Data} instance that can handle different types.

For example, the code below erroneously throws a @reachin{UInt} and a @reachin{Bool}:

@reach{
  Alice.only(() => {
    const transferAll = declassify(interact.transferAll);
  });
  Alice.publish(transferAll);

  try {
    if (transferAll) {
      throw true;
    } else {
      throw 1;
    }
  } catch (e) {
    if (e == true) {
      transfer(balance()).to(Alice);
    } else {
      transfer(e).to(Alice);
    }
  }
}

You can fix this code by abstracting the @reachin{Type}s of values thrown into a new @reachin{Data} type:

@reach{
  const TransferType = Data({
    CERTAIN_AMT: UInt,
    TRANSFER_ALL: Null
  });

  Alice.only(() => {
    const transferAll = declassify(interact.transferAll);
  });
  Alice.publish(transferAll);

  try {
    if (transferAll) {
      throw TransferType.TRANSFER_ALL();
    } else {
      throw TransferType.CERTAIN_AMT(1);
    }
  } catch (e) {
    switch (e) {
      case CERTAIN_AMT: {
        transfer(e).to(Alice);
      }
      case TRANSFER_ALL: {
        transfer(balance()).to(Alice);
      }
    }
  }
}

@error{RE0101}

This error occurs when you attempt to use a @reachin{throw} statement outside of a @reachin{try}
block.

You can fix this error by moving your @reachin{throw} statement inside the appropriate block of code
or wrapping the necessary code into a @reachin{try/catch} block.

@error{RE0102}

This error indicates that you are attempting to @reachin{pay} on the first publication of
a program that uses @reachin{setOptions({ deployMode: 'firstMsg' })}. This is not possible
because the contract will not yet exist. Therefore, it cannot receive tokens.

You can fix this by either using a different @reachin{deployMode} or paying into the
contract after the first publication.

@error{RE0103}

This error indicates that you are attempting to @reachin{publish} a @reachin{Token} within
a @reachin{while} loop. This is not currently possible in Reach. You must publish @reachin{Token}
values outside of loops.

For example, the code below erroneously publishes a @reachin{Token} inside a loop:

@reach{
  var [] = [];
  invariant(balance() == balance());
  while ( true ) {
    commit();
    A.only(() => {
      const [tok, amt] = declassify(interact.get()); });
    A.publish(tok, amt)
      .pay([amt, [amt, tok]]);
    continue;
  }
}

You can fix this code by publishing @reachin{tok} before the loop:

@reach{
  A.only(() => {
    const [tok, amt] = declassify(interact.get()); });
  A.publish(tok, amt);

  var rounds = 0;
  invariant(balance() == rounds * amt && balance(tok) == rounds * amt);
  while ( true ) {
    commit();

    A.pay([amt, [amt, tok]]);

    rounds = rounds + 1;
    continue;
  }
}

@error{RE0104}

This error indicates that you are attempting to reference a @reachin{Token} that
was computed dynamically. Reach does not yet support this.

For example, the code below attempts to transfer the balance of an @reachin{array} of @reachin{Token}s
to a @reachin{Participant}:

@reach{
  const allTokens = array(Token, toks);
  Foldable_forEach(allTokens, (tok) => transfer(balance(tok), tok).to(Who));
}

You can work around this issue by writing out the @reachin{Token} values explicitly:

@reach{
  const nonNetPayAmt = [ [balance(tok), tok], [balance(tok2), tok2] ];
  transfer([ balance(), ...nonNetPayAmt ]).to(Who);
}

@error{RE0105}

This error indicates that the incorrect arguments were supplied to @reachin{withBill}.
@reachin{withBill} either expects zero arguments, when only receiving @tech{network tokens}
or a @reachin{Tuple} of @reachin{Token}s when receiving @tech{non-network tokens}.

For example, the code below erroneously provides multiple @tech{non-network tokens} to
@reachin{withBill}:

@reach{
  const [ returned, [gilRecv, zmdRecv], randomValue ] =
    randomOracle.getRandom.pay(stipend).withBill(gil, zmd)();
}

You can fix this by wrapping all the arguments into a single @reachin{Tuple}:

@reach{
  const [ returned, [gilRecv, zmdRecv], randomValue ] =
    randomOracle.getRandom.pay(stipend).withBill([gil, zmd])();
}

@error{RE0106}

This error indicates that a program declared multiple @reachin{View}s with the same
name.

You can fix this error by renaming the duplicate @reachin{View}s, ensuring that every name is unique.

@error{RE0107}

@error-version[#:to "v0.1"]

This error indicates that the value of a @reachin{View} cannot be exposed. This would
only occur if the value cannot be represented at runtime.

@error{RE0108}

This error indicates that a @reachin{View} function has an unconstrained domain. Every
@reachin{View} must explicitly state the @reachin{Type} of function arguments it accepts.

If your @reachin{View} function relies on a varying number of arguments or @reachin{Type}s, you
can either abstract the arguments into a new @reachin{Data} type or make separate @reachin{View}s.

@error{RE0109}

This error indicates that there are multiple @reachin{Participant}s or @reachin{ParticipantClass}es
with the same name. Each participant name must be unique.

You can fix this error by renaming the duplicate names.

@error{RE0110}

This error indicates that a @reachin{Struct} contains an invalid field name.
A field name may be invalid if it is a reserved word in the @tech{connector} you are targeting.

For example, the code below erroneously uses the field name @reachin{"super"}, which is
reserved in Solidity:

@reach{
  const A = Participant('A', {
    get: Fun([], Struct([
      ['super', Address]
    ]))
  });
}

You can fix this by renaming the erroneous field names:

@reach{
  const A = Participant('A', {
    get: Fun([], Struct([
      ['super1', Address]
    ]))
  });
}

@error{RE0111}

This error indicates that an unexpected key was provided to the @reachin{Token} constructor.
You may find the acceptable parameters in the following section: @tech{token minting}.

@error{RE0112}

This error indicates that you are attempting to perform an invalid operation on a @reachin{Token}.
Some @reachin{Token} methods such as @reachin{destroy}, @reachin{burn}, and
@reachin{supply} are only valid for tokens that were created in your program.

You can fix this by removing the erroneous statement.

@error{RE0113}

This error indicates that you provided an incorrect value to the @reachin{.define} component of a
@reachin{parallelReduce} statement. The argument to @reachin{.define} should be of the form: @reachin{() => DEFINE_BLOCK}.
Please review the @reachin{parallelReduce} documentation for information on how @reachin{.define} works.

@error{REP0000}

This error indicates that the body of a @reachin{while} loop does not make a publication before the @reachin{continue}
statement.

For example, the code below does not make any publications before continuing the loop:

@reach{
  var x = 0;
  invariant(balance() == 0);
  while (true) {
    x = x + 1;
    continue;
  }
}

You can fix this code by making a publication within the @reachin{loop}:

@reach{
  var x = 0;
  invariant(balance() == 0);
  while (true) {
    commit();
    Alice.publish();
    x = x + 1;
    continue;
  }
}

Note that the body a @reachin{while} starts in a @tech{consensus step} so you must first
@reachin{commit} before making a publication.

@error{RI0000}

This error indicates that @tt{git clone} has failed when trying to download the dependencies
of your project. This error will tell you the issue that was encountered.

@error{RI0001}

This error indicates that @tt{git checkout} has failed when trying to checkout the specific
revision of your dependency. This error will tell you the issue that was encountered.

@error{RI0002}

This error indicates that the dependency required by your project does not contain either
the branch specified, or a @tt{master/main} branch.

Please ensure you have specified the correct import.

@error{RI0003}

This error indicates that your project dependencies need to be retrieved but you did not specify
@reachin{--install-pkgs} with your @tt{reach} command.

You can fix this by specifying the needed flag with your command.

@error{RI0004}

This error indicates that the syntax you used to specify your import is incorrect.

You can fix this by using the correct syntax. Please view the documentation for @tech{package imports}.

@error{RL0000}

This error indicates that the given code must not be reachable because it would
result in an error if reached. This error may be caused for different reasons,
which will be explained if encountered.

One reason this code could be encountered is if there is a branch within a @reachin{while}
loop, which does not contain a @reachin{continue} statement when it is expected.
You can fix this by explicitly adding the @reachin{continue} statement to the erroneous block
of code.

@error{RP0000}

This error indicates that there is a @link["https://en.wikipedia.org/wiki/Circular_dependency"]{circular dependency} in the @reachin{import}s of your application.

You can fix this by refactoring your code to remove the cyclic imports.

@error{RP0001}

@error-version[#:to "v0.1"]

This error indicates that you have specified a function without an argument list.

You can fix this by adding an argument list.

@error{RP0002}

@error-version[#:to "v0.1"]

This error indicates that an identifier was expected during parsing, but an expression
was received.

@error{RP0003}

This error indicates that a key or a key/value pair was expected in a
destructuring assignment, but an object method was received.

For example, the code below creates a method within an destructuring assignment:

@reach{
  const {x() { return 1 }} = {x: 2};
}

You can fix this code by simply specifying @reachin{x} in the assignment:

@reach{
  const { x } = {x: 2};
}

@error{RP0004}

This error indicates that an unsupported binary operator was encountered.
Reach is a subset of JavaScript and does not support all of the binary operators
JavaScript supports.

You can fix this by utilizing different operators or functions depending
on the logic of your program.

@error{RP0005}

@error-version[#:to "v0.1"]

This error indicates that an unsupported literal was encountered.
Reach is a subset of JavaScript and does not support all of the literals that
JavaScript supports.

@error{RP0006}

This error indicates that an unsupported unary operator was encountered.
Reach is a subset of JavaScript and does not support all of the unary operators
JavaScript supports.

You can fix this by utilizing different operators or functions depending
on the logic of your program.

@error{RP0007}

This error indicates that you are attempting to @reachin{import} a file using an absolute
path which is not supported.

You can fix this by using a relative path for your @reachin{import}.

@error{RP0008}

This error indicates that you are trying to @reachin{import} a path
that is accessing its parent directory via @reachin{..}. This type of
import is not allowed. Please view the documentation for @tech{package imports}.

For example, the code below erroneously @reachin{import}s a file from its parent
directory:

@reach{
  "reach 0.1";
  import "../a.rsh";
}

You can fix this error by moving your file, @reachin{"../a.rsh"}, to the same
directory your program is in. Then, reference it using a relative import:

@reach{
  "reach 0.1";
  import "./a.rsh";
}

@error{RP0009}

@error-version[#:to "v0.1"]

This error indicates that the Reach file could not be parsed as a module.

@error{RP0010}

This error indicates that a call-like expression was expected, but another
value was provided.

For example, the code below erroneously passes @reachin{_x}, a @tech{secret} value of
@reachin{Bob}, to @reachin{unknowable}:

@reach{
  unknowable(A, _x);
}

You can fix this by providing a call-like expression to the function:

@reach{
  unknowable(A, B(_x));
}

@error{RP0011}

@error-version[#:to "v0.1"]

This error indicates that Reach expected to parse an identifier, but none was given.

You can fix this error by adding an identifier name to the erroneous location.

@error{RX0000}

This error indicates that you are trying to inspect or use the value produced from @reachin{forall}
outside of an @reachin{assert}.

For example, the code below attempts to verify that all @reachin{UInt}s are greater than or
equal to zero via a @reachin{require}:

@reach{
  const x = forall(UInt);
  require(x >= 0);
}

This is invalid because the result of @reachin{forall} is an abstract value, which cannot exist
at runtime. You can fix this code by verifying the claim via an @reachin{assert}:

@reach{
  const x = forall(UInt);
  assert(x >= 0);
}

