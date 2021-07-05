#lang scribble/manual
@(require "lib.rkt")

@(define (error x) @section[#:tag x]{@|x|}))

@title[#:version reach-vers #:tag "ref-error-codes" #:style 'toc]{Error Codes}

This section provides an in depth explanation of the error codes produced from
the Reach compiler.

@;{
  What is a "good" error description?
  * Intuitive explanation
  * Example errorneous program
  * How to fix to fix that program (use active language.)
  * Any other advice
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
can fix this by addiing a @reachin{var} and @reachin{invariant} declaration before the loop:

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
wrappping @reachin{closeTo(Bob)} in an @tech{arrow expression}:

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

This error indicates that an object spead is occuring before the last position in a
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

This error indicates that an array spead is occuring before the last position in a
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
expected. Additionally, see the figure on @Secref["ref-programs"] for a diagram regarding the modes
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

This error indicates you are using an illegal Javascript expression in Reach. Not all Javascript
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

This error indicates that the argument provided to @reachin{Array.itoa} is
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

