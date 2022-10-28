# {#ref-error-codes} Error Codes

This section provides an in depth explanation of the error codes produced from
the Reach compiler.

## {#how-to-read-errors} How to read error messages and stack traces

Suppose you try to run the following program:

```reach
load: /hs/t/n/stack-example.rsh
md5: b65abc2e8bf51b56d0217065cc56b912
```

This program contains an error on line 8.
It tries to pass `{!rsh} "five"` to a function that expects a number.

When you try to compile this program, you will get the following error:

```
load: /hs/t/n/stack-example.txt
md5: 5d56c6a6b2a609bd949a3062eb9c2dd1
```

The first block (lines 1-4) provides the category (`error` in this case, but could be `warning`), a code (`RE0088`), and an explanation (`These types are mismatched`).
Each code has a corresponding section in this document that explains the error in more detail.
See for example @{seclink("RE0088")}.

The second block (lines 5-7) provides an excerpt of the line of code that the error occurred on.
Sometimes this block is missing when Reach cannot track the source location of your error.

The third block (lines 9-11) provides a stack trace that shows how this line of code was arrived at.
The stack trace shows the newest frames at the top and the oldest frames at the bottom.
Each frame contains information about the name of the function, where it is defined, and where it was called from.

For example, this error says that the error was detected on line 7 and column 25: the `{!rsh} +`.
This was being run inside the function defined on line 7 column 20 (the `{!rsh} =>`) whose name is `g` and that it was called on line 8 column 21 (the `{!rsh} (` after `g`).
This was being run inside the function defined on line 8 column 17 (the `{!rsh} =>`) and that it was called on line 9 column 16 (the `{!rsh} (` in the definition of `ten`).

Finally, the last block (line 13) contains a link to the explanation of the given error.

## {#how-to-read-verification-failures} How to read verification failures

If you use Reach for anything but the most trivial programs, you are going to see some verification failure messages.
Every time you compile your program, Reach is going to attempt to prove a variety of theorems about your code.
If it cannot prove those theorems, then it will generate a description of what went wrong.

This description can be exhaustively exhausting and learning how to interpret them effectively is extremely valuable in writing Reach programs effectively.
Please use this guide to aid you in learning how to do that.

You have to actually read and try to understand these messages.
They will never be "Google-able", because they are intimately connected with your particular program.

For this guide, we will take as an example the erroneous program from the fourth step of the _Rock, Paper, Scissors!_ tutorial, @{seclink("tut-5")}.
Here's a sample of and link to that program:

```
load: /examples/rps-4-attack/index-bad.rsh
md5: c0015df1e967946be01b3bdab70c9c12
range: 35-41
```

The problem with this program is that it pays out only one wager when Alice wins, rather than two.
When you compile this program, Reach detects the problem and produces a lot of output; let's look at each line and explain it.

```
load: /examples/rps-4-attack/index-bad.txt
md5: f3e303f988d9e814893c783592a61280
range: 2-2
```

First, Reach will check knowledge assertions that are introduced by `{!rsh} unknowable`.
This program does not have a violation of knowledge assertions, so there is no message.

```
load: /examples/rps-4-attack/index-bad.txt
md5: f3e303f988d9e814893c783592a61280
range: 3-3
```

Next, Reach begins checking the body of your code.
Normally, it performs this check abstractly with a "generic" network.
The generic network is more abstract (and thus more strict) than any particular connector.
For example, the value of `{!rsh} UInt.max` could be anything.
If you want to check with the actual connectors you will use, then you can use the `{!rsh} verifyPerConnector` option in `{!rsh} setOptions`.

```
load: /examples/rps-4-attack/index-bad.txt
md5: f3e303f988d9e814893c783592a61280
range: 4-4
```

Next, Reach will check your program when the participants in it are "honest".
This means that it will trust `{!rsh} assume` statements and try to enforce `{!rsh} require` statements.
You will get extra errors in this mode if you include a `{!rsh} require`, but forget the corresponding `{!rsh} assume`.

Later (line 34), Reach will check when all the participants are not honest.
This means that it will ignore all `{!rsh} assume` statements and instead turn the `{!rsh} require` statements into assumptions.
You will get extra errors in this mode if you include an `{!rsh} assume`, but forget the corresponding `{!rsh} require`.

```
load: /examples/rps-4-attack/index-bad.txt
md5: f3e303f988d9e814893c783592a61280
range: 5-9
```

When Reach finds a problem, it will output a block of text that starts like this.
First, it tells you that the verification failed (line 4) and what the mode is (line 5).
Next (line 6), it tells you what kind of theorem it was.
This is typically `assert` for an assertion that you put in your program or that Reach put in.
It is also very common to see `invariant` for when a loop invariant is being checked.
Finally (lines 7 and 8), Reach will tell you the message that was attached to the theorem and where it occurred in your program.
If Reach inserted the theorem for you, then the line number may not be in an intuitive place.

In this case, the error says that at the end of the program ("application exit"), the balance of the contract was supposed to be zero, but that the proof failed.

```
load: /examples/rps-4-attack/index-bad.txt
md5: f3e303f988d9e814893c783592a61280
range: 11-11
```

In the next section of text, Reach is going to provide information about the "violation witness".
Every theorem failure means that there is input to your program that would result in something bad happening.
The failure is called a "violation", because the theorem was supposed to always be true, but that assumption was "violated".
Reach can synthesize example inputs to provide evidence that the violation is possible; that evidence is called a "witness".

```
load: /examples/rps-4-attack/index-bad.txt
md5: f3e303f988d9e814893c783592a61280
range: 13-13
```

Reach will report the values of all the variables that are unconstrained by your program; that is, the ones that are input.
In this case, since we are checking with a generic connector, it first tells us that the maximum value of numbers on this connector is 2.
This is a ridiculously small number.
You should rarely read too much into this number... effectively, Reach is telling you that it didn't need to do a lot of numeric exploration to find this problem, since your program uses so few numbers.

```
load: /examples/rps-4-attack/index-bad.txt
md5: f3e303f988d9e814893c783592a61280
range: 15-17
```

This is a more interesting witness.
Line 14 says that Alice's `wager` field of her `{!rsh} interact` object is bound inside the program to the variable `wager`.
(`wager` is annotated with `81`, because Reach will index every variable in your program with a number, in case you use the same variable (I'm looking at you, `x`!) in different places, so that you and Reach can keep track of them.)
Line 15 says that it could equal `1`, meaning that 1 atomic network unit is being wagered.
Line 16 gives you source location information for where this variable was actually defined.
It is referring to this line:

```
load: /examples/rps-4-attack/index-bad.rsh
md5: c0015df1e967946be01b3bdab70c9c12
range: 11-11
```

This variable is free, because Alice can choose anything she wants for the wager.

```
load: /examples/rps-4-attack/index-bad.txt
md5: f3e303f988d9e814893c783592a61280
range: 18-23
```

Next, we see two more free variables: the handles that Alice and Bob choose when they play the game.
In this case (line 18), Alice chose 0 (Rock) and Bob chose 2 (Scissors), which means that Alice won, which is exactly where we know the problem in the program happens.
When Reach prints out the definition of the variable, it adds more information than you normally see in your program.
For example, we see that `getHand` has to be a `{!rsh} UInt`.
This information is in your program in the definition of `getHand`, but we can't see that right here, so Reach is adding extra context for you to better understand the witness.

```
load: /examples/rps-4-attack/index-bad.txt
md5: f3e303f988d9e814893c783592a61280
range: 25
```

After showing the violation witness, Reach switches and shows you the way that it has represented the theorem "balance zero at application exit" as a program.
This is called the "formalization" of the theorem.

This section looks like the witness section, except that the phrase `could` is going to be replaced with `would`, because these variables are not freely chosen by your users, but instead are determined by the program text.

```
load: /examples/rps-4-attack/index-bad.txt
md5: f3e303f988d9e814893c783592a61280
range: 27-28
```

Here we see Reach reporting that the `outcome` of the game would be 2 (Alice wins), which we expect.
It also reports the definition of the variable, so we can remind ourselves of that.

```
load: /examples/rps-4-attack/index-bad.txt
md5: f3e303f988d9e814893c783592a61280
range: 29-30
```

Next, we see Reach reporting what the internal variable `v110` would be.
This variable is the tuple that is on the right-hand side of line 35 of the source program (shown above) that is immediately deconstructed into the variables `forAlice` and `forBob`.
Since the program doesn't give this variable a name, Reach can't show a name and instead uses the generic `v110`.

```
load: /examples/rps-4-attack/index-bad.txt
md5: f3e303f988d9e814893c783592a61280
range: 31-33
```

Finally, we see the definition of the property being checked.
The property is that 0 must be equal to `wager + wager` (the total number of wagers) less what Alice gets (`v110[0] * wager`) and what Bob gets (`v110[1] * wager`).
Reach reports that this would be `{!rsh} false`.

If you step through these definitions, you can see why:
```
Given
  wager/81 = 1
      v110 = [1, 0]

Evaluate
  0 == (((wager/81 + wager/81) - (v110[0] * wager/81)) - (v110[1] * wager/81))
  0 == (((1 + 1) - (1 * 1)) - (0 * 1))
  0 == ((2 - 1) - 0)
  0 == (1 - 0)
  0 == 1
  false
```

Basically, we've been shown that there would be 1 atomic network unit left in the contract if we ran this program, so the theorem is false.

```
load: /examples/rps-4-attack/index-bad.txt
md5: f3e303f988d9e814893c783592a61280
range: 35-38
```

The rest of the output shows that the other mode is checked and summarizes the run.
Reach will not print the same violation multiple times.
That's why this violation is only printed in one mode, but Reach did check it and that's why Reach reports that there were 2 errors, but 1 was repeated and so its report is omitted.

## {#RA0000} RA0000

This error indicates that a program, targeting the Algorand connector, is attempting to
transfer a `{!rsh} Token` in the same consensus step it was created in. It is impossible
to perform this action because one must opt-in to receive a token on Algorand. To opt-in, one must
know the id, however the id of the token cannot be known until after the transaction that created it.

The following code erroneously tries to transfer a newly created `{!rsh} Token`:

```reach
Alice.publish();
const tok = new Token({ supply: 5 });
transfer(5, tok).to(Alice);
commit();
```

This can be fixed by performing a `{!rsh} commit` after creating the token and transferring the
token in the next consensus step:

```reach
Alice.publish();
const tok = new Token({ supply: 5 });
commit();
Alice.interact.informOfTokenId(tok);
Alice.publish();
transfer(5, tok).to(Alice);
commit();
```

The frontend can have `{!rsh} Alice` opt-in to the token in `{!rsh} informOfTokenId` by utilizing
`{!js} acc.tokenAccept()`.

## {#RA0001} RA0001

This error indicates that a program, targeting the Algorand connector, is attempting to pay a `{!rsh} Token` at the same time it is published.
It is impossible to perform this action because one must opt-in to receive a token on Algorand.
To opt-in, one must know the id, however the application cannot learn the application until after it has received the publication, which must occur after all pay transactions.

The following code has this error:

```reach
Alice.publish(tok).pay([[5, tok]]);
commit();
```

This can be fixed by performing the `{!rsh} publish` and `{!rsh} pay` in two steps:
token in the next consensus step:

```reach
Alice.publish(tok);
commit();
Alice.pay([[5, tok]]);
commit();
```

## {#RC0000} RC0000

This error indicates that the program uses a number that is beyond the range of acceptable numbers for the given
connector.

For example, the code below uses a value too large for the `{!rsh} ALGO` connector:

```reach
const y = 18446744073709551616;
```

You can fix this error by having your frontend provide the value and accessing it
via the participant interact interface:

```reach
const A = Participant('A', {
  // extend participant interact interface
  y: UInt,
});
A.only(() => {
  const y = declassify(interact.y);
});
```

Alternatively, you can fix this by not compiling to the given connector, in which
case, your application will no longer be blockchain agnostic.

## {#RE0000} RE0000

This error indicates that you provided an incorrect number of arguments to a function.

For example, the code below applies one value to `{!rsh} f`:

```reach
const f = () => { return 3 };
const x = f(5);
```

You can fix this by providing the same amount of arguments expected:

```reach
const f = () => { return 3 };
const x = f();
```

## {#RE0001} RE0001

This error indicates that a program uses an invalid assignment operator.
Reach only supports assignment using the `{!rsh} =` operator. Any other operator,
such as `{!rsh} +=, *=, ...` is not allowed.

For example, the code below erroneously tries to re-assign `{!rsh} x` at the end of
a `{!rsh} while` loop:

```reach
x *= 2;
continue;
```

You can fix this by explicitly writing out the operation on the right hand
side of `{!rsh} =`:

```reach
x = x * 2;
continue;
```

Keep in mind, that the assignment operator is a form of mutation and is only allowed
immediately before a `{!rsh} continue`.

## {#RE0002} RE0002

This error indicates that a program uses an invalid statement.
Reach is a strict subset of JavaScript and does not accept every statement
that is valid JavaScript. It may be necessary to express your program
with different constructs than you would JavaScript.

For example, the code below erroneously uses a `{!js} for` loop, which is not
supported in Reach:

```reach
for (let i = 0; i < arr.length; i++) {
  // ...
}
```

You can fix this by either using a `{!rsh} while` loop or a combination of
`{!rsh} Array.iota` and `{!rsh} Array.map/Array.forEach`:

```reach
Array.iota(arr.length).map((i) => {
  // ...
});
```

## {#RE0003} RE0003

This error indicates that a statement block returns a non-null value although a `{!rsh} null` value is expected.
The block should either use a `{!rsh} return null;` statement or no return statement at all.

This statement block has a `{!rsh} return` of 1, but it should not have a `{!rsh} return`.

``` reach
load: /hs/t/n/Err_Block_NotNull.rsh
md5: 3176239134a51698e6f8ab7e6ec0765c
range: 3 - 6
```

This error can be corrected by putting the `{!rsh} return` inside of a `{!rsh} const` statement block:

```reach
load: /hs/t/y/data_null.rsh
md5: f5a4d30b4eb5ea8890b3889c5f4f06c6
range: 5 - 14
```

## {#RE0004} RE0004

This error indicates that the program uses `{!rsh} var` incorrectly.
In Reach, `{!rsh} var` is only allowed immediately before a while loop and its `{!rsh} invariant`.

For example, this code erroneously tries to declare `{!rsh} x` as a mutable variable then
re-assign it at some point:

```reach
var x = 0;
if (iAmLegend) {
  x = 5;
}
```

You can fix this by using `{!rsh} const` and either creating fresh variables or collapsing the logic if simple enough:

```reach
const x = 0;
const xPrime = iAmLegend ? 5 : x
// or
const x = iAmLegend ? 5 : 0;
```

## {#RE0005} RE0005

This error indicates an incorrect use of `{!rsh} while`. `{!rsh} while` must be immediately
prefaced by a `{!rsh} var` and `{!rsh} invariant` declaration.

For example, this code erroneously tries to run a continuous loop where Alice pays `{!rsh} 1` network token
per loop:

```reach
while (true) {
  commit();
  Alice.pay(1);
  continue;
}
```

Reach requires the `{!rsh} invariant` to reason about the `{!rsh} while` loop during verification. You
can fix this by adding a `{!rsh} var` and `{!rsh} invariant` declaration before the loop:

```reach
var rounds = 0;
invariant(balance() == rounds);
while (true) {
  commit();
  Alice.pay(1);
  rounds = rounds + 1;
  continue;
}
```

## {#RE0006} RE0006

@{errver(false, "v0.1")}

This error indicates that `{!rsh} return` may not be used within the current statement block.

## {#RE0007} RE0007

This error indicates that the `{!rsh} timeout` branch of a statement such
as `{!rsh} publish, pay, fork` has been given the wrong arguments.

For example, the following code erroneously attempts to `{!rsh} closeTo(Bob)` in the event that
`{!rsh} Alice` does not publish in time:

```reach
Alice
  .publish()
  .timeout(5, closeTo(Bob));
```

However, the second argument of the `{!rsh} timeout` branch must be a syntactic thunk.
You can fix this by wrapping `{!rsh} closeTo(Bob)` in an arrow expression:

```reach
Alice
  .publish()
  .timeout(5, () => closeTo(Bob));
```

## {#RE0008} RE0008

This error indicates that a `{!rsh} timeout` branch of a statement such as
`{!rsh} publish, pay, fork` has not been given a block of code to execute in the
event of a `{!rsh} timeout`.

For example, the code below erroneously provides a `{!rsh} timeout` delay, but does
not specify a function to run if the timeout occurs:

```reach
A.pay(0)
 .timeout(1);
```

You can fix this by providing a function as a second argument to `{!rsh} timeout`:

```reach
A.pay(0)
 .timeout(1, () => {
    // ...
  });
```

## {#RE0009} RE0009

This error indicates that the pay amount provided states the amount of
network tokens more than once.

For example, the code below erroneously provides two atomic values in the pay amount,
which are both interpreted as the amount of network tokens to pay:

```reach
A.pay([ amt, amt, [ amt, tok ]]);
```

You can fix this by deleting one of the atomic values:

```reach
A.pay([ amt, [ amt, tok ]]);
```

## {#RE0010} RE0010

@{errver(false, "v0.1.8")}

This error indicates that the pay amount provided states the amount of
a specific non-network token more than once.

For example, the code below erroneously provides two tuples in the pay amount,
both of which specify the amount of the same token:

```reach
A.pay([ amt, [amt, tok ], [ amt, tok ] ]);
```

You can fix this by deleting one of the tuples:

```reach
A.pay([ amt, [amt, tok ] ]);
```

## {#RE0011} RE0011

This error indicates that the pay amount provided does not provide values of the correct
type.

For example, the code below erroneously provides a pay amount that consists of a one element
tuple:

```reach
A.pay([ amt, [ amt ] ];)
```

However, a tuple in a pay amount must specify the amount and the `{!rsh} Token`. You can fix this
by adding the `{!rsh} Token` to the tuple:

```reach
// tok : Token
A.pay([ amt, [ amt, tok ] ];)
```

## {#RE0012} RE0012

This error indicates that a participant interact interface field has a type that is not first-order.
That is, an interact function has a return type of a function, which is not allowed in Reach.

For example, the code below erroneously provides an interact function that returns another function:

```reach
const A = Participant('A', {
  'curriedAdd': Fun([UInt], Fun([UInt], UInt));
});
```

the frontend may look like this:

```js
await Promise.all([
  backend.A(ctcA, {
    curriedAdd: (x) => (y) => x + y;
  })
]);
```

You can fix this by decoupling the functions and calling them sequentially. This technique requires
changes to the frontend as well since we are changing the signature:

```reach
const A = Participant('A', {
  'curriedAdd1': Fun([UInt], Null);
  'curriedAdd2': Fun([UInt], UInt);
});
// ...
A.only(() => {
  interact.curriedAdd1(1);
  const result = declassify(interact.curriedAdd2(1));
});
```

the frontend may look like this:

```js
let x_;
await Promise.all([
  backend.A(ctcA, {
    curriedAdd1: (x) => { x_ = x; },
    curriedAdd2: (y) => { return x_ + y; }
  })
]);
```

## {#RE0013} RE0013

This error indicates that you provided an unrecognized option to `{!rsh} setOptions`.
There is most likely a typo in your code.

Please review the recognized options in the documentation for `{!rsh} setOptions`.

## {#RE0014} RE0014

This error indicates that you did not provide an acceptable value for a specific option
in `{!rsh} setOptions`.

Please review the eligible values listed in the documentation for `{!rsh} setOptions`.

## {#RE0015} RE0015

This error indicates that your participant interact interface does not provide a `{!rsh} Type`
for a given field.

For example, in the erroneous code below, `{!rsh} x` is assigned `{!rsh} 3` in the participant interact interface
of `{!rsh} Alice`:

```reach
const Alice = Participant('Alice', {
  'x': 3,
});
```

However, the interact interface specifies the type of values that will be provided at runtime. You can fix this by
either making `{!rsh} x` a variable within Alice's scope inside the program:

```reach
const Alice = Participant('Alice', {});
init();
Alice.only(() => {
  const x = 3;
});
```

or by putting `{!rsh} 3` as the value of `{!rsh} x` in your frontend and adjusting the
participant interact interface to list the type:

```reach
const Alice = Participant('Alice', {
  'x': UInt,
});
```

## {#RE0016} RE0016

This error indicates that the arguments passed to `{!rsh} Reach.App` are incorrect.
`{!rsh} Reach.App` accepts a single thunk as its argument.

For example, the code below erroneously declares a `{!rsh} Reach.App` without too
many arguments:

```reach
export const main = Reach.App({}, () => {
});
```

You can fix this by ensuring only one argument is passed to `{!rsh} Reach.App`,
which is a function with no arguments:

```reach
export const main = Reach.App(() => {
});
```

## {#RE0017} RE0017

This error indicates that the name of the `{!rsh} Participant` or `{!rsh} View` provided
is invalid. These names must satisfy the regex `{!rsh} [a-zA-Z][_a-zA-Z0-9]*`.

For example, the code below provides a Participant name that is unsatisfactory:

```reach
const P = Participant('Part 4!', {});
```

You can fix this by removing any illegal characters and replacing spaces with underscores:

```reach
const P = Participant('Part_4', {});
```

## {#RE0018} RE0018

This error indicates that an invalid expression is used on the left hand side of an
assignment.

For example, the code below erroneously puts an arithmetic expression on the left hand
side of an assignment:

```reach
const (f + 1) = 10;
```

You can fix this by moving all the arithmetic to the right hand side, leaving only the variable
on the left:

```reach
const f = 10 - 1;
```

## {#RE0019} RE0019

This error indicates that an object spread is occurring before the last position in a
destructuring assignment. It must come last due to the fact it binds the remaining
elements to the given variable.

For example, the code below erroneously attempts to destructure one element `{!rsh} y`
and the remaining elements into `{!rsh} x`:

```reach
const {...x, y} = {x: 1, y: 2, z: 3};
```

You can fix this by moving `{!rsh} ...x` to the last position:

```reach
const {y, ...x} = {x: 1, y: 2, z: 3};
```

## {#RE0020} RE0020

This error indicates that an array spread is occurring before the last position in a
destructuring assignment. It must come last due to the fact it binds the remaining
elements to the given variable.

For example, the code below erroneously attempts to destructure one element `{!rsh} y`
and the remaining elements into `{!rsh} x`:

```reach
const [...x, y] = [1, 2, 3];
```

You can fix this by moving `{!rsh} ...x` to the last position:

```reach
const [y, ...x] = [1, 2, 3];
```

## {#RE0021} RE0021

This error indicates that the compiler expected to receive a closure, but
it was given a different value.

For example, the code below erroneously attempts to provide a value for
the `{!rsh} match` case of a nullary `{!rsh} Data` constructor:

```reach
Maybe(UInt).None().match({
  None: 0,
  Some: (x) => x
});
```

You can fix this by wrapping the value `{!rsh} 0` in an arrow expression because
`{!rsh} match` expects all cases to be bound to closures:

```reach
Maybe(UInt).None().match({
  None: () => 0,
  Some: (x) => x
});
```

## {#RE0022} RE0022

This error indicates that there was an invalid declaration. This error will
occur when attempting to bind multiple variables within a single `{!rsh} const`.

For example, the code below erroneously attempts to bind `{!rsh} x` and `{!rsh} y`
within one `{!rsh} const` assignment:

```reach
const x = 1, y = 2;
```

You can fix this by breaking apart the declarations into two `{!rsh} const` statements:

```reach
const x = 1;
const y = 2;
```

## {#RE0023} RE0023

@{errver(false, "v0.1")}

This error indicates that there was an invalid declaration.

## {#RE0024} RE0024

This error indicates that there is an attempt to unpack an `{!rsh} array`, but the binding
does not expect the same amount of values that the `{!rsh} array` contains.

For example, the code below erroneously tries to unpack an `{!rsh} array` of 3 values into
2 variables:

```reach
const [ x, y ] = [ 1, 2, 3 ];
```

You can fix this by either binding or ignoring, via `{!rsh} _`, the last element of the `{!rsh} array`:

```reach
const [ x, y, _ ] = [ 1, 2, 3 ];
```

## {#RE0025} RE0025

This error indicates that there is an attempt to access a field of an `{!rsh} object`
that does not exist. Ensure that you are referring to the correct name, or
add the needed field to the `{!rsh} object` if necessary.

## {#RE0026} RE0026

This error indicates that the `{!rsh} continue` statement is used outside of a
`{!rsh} while` loop.

Example:

```reach
load: /hs/t/n/continue.rsh
md5: 27ee6a16a890c05d6fe16640df03949a
range: 4 - 9
```

To fix this issue, delete the erroneous `{!rsh} continue`,
or move it to the end of your `{!rsh} while` loop.

Example:

```reach
load: /hs/t/y/while_multi_inv.rsh
md5: ba7262e27a3f5f48720b3fd88dbf16cb
range: 14 - 24
```

## {#RE0027} RE0027

This error indicates that there is an attempt to consult the consensus time before the first publication.
This situation is not allowed, because before the first publication, there is no consensual agreement on time in the decentralized computation.

```reach
export const main = Reach.App(() => {
  const A = Participant('Alice', {});
  init();
  wait(relativeTime(1));
});
```

You can fix this by having a `{!rsh} Participant` `{!rsh} publish` first:

```reach
export const main = Reach.App(() => {
  const A = Participant('Alice', {});
  init();
  A.publish();
  wait(relativeTime(1));
});
```

## {#RE0028} RE0028

This error indicates that a the variable update inside of a loop, e.g. `{!rsh} while`,
is attempting to mutate variables that are not mutable. For a `{!rsh} while` loop, this
means the variable was not declared with `{!rsh} var` prior to the loop.

For example, the code below erroneously attempts to mutate `{!rsh} y` which has not been
defined via `{!rsh} var`:

```reach
var [x] = [1];
invariant(true);
while(x < 2) {
  [ x, y ] = [ x + 1, x ];
  continue;
}
```

You can fix this by either deleting `{!rsh} y` or adding it to the variable list:

```reach
var [ x, y ] = [ 1, 1 ];
invariant(true);
while(x < 2) {
  [ x, y ] = [ x + 1, x ];
  continue;
}
```

## {#RE0029} RE0029

This error indicates an attempt to bind a `{!rsh} ParticipantClass` to a
specific `{!rsh} Address`.

For example, the example code below erroneously tries to `{!rsh} set` a `{!rsh} ParticipantClass`
to a specific address:

```reach
const C = ParticipantClass('C', {});
// ...
C.set(addr);
```

You can fix this by using a `{!rsh} Participant`, which may be associated with a single address:

```reach
const C = Participant('C', {});
// ...
C.set(addr);
```

## {#RE0030} RE0030

This error indicates an attempt to re-bind a `{!rsh} Participant` to another `{!rsh} Address`.
Once a `{!rsh} Participant` is bound to an `{!rsh} Address`, either by making a publication
or explicitly via `{!rsh} Participant.set`, they may not be re-bound.

For example, the code below erroneously has `{!rsh} Bob` make a publication, then later,
attempts to bind him to a specific address:

```reach
Bob.publish();
// ...
Bob.set(addr);
```

You can fix this by deleting one of the statements (depending on the logic of your application).

## {#RE0031} RE0031

This error indicates that you are attempting to use a specific statement or expression in the wrong
mode. Consult the documentation for the specific keyword to learn more about what mode is
expected. Additionally, see the figure on @{seclink("ref-programs")} for a diagram regarding the modes
of a Reach application.

## {#RE0032} RE0032

This error indicates that you are attempting to mutate a variable in an inappropriate place.
Variable mutation is only allowed to occur on variables declared via `{!rsh} var` and immediately
before a `{!rsh} continue` statement of a loop.

For example, the code below attempts to mutate a loop variable improperly:

```reach
var [ x ] = [ 0 ];
invariant(balance() == 0);
while (true) {
  commit();
  x = 1;
  // ...
  continue;
}
```

You can fix this issue by moving the mutation directly before the `{!rsh} continue`:

```reach
var [ x ] = [ 0 ];
invariant(balance() == 0);
while (true) {
  commit();
  // ...
  x = 1;
  continue;
}
```

## {#RE0033} RE0033

@{ref("rsh", "async")}@{ref("rsh", "await")}@{ref("rsh", "break")}@{ref("rsh", "catch")}@{ref("rsh", "class")}@{ref("rsh", "debugger")}@{ref("rsh", "delete")}@{ref("rsh", "do")}@{ref("rsh", "enum")}@{ref("rsh", "extends")}@{ref("rsh", "for")}@{ref("rsh", "in")}@{ref("rsh", "instanceOf")}@{ref("rsh", "let")}@{ref("rsh", "of")}@{ref("rsh", "static")}@{ref("rsh", "throw")}@{ref("rsh", "try")}@{ref("rsh", "with")}@{ref("rsh", "yield")}

This error indicates you are using an illegal JavaScript expression in Reach. Not all JavaScript
expressions are valid Reach, as they are not applicable to the language.

## {#RE0034} RE0034

This error indicates that there is nowhere to return to in the current statement block.
This may occur if you write a `{!rsh} return` statement at the top level of a file
or if you've already wrote a `{!rsh} return` statement.

For example, the code below has two `{!rsh} return` statements, the first of which will
always occur, since it is not within a conditional:

```reach
const f = () => {
  return 0;
  return 1;
};
```

You can fix this by removing the second `{!rsh} return` which is dead code:

```reach
const f = () => {
  return 0;
}
```

## {#RE0035} RE0035

This error indicates that a value, which is not a function, is being applied as if it were a function.
Ensure you are writing the correct name of the function you intend to use.

For example, the code below has two variables: `{!rsh} f` and `{!rsh} g`:

```reach
const f = () => 2;
const g = 2;
const h = g();
```

`{!rsh} g` is being applied as if it were a function, although we really intended on calling `{!rsh} f`.
This can be fixed by ensuring we call a function:

```reach
const f = () => 2;
const g = 2;
const h = f();
```

## {#RE0036} RE0036

@{errver(false, "v0.1.9")}

It means the same thing as @{seclink('RE0035')}.

## {#RE0037} RE0037

This error indicates that a value, which is not an `{!rsh} Object`, is being
treated as if it were an `{!rsh} Object`. This error occurs when you try to access
a field of an erroneous value. This issue is most likely caused by a typo in your
program.

## {#RE0038} RE0038

This error indicates that a value, which is not an `{!rsh} Array` or `{!rsh} Tuple`, is being
treated as if it were. This error occurs when you try to access
an element of an erroneous value. This issue is most likely caused by a typo in your
program.

## {#RE0039} RE0039

This error indicates that there is an attempt to dereference an `{!rsh} Array` or `{!rsh} Tuple`
with a non-numerical value. You must use a value of type `{!rsh} UInt` to dereference
an `{!rsh} Array`.

## {#RE0040} RE0040

This error indicates that you are using a dynamic value to dereference a value which is not an `{!rsh} Array`.
This issue is most likely caused by a typo. Please ensure you are dereferencing an `{!rsh} Array`.

## {#RE0041} RE0041

This error indicates that there is an attempt to statically dereference an `{!rsh} Array` beyond its bounds.
Ensure you are using an index that is between `0` and `1` less than the length of the `{!rsh} Array`.

## {#RE0042} RE0042

This error indicates that there is an attempt to reference an identifier that is not in scope. This issue may be
caused by a typo, a scoping issue, or a missing `{!rsh} import`.

For example, the code below declares a function with a variable `{!rsh} x` declared within it. Attempting to reference
`{!rsh} x` outside of the function will result in an error:

```reach
const f = () => {
  const x = 5;
}
const y = x;
```

You can fix this issue by returning the value of `{!rsh} x` from the function:

```reach
const f = () => {
  const x = 5;
  return x;
}
const y = f();
```

If you are attempting to use a value from a library, simply add the necessary `{!rsh} import` to the top
of the Reach file.

## {#RE0043} RE0043

This error indicates that there is a mismatch between the expected security levels of a variable
and the actual one provided. This may happen if you use a public variable where a secret
is expected, or vice versa.

For example, the code below erroneously declassifies the variable `{!rsh} x`, which is not secret:

```reach
const x = 0;
A.only(() => {
  const y = declassify(x);
});
```

You can fix this issue by simply assigning `{!rsh} y` to `{!rsh} x`.

## {#RE0044} RE0044

This error indicates that you provided an incorrect number of arguments to a function. You can fix this
by providing the same amount of arguments expected.

## {#RE0045} RE0045

This error indicates that an anonymous function was provided a name, which is not allowed.

For example, the code below names the anonymous function `{!rsh} m`:

```reach
const x = array(UInt, [0, 1, 2]);
const y = x.map(function m(i){ return i + 1; });
```

You can fix this by removing the function name:

```reach
const x = array(UInt, [0, 1, 2]);
const y = x.map(function (i){ return i + 1; });
```

## {#RE0046} RE0046

This error indicates that there was an invalid syntax used for an `{!rsh} import`.
The acceptable `{!rsh} import` formats are defined in the documentation for the keyword.

For example, the code below erroneously performs a default `{!rsh} import`:

```reach
import blah from 'sample_lib.rsh';
```

You can fix this code by explicitly importing the bindings you want:

```reach
import {a,b,c} from 'sample_lib.rsh';
```

or by binding all the exports to an identifier:

```reach
import * as lib from 'sample_lib.rsh';
```

## {#RE0047} RE0047

@{errver(false, "v0.1")}

This error indicates that there was a `{!rsh} return` statement
at the top level.

## {#RE0048} RE0048

This error indicates that the Reach file does not have a header at
the top of the file. The first top level statement of a Reach module
must indicate what version of Reach the file uses.

For example, the code below erroneously exports an application
without specifying what version of Reach it uses:

```reach
export const main = Reach.App(() => {});
```

Fix this by adding a header to the file:

```reach
"reach 0.1";

export const main = Reach.App(() => {});
```

## {#RE0049} RE0049

This error indicates that an `{!rsh} object` has been given a field
that is not an identifier or a `{!rsh} string`.

For example, the code below erroneously uses a dynamic string as an object key:

```reach
A.only(() => {
  const x = declassify(interact.x);
});
A.publish(x);
const o = {
  [x]: 4,
};
```

You can fix this by using a static string as the key.

```reach
load: /hs/t/y/dynamic_pay.rsh
md5: d2b6bbbbc740d81db1ba2097f250a1b5
range: 21 - 24
```

This code snippet uses a static string as an object key instead of using a dynamic string.
The object keys `T` and `S` are computable at compile time.

## {#RE0050} RE0050

@{errver(false, "v0.1")}

This error indicates an `{!rsh} Object` has an incorrect number of values
associated with a field.

## {#RE0051} RE0051

This error indicates that the field of an `{!rsh} Object` uses the incorrect
syntax for defining a function.

For example, the code below declares a field as a function with the following syntax:

```reach
const o = {
  f() {
    return 1;
  }
};
```

You can fix this by using the following arrow expression syntax:

```reach
const o = {
  f: () => { return 1; }
}
```

## {#RE0052} RE0052

This error indicates that a `{!rsh} UInt` has been used as the key of an
`{!rsh} Object`. However, only identifiers and values of type `{!rsh} Bytes`
are valid object keys.

``` reach
load: /hs/t/n/Err_Obj_IllegalNumberField.rsh
md5: eece701143d5714f06ba03d48860eae2
range: 3 - 3
```

You can fix this issue by replacing the erroneous key with a static string.

``` reach
load: /hs/t/y/objects.rsh
md5: 7ae70fab31a04e51d2150c6507ecd048
range: 9 - 9
```

## {#RE0053} RE0053

This error indicates that you are attempting to spread a value that is not
an object. This issue is most likely caused by a typo in your program.

## {#RE0054} RE0054

This error indicates that the argument provided to `{!rsh} Array.iota` is
not static. `{!rsh} Array.iota` requires its argument to be computable at
compile time.

You can fix this issue by providing a static `{!rsh} UInt` to the function.

## {#RE0055} RE0055

This error occurs when you provide a primitive operation with the incorrect
number of arguments or arguments of the wrong type. Please review the documentation
for the function you are attempting to use and provide it with the correct arguments.

## {#RE0056} RE0056

This error indicates that you are attempting to create a variable, although another variable in the scope uses the same name.

Example:

```reach
load: /hs/t/n/Err_Shadowed.rsh
md5: 87f19538850309dc0a0093898a961b8d
range: 3 - 8
```

In Reach, identifier shadowing is not allowed.
You can fix this issue by renaming your variable or moving one of the variable declarations to another scope where it does not conflict with the other.

Example:

```reach
load: /hs/t/y/Fix_Shadowed.rsh
md5: 1c5e96ed4c3b89234f94d4a9b7e9649a
range: 3 - 8
```

## {#RE0057} RE0057

This error indicates that the compiler expected the tail of a statement block
to be empty, but it wasn't. This issue may arise if there are statements beyond
a `{!rsh} return` or `{!rsh} exit` statement. These statements are dead code
and you can fix this issue by deleting them.

```reach
load: /hs/t/n/Err_Return_MustBeTail.rsh
md5: 1aa6de133c2a6fd17a95b79a614adee4
range: 13 - 20
```

The `{!rsh} return` statement on line 19 will never be executed.
The `{!rsh} return` statement in the `{!rsh} if ... else ...` block already handled what is needed for the `g` `{!rsh} function`.

To fix the code remove line 19:

```reach
load: /hs/t/y/Fix_Return_MustBeTail.rsh
md5: 8847375616a4a00808457af3868d9825
range: 13 - 19
```

## {#RE0058} RE0058

@{errver(false, "v0.1")}

This error indicates that you tried to use the `{!rsh} publish` keyword twice
in a publication.

## {#RE0059} RE0059

@{errver(false, "v0.1")}

This error indicates that there is a function at the top level without a name.
You can fix this by naming your function.

## {#RE0060} RE0060

@{errver(false, "v0.1")}

This error indicates that there is an illegal `{!rsh} while` loop `{!rsh} invariant`.
You can fix this issue by providing only one expression to `{!rsh} invariant`.

## {#RE0061} RE0061

@{errver(false, "v0.1")}

This error indicates that `{!rsh} Participant.only` was not supplied a single thunk
as its argument.

You can fix this by providing the expected value to the function.

## {#RE0062} RE0062

@{errver(false, "v0.1")}

This error indicates that `{!rsh} each` was not given a `{!rsh} Tuple` of `{!rsh} Participant`s
as its first argument.

You can fix this by providing the expected value to the function.

## {#RE0063} RE0063

This error indicates that a given function expects a `{!rsh} Participant` or `{!rsh} ParticipantClass`
as an argument, but it was given something else.

For example, the code below erroneously provides `{!rsh} false` instead of a `{!rsh} Participant` to
`{!rsh} unknowable`:

```reach
A.only(() => {
  const _x = interact.x;
});
unknowable(false, A(_x));
```

You can fix this by passing a `{!rsh} Participant` as the first argument to `{!rsh} unknowable`:

```reach
A.only(() => {
  const _x = interact.x;
});
unknowable(B, A(_x));
```

## {#RE0064} RE0064

This error indicates that the program is attempting to transfer funds to a `{!rsh} Participant`
that is not yet bound to an `{!rsh} Address`.

For example, the code below transfers funds to `{!rsh} Bob` before he has a set `{!rsh} Address`:

```reach
Alice.publish().pay(100);
transfer(100).to(Bob);
```

You can fix this by using `{!rsh} Participant.set` first or having `{!rsh} Bob` publish before the
`{!rsh} transfer`:

```reach
Bob.publish();
commit();
Alice.publish().pay(100);
transfer(100).to(Bob);
```

## {#RE0065} RE0065

This error indicates that you are attempting to `{!rsh} transfer` funds to a `{!rsh} ParticipantClass`.
This is not possible because `{!rsh} transfer` expects a single `{!rsh} Address` to transfer to.

For example, the code below erroneously attempts to transfer the `{!rsh} balance` of the contract to a class:

```reach
const Alice = Participant('Alice', {});
const Bob   = ParticipantClass('Bob', {});
init();
Alice.publish().pay(100);
transfer(100).to(Bob);
```

You can fix this code by specifying a specific `{!rsh} Address` to use. For example, the
class could `{!rsh} race` to specify their own address:

```reach
const Alice = Participant('Alice', {});
const Bob   = ParticipantClass('Bob', {});
init();
Alice.publish().pay(100);
commit();
Bob.only(() => {
  const b = this;
});
Bob.publish(b);
transfer(100).to(b);
commit();
```

## {#RE0066} RE0066

This error indicates that the state of the program differs in the continuation of a
branching statement. That is, if a Reach program may execute multiple different code paths at runtime, the continuation of those branches must make the same assumption about state.

For example, this error may be caused by having one branch end in consensus step and
the other in a step. You can fix this by ensuring both branches end in the same mode.

Example:

```reach
load: /hs/t/n/Err_IncompatibleStates.rsh
md5: b2e9bd138a8c42e7e31f4e5928136a46
range: 38 - 51
```

This fork ends in two different states, Step and Consensus, but after removing the `{!rsh} commit` both fork statements end in Consensus Step.

Another example is a `{!rsh} Participant` makes their first publication in the branch
of a conditional. You can fix this by having the `{!rsh} Participant` make their first
publication before the conditional statement.

## {#RE0067} RE0067

This error indicates that you are attempting to bind a secret value to an identifier
of the wrong format. secret identifiers must be prefixed with `{!rsh} _`.

For example, the code below erroneously assigns a secret value to a public identifier, `{!rsh} x`:

```reach
A.only(() => {
  const x = interact.x;
});
```

You can fix this by either changing the identifier to start with `{!rsh} _` or using `{!rsh} declassify`
to make the value public:

```reach
A.only(() => {
  const _x = interact.x;
});
// or
A.only(() => {
  const x = declassify(interact.x);
});
```

## {#RE0068} RE0068

This error indicates that you are attempting to bind a public value to an identifier
of the wrong format. public identifiers cannot be prefixed with `{!rsh} _`.

For example, the code below erroneously assigns a public value to a secret identifier, `{!rsh} _x`:

```reach
const _x = 1;
```

You can fix this by removing the `{!rsh} _` prefix:

```reach
const x = 1;
```

## {#RE0069} RE0069

This error indicates that you are attempting to read the value of `{!rsh} _`. Any binding to `{!rsh} _` is
ignored and therefore cannot be read from.

Example:

```reach
load: /hs/t/n/Err_Eval_LookupUnderscore.rsh
md5: 2ccbb40b7b71cfa9ba9213a0c840524f
range: 3 - 6
```

You can fix this by using another identifier and referencing it as usual.

```reach
load: /hs/t/y/underscore_unread.rsh
md5: d3ed4b3338138e2f98c68d57d054f023
range: 5-6
```

## {#RE0070} RE0070

This error indicates that you are attempting to spread a value as if it were a
`{!rsh} Tuple`, `{!rsh} Array`, or `{!rsh} Struct`, but it is not. This issue
is likely caused by a typo in your code.

For example, the code below erroneously spreads the wrong values:

```reach
const xi = 1;
const xa = [2, 3];
add(1, ...xi);
```

You can fix this code by spreading a tuple-like value for the second argument of `{!rsh} add`:

```reach
const xi = 1;
const xa = [2, 3];
add(1, ...xa);
```

## {#RE0071} RE0071

This error indicates that the two `{!rsh} Array`s given to `{!rsh} Array.zip` are not of
equal length. You can fix this error by providing two `{!rsh} Array`s of equal length to
the function.

## {#RE0072} RE0072

This error indicates that a `{!rsh} switch` statement was supplied with a value that is not a
`{!rsh} Data` instance.

For example, the code below expects a `{!rsh} Maybe` type, but is erroneously provided with a `{!rsh} UInt`:

```reach
const f = (mx) => {
  switch (mx) {
    case Some: { return mx; }
    case None: { return 0; }
  };
};

f(1);
```

You can fix this code by providing a value with the correct `{!rsh} Type` to the `{!rsh} switch` statement:

```reach
const f = (mx) => {
  switch (mx) {
    case Some: { return mx; }
    case None: { return 0; }
  };
};

f(Maybe(UInt).Some(1));
```

## {#RE0073} RE0073

This error indicates that there are multiple cases for the same variant in a `{!rsh} switch` statement or
`{!rsh} match` expression.

Example:

```reach
load: /hs/t/n/Err_Switch_DoubleCase.rsh
md5: 0cf84c9f7a8802c527196b14bce8092c
range: 10 - 16
```

You can fix this error by deleting one of the branches, ensuring there is only one branch per variant.

Example:

```reach
load: /hs/t/y/Fix_Switch_DoubleCase.rsh
md5: 80c1e033cdc7da09d32bc1fa8f126478
range: 10 - 15
```

## {#RE0074} RE0074

This error indicates that a `{!rsh} switch` statement or `{!rsh} match` expression does not have a case
for every variant of a `{!rsh} Data` instance.

Example:

```reach
load: /hs/t/n/Err_Switch_MissingCases.rsh
md5: bcf5b2427943c6e5b432cf8d8e54bf8d
range: 9 - 14
```

You can fix this issue by adding the missing cases listed
in the error message.

Example:

```reach
load: /hs/t/y/switch_cases.rsh
md5: 1442b3c48fa0604f624a312562a2fd2c
range: 9 - 15
```

## {#RE0075} RE0075

This error indicates that a `{!rsh} switch` statement or `{!rsh} match` expression contains cases
for unknown variants. These erroneous variants are not listed in the `{!rsh} Data` definition.

Example:

```reach
load: /hs/t/n/Err_Switch_ExtraCases.rsh
md5: 403e1a9741e81871ecf6b3af69405cd9
range: 11 - 15
```

`Mone` is not added to the `{!rsh} Data` field and so is an unknown variant.
You can fix this issue by adding the unknown variant to the `{!rsh} Data` definition or removing the case.

```reach
load: /hs/t/y/data.rsh
md5: 4e8539b9c12ac8b7f52a3dafd60d5497
range: 19 - 21
```

## {#RE0076} RE0076

This error indicates that the `{!rsh} Type` of a value you provided to a function or operation does not match the expected `{!rsh} Type`.

For example, the code below erroneously provides a number as the second argument to `{!rsh} assert`:

```reach
load: /hs/t/n/Err_Expected.rsh
md5: 9f0b04d6b134b00294080d9e783241ff
range: 10-10
```

However, the second argument of `{!rsh} assert` is expected to be of type `{!rsh} Bytes`.
You can fix this issue by providing a value of the correct type:

```reach
load: /hs/t/y/shared_checks_api_call.rsh
md5: 360d86d0c94bb43c7f51cba6565aa913
range: 23-23
```

## {#RE0077} RE0077

This error indicates that the depth of recursion for a function call exceeded the limit allowed.
This issue may indicate that the recursive function does not have a base case.

Example:

```reach
load: /hs/t/n/Err_RecursionDepthLimit.rsh
md5: 6a77966576b13f80288c1bb40c96fa2f
range: 3 - 5
```

You can fix this issue by rewriting the recursive function as an iterative set of statements, e.g. a `{!rsh} while` loop.

```reach
load: /hs/t/y/while_preamble.rsh
md5: 6876b2cb3443237b8594c8100a2124ab
range: 13 - 17
```

## {#RE0078} RE0078

This error indicates that the program is no longer live by the time it reaches a publication.
That is, the program will have already `{!rsh} exit`ed before the given point.

For example, the code below will always `{!rsh} exit` before calling `{!rsh} publish`:

```reach
const f = () => { exit(); };
f();
Alice.publish();
commit();
```

You can fix this code by wrapping the `{!rsh} exit` in a conditional:

```reach
const f = () => { exit(); };
if (/* ... */) {
  f();
}
Alice.publish();
commit();
```

## {#RE0079} RE0079

@{errver(false, "v0.1")}

This error indicates that a statement is being used in place of an expression.
Refer to the documentation for the statement you are attempting to use for more
information on how to use it.

## {#RE0080} RE0080

This error indicates that there is an attempt to conditionally transition to consensus
without a `{!rsh} timeout`. When making a conditional publication, such as `{!rsh} A.publish(x).when(shouldPublish)`,
there needs to be a timeout associated with the publication if `{!rsh} shouldPublish` is not statically `{!rsh} true`.

In `{!rsh} parallelReduce` or `{!rsh} fork`, a `{!rsh} timeout` is required unless one `{!rsh} Participant`
always races, the `{!rsh} when` field in their `PUBLISH_EXPR` is statically `{!rsh} true`, or if one
`{!rsh} ParticipantClass` or `{!rsh} API` will attempt to race.

:::note
Since `{!rsh} ParticipantClass` is being deprecated, it is preferable to use `{!rsh} API`.
:::

For example, the code below erroneously attempts to publish a value if a certain condition holds:

```reach
A.only(() => {
  const { x, shouldPublish } = declassify(interact.getParams());
});
A.publish(x)
 .when(shouldPublish);
commit();
```

You can fix this issue by providing a `{!rsh} timeout` case for

```reach
A.only(() => {
  const { x, shouldPublish, deadline } = declassify(interact.getParams());
});
A.publish(x)
 .when(shouldPublish)
 .timeout(deadline, () => closeTo(Bob));
commit();
```

## {#RE0081} RE0081

This error indicates that the result of `PUBLISH_EXPR` for a `{!rsh} fork` or `{!rsh} parallelReduce`
is not of the right `{!rsh} Type`. It is expected to be an `{!rsh} Object` with a `{!rsh} when` field,
and optionally a `{!rsh} msg` field.

For example, the code below erroneously tries to publish a value in a `{!rsh} parallelReduce` case:

```reach
parallelReduce(/* ... */)
  // ...
  .case(Alice,
    (() => {
      const x = declassify(interact.x);
      return x;
    })
  // ...
  )
```

This code can be fixed by using an `{!rsh} Object` and assigning the value to be published to
the `{!rsh} msg` field of the object:

```reach
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
```

## {#RE0082} RE0082

This error indicates that the parameters of a `CONSENSUS_EXPR` in a `{!rsh} fork` or
`{!rsh} parallelReduce` are incorrect. The function provided should either accept zero
parameters or one parameter, which represents the `{!rsh} msg` of the `{!rsh} PUBLISH_EXPR`.

For example, the code below erroneously tries to publish multiple values and bind them in
the function provided to `CONSENSUS_EXPR`:

```reach
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
```

You can fix this code by changing the arrow expression to accept one parameter. You can
either destructure the argument with a `{!rsh} const` assignment or as part of the function
syntax:

```reach
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
```

## {#RE0083} RE0083

This error indicates that not all the components of the `{!rsh} parallelReduce` statement are provided.
Please refer to the documentation of `{!rsh} parallelReduce` to see the required components.

You can fix this error by adding any components the compiler has listed.

Example of the issue:

```reach
load: /hs/t/n/Err_ParallelReduceIncomplete.rsh
md5: a50e4be09db057a73abc83446c33b4bf
range: 8 - 10
```

To fix this, make sure the `{!rsh} parallelReduce` has all required elements.

```reach
load: /hs/t/y/throw_timeout.rsh
md5: 90656b6c80dca044b5723aa02428e138
range: 26 - 40
```

## {#RE0084} RE0084

This error indicates that you have provided the wrong number of arguments to a component of
`{!rsh} parallelReduce`. Please refer to the documentation for the specific component you
are trying to use.

For example, the code below erroneously supplies a closure as the second argument to `{!rsh} timeRemaining`.

```reach
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
```

However, `{!rsh} timeRemaining` is a shorthand for a timeout which automatically publishes and returns the
`{!rsh} parallelReduce` accumulator. The component only expects one argument. You can fix this code by removing
the second argument supplied.

## {#RE0085} RE0085

This error indicates that your program would contain a value at runtime which would not be allowed. This
error usually stems from not fully applying a primitive function or using a value incorrectly, such
as the participant interact interface of a `{!rsh} Participant`.

For example, the code below erroneously tries to publish `{!rsh} Alice`'s interact interface:

```reach
Alice.only(() => {
  const aInteract = declassify(interact);
});
Alice.publish(aInteract);
```

You can fix this code by specifying a specific field of `{!rsh} Alice`'s interact interface to `{!rsh} publish`:

```reach
Alice.only(() => {
  const aX = declassify(interact.x);
});
Alice.publish(aX);
```

## {#RE0086} RE0086

This error indicates that the `{!rsh} Type` of a value cannot exist at runtime. This error
may be caused by a `{!rsh} Fun` in a participant interact interface having a return
type of another `{!rsh} Fun`.

For examples of this error and how to fix it, see @{seclink("RE0012")}.

## {#RE0087} RE0087

@{errver(false, "v0.1")}

This error indicates that you are attempting to apply a non-function value as if it were a function. This
issue is most likely caused by a typo with an identifier.

## {#RE0088} RE0088

This error indicates that there is a mismatch between the actual `{!rsh} Type` of a value and the expected
`{!rsh} Type` of a value.

For example, the code below erroneously returns a `{!rsh} Bool` when the type annotation states that
the function should return a `{!rsh} UInt`.

```reach
export const f =
  is(((x) => true),
     Fun([UInt], UInt));
```

You can fix this code by returning a `{!rsh} UInt` from the function or changing the return type of the function.

This error may be caused by using a value of the incorrect type in an operation. The code below erroneously uses
a `{!rsh} Maybe` value in a `{!rsh} +` expression:

```reach
A.only(() => {
  const mi = declassify(interact.get1());
  const i = (() => {
    switch (mi) {
    case None: return 42;
    default: return mi+1; } })(); });
```

In this code, `{!rsh} mi` is still of `{!rsh} Maybe` type. You can fix this code by changing `{!rsh} default`
to `{!rsh} case Some`, which will re-bind `{!rsh} mi` to the value contained within `{!rsh} Some`:

```reach
A.only(() => {
  const mi = declassify(interact.get1());
  const i = (() => {
    switch (mi) {
    case None: return 42;
    case Some: return mi+1; } })(); });
```

## {#RE0089} RE0089

This error indicates that a `{!rsh} Map.reduce` is being performed outside of an `{!rsh} invariant`, which
is the only place map reductions are allowed to occur.

For example, the code below erroneously attempts to keep the sum of the `{!rsh} Map` as a loop variable:

```reach
var [keepGoing, sum] = [true, m.sum()];
invariant(balance() == sum);
while (keepGoing) {
  commit();

  Alice.pay(1);
  m[Alice] = fromSome(m[Alice], 0) + 1;

  [keepGoing, sum ] = [true,  m.sum()];
  continue;
}
```

You can fix this code by moving any `{!rsh} Map` reductions to inside the `{!rsh} invariant`:

```reach
var keepGoing = true;
invariant(balance() == m.sum());
while (keepGoing) {
  commit();

  Alice.pay(1);
  m[Alice] = fromSome(m[Alice], 0) + 1;

  keepGoing = true;
  continue;
}
```

## {#RE0090} RE0090

This error indicates that a `{!rsh} Map` was expected in an expression, but a value
of a different type was provided. This issue is most likely caused by a typo in an
identifier.

## {#RE0091} RE0091

This error indicates that you are attempting to create a `{!rsh} Foldable` value, which is
not possible. `{!rsh} Foldable` is an interface that `{!rsh} Array` and `{!rsh} Map` implement.

For example, the code below erroneously tries to create a `{!rsh} Foldable` value:

```reach
const container = Foldable();
```

You can fix this code by instead creating a `{!rsh} Map` or an `{!rsh} Array`.

## {#RE0092} RE0092

This error indicates that there are normal parameters listed after parameters with default arguments
in a function definition. Parameters with default arguments must come after all other arguments.

For example, the code below erroneously lists its parameters:

```reach
const f = (name = "Reach", msg) => {
  // ...
}
```

You can fix this error by rearranging the parameters so that the ones with default arguments are last:

```reach
const f = (msg, name = "Reach") => {
  // ...
}
```

## {#RE0093} RE0093

This error indicates that you are attempting to bind an effect or statement, which is not allowed.

For example, the code below supplies a statement as an argument to a function:

```reach
closeTo(Bob,
  each([Alice, Bob], () => {
    interact.showResult(5); }));
```

The result of `{!rsh} each` cannot be bound as a function argument. You can fix this code by
wrapping the statement in an arrow expression:

```reach
closeTo(Bob, () => {
  each([Alice, Bob], () => {
    interact.showResult(5); })});
```

## {#RE0094} RE0094

This error indicates that there are unused variables in your program. This error will
only occur with `{!rsh} 'use strict'`.

You can fix this error by either replacing the unused variable names with `{!rsh} _` or
subsequently using `{!rsh} void(x)`.

## {#RE0095} RE0095

This error indicates that a field in a `{!rsh} Remote` object is not a function. You
can fix this by ensuring your `{!rsh} Remote` object only contains fields that are functions.
This fix may require changes to the foreign contract you are attempting to connect to.

## {#RE0096} RE0096

This error indicates that the key supplied to a `{!rsh} Struct` does not match the required regex.
`{!rsh} Struct` keys must satisfy the regex: `[_a-zA-Z][_a-zA-Z0-9]*`.

For example, the code below provides an erroneous key value:

```reach
const s = Struct([["$x ", UInt]]);
```

You can fix this by removing any illegal characters:

```reach
const s = Struct([["x", UInt]]);
```

## {#RE0097} RE0097

This error indicates that a key in a `{!rsh} Struct` has been used more than once.
Every key must be unique in a `{!rsh} Struct`.

For example, the code below erroneously uses the same key twice:

```reach
const s = Struct([["x", UInt], ["y", UInt], ["x", UInt]]);
```

You can fix this by renaming one of the `{!rsh} "x"` fields:

```reach
const s = Struct([["x", UInt], ["y", UInt], ["x2", UInt]]);
```

## {#RE0098} RE0098

This error indicates that you are attempting to export a name that the
Reach backend already produces. For example, the names provided in `{!rsh} Participant`, `{!rsh} ParticipantClass`, `{!rsh} API`, and `{!rsh} View` will be exported by the Reach backend.

Reach exports a few names from the backend automatically, such as `{!rsh} getExports`.
Therefore, you cannot export a `{!rsh} Participant` named `{!rsh} getExports` as such:

```reach
const P = Participant('getExports', {});
```

You can fix this error by choosing a different name.

## {#RE0099} RE0099

This error indicates that you are attempting to use a value that is not a `{!rsh} Bool`
in a condition, while using strict mode.

For example, the code below erroneously uses a number as the condition to `{!rsh} if`:

```reach
const y = declassify(interact.getInt());
const x = y ? 2 : 3;
```

You can fix this code by using a `{!rsh} Bool` instead. The following code will consider
any number that is not `{!rsh} 0` `{!rsh} true`:

```reach
const y = declassify(interact.getInt());
const x = (y != 0) ? 2 : 3;
```

## {#RE0100} RE0100

This error indicates that there are multiple `{!rsh} throw` statements inside a `{!rsh} try`
block and the values thrown are of different `{!rsh} Type`s.

You can fix this error by ensuring that every value thrown is of the same type. It may be necessary
to create a new `{!rsh} Data` instance that can handle different types.

For example, the code below erroneously throws a `{!rsh} UInt` and a `{!rsh} Bool`:

```reach
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
```

You can fix this code by abstracting the `{!rsh} Type`s of values thrown into a new `{!rsh} Data` type:

```reach
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
```

## {#RE0101} RE0101

This error occurs when you attempt to use a `{!rsh} throw` statement outside of a `{!rsh} try`
block.

Example:

```reach
load: /hs/t/n/Err_Throw_No_Catch.rsh
md5: d21f3eba7c606b04750a14b7bfcb04a0
range: 7 - 9
```

You can fix this error by moving your `{!rsh} throw` statement inside the appropriate block of code
or wrapping the necessary code into a `{!rsh} try`/`{!rsh} catch` block.

Example:

```reach
load: /hs/t/y/Fix_Throw_No_Catch.rsh
md5: 85b234fb9e0725b166cb278b0433281a
range: 10 - 14
```

## {#RE0102} RE0102

This error indicates that you are attempting to `{!rsh} pay` on the first publication.
This is not possible, because the contract will not yet exist, and receiving tokens depends on knowing the address of a contract first on those networks.

Example:

``` reach
load: /hs/t/n/Err_Token_DynamicRef.rsh
md5: c753ac8153539f825cd480f9a9e4a518
range: 21 - 25
```

You can fix this by paying into the contract after the first publication.

Example:

``` reach
load: /examples/chicken-race/index.rsh
md5: 2f62423e6b6ea82b9c1c89cba69104a1
range: 28 - 34
```

Alice creates the `wager` and the `deadline`, then declassifies them in the `{!rsh} interact` object to create the contract.
Alice is then able to `{!rsh} publish` both `wager` and `deadline`, and pay the `wager` because the contract has already been created.

## {#RE0103} RE0103

This error indicates that you are attempting to `{!rsh} publish` a `{!rsh} Token` within
a `{!rsh} while` loop. This is not currently possible in Reach. You must publish `{!rsh} Token`
values outside of loops.

For example, the code below erroneously publishes a `{!rsh} Token` inside a loop:

```reach
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
```

You can fix this code by publishing `{!rsh} tok` before the loop:

```reach
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
```

## {#RE0104} RE0104

This error indicates that you are attempting to reference a `{!rsh} Token` that
was computed dynamically. Reach does not yet support this.

For example, the code below attempts to transfer the balance of an `{!rsh} array` of `{!rsh} Token`s
to a `{!rsh} Participant`:

```reach
const allTokens = array(Token, toks);
Foldable_forEach(allTokens, (tok) => transfer(balance(tok), tok).to(Who));
```

You can work around this issue by writing out the `{!rsh} Token` values explicitly:

```reach
const nonNetPayAmt = [ [balance(tok), tok], [balance(tok2), tok2] ];
transfer([ balance(), ...nonNetPayAmt ]).to(Who);
```

Sometimes this is caused when you did not include `{!rsh} paySpec` in a use of
`{!rsh} fork` or `{!rsh} parallelReduce`.

## {#RE0105} RE0105

This error indicates that the incorrect arguments were supplied to `{!rsh} withBill`.
`{!rsh} withBill` either expects zero arguments, when only receiving network tokens,
or a `{!rsh} Tuple` of `{!rsh} Token`s when receiving non-network tokens.

For example, the code below erroneously provides multiple non-network tokens to
`{!rsh} withBill`:

```reach
const [ returned, [gilRecv, zmdRecv], randomValue ] =
  randomOracle.getRandom.pay(stipend).withBill(gil, zmd)();
```

You can fix this by wrapping all the arguments into a single `{!rsh} Tuple`:

```reach
const [ returned, [gilRecv, zmdRecv], randomValue ] =
  randomOracle.getRandom.pay(stipend).withBill([gil, zmd])();
```

## {#RE0106} RE0106

This error indicates that a program declared multiple `{!rsh} View`s with the same
name.

You can fix this error by renaming the duplicate `{!rsh} View`s, ensuring that every name is unique.

## {#RE0107} RE0107

@{errver(false, "v0.1")}

This error indicates that the value of a `{!rsh} View` cannot be exposed. This would
only occur if the value cannot be represented at runtime.

## {#RE0108} RE0108

This error indicates that a `{!rsh} View` function has an unconstrained domain. Every
`{!rsh} View` must explicitly state the `{!rsh} Type` of function arguments it accepts.

If your `{!rsh} View` function relies on a varying number of arguments or `{!rsh} Type`s, you
can either abstract the arguments into a new `{!rsh} Data` type or make separate `{!rsh} View`s.

## {#RE0109} RE0109

@{errver(false, "v0.1")}

This error indicates that there are multiple `{!rsh} API`s, `{!rsh} Participant`s or `{!rsh} ParticipantClass`es with the same name.

Example:

```reach
load: /hs/t/n/Err_Part_DuplicatePart.rsh
md5: f02e5e8d6b0d95f3015b26462c9936a4
range: 4 - 7
```

Each participant name must be unique.

You can fix this error by renaming the duplicate names.

Example:

```reach
load: /hs/t/y/Fix_Part_DuplicatePart.rsh
md5: e885c1a1b11069f67059000ea89cdd17
range: 4 - 7
```

:::note
Since `{!rsh} ParticipantClass` is being deprecated, it is preferable to use `{!rsh} API`.
:::

## {#RE0110} RE0110

This error indicates that a `{!rsh} Struct` contains an invalid field name.
A field name may be invalid if it is a reserved word in the connector you are targeting.

For example, the code below erroneously uses the field name `{!rsh} "super"`, which is
reserved in the ETH connector:

```reach
const A = Participant('A', {
  get: Fun([], Struct([
    ['super', Address]
  ]))
});
```

You can fix this by renaming the erroneous field names:

```reach
const A = Participant('A', {
  get: Fun([], Struct([
    ['super1', Address]
  ]))
});
```

## {#RE0111} RE0111

This error indicates that an unexpected key was provided to the `{!rsh} Token` constructor.
You may find the acceptable parameters in the following section: [token minting](##ref-programs-consensus-token-minting).

## {#RE0112} RE0112

This error indicates that you are attempting to perform an invalid operation on a `{!rsh} Token`.
Some `{!rsh} Token` methods such as `{!rsh} destroy`, `{!rsh} burn`, and
`{!rsh} supply` are only valid for tokens that were created in your program.

You can fix this by removing the erroneous statement.

## {#RE0113} RE0113

This error indicates that you provided an incorrect value to the `{!rsh} .define` component of a
`{!rsh} parallelReduce` statement. The argument to `{!rsh} .define` should be of the form: `{!rsh} () => DEFINE_BLOCK`.
Please review the `{!rsh} parallelReduce` documentation for information on how `{!rsh} .define` works.

## {#RE0114} RE0114

This error indicates that you provided an incorrect value to a function.
The function expected to receive a `{!rsh} Type`, but received something else.
This issue can arise when a `{!rsh} Type` constructor does not have any arguments applied to it.

For example, the code below erroneously creates an `{!rsh} Object` type with a field `{!rsh} name`:

```reach
const Person = Object({
  name: Bytes
});
```

This code is incorrect because `{!rsh} Bytes` is not a type; it is a function that accepts a `{!rsh} UInt` and returns
a `{!rsh} Type`. This code can be fixed by providing an argument to `{!rsh} Bytes` that represents the length:

```reach
const Person = Object({
  name: Bytes(32)
});
```

For more information about data types, visit @{seclink("ref-programs-types")}.

## {#RE0115} RE0115

This error indicates that a time argument's type is not known at compile-time.

For example, if `{!rsh} x` is not known at compile-time, then

```reach
wait(x ? relativeTime(10) : relativeSecs(10));
```

results in this error.

## {#RE0116} RE0116

This error means that a `{!rsh} return` statement, typically inside of an `{!rsh} if` statement, does not occur in tail position.

For example, this function would have this error:

```reach
const f = (x) => {
  if ( x > 20 ) {
    return 0;
  } else {
    return 1;
  }
  return 2;
};
```

The third `{!rsh} return` can never be reached, so the way to correct the program is to remove it.

## {#RE0117} RE0117

This error means that only one branch of a conditional (`{!rsh} if` or `{!rsh} switch`) contains a `{!rsh} return` statement.

For example, this function would have this error:

```reach
const f = (x) => {
  if ( x > 20 ) {
    return 0;
  }
  return 1;
};
```

It should be corrected by moving the tail of the `{!rsh} if` into the `{!rsh} else` branch:

```reach
const f = (x) => {
  if ( x > 20 ) {
    return 0;
  } else {
    return 1;
  }
};
```

## {#RE0118} RE0118

This error indicates that a switch case is unreachable.
This issue will occur when a `{!rsh} case` is listed after `{!rsh} default`.

For example, the code below erroneously puts a `{!rsh} case` after `{!rsh} default`:

```reach
Maybe(UInt).Some(5).match({
  default: (() => 0),
  Some: ((i) => i),
});
```

This error can be corrected by either removing the `{!rsh} Some` case or placing it before the `{!rsh} default` case:

```reach
Maybe(UInt).Some(5).match({
  Some: ((i) => i),
  default: (() => 0),
});
```

## {#RE0119} RE0119

This error indicates that you have inspected the details about a publication, such as via `{!rsh} didPublish()`, before there has been a publication.
This is impossible, so the expression must be moved after the first publication.

For example, the code below erroneously puts `{!rsh} didPublish()` before `{!rsh} publish`:

``` reach
load: /hs/t/n/Err_NotAfterFirst.rsh
md5: 46e1eb604c95c16c2c5dcfeeb5e94aa7
range: 6 - 10
```

This error can be corrected by placing `{!rsh} publish` before `{!rsh} didPublish()`:

```reach
load: /examples/raffle/index.rsh
md5: b6632a71c54afc8e1da08f2d6c0dbad3
range: 35 - 35
```

```reach
load: /examples/raffle/index.rsh
md5: b6632a71c54afc8e1da08f2d6c0dbad3
range: 59 - 65
```

:::note
Notice that the `Sponsor` `{!rsh} publish`ed on line 35. On Line 62, `{!rsh} didPublish()` is used to make a check.
:::

## {#RE0120} RE0120

This error indicates that an actor who is not a `{!rsh} Participant`, e.g. a `{!rsh} ParticipantClass`, is
attempting to make the first publication of a Reach program.

You can fix this error by assigning the first publication to one of your `{!rsh} Participant`s.
Additionally, you can create a new `{!rsh} Participant` to specifically perform this action.

```reach
const Constructor = Participant('Constructor', {});
// ...
init();
Constructor.publish();
commit();
// ...
```

## {#RE0121} RE0121

This error indicates that one of an API's interface members is not a function.
For example:

```reach
const A = API('api', {
  tastiness: UInt,
});
```

## {#RE0122} RE0122

@{errver(false, '0.1.7')}

This error indicates that the left-hand side of a `{!rsh} call` assignment is not a pair of the domain and a function to return a result to the function.
For example:

```reach
const x = call(Voter.vote);
```

## {#RE0123} RE0123

This error indicates that the name provided to a `{!rsh} Participant`, `{!rsh} ParticipantClass`, `{!rsh} API`, or `{!rsh} View` is already in use.
There is a single namespace for all of these entities.

For example, the code below erroneously uses the same name multiple times:

```reach
const A = Participant('Flower_girl', {});
const B = API('Flower', { girl: Fun([UInt], Null) });
```

`{!rsh} 'Flower_girl'` is used multiple times because every method of an `{!rsh} API` will
introduce a binding, of the format: `<API name>_<method name>`, into the namespace.

You can fix this error by using different names:

```reach
const A = Participant('Flower_girl', {});
const B = API('Flower', { girl2: Fun([UInt], Null) });
```

## {#RE0124} RE0124

This error indicates that there is an attempt to make a publication in your program, but there are
no `{!rsh} Participant`s or `{!rsh} ParticipantClass`es declared.

This issue can arise when you use `{!rsh} Anybody.publish()`. To fix this issue, ensure you declare
a `{!rsh} Participant` or `{!rsh} ParticipantClass`.

For example, the program below erroneously uses `{!rsh} Anybody.publish()` without declaring any `{!rsh} Participant` or `{!rsh} ParticipantClass`:

```reach
load: /hs/t/n/Err_No_Participants.rsh
md5: cdb92dfbbb05b2f6f9eda2d462140e57
range: 3 - 9
```

However, the correct thing to do is to declare at least one `{!rsh} Participant` or `{!rsh} ParticipantClass` before using `{!rsh} Anybody.publish()` like in the program below:

```reach
load: /examples/api-call/index.rsh
md5: 5bb3b74980f2ba84d75c68edc5b9e4e5
range: 4 - 7
```

```reach
load: /examples/api-call/index.rsh
md5: 5bb3b74980f2ba84d75c68edc5b9e4e5
range: 47 - 49
```

:::note
Since `{!rsh} ParticipantClass` is being deprecated, it is preferable to use `{!rsh} API`.
:::

## {#RE0125} RE0125

This error indicates that an `{!rsh} API` is explicitly attempting to make a publication, e.g. `{!rsh} api.publish()`.
An API may only make a publication through a `{!rsh} fork`, `{!rsh} parallelReduce`, or `{!rsh} call`.

Example:

```reach
load: /hs/t/n/Err_Api_Publish.rsh
md5: 6ef69e7389dd90d7dc3646e832524326
range: 3 - 11
```

Depending on your program, you can fix this error by performing a `{!rsh} call` or adding an `{!rsh} .api`
case to your `{!rsh} fork` or `{!rsh} parallelReduce` statement.

```reach
load: /hs/t/y/obj_view_api.rsh
md5: f64aaa770eb9d6fe07c5074fda861b90
range: 8 - 17
```

This snippet `{!rsh} call`s the `{!rsh} API` instead of publishing.

## {#RE0126} RE0126

This error indicates that there is a `{!rsh} fork` or `{!rsh} parallelReduce` in the program that does not specify a `{!rsh} .case` or `{!rsh} .api` component.
At least one of these components must be specified.

```reach
load: /hs/t/n/Err_ForkNoCases.rsh
md5: 2c6e793910f37eb74baa13e85c65cb0e
range: 7 - 9
```

You can fix this error by adding a `{!rsh} .case` or `{!rsh} .api` component to your statement.

```reach
load: /hs/t/y/parallelReduce_no_timeout.rsh
md5: a30b3a95795b87042c7529ad64af3ce2
range: 15 - 24
```

## {#RE0127} RE0127

This error indicates that the `.paySpec` component of a `parallelReduce`, or `fork` does not provide a syntactic tuple of identifiers.

For example, the code below provides `.paySpec` with a variable that is equal to a tuple of `{!rsh} Token` identifiers:

```reach
  const tokens = [ tok1, tok2 ];

  const alive =
    parallelReduce([ true ])
    .while(true)
    .invariant(balance() == 0)
    .paySpec(tokens)
    .case(A,
      () => ({ when: true }),
      () => { return [ true ]; })
    .timeout(false);
```

You can fix this error by inlining the tuple directly:

```reach
  const alive =
    parallelReduce([ true ])
    .while(true)
    .invariant(balance() == 0)
    .paySpec([ tok1, tok2 ])
    .case(A,
      () => ({ when: true }),
      () => { return [ true ]; })
    .timeout(false);
```
## {#RE0128} RE0128

This error indicates that you are attempting to update loop variables to a `{!rsh} Tuple` of different length.
This error can occur in the assignments before a `{!rsh} while` loop's `{!rsh} continue`, or the `{!rsh} return` expression in a `{!rsh} parallelReduce` case.

The code below erroneously attempts to update the loop variable to a 2-tuple in the timeout, although the loop variable is a 1-tuple:

```reach
const [ a ] =
  parallelReduce([ true ])
  .invariant(balance() == 0)
  .while(a)
  .case(A,
    () => ({
        when: declassify(interact.when()),
    }),
    (_) => {
      return [ false ];
    })
    .timeout(relativeSecs(10), () => {
      Anybody.publish();
      return [ true, 43 ];
    });
```

Depending on the intended behavior of the program, you can fix this error in different ways:
* You can remove the unnecessary value `{!rsh} 43` from the `{!rsh} Tuple` since you are only tracking one loop variable, `{!rsh} a`.
* You can update the loop variable `{!rsh} Tuple` to track more variables:

  ```reach
  const [ a, b ] =
    parallelReduce([ true ])
    .invariant(balance() == 0)
    .while(a)
    .case(A,
      () => ({
         when: declassify(interact.when()),
      }),
      (_) => {
        return [ false, 0 ]; // Don't forget to adjust the other cases too!
      })
     .timeout(relativeSecs(10), () => {
       Anybody.publish();
       return [ true, 43 ];
     });
  ```

## {#RE0129} RE0129

This error indicates that you are attempting to perform an `{!rsh} xor` operation on an unsupported type.

Refer to the `{!rsh} xor` documentation to see what types are supported.

## {#RE0130} RE0130

This error indicates that you are attempting to perform a `{!rsh} mod` operation on an unsupported type.

Refer to the `{!rsh} mod` documentation to see what types are supported.

## {#RE0131} RE0131

This error indicates that you are attempting to overload a method with multiple functions that have the same domain.

```reach
load: /hs/t/n/Err_Alias_Type_Clash.rsh
md5: 9400699b8ccac8f5d1cd1d2e2aa2631f
range: 6 - 15
```

This code overloads `c` by setting `a`, `b`, and `d` all to `c`.

You can fix this issue by either removing one of the overloads or changing one of the domains.
You can change the domain of a function by altering the type of its parameters or adding/removing parameters.

```reach
load: /hs/t/y/Fix_Alias_Type_Clash.rsh
md5: 471eaca7a21dd2c0a94c48fb840537ed
range: 6 - 12
```

## {#RE0132} RE0132

This error indicates you had extra fields in `{!rsh} REMOTE_FUN.ALGO` that are not supported.

In this code, `foo` is an illegal field:

```reach
load: /hs/t/n/Err_Remote_ALGO_extra.rsh
md5: 0bdaf10698fd93941e8e4592ba02f2a4
range: 10 - 10
```

Remove `foo` to fix the error code:

```reach
load: /hs/t/y/Fix_Remote_ALGO_extra.rsh
md5: e7e58888f151cdd04f0340e4c5d013dc
range: 10 - 10
```

## {#RE0133} RE0133

This error indicates that a thunk, or function with no parameters, was expected but something else was provided.

For example, the code below erroneously provides a function with one parameter to the `{!rsh} .check` function:

```reach
A.publish(x)
  .check((x) => {
    check(x > 0, "x > 0");
  });
```

You can fix this error by removing the unnecessary function parameter.

```reach
A.publish(x)
  .check(() => {
    check(x > 0, "x > 0");
  });
```

## {#RE0134} RE0134

This error indicates that you returned the wrong values in `{!rsh} CHECKED_CONSENSUS_EXPR` of an `{!rsh} .api_` branch.

For example, the code below erroneously returns the new accumulator for `{!rsh} parallelReduce`:

```reach
.api_(B.setX,
  (x) => {
    check(x > 0);
    return [ x ];
  })
```

You can fix this error by returning a function that accepts the API return function as a parameter.

```reach
.api_(B.setX,
  (x) => {
    check(x > 0);
    return [ (k) => {
      return [ x ];
    }]
  })
```

Alternatively, if you want the api `B.setX` to pay into the contract, you can specify the pay amount as the first element in the return value:

```reach
.api_(B.setX,
  (x) => {
    check(x > 0);
    const payAmt = 5;
    return [ payAmt, (k) => {
      return [ x ];
    }]
  })
```

## {#RE0135} RE0135

This error indicates that you attempted to create a `{!rsh} Data` instance with no variants.
This is not allowed.

## {#RE0136} RE0136

This error indicates that you are using a function, like `{!rsh} ContractCode`, that requires representing values as JSON, but giving it a value that cannot be represented as JSON.

## {#RE0137} RE0137

This error indicates that there was an error while parsing or compiling contract code for `{!rsh} ContractCode`.

## {#RE0138} RE0138

This error indicates that there was an error while parsing a number in exponential notation.

You can fix this error by ensuring you use the correct exponential notation syntax as specified in the numeric literals documentation.

## {#RE0139} RE0139

This error indicates that you called `{!rsh} Bytes.fromHex` with an invalid hex string.
A hex string may only contain characters `0-9` and `a-f`.

You can fix this error by providing a string in the correct format.

## {#RE0140} RE0140

This error indicates that you are trying to access a value from outside of a `{!rsh} Reach.App` whose value is not known at compile time.

For example, the code below erroneously tries to access `{!rsh} c` from within the nested `{!rsh} Reach.App`:

```
A.publish(a);
const c = a + 5;

const ctor = new Contract(Reach.App(() => {
  // ...
  const b = c;
  // ...
}));
```

`!{rsh} c` relies on the value of `{!rsh} a`, which is only known at runtime.
Since nested applications can only reference values that are known at compile time, this code is invalid.

You can fix this error by removing the erroneous reference to a dynamic value.

## {#RETH0001} RETH0001

This error indicates that a View or API uses more arguments than are supported on the ETH connector.

## {#REP0000} REP0000

This error indicates that the body of a `{!rsh} while` loop does not make a publication before the `{!rsh} continue`
statement.

For example, the code below does not make any publications before continuing the loop:

```reach
var x = 0;
invariant(balance() == 0);
while (true) {
  x = x + 1;
  continue;
}
```

You can fix this code by making a publication within the `{!rsh} loop`:

```reach
var x = 0;
invariant(balance() == 0);
while (true) {
  commit();
  Alice.publish();
  x = x + 1;
  continue;
}
```

Note that the body of a `{!rsh} while` starts in a consensus step so you must first `{!rsh} commit` before making a publication.

## {#REP0001} REP0001

This error indicates that the result of setting a `{!rsh} View` will never be observable.
Either the set does not [dominate](https://en.wikipedia.org/wiki/Dominator_(graph_theory)) any `{!rsh} commit`s, or the program `{!rsh} exit`s in the next step.

For example, the code below attempts to set a loop variable as the value of a `{!rsh} View`:

```reach
export const main = Reach.App(() => {
  const A = Participant('Alice', {
    observe: Fun([], Null)
  });
  const I = View('I', { i: UInt });
  init();

  A.publish();

  var [ i ] = [0];
  { }
  invariant (balance() == 0);
  while ( i < 5 ) {
    commit();

    A.interact.observe();
    A.publish();

    I.i.set(i);

    i = i + 1;
    continue;
  }

  commit();
});
```

The effect of `{!rsh} I.i.set(i)` is only observable after the next `{!rsh} commit` in its scope.
Since, there are no `{!rsh} commit`s between `{!rsh} I.i.set(i)` and `{!rsh} continue`, which is the end of the lexical scope, there is no
way to observe the effect of setting `{!rsh} I.i`.

You can generally fix this error by inserting a `{!rsh} commit` in the area where you'd like
the effect of setting a `{!rsh} View` to be observable. In the case of a `{!rsh} while` loop, like the
program above, it can be fixed the following way:

```reach
export const main = Reach.App(() => {
  const A = Participant('Alice', {
    observe: Fun([], Null)
  });
  const I = View('I', { i: UInt });
  init();

  A.publish();

  var [ i ] = [0];
  {
    I.i.set(i);
  }
  invariant (balance() == 0);
  while ( i < 5 ) {
    commit();

    A.interact.observe();
    A.publish();

    i = i + 1;
    continue;
  }

  commit();
});
```

This change will ensure the `{!rsh} View` `{!rsh} I.i` is set to `{!rsh} i` on every iteration of the
loop. Additionally, the continuation of the loop will have `{!rsh} I.i` set to the last value of
the loop variable `{!rsh} i`.

## {#REP0002} REP0002

This error indicates that you called an API multiple times within the same consensus step.

For example, the erroneous code below calls the API, `{!rsh} U.f`, many times in the same `{!rsh} parallelReduce`.

```reach
const x =
  parallelReduce(0)
  .invariant(balance() == 0)
  .while( x < 10 )
  .api(U.f, (k) => {
      k(null);
      return x + 1;
  })
  .api(U.f, (k) => {
      k(null);
      return x + 2;
  });
```

You can fix this error by removing the duplicate `{!rsh} .api` case.
Alternatively, you can create a new API method and replace one of the offending cases.

## {#RI0000} RI0000

This error indicates that `git clone` has failed when trying to download the dependencies
of your project. This error will tell you the issue that was encountered.

## {#RI0001} RI0001

This error indicates that `git checkout` has failed when trying to checkout the specific
revision of your dependency. This error will tell you the issue that was encountered.

## {#RI0002} RI0002

This error indicates that the dependency required by your project does not contain either
the branch specified, or a `master/main` branch.

Please ensure you have specified the correct import.

## {#RI0003} RI0003

This error indicates that your project dependencies need to be retrieved but you did not specify
`{!rsh} --install-pkgs` with your `reach` command.

You can fix this by specifying the needed flag with your command.

## {#RI0004} RI0004

This error indicates that the syntax you used to specify your import is incorrect.

You can fix this by using the correct syntax. Please view the documentation for [package imports](##ref-programs-import-package).

## {#RL0000} RL0000

This error indicates that the given code must not be reachable because it would result in an error if reached. This error may be caused for different reasons, which will be explained if encountered.

One reason this code could be encountered is if there is a branch within a `{!rsh} while` loop, which does not contain a `{!rsh} continue` statement when it is expected.

```reach
load: /hs/t/n/pr-149.rsh
md5: a96b9457368e1246a8e5ee55e93b34f6
range: 60-71
```

You can fix this by explicitly adding the `{!rsh} continue` statement to the erroneous block of code.

```reach
load: /hs/t/y/pr-149.rsh
md5: 6acb94a95c4f7d5a333def81772083f4
range: 60-73
```

## {#RP0000} RP0000

This error indicates that there is a [circular dependency](https://en.wikipedia.org/wiki/Circular_dependency) in the `{!rsh} import`s of your application.

You can fix this by refactoring your code to remove the cyclic imports.

## {#RP0001} RP0001

@{errver(false, "v0.1")}

This error indicates that you have specified a function without an argument list.

You can fix this by adding an argument list.

## {#RP0002} RP0002

@{errver(false, "v0.1")}

This error indicates that an identifier was expected during parsing, but an expression
was received.

## {#RP0003} RP0003

This error indicates that a key or a key/value pair was expected in a
destructuring assignment, but an object method was received.

For example, the code below creates a method within a destructuring assignment:

```reach
const {x() { return 1 }} = {x: 2};
```

You can fix this code by simply specifying `{!rsh} x` in the assignment:

```reach
const { x } = {x: 2};
```

## {#RP0004} RP0004

This error indicates that an unsupported binary operator was encountered.
Reach is a subset of JavaScript and does not support all of the binary operators
JavaScript supports.

You can fix this by utilizing different operators or functions depending
on the logic of your program.

## {#RP0005} RP0005

@{errver(false, "v0.1")}

This error indicates that an unsupported literal was encountered.
Reach is a subset of JavaScript and does not support all of the literals that
JavaScript supports.

## {#RP0006} RP0006

This error indicates that an unsupported unary operator was encountered.
Reach is a subset of JavaScript and does not support all of the unary operators
JavaScript supports.

You can fix this by utilizing different operators or functions depending
on the logic of your program.

## {#RP0007} RP0007

This error indicates that you are attempting to `{!rsh} import` a file using an absolute
path which is not supported.

You can fix this by using a relative path for your `{!rsh} import`.

## {#RP0008} RP0008

This error indicates that you are trying to `{!rsh} import` a path
that is accessing its parent directory via `{!rsh} ..`. This type of
import is not allowed. Please view the documentation for package imports.

For example, the code below erroneously `{!rsh} import`s a file from its parent
directory:

```reach
"reach 0.1";
import "../a.rsh";
```

You can fix this error by moving your file, `{!rsh} "../a.rsh"`, to the same
directory your program is in. Then, reference it using a relative import:

```reach
"reach 0.1";
import "./a.rsh";
```

## {#RP0009} RP0009

@{errver(false, "v0.1")}

This error indicates that the Reach file could not be parsed as a module.

## {#RP0010} RP0010

This error indicates that a call-like expression was expected, but another
value was provided.

For example, the code below erroneously passes `{!rsh} _x`, a secret value of
`{!rsh} Bob`, to `{!rsh} unknowable`:

```reach
unknowable(A, _x);
```

You can fix this by providing a call-like expression to the function:

```reach
unknowable(A, B(_x));
```

## {#RP0011} RP0011

@{errver(false, "v0.1")}

This error indicates that Reach expected to parse an identifier, but none was given.

You can fix this error by adding an identifier name to the erroneous location.

## {#RP0012} RP0012

This error indicates that there is a syntax error in your program.
This error will provide the specific token that is erroneous.

For example, the code below erroneously provides an extra comma after the last argument of a function call:

```reach
const x = f(1, 2, 3,);
```

You can fix this error by removing the unnecessary trailing comma:

```reach
const x = f(1, 2, 3);
```

## {#RX0000} RX0000

This error indicates that you are trying to inspect or use the value produced from `{!rsh} forall`
outside of an `{!rsh} assert`.

For example, the code below attempts to verify that all `{!rsh} UInt`s are greater than or
equal to zero via a `{!rsh} require`:

```reach
const x = forall(UInt);
require(x >= 0);
```

This is invalid because the result of `{!rsh} forall` is an abstract value, which cannot exist
at runtime. You can fix this code by verifying the claim via an `{!rsh} assert`:

```reach
const x = forall(UInt);
assert(x >= 0);
```

<!-- Not a user facing error, will be thrown as `impossible` -->
<!-- ## {#RX0001} RX0001 -->

## {#RAPI0000} RAPI0000

This error means that you defined an API but did not actually use it in your prorgam.

## {#RAPI0001} RAPI0001

This error means that you returned the result to an API without calling it.
This is generally not possible unless you directly use the internal representation of APIs.

## {#RAPI0002} RAPI0002

@{errver(false, "v0.1.10")}

The error means that you use an API in two places in your program, which is not allowed.

This might look like the following in the `{!rsh} API_CONSENSUS_EXPR`:
```reach
.api(User.f, (x, ret) => {
  if (x > 5) {
    ret(true);
  }
  ret(false);
})
```

You cannot return from an API call twice.

Sometimes the second instance is a by-product of an effect duplicating a continuation, such as:
```reach
.api(User.f, (x, ret) => {
  if (x > 5) {
    m[this] = x;
  }
  ret(false);
})
```

This is because this is equivalent to:
```reach
.api(User.f, (x, ret) => {
  if (x > 5) {
    m[this] = x;
    ret(false);
  } else {
    ret(false);
  }
})
```

Instead, the effect should happen after the return:
```reach
.api(User.f, (x, ret) => {
  ret(false);
  if (x > 5) {
    m[this] = x;
  }
})
```

## {#RAPI0003} RAPI0003

This error means that you did not return a result from an API call.

## {#RAPI0004} RAPI0004

This error means that an API caller relies on non-consensus state to operate.
Typically, this is because the `{!rsh} assume` uses a value not needed by the consensus.
This is likely a sign of a bigger error, because a `{!rsh} assume` that does not have a "matching" `{!rsh} require` in the consensus is not useful.

## {#RW0000} RW0000

This warning means the syntax or function you are trying to use is deprecated. It is still supported
by the Reach compiler, but future versions of Reach may stop supporting it.

You can fix this warning by using the new syntax or function the message suggests.

## {#RW0001} RW0001

This warning indicates there is an issue with the Solidity compiler. The message provided can
be reported to Solidity.

## {#RW0002} RW0002

This warning indicates that your program either uses a feature that is not yet supported on Algorand
or surpasses Algorand's limit on resources.

## {#RW0003} RW0003

This warning indicates your program will not run on Algorand for the listed reasons. These
reasons include a limit on the total computation cost.

## {#RW0004} RW0004

This warning indicates that your program does not contain any publications.

You can fix this issue by making sure at least one `{!rsh} Participant` performs a `{!rsh} publish`.

### Problematic code:

```reach
'reach 0.1';

export const main = Reach.App(() => {
  const aisha = Participant('Aisha', {});
});
```

### Correct code:

```reach
'reach 0.1';

export const main = Reach.App(() => {
  const aisha = Participant('Aisha', {});
  init();
  aisha.publish(); // Missing publish is now present
  commit();
});
```

## {#RW0005} RW0005

This warning indicates that a `{!rsh} View` or `{!rsh} API` produces or consumes an `{!rsh} Object`,
which is a type internal to Reach.
It has an opaque and unspecified representation that can only be automatically consumed by other Reach programs, so it is probably a bad choice for general purpose interfaces.

You can remove this warning by using a `{!rsh} Struct` instead of the `{!rsh} Object`.

### Problematic code:

```reach
const V = View({ o1: Object({ a1: UInt }) }); // View is an Object
```

### Correct code:

```reach
const V = View({ o1: Struct( [ ["a1", UInt] ] ) }); // View produces a Struct
```
## {#RW0006} RW0006

This warning indicates that you referenced network seconds in your program.

On most consensus networks, network seconds are completely unrelated to reality.
The only invariant about these timestamps are that they never go down and they never increase by more than a fixed amont of seconds at each increment.
(For example, on Ethereum, the fixed amount is 15, and on Algorand the amount
is 25.
Typically these are configurable parameters of the consensus, but have never
actually been changed.)

For example, time 20 could actually occur on Monday at 17:45:00, but be assigned the timestamp of Sunday at 05:08:32, because that's when time 19 was and there was network downtime.
Furthermore, if time 21 was "on time" and actually occurred on Monday at 17:45:05, it would be assigned to Sunday at 05:08:57 (25 seconds after time 20's assignment).

This process would slowly "resychronize"; on Algorand, it would gain 20 seconds on the "actual time" every 5 second round.
This means it would take roughly 5 hours to resychronize after a day of downtime.
However, this is not guaranteed to occur at any particular time, because block proposers (on Algorand, at least) are free to leave the timestamp unchanged from the last block (i.e. there is no minimum increment), so it is possible that time would never be synchronized with reality at all.

Thus, it is unsafe to rely on network seconds for most purposes (such as interest on loans, time limits on auctions, and so forth), because network downtime (even intermittent) and adversarial block proposers (acting alone) can delay and influence the block time.

### Problematic code:

```reach
relativeSecs(t) // references network seconds
absoluteSecs(t) // references network seconds
lastConsensusSecs() // references network seconds
```

### Correct code:

``` reach
load: /hs/t/y/Fix_LoopVariableLength.rsh
md5: a7450399a78f1da3e595dbdbdf7035e9
range: 24 - 24
```

This code uses `{!rsh} relativeTime` instead of `{!rsh} relativeSecs` to use real time instead of network time.

## {#RV0000} RV0000

This error indicates your program has failed verification.
This error code is nonspecific, and is invoked for any verification error.
