#lang scribble/manual
@(require "lib.rkt")

@title[#:version reach-vers #:tag "ref-backends-js"]{JavaScript}

The Reach JavaScript @tech{backend} produces a compilation output named @filepath{input.export.mjs} which exports an asynchronous function for each @tech{participant}.
This will normally be imported by writing:

@(mint-define! '("backend"))
@js{
 import * as backend from './build/index.main.mjs';
}

Each function accepts two arguments: @jsin{ctc} and @jsin{interact}. These functions should be called by the @tech{frontend}.
For example, if a Reach program contains a participant named @reachin{'A'} in the argument to @reachin{Reach.App}, then the JavaScript backend will include a function named @jsin{A}.

The @jsin{ctc} argument is the result of a call to the functions @jsin{acc.deploy} or @jsin{acc.attach} provided by the @seclink["ref-frontends-js"]{JavaScript frontend support library}.

The @jsin{interact} argument is an object matching the @tech{participant interact interface} for the corresponding @tech{participant}.
The types of values this object contains must match those specified
@seclink["ref-frontends-js-types"]{on this list}.

The JavaScript backend also provides an export named @jsin{_version}, which is a string representation of the Reach version used to compile the program.
For example, the version of Reach used to produce this documentation would contain the string @jsin{'@|reach-vers|'}.

Finally, the backend will provide an export named @jsin{_Connectors}, which is an opaque object representing the @tech{connectors} this app was compiled to.

@section[#:tag "ref-backends-js-guarantees"]{Guarantees}

This backend does not guarantee that values in a positive position in a @tech{participant interact interface}, that are later passed to a negative position in a @tech{participant interact interface}, will be identical, in the sense of JavaScript's @jsin{===} operator, to the original value.
In other words, this backend does not ensure that Reach programs are parametric over JavaScript values that they interact with.

Positive and negative are best understood by example with a function type: a positive position is supplied by the function, such as the result; while a negative position is supplied by the caller, such as the arguments.
These notions generalize, however, to higher (and lower) order contexts.
In the case of Reach, this means that non-function values in a @tech{participant interact interface} are positive.

For example, if the Reach program,

@reach{
 Reach.App({},
  [Participant("A", { get: Bytes(32), give: Fun([Bytes(32)], Bool) })],
  (A) => {
   A.only(() => {
    const x = interact.give(interact.get); });
   A.publish(x);
   commit(); }); }

is given the @jsin{interact} object,

@js{
 const x = "A string";
 { get: x,
   give: (str) => x === str } }

then it is not guaranteed that @reachin{A} will publish @reachin{true}, because the @jsin{str} given to @jsin{give} may not be identical to @jsin{x}.
(However, they are @jsin{bytesEq}.)

