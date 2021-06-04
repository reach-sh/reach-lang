#lang scribble/manual
@(require "lib.rkt")

@title[#:version reach-vers #:tag "ref-backends-js"]{JavaScript}

The Reach JavaScript @tech{backend} produces a compilation output named @filepath{input.APP.mjs}.
This will normally be imported by writing:

@(mint-define! '("backend"))
@js{
 import * as backend from './build/index.main.mjs';
}

This module exports an asynchronous function for each @tech{participant}.
For example, if a Reach program contains a participant named @reachin{'A'} in the @reachin{Reach.App}, then the JavaScript backend will include a function named @jsin{A} (i.e. @jsin{backend.A}).
The @jsin{Promise} returned by these functions is resolved when the Reach program terminates (i.e. reaches @reachin{exit();}).

Each function accepts two arguments: @jsin{ctc} and @jsin{interact}. These functions should be called by the @tech{frontend}.

The @jsin{ctc} argument is the result of a call to the functions @jsin{acc.deploy} or @jsin{acc.attach} provided by the @seclink["ref-frontends-js"]{JavaScript frontend support library}.

The @jsin{interact} argument is an object matching the @tech{participant interact interface} for the corresponding @tech{participant}.
The types of values this object contains must match those specified
@seclink["ref-frontends-js-types"]{on this list}.
Each function may return a @jsin{Promise}, which the @tech{backend} will @jsin{await}, if it needs to perform an asynchronous action.

The backend provides a value, @jsin{_version}, which is a string representation of the Reach version used to compile the program.
For example, the version of Reach used to produce this documentation would contain the string @jsin{'@|reach-vers|'}.

The backend provides a function, @(mint-define! '("getExports")) @jsin{getExports}, which exposes the @tech{exports} of a Reach program.
This function receives the standard library as an argument and returns an object with all the exports.
For example, if a Reach program
exported a variable @tt{x}, i.e. @reachin{export const x = 5}, the frontend could access the value in the following manner:

@js{
   const stdlib = await loadStdlib();
   backend.getExports(stdlib).x; // 5
}

Finally, the backend provides a value, @jsin{_Connectors}, which is an opaque object representing the @tech{connectors} the app was compiled for.

@section[#:tag "ref-backends-js-guarantees"]{Guarantees}

This backend does not guarantee that values in a positive position in a @tech{participant interact interface}, that are later passed to a negative position in a @tech{participant interact interface}, will be identical, in the sense of JavaScript's @jsin{===} operator, to the original value.
In other words, this backend does not ensure that Reach programs are parametric over JavaScript values that they interact with.

Positive and negative are best understood by example with a function type: a positive position is supplied by the function, such as the result; while a negative position is supplied by the caller, such as the arguments.
These notions generalize, however, to higher (and lower) order contexts.
In the case of Reach, this means that non-function values in a @tech{participant interact interface} are positive.

For example, if the Reach program,

@reach{
 Reach.App( {},
  [ Participant("A", {
      get: Bytes(32),
      give: Fun([Bytes(32)], Bool) } ) ],
  (A) => {
   A.only(() => {
    const x = interact.give(interact.get); });
   A.publish(x);
   commit(); });
}

is given the @jsin{interact} object,

@js{
 const x = "A string";
 { get: x,
   give: (str) => x === str } }

then it is not guaranteed that @reachin{A} will publish @reachin{true}, because the @jsin{str} given to @jsin{give} may not be identical to @jsin{x}.
(However, they are @jsin{bytesEq}.)

