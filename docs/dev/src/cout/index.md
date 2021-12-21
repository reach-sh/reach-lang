# {#ref-backends} Compiled Output

This section describes the compilation output of Reach version @{VERSION},
which are participant backends.

They are accessed via [JavaScript](##ref-backends-js) or via [the RPC server](##ref-backends-rpc).

## {#ref-backends-js} JavaScript

The Reach JavaScript backend produces a compilation output named `input.APP.mjs`.
This will normally be imported by writing:

@{ref("js", "backend")}
```js
import * as backend from './build/index.main.mjs';
```


This module exports an asynchronous function for each participant.
For example, if a Reach program contains a participant named `{!rsh} 'A'` in the `{!rsh} Reach.App`, then the JavaScript backend will include a function named `{!js} A` (i.e. `{!js} backend.A`).
The `{!js} Promise` returned by these functions is resolved when the Reach program terminates (i.e. reaches `{!rsh} exit();`).

Each function accepts two arguments: `{!js} ctc` and `{!js} interact`. These functions should be called by the frontend.

The `{!js} ctc` argument is the result of a call to the functions `{!js} acc.deploy` or `{!js} acc.attach` provided by the [JavaScript frontend support library](##ref-frontends-js).

The `{!js} interact` argument is an object matching the participant interact interface for the corresponding participant.
The types of values this object contains must match those specified
[on this list](##ref-frontends-js-types).
Each function may return a `{!js} Promise`, which the backend will `{!js} await`, if it needs to perform an asynchronous action.

The backend provides a value, `{!js} _version`, which is a string representation of the Reach version used to compile the program.
For example, the version of Reach used to produce this documentation would contain the string `{!js} 'reach-vers'`.

The backend provides a function, @{ref("js", "getExports")} `{!js} getExports`, which exposes the exports of a Reach program.
This function receives the standard library as an argument and returns an object with all the exports present in the module being compiled.
:::note
It's possible to expose bindings from other modules to `{!js} getExports` by re-exporting them in your "top-level" module.
:::


For example, if a Reach program
exported a variable `x`, i.e. `{!rsh} export const x = 5`, the frontend could access the value in the following manner:

```js
const stdlib = await loadStdlib();
backend.getExports(stdlib).x; // 5
```


Finally, the backend provides a value, `{!js} _Connectors`, which is an opaque object representing the connectors the app was compiled for.

### {#ref-backends-js-guarantees} Guarantees

This backend does not guarantee that values in a positive position in a participant interact interface, that are later passed to a negative position in a participant interact interface, will be identical, in the sense of JavaScript's `{!js} ===` operator, to the original value.
In other words, this backend does not ensure that Reach programs are parametric over JavaScript values that they interact with.

Positive and negative are best understood by example with a function type: a positive position is supplied by the function, such as the result; while a negative position is supplied by the caller, such as the arguments.
These notions generalize, however, to higher (and lower) order contexts.
In the case of Reach, this means that non-function values in a participant interact interface are positive.

For example, if the Reach program,

```reach
Reach.App( {},
 [ Participant("A", {
     get: Bytes(32),
     give: Fun([Bytes(32)], Bool) } ) ],
 (A) => {
  A.only(() => {
   const x = interact.give(interact.get); });
  A.publish(x);
  commit(); });
```


is given the `{!js} interact` object,

```js
const x = "A string";
{ get: x,
  give: (str) => x === str } 
```


then it is not guaranteed that `{!rsh} A` will publish `{!rsh} true`, because the `{!js} str` given to `{!js} give` may not be identical to `{!js} x`.
(However, they are `{!js} bytesEq`.)

