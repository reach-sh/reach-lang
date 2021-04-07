#lang scribble/manual
@(require "lib.rkt")

@title[#:version reach-vers #:tag "ref-backends-rpc-client"]{Reach RPC Protocol Client Implementation Walkthrough}

The @seclink["ref-backends-rpc-proto"]{Reach RPC Protocol} is designed to be simple to implement in languages that support HTTP and JSON interaction.
This document walks through the implementation of an RPC client in @link["https://www.python.org"]{Python}.
An example use of this library is shown in the @seclink["tut-7-rpc"]{tutorial section on RPC-based frontends}.
The entire library is 80 lines of code.

@(define py-impl "py/src/reach_rpc/__init__.py")

The library uses a few standard Python libraries for interacting with JSON,
HTTP servers, and networking:

@reachex[#:show-lines? #t #:dir "rpc-client" py-impl #:link #t
         'only 1 9 "# ..."]

@reachex[#:show-lines? #t #:dir "rpc-client" py-impl #:link #t
         'only 11 27 "# ..."]

The library provides a single function, @pyin{mk_rpc}, that accepts the @secref["ref-backends-rpc-opts"].

@reachex[#:show-lines? #t #:dir "rpc-client" py-impl #:link #t
         'only 28 34 "# ..."]

It starts by observing the @litchar{verify} option and informing the Python library it uses for HTTPS interaction to turn off warnings.
It displays a warning to users that they should be nervous about using this setting.

@reachex[#:show-lines? #t #:dir "rpc-client" py-impl #:link #t
         'only 35 47 "# ..."]

Next, it attempts to connect to the Reach RPC Server and throws an error if it does not respond quickly enough.

@reachex[#:show-lines? #t #:dir "rpc-client" py-impl #:link #t
         'only 52 62 "# ..."]

It defines a function, @pyin{rpc}, which will be returned later on, that
implements the protocol for @tech{synchronous value RPC methods}.
It formats a given request, posts it, and then returns the deserialized result.
It prints debugging information for convenience.

@reachex[#:show-lines? #t #:dir "rpc-client" py-impl #:link #t
         'only 63 79 "# ..."]

It defines a function, @pyin{rpc_callbacks}, which will be returned later on, that
implements the protocol for @tech{interactive RPC methods}.
On lines 64 and 65, this function inspects its third argument, @pyin{cbacks},
and separates the @pyin{callable} arguments from the values and creates the
intermediate objects, @pyin{vals} and @pyin{meths}, to provide the RPC
invocation.
After it makes the call, in the @pyin{while} loop starting on line 68, it
inspects the result to determine if it is a final answer or an
@tech{interactive RPC callback}.
If it is a callback, as indicated by the test on line 72, then it extracts the
name of the method, @pyin{p['m']}, and invokes it in the original third
argument, @pyin{cbacks}, with the provided arguments.
It replaces the @pyin{p} value with the result of that continuation invocation and continues.

@reachex[#:show-lines? #t #:dir "rpc-client" py-impl #:link #t
         'only 80 80 "# ..."]

Finally, it returns @pyin{rpc} and @pyin{rpc_callbacks} to the user.
