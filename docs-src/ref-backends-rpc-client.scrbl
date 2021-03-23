#lang scribble/manual
@(require "lib.rkt")

@title[#:version reach-vers #:tag "ref-backends-rpc-client"]{Reach RPC Protocol Client Implementation Walkthrough}

@;TODO fix linking (frontend example -> client library), referenced code line numbers

The @seclink["ref-backends-rpc-proto"]{Reach RPC Protocol} is designed to be simple to implement in languages that support HTTP and JSON interaction.
This document walks through the implementation of an RPC client in @link["https://www.python.org"]{Python}.
An example use of this library is shown in the @seclink["tut-7-rpc"]{tutorial section on RPC-based frontends}.
The entire library is 75 lines of code.

@(define py-impl "tut-7-rpc/client-py/index.py")

@reachex[#:show-lines? #t py-impl #:link #t
         'only 1 7 "# ..."]

The library uses a few standard Python libraries for interacting with JSON, HTTP servers, and networking:

@reachex[#:show-lines? #t py-impl #:link #t
         'only 8 26 "# ..."]

The library provides a single function, @pyin{mk_rpc}, that accepts the @secref["ref-backends-rpc-opts"].

@reachex[#:show-lines? #t py-impl #:link #t
         'only 27 33 "# ..."]

It starts by observing the @litchar{verify} option and informing the Python library it uses for HTTPS interaction to turn off warnings.
It displays a warning to users that they should be nervous about using this setting.

@reachex[#:show-lines? #t py-impl #:link #t
         'only 34 46 "# ..."]

Next, it attempts to connect to the Reach RPC Server and throws an error if it does not respond quickly enough.

@reachex[#:show-lines? #t py-impl #:link #t
         'only 47 57 "# ..."]

It defines a function, @pyin{rpc}, which will be returned later on, that
implements the protocol for @tech{synchronous value RPC methods}.
It formats a given request, posts it, and then returns the result as JSON.
It prints debugging information for convenience.

@reachex[#:show-lines? #t py-impl #:link #t
         'only 58 74 "# ..."]

It defines a function, @pyin{rpc_callbacks}, which will be returned later on, that
implements the protocol for @tech{interactive RPC methods}.
On lines 47 and 48, this function inspects its second argument, @pyin{cbacks}, and separates the @pyin{callable} arguments from the values and creates the intermediate objects, @pyin{vals} and @pyin{meths}, to provide the RPC invocation.
After it makes the call, in the @pyin{while} loop starting on line 51, it inspects the result to determine if it is a final answer or an @tech{interactive RPC callback}.
If it is a callback, as indicated by the test on line 55, then it extracts the name of the method, @pyin{p['m']}, and invokes it in the original second argument, @pyin{cbacks}, with the provided arguments.
It replaces the @pyin{p} value with the result of that continuation invocation and continues.

@reachex[#:show-lines? #t py-impl #:link #t
         'only 75 75 "# ..."]

Finally, it returns @pyin{rpc} and @pyin{rpc_callbacks} to the user.
