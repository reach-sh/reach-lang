


# {#ref-backends-rpc-client} Reach RPC Protocol Client Implementation Walkthrough

The [Reach RPC Protocol](##ref-backends-rpc-proto) is designed to be simple to implement in languages that support HTTP and JSON interaction.
This document walks through the implementation of an RPC client in [Python](https://www.python.org).
An example use of this library is shown in the [tutorial section on RPC-based frontends](##tut-7-rpc).
The entire library is 80 lines of code.



The library uses a few standard Python libraries for interacting with JSON,
HTTP servers, and networking:

${code("/rpc-client/py/src/reach_rpc/__init__.py", 1, 9)}

${code("/rpc-client/py/src/reach_rpc/__init__.py", 11, 27)}

The library provides a single function, `mk_rpc`, that accepts the ${seclink("ref-backends-rpc-opts")}.

${code("/rpc-client/py/src/reach_rpc/__init__.py", 28, 34)}

It starts by observing the `verify` option and informing the Python library it uses for HTTPS interaction to turn off warnings.
It displays a warning to users that they should be nervous about using this setting.

${code("/rpc-client/py/src/reach_rpc/__init__.py", 35, 47)}

Next, it attempts to connect to the Reach RPC Server and throws an error if it does not respond quickly enough.

${code("/rpc-client/py/src/reach_rpc/__init__.py", 52, 62)}

It defines a function, `rpc`, which will be returned later on, that
implements the protocol for synchronous value RPC methods.
It formats a given request, posts it, and then returns the deserialized result.
It prints debugging information for convenience.

${code("/rpc-client/py/src/reach_rpc/__init__.py", 63, 79)}

It defines a function, `rpc_callbacks`, which will be returned later on, that
implements the protocol for interactive RPC methods.
On lines 64 and 65, this function inspects its third argument, `cbacks`,
and separates the `callable` arguments from the values and creates the
intermediate objects, `vals` and `meths`, to provide the RPC
invocation.
After it makes the call, in the `while` loop starting on line 68, it
inspects the result to determine if it is a final answer or an
interactive RPC callback.
If it is a callback, as indicated by the test on line 72, then it extracts the
name of the method, `p['m']`, and invokes it in the original third
argument, `cbacks`, with the provided arguments.
It replaces the `p` value with the result of that continuation invocation and continues.

${code("/rpc-client/py/src/reach_rpc/__init__.py", 80, 80)}

Finally, it returns `rpc` and `rpc_callbacks` to the user.
