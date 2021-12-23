# {#ref-backends-rpc} RPC Server

The Reach RPC Server provides access to compiled JavaScript backends via an [HTTPS-accessible JSON-based RPC protocol](##ref-backends-rpc-proto).
The server allows frontends to be written in any programming language.
Reach provides client libraries for
[C#](##ref-frontends-rpc-cs),
[JavaScript](##ref-frontends-rpc-js),
[Python](##ref-frontends-rpc-py), and
[Go](##ref-frontends-rpc-go).
It is easy to [implement a client library yourself](##ref-backends-rpc-client).
An example frontend written using the Reach RPC Server is shown in the [tutorial section on RPC-based frontends](##tut-7-rpc).

The command
```cmd
$ reach rpc-server
```
 starts an instance of the Reach RPC Server.

---

The Reach RPC Server supports the following @{defn("RPC methods")}:

+ `/health` returns `{!js} true` to indicate the server is running properly.
+ `/stdlib/$METHOD` where `$METHOD` is a function of the [JavaScript standard library](##ref-frontends-js).

All `/stdlib` methods are synchronous value RPC methods that accept and produce the same arguments and return values as the corresponding function, encoded as JSON objects, except those that produce or consume account representations.

Those methods instead accept and produce account RPC handles, which are random strings that represent the corresponding account representations.
For example, `/stdlib/newTestAccount` does not return an account like `{!js} newTestAccount`, but instead returns an account RPC handle.

+ `/forget/acc` accepts an account RPC handle and deletes it from the Reach RPC Server's memory.
+ `/acc/$METHOD` where `$METHOD` is a method of an account representation of the [JavaScript standard library](##ref-frontends-js).

All `/acc` methods are synchronous value RPC methods that accept and produce the same arguments and return values as the corresponding function, encoded as JSON objects, except they accept an additional first argument, which is the account RPC handle returned by a prior RPC method invocation; and, a method that accepts a backend (like `/acc/attach` (i.e. `{!js} acc.attach`) or `/acc/deploy` (i.e. `{!js} acc.deploy`) does not accept a backend argument, but has it implicitly provided by the Reach RPC Server.

Furthermore, those that produce contract representations instead produce contract RPC handles.
For example, `/acc/deploy` does not return a contract representation like `{!js} acc.deploy`, but instead returns a contract RPC handle.

+ `/forget/ctc` accepts a contract RPC handle and deletes it from the Reach RPC Server's memory.
+ `/ctc/$METHOD` where `$METHOD` is a method of a contract representation of the [JavaScript standard library](##ref-frontends-js).

Most `/ctc` methods are synchronous value RPC methods that accept and produce the same arguments and return values as the corresponding function, encoded as JSON objects, except they accept an additional first argument, which is the contract RPC handle returned by a prior RPC method invocation.

`/ctc/p/$PARTICIPANT` and `/ctc/participants/$PARTICIPANT` are interactive RPC method alternatives to the `/backend/$PARTICIPANT` method described below.
They accept the same arguments and behave identically.

+ `/forget/token` accepts a token RPC handle and deletes it from the Reach RPC Server's memory.
+ `/backend/$PARTICIPANT` where `$PARTICIPANT` is a participant of the backend compiled by the [JavaScript backend](##ref-backends-js).

All `/backend/$PARTICIPANT` methods are interactive RPC methods that accept three arguments:
+ `ctcId` --- A contract RPC handle to provide as the contract to the backend
+ `values` --- An object containing the non-function components of the participant interact interface of the backend.
+ `methods` --- An object whose keys correspond to the function components of the participant interact interface of the backend, but whose values are `{!js} true`.


As a special case, if `values` contains `stdlib.hasRandom` bound to `{!js} true`, then the [JavaScript standard library](##ref-frontends-js)'s implementation of `{!js} hasRandom` is provided to the backend.

As the backend executes, any of the components of `methods` invoked will be executed as interactive RPC callbacks as described by the [Reach RPC Protocol Specification](##ref-backends-rpc-proto).
Reach RPC Client libraries _should_ expose a function that hides the details of the construction of the `values` and `methods` objects and implements interactive RPC callback handlers automatically.

+ `/backend/getExports/$EXPORT_NAME` where `$EXPORT_NAME` is an export of the backend compiled by the [JavaScript backend](##ref-backends-js).

Any Reach module exports, including functions, may be accessed via this method.
Field accessors and zero-indexed array references support arbitrarily deep nesting.

For example:
```
RPC /backend/getExports/o          # Top-level data
RPC /backend/getExports/o/foo/bar  # Data nested within data
RPC /backend/getExports/a/0/2      # A deeply-nested element of an array
RPC /backend/getExports/add 2 3    # 5; i.e. `add(2, 3)`
```


Requests for non-existent exports and non-existent nested fields or elements always return `null`.
+ `/kont` handles interactive RPC continuation completion during an interactive RPC method.
It should not be invoked directly by frontends.

+ `/stop` quits the server.


## {#ref-backends-rpc-opts} Reach RPC Client Standard Options

Reach RPC client libraries must accept a dictionary data structure with the following keys for customizing their behavior:

+ `host` --- This value sets the hostname to contact for the Reach RPC Server instance.
If it is not present, the client library must default to the value of the environment variable `REACH_RPC_SERVER`.
+ `port` --- This value sets the TCP port to contact for the Reach RPC Server instance.
If it is not present, the client library must default to the value of the environment variable `REACH_RPC_PORT`.
+ `verify` --- This value determines whether to verify the TLS certificate of the Reach RPC Server instance.
If it is not present, the client library must default to the value of the environment variable `REACH_RPC_TLS_REJECT_UNVERIFIED`.
If that is not present, it must default to true.

To disable verification, set this value to the string: `"0"`; any other value will be considered to mean "enable".
+ `timeout` --- This value sets the number of seconds to wait for the Reach RPC Server instance to respond to its first request.
If it is not present, the client library must default to the value of the environment variable `REACH_RPC_TIMEOUT`.
If that is not present, it must default to 5 seconds.
+ `key` --- This value sets the API key for the Reach RPC Server instance.
If it is not present, the client library must default to the value of the environment variable `REACH_RPC_KEY`.




