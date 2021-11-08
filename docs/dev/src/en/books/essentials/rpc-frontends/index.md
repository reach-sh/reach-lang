---
menuItem: mi-docs
---

# Reach RPC Server

The Reach RPC Server provides access to compiled JavaScript backends via an HTTPS-accessible JSON-based RPC protocol. The server allows frontends to be written in any programming language. Reach provides client libraries for JavaScript, Python, and Go. It is easy to implement a client library yourself. An example frontend written using the Reach RPC Server is shown in the tutorial section on RPC-based frontends.

The command `reach rpc-server` starts an instance of the Reach RPC Server.

# RPC Methods

The Reach RPC Server supports the following RPC methods:

* `/health` returns true to indicate the server is running properly.

* `/stdlib/$METHOD` where `$METHOD` is a function of the JavaScript standard library.

    All `/stdlib` methods are synchronous value RPC methods that accept and produce the same arguments and return values as the corresponding function, encoded as JSON objects, except those that produce or consume account representations.

    Those methods instead accept and produce account RPC handles, which are random strings that represent the corresponding account representations. For example, `/stdlib/newTestAccount` does not return an account like `newTestAccount`, but instead returns an account RPC handle.

* `/forget/acc` accepts an account RPC handle and deletes it from the Reach RPC Server’s memory.

* `/acc/$METHOD` where `$METHOD` is a method of an account representation of the JavaScript standard library.

    All `/acc` methods are synchronous value RPC methods that accept and produce the same arguments and return values as the corresponding function, encoded as JSON objects, except they accept an additional first argument, which is the account RPC handle returned by a prior RPC method invocation; and, a method that accepts a backend (like /acc/attach (i.e. acc.attach) or /acc/deploy (i.e. acc.deploy) does not accept a backend argument, but has it implicitly provided by the Reach RPC Server.

    Furthermore, those that produce contract representations, instead produce contract RPC handles. For example, /acc/deploy does not return a contract representation like acc.deploy, but instead returns a contract RPC handle.

* `/forget/ctc` accepts a contract RPC handle and deletes it from the Reach RPC Server’s memory.

* `/ctc/$METHOD` where `$METHOD` is a method of a contract representation of the JavaScript standard library.

    All `/ctc` methods are synchronous value RPC methods that accept and produce the same arguments and return values as the corresponding function, encoded as JSON objects, except they accept an additional first argument, which is the contract RPC handle returned by a prior RPC method invocation.

* `/backend/$PARTICIPANT` where `$PARTICIPANT` is a participant of the backend compiled by the JavaScript backend.

    All `/backend` methods are interactive RPC methods that accept three arguments:

    `ctcId` — A contract RPC handle to provide as the contract to the backend

    `values` — An object containing the non-function components of the participant interact interface of the backend.

    `methods` — An object whose keys correspond to the function components of the participant interact interface of the backend, but whose values are true.

    As a special case, if values contains stdlib.hasRandom bound to true, then the JavaScript standard library’s implementation of hasRandom is provided to the backend.

    As the backend executes, any of the components of methods invoked will be executed as interactive RPC callbacks as described by the Reach RPC Protocol Specification. Reach RPC Client libraries should expose a function that hides the details of the construction of the values and methods objects and implements interactive RPC callback handlers automatically.

* `/kont` handles interactive RPC continuation completion during an interactive RPC method. It should not be invoked directly by frontends.

* `/stop` quits the server.

# Client Options

Reach RPC client libraries must accept a dictionary data structure with the following keys for customizing their behavior:

* host — This value sets the hostname to contact for the Reach RPC Server instance. If it is not present, the client library must default to the value of the environment variable REACH_RPC_SERVER.

* port — This value sets the TCP port to contact for the Reach RPC Server instance. If it is not present, the client library must default to the value of the environment variable REACH_RPC_PORT.

* verify — This value determines whether to verify the TLS certificate of the Reach RPC Server instance. If it is not present, the client library must default to the value of the environment variable REACH_RPC_TLS_REJECT_UNVERIFIED. If that is not present, it must default to true.

    To disable verification, set this value to the string: "0"; any other value will be considered to mean "enable".

* timeout — This value sets the number of seconds to wait for the Reach RPC Server instance to respond to its first request. If it is not present, the client library must default to the value of the environment variable REACH_RPC_TIMEOUT. If that is not present, it must default to 5 seconds.

* key — This value sets the API key for the Reach RPC Server instance. If it is not present, the client library must default to the value of the environment variable REACH_RPC_KEY.

# Protocol Specification

The Reach RPC Protocol (hereafter, "the protocol" or "it") is an instance of JSON-based RPC protocol.

It should be transported over HTTPS (i.e. HTTP over TLS).

Requests must include an X-API-Key header whose value is a shared secret between a server instance and an RPC client, referred to as the API key. Typically this value comes from the environment variable REACH_RPC_KEY and is the Base64 encoding of 24 random bytes.

Requests must use the POST HTTP method.

Requests specify the RPC method to be invoked via the HTTP request target.

Requests must include a JSON-encoded array in their body. Requests should indicate this by setting the Content-Type header to application/json; charset=utf-8. This array is interpreted as the arguments to the RPC method.

Responses must include a JSON-encoded value in their body. Responses should indicate this by setting the Content-Type header to application/json; charset=utf-8.

Responses may include RPC handles, which are strings that represent intermediate resources held on the RPC server that cannot be serialized to JSON.

RPC methods are either synchronous value RPC methods or interactive RPC methods.

## Synchronous Value RPC Methods

Synchronous value RPC methods consume arguments and produce a single result without further interaction with the client. The result is the body of the response.

For example, formatCurrency is a synchronous value RPC method. A call to formatCurrency("19283.1035819471", 4) would be represented by the following HTTP session, with request lines indicated by + and response lines indicated by -:

```
+ POST /stdlib/formatCurrency HTTP/1.1
+ X-API-Key: OpenSesame
+ Content-Type: application/json; charset=utf-8
+
+ [ "19283.1035819471", 4 ]
- HTTP/1.1 200 OK
- Content-Type: application/json; charset=utf-8
-
- "19283.1035"
```

## Interactive RPC Methods

Interactive RPC methods consume arguments, including a specification of interactive RPC callbacks, and produce an interactive RPC continuation.

An interactive RPC callback is a key of a JSON object, bound to true, that indicates that the initiator of an interactive RPC method responds to requests for further data during the execution of this call.

An interactive RPC continuation is a JSON object that matches either:

* `{t: "Done", ans}`, where ans is the final result of the original interactive RPC method.

* `{t: "Kont", kid, m, args}`, where kid is an RPC handle, m is a string naming one of the interactive RPC callback methods, and args is an array of the arguments to that method.

When a `Kont` value is produced, then the interactive RPC method is suspended until the /kont RPC method is invoked with the continuation RPC handle and the return value of the interactive RPC callback. The result of the /kont RPC method is another interactive RPC continuation.

Clients may perform any RPC methods while an interactive RPC method is suspended.

The server may re-use the same interactive RPC continuation handle many times.

For example, the execution of a backend is an interactive RPC method. An example interaction might be represented by the following HTTP session, with request lines indicated by + and response lines indicated by -:

```
+ POST /backend/Alice HTTP/1.1
+ X-API-Key: OpenSesame
+ Content-Type: application/json; charset=utf-8
+
+ [ "Contract-42", { "price": 10 }, { "showX": true } ]
- HTTP/1.1 200 OK
- Content-Type: application/json; charset=utf-8
-
- { t: "Kont", kid: "Kont-A", m: "showX", args: [ "19283.1035819471" ] }
+ POST /stdlib/formatCurrency HTTP/1.1
+ X-API-Key: OpenSesame
+ Content-Type: application/json; charset=utf-8
+
+ [ "19283.1035819471", 4 ]
- HTTP/1.1 200 OK
- Content-Type: application/json; charset=utf-8
-
- "19283.1035"
```

```
+ POST /kont HTTP/1.1
+ X-API-Key: OpenSesame
+ Content-Type: application/json; charset=utf-8
+
+ [ "Kont-A", null ]
- HTTP/1.1 200 OK
- Content-Type: application/json; charset=utf-8
-
- { t: "Done", ans: null }
```

# Implementation Walkthrough

The Reach RPC Protocol is designed to be simple to implement in languages that support HTTP and JSON interaction. This document walks through the implementation of an [RPC client in Python](https://github.com/reach-sh/reach-lang/blob/master/rpc-client/py/src/reach_rpc/__init__.py). An example use of this library is shown in the tutorial section on RPC-based frontends. The entire library is 80 lines of code.

## Standard Libraries

The library uses a few standard Python libraries for interacting with JSON, HTTP servers, and networking:

```python
load: https://raw.githubusercontent.com/reach-sh/reach-lang/master/rpc-client/py/src/reach_rpc/__init__.py
range: 1-8
```

## Define mk_rpc

The library provides a single function, `mk_rpc`, that accepts the Reach RPC Client Standard Options:

```python
load: https://raw.githubusercontent.com/reach-sh/reach-lang/master/rpc-client/py/src/reach_rpc/__init__.py
range: 11-26
```

## Turn off warnings

It starts by observing the verify option and informing the Python library it uses for HTTPS interaction to turn off warnings. It displays a warning to users that they should be nervous about using this setting:

```python
load: https://raw.githubusercontent.com/reach-sh/reach-lang/master/rpc-client/py/src/reach_rpc/__init__.py
range: 28-33
```

## Connect to server

Next, it attempts to connect to the Reach RPC Server and throws an error if it does not respond quickly enough:

```python
load: https://raw.githubusercontent.com/reach-sh/reach-lang/master/rpc-client/py/src/reach_rpc/__init__.py
range: 35-46
```

## Define rpc

It defines a function, `rpc`, which will be returned later on, that implements the protocol for synchronous value RPC methods. It formats a given request, posts it, and then returns the deserialized result. It prints debugging information for convenience.

```python
load: https://raw.githubusercontent.com/reach-sh/reach-lang/master/rpc-client/py/src/reach_rpc/__init__.py
range: 52-61
```

## Define rpc_callbacks

It defines a function, rpc_callbacks, which will be returned later on, that implements the protocol for interactive RPC methods. On lines 64 and 65, this function inspects its third argument, cbacks, and separates the callable arguments from the values and creates the intermediate objects, vals and meths, to provide the RPC invocation. After it makes the call, in the while loop starting on line 68, it inspects the result to determine if it is a final answer or an interactive RPC callback. If it is a callback, as indicated by the test on line 72, then it extracts the name of the method, p['m'], and invokes it in the original third argument, cbacks, with the provided arguments. It replaces the p value with the result of that continuation invocation and continues:

```python
load: https://raw.githubusercontent.com/reach-sh/reach-lang/master/rpc-client/py/src/reach_rpc/__init__.py
range: 63-78
```

## Return references

Finally, it returns rpc and rpc_callbacks to the user:

```python
load: https://raw.githubusercontent.com/reach-sh/reach-lang/master/rpc-client/py/src/reach_rpc/__init__.py
range: 80
```
