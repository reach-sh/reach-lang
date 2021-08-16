


# {#ref-backends-rpc-proto} Reach RPC Protocol Specification

The Reach RPC Protocol (hereafter, "the protocol" or "it") is an instance of [JSON](https://en.wikipedia.org/wiki/JSON)-based RPC protocol.

It should be transported over [HTTPS](https://en.wikipedia.org/wiki/HTTPS) (i.e. HTTP over TLS).

Requests must include an `X-API-Key` header whose value is a shared secret between a server instance and an RPC client, referred to as the <Defn :name="API key">API key</Defn>.
Typically this value comes from the environment variable `REACH_RPC_KEY` and is the [Base64](https://en.wikipedia.org/wiki/Base64) encoding of 24 random bytes.

Requests must use the `POST` HTTP method.

Requests specify the RPC method to be invoked via the HTTP request target.

Requests must include a [JSON](https://en.wikipedia.org/wiki/JSON)-encoded array in their body.
Requests should indicate this by setting the `Content-Type` header to `application/json; charset=utf-8`.
This array is interpreted as the arguments to the RPC method.

Responses must include a [JSON](https://en.wikipedia.org/wiki/JSON)-encoded value in their body.
Responses should indicate this by setting the `Content-Type` header to `application/json; charset=utf-8`.

Responses may include <Defn :name="RPC handles">RPC handles</Defn>, which are strings that represent intermediate resources held on the RPC server that cannot be serialized to JSON.

RPC methods are either synchronous value RPC methods or interactive RPC methods.

---

<Defn :name="Synchronous value RPC methods">Synchronous value RPC methods</Defn> consume arguments and produce a single result without further interaction with the client.
The result is the body of the response.

For example, `formatCurrency` is a synchronous value RPC method.
A call to `formatCurrency("19283.1035819471", 4)` would be represented by the following HTTP session, with request lines indicated by `+` and response lines indicated by `-`:

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


---

<Defn :name="Interactive RPC methods">Interactive RPC methods</Defn> consume arguments, including a specification of interactive RPC callbacks, and produce an interactive RPC continuation.

An <Defn :name="interactive RPC callback">interactive RPC callback</Defn> is a key of a JSON object, bound to `true`, that indicates that the initiator of an interactive RPC method responds to requests for further data during the execution of this call.

An <Defn :name="interactive RPC continuation">interactive RPC continuation</Defn> is a JSON object that matches either:
+ `{t: "Done", ans}`, where `ans` is the final result of the original interactive RPC method.
+ `{t: "Kont", kid, m, args}`, where `kid` is an RPC handle, `m` is a string naming one of the interactive RPC callback methods, and `args` is an array of the arguments to that method.


When a ``Kont`` value is produced, then the interactive RPC method is suspended until the `/kont` RPC method is invoked with the continuation RPC handle and the return value of the interactive RPC callback.
The result of the `/kont` RPC method is another interactive RPC continuation.

Clients may perform any RPC methods while an interactive RPC method is suspended.

The server may re-use the same interactive RPC continuation handle many times.

For example, the execution of a backend is an interactive RPC method.
An example interaction might be represented by the following HTTP session, with request lines indicated by `+` and response lines indicated by `-`:

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


