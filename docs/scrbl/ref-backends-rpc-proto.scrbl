#lang scribble/manual
@(require "lib.rkt")

@title[#:version reach-vers #:tag "ref-backends-rpc-proto"]{Reach RPC Protocol Specification}

The Reach RPC Protocol (hereafter, "the protocol" or "it") is an instance of @link["https://en.wikipedia.org/wiki/JSON"]{JSON}-based RPC protocol.

It should be transported over @link["https://en.wikipedia.org/wiki/HTTPS"]{HTTPS} (i.e. HTTP over TLS).

Requests must include an @litchar{X-API-Key} header whose value is a shared secret between a server instance and an RPC client, referred to as the @deftech{API key}.
Typically this value comes from the environment variable @envvar{REACH_RPC_KEY} and is the @link["https://en.wikipedia.org/wiki/Base64"]{Base64} encoding of 24 random bytes.

Requests must use the @litchar{POST} HTTP method.

Requests specify the @tech{RPC method} to be invoked via the HTTP request target.

Requests must include a @link["https://en.wikipedia.org/wiki/JSON"]{JSON}-encoded array in their body.
Requests should indicate this by setting the @litchar{Content-Type} header to @litchar{application/json; charset=utf-8}.
This array is interpreted as the arguments to the @tech{RPC method}.

Responses must include a @link["https://en.wikipedia.org/wiki/JSON"]{JSON}-encoded value in their body.
Responses should indicate this by setting the @litchar{Content-Type} header to @litchar{application/json; charset=utf-8}.

Responses may include @deftech{RPC handles}, which are strings that represent intermediate resources held on the RPC server that cannot be serialized to JSON.

@tech{RPC methods} are either @tech{synchronous value RPC methods} or @tech{interactive RPC methods}.

@(hrule)

@deftech{Synchronous value RPC methods} consume arguments and produce a single result without further interaction with the client.
The result is the body of the response.

For example, @jsin{formatCurrency} is a @tech{synchronous value RPC method}.
A call to @jsin{formatCurrency("19283.1035819471", 4)} would be represented by the following HTTP session, with request lines indicated by @tt{+} and response lines indicated by @tt{-}:

@verbatim{
+ POST /stdlib/formatCurrency HTTP/1.1
+ X-API-Key: OpenSesame
+ Content-Type: application/json; charset=utf-8
+
+ [ "19283.1035819471", 4 ]

- HTTP/1.1 200 OK
- Content-Type: application/json; charset=utf-8
-
- "19283.1035"
}

@(hrule)

@deftech{Interactive RPC methods} consume arguments, including a specification of @tech{interactive RPC callbacks}, and produce an @tech{interactive RPC continuation}.

An @deftech{interactive RPC callback} is a key of a JSON object, bound to @jsin{true}, that indicates that the initiator of an @tech{interactive RPC method} responds to requests for further data during the execution of this call.

An @deftech{interactive RPC continuation} is a JSON object that matches either:
@itemlist[

@item{@jsin{{t: "Done", ans}}, where @jsin{ans} is the final result of the original @tech{interactive RPC method}.}

@item{@jsin{{t: "Kont", kid, m, args}}, where @jsin{kid} is an @tech{RPC handle}, @jsin{m} is a string naming one of the @tech{interactive RPC callback} methods, and @jsin{args} is an array of the arguments to that method.}

]

When a @jsin{`Kont`} value is produced, then the @tech{interactive RPC method} is suspended until the @tt{/kont} @tech{RPC method} is invoked with the continuation @tech{RPC handle} and the return value of the @tech{interactive RPC callback}.
The result of the @tt{/kont} @tech{RPC method} is another @tech{interactive RPC continuation}.

Clients may perform any @tech{RPC methods} while an @tech{interactive RPC method} is suspended.

The server may re-use the same @tech{interactive RPC continuation} handle many times.

For example, the execution of a @tech{backend} is an @tech{interactive RPC method}.
An example interaction might be represented by the following HTTP session, with request lines indicated by @tt{+} and response lines indicated by @tt{-}:

@verbatim{
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

}

