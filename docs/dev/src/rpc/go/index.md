# {#ref-frontends-rpc-go} Go

:::note
This frontend library relies on the [Reach RPC Server](##ref-backends-rpc).
:::


A [Go](https://golang.org) client library for the
[Reach RPC protocol](##ref-backends-rpc) may be installed by running:

```
$ go get github.com/reach-sh/reach-lang/rpc-client/go
```


Once installed, add the following import line to your Go file which will connect
to the [RPC server](##ref-backends-rpc):
```go
import reachrpc "github.com/reach-sh/reach-lang/rpc-client/go"
```


The library provides the following bindings:

---
@{ref("go", "reachrpc.Mk")}
```go
rpc, rpcCallbacks := reachrpc.Mk(opts)
```


`{!go} reachrpc.Mk` accepts the @{seclink("ref-backends-rpc-opts")} as a `{!go} map`
and returns two functions, traditionally called `{!go} rpc` and
`{!go} rpcCallbacks`.

@{ref("go", "rpc")}
`{!go} rpc` is a function that invokes a synchronous value RPC method.
It takes a string, naming the RPC method, and some JSON values to provide as arguments.
It returns a single JSON value as the result.

For example,

```go
rpc("/stdlib/formatCurrency", i, 4).(string)
```


calls `{!js} formatCurrency` with some value `{!go} i` and `{!go} 4` and returns the result as a string.

@{ref("go", "rpcCallbacks")}
`{!go} rpcCallbacks` is a function that invokes an interactive RPC method, such as for a backend.
It takes a string, naming the RPC method, a JSON value as an argument, and map from strings to JSON values or functions.
The functions will be provided as interactive RPC callbacks to the RPC method and should expect JSON values as arguments and return a JSON value as a result.
It returns `{!go} void`.

For example,

```go
showX := func(xo interface{}) {
  x := int(rpc("/stdlib/bigNumberToNumber", xo).(float64))
  fmt.Printf("Alice saw that X is %f\n", x)
}
ms := map[string]interface{} {
  "price": 10,
  "showX": showX,
}
rpcCallbacks("/backend/Alice", ctc, ms)
```


calls a backend named `Alice` with the contract `{!go} ctc` and a value named `price` and a method named `showX` that prints out a result from the Reach backend.

