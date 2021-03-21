#lang scribble/manual
@(require "lib.rkt")

@title[#:version reach-vers #:tag "ref-frontends-rpc-go"]{Go (RPC)}

@margin-note{This frontend library relies on the @seclink["ref-backends-rpc"]{Reach RPC Server}.}

A client library for @link["https://golang.org"]{Go} for the @seclink["ref-backends-rpc"]{Reach RPC protocol} is provided by:

@go{
  import ("reach.sh/rpc")
}

The library provides the following bindings:

@(hrule)
@(mint-define! '("mkRpc"))
@go{
  rpc, rpcCallbacks := mkRpc(opts)
}

@goin{mkRpc} accepts the @secref["ref-backends-rpc-opts"] as a @goin{map} and returns a two functions, traditionally called @goin{rpc} and @goin{rpcCallbacks}.

@(mint-define! '("rpc"))
@goin{rpc} is a function that invokes a @tech{synchronous value RPC method}.
It takes a string, naming the @tech{RPC method}, and some JSON objects to provide as arguments.
It returns a single JSON object as the result.

For example,

@go{
  rpc("/stdlib/formatCurrency", i, 4).(string)
}

calls @jsin{formatCurrency} with some object @goin{i} and @goin{4} and returns the result as a string.

@(mint-define! '("rpcCallbacks"))
@goin{rpcCallbacks} is a function that invokes an @tech{interactive RPC method}, such as for a @tech{backend}.
It takes a string, naming the @tech{RPC method}, a JSON object as an argument, and map from strings to JSON objects or functions.
The functions will be provided as @tech{interactive RPC callbacks} to the @tech{RPC method} and should expect JSON objects as arguments and return a JSON object as a result.
It returns @goin{void}.

For example,

@go{
  showX := func(xo interface{}) {
    x := int(rpc("/stdlib/bigNumberToNumber", xo).(float64))
    fmt.Printf("Alice saw that X is %f\n", x)
  }
  ms := map[string]interface{} {
    "price": 10,
    "showX": showX,
  }
  rpcCallbacks("/backend/Alice", ctc, ms)
}

calls a @tech{backend} named @litchar{Alice} with the @tech{contract} @goin{ctc} and a value named @litchar{price} and a method named @litchar{showX} that prints out a result from the Reach @tech{backend}.

