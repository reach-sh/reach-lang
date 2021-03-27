#lang scribble/manual
@(require "lib.rkt")

@title[#:version reach-vers #:tag "ref-frontends-rpc-go"]{Go (RPC)}

@margin-note{This frontend library relies on the @seclink["ref-backends-rpc"]{Reach RPC Server}.}

A @link["https://golang.org"]{Go} client library for the
@seclink["ref-backends-rpc"]{Reach RPC protocol} may be installed by running:
@shell{
  $ go get github.com/reach-sh/reach-lang/rpc-client/go
}

Once installed, add the following import line to your Go file which will connect
to the @seclink{ref-backends-rpc}:
@go{
  import reachrpc "github.com/reach-sh/reach-lang/rpc-client/go"
}

The library provides the following bindings:

@(hrule)
@(mint-define! '("reachrpc.Mk"))
@go{
  rpc, rpcCallbacks := reachrpc.Mk(opts)
}

@goin{reachrpc.Mk} accepts the @secref["ref-backends-rpc-opts"] as a @goin{map}
and returns two functions, traditionally called @goin{rpc} and
@goin{rpcCallbacks}.

@(mint-define! '("rpc"))
@goin{rpc} is a function that invokes a @tech{synchronous value RPC method}.
It takes a string, naming the @tech{RPC method}, and some JSON values to provide as arguments.
It returns a single JSON value as the result.

For example,

@go{
  rpc("/stdlib/formatCurrency", i, 4).(string)
}

calls @jsin{formatCurrency} with some value @goin{i} and @goin{4} and returns the result as a string.

@(mint-define! '("rpcCallbacks"))
@goin{rpcCallbacks} is a function that invokes an @tech{interactive RPC method}, such as for a @tech{backend}.
It takes a string, naming the @tech{RPC method}, a JSON value as an argument, and map from strings to JSON values or functions.
The functions will be provided as @tech{interactive RPC callbacks} to the @tech{RPC method} and should expect JSON values as arguments and return a JSON value as a result.
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

