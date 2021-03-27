#lang scribble/manual
@(require "lib.rkt")

@title[#:version reach-vers #:tag "ref-frontends-rpc-py"]{Python (RPC)}

@margin-note{This frontend library relies on the @seclink["ref-backends-rpc"]{Reach RPC Server}.}

A @link["https://www.python.org"]{Python} client library for the
@seclink["ref-backends-rpc"]{Reach RPC protocol} may be installed by running:
@shell{
  $ pip install --upgrade reach-rpc-client
}

Once installed, add the following import line to the Python file which will
connect to the @seclink{ref-backends-rpc}:
@py{
  from reach_rpc import mk_rpc
}

The library provides the following bindings:

@(hrule)
@(mint-define! '("mk_rpc"))
@py{
  rpc, rpc_callbacks = mk_rpc(opts)
}

@pyin{mk_rpc} accepts the @secref["ref-backends-rpc-opts"] as a dictionary and returns two functions, traditionally called @pyin{rpc} and @pyin{rpc_callbacks}.

@(mint-define! '("rpc"))
@pyin{rpc} is a function that invokes a @tech{synchronous value RPC method}.
It takes a string, naming the @tech{RPC method}, and some JSON values to provide as arguments.
It returns a single JSON value as the result.

For example,

@py{
  rpc('/stdlib/formatCurrency', i, 4)
}

calls @jsin{formatCurrency} with some value @pyin{i} and @pyin{4}.

@(mint-define! '("rpc_callbacks"))
@pyin{rpc_callbacks} is a function that invokes an @tech{interactive RPC method}, such as for a @tech{backend}.
It takes a string, naming the @tech{RPC method}, a JSON value as an argument,
and dictionary from strings to JSON values or functions.
The functions will be provided as @tech{interactive RPC callbacks} to the
@tech{RPC method} and should expect JSON values as arguments and return a JSON
value as a result.
It does not return a value.

For example,

@py{
  def showX(x):
      print('Alice saw that X is %s'
            % rpc('/stdlib/bigNumberToNumber', x))

  ms = { 'price': 10,
         'showX': showX,
       }
  rpc_callbacks("/backend/Alice", ctc, ms)
}

calls a @tech{backend} named @litchar{Alice} with the @tech{contract} @pyin{ctc} and a value named @litchar{price} and a method named @litchar{showX} that prints out a result from the Reach @tech{backend}.

