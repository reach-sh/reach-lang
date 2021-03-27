#lang scribble/manual
@(require "lib.rkt")

@title[#:version reach-vers #:tag "ref-frontends-rpc-js"]{JavaScript (RPC)}

@margin-note{This frontend library relies on the @seclink["ref-backends-rpc"]{Reach RPC Server}.}

A @link["https://www.javascript.com"]{JavaScript} client library for the
@seclink["ref-backends-rpc"]{Reach RPC protocol} may be installed by running:
@shell{
  $ npm install --save @"@"reach-sh/rpc-client
}

Once installed, add the following import line to your JavaScript file which will
connect to the @seclink{ref-backends-rpc}:
@js{
  import { mkRPC } from '@"@"reach-sh/rpc-client';
}

The library provides the following bindings:

@(hrule)
@(mint-define! '("mkRPC"))
@js{
  const { rpc, rpcCallbacks } = await mkRPC(opts);
}

@jsin{mkRPC} accepts the @secref["ref-backends-rpc-opts"] as an object and returns a Promise of an object with two fields, @jsin{rpc} and @jsin{rpcCallbacks}.

@(mint-define! '("rpc"))
@jsin{rpc} is a function that invokes a @tech{synchronous value RPC method}.
It takes a string, naming the @tech{RPC method}, and some JSON values to provide as arguments.
It returns a Promise of a single JSON value as the result.

For example,

@js{
  await rpc(`/stdlib/formatCurrency`, i, 4);
}

calls @jsin{formatCurrency} with some value @jsin{i} and @jsin{4}.

@(mint-define! '("rpcCallbacks"))
@jsin{rpcCallbacks} is a function that invokes an @tech{interactive RPC method}, such as for a @tech{backend}.
It takes a string, naming the @tech{RPC method}, a JSON value as an argument, and dictionary from strings to JSON values or @jsin{async} functions.
The functions will be provided as @tech{interactive RPC callbacks} to the @tech{RPC method} and should expect JSON values as arguments and return a Promise of a JSON value as a result.
It returns a Promise that does not contain a value.

For example,

@js{
  const showX = async (xo) => {
    const x = await rpc(`/stdlib/bigNumberToNumber`, xo);
    console.log(`Alice saw that X is ${x}`);
  };
  const ms = {
    'price': 10,
    'showX': showX,
  };
  await rpcCallbacks(`/backend/Alice`, ctc, ms)
}

calls a @tech{backend} named @litchar{Alice} with the @tech{contract} @jsin{ctc} and a value named @litchar{price} and a method named @litchar{showX} that prints out a result from the Reach @tech{backend}.

