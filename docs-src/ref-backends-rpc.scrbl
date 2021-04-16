#lang scribble/manual
@(require "lib.rkt")

@title[#:version reach-vers #:tag "ref-backends-rpc" #:style 'toc]{Reach RPC Server}

The Reach RPC Server provides access to compiled JavaScript @tech{backends} via an @seclink["ref-backends-rpc-proto"]{HTTPS-accessible JSON-based RPC protocol}.
The server allows @tech{frontends} to be written in any programming language.
Reach provides client libraries for
@seclink["ref-frontends-rpc-js"]{JavaScript},
@seclink["ref-frontends-rpc-py"]{Python}, and
@seclink["ref-frontends-rpc-go"]{Go}.
It is easy to @seclink["ref-backends-rpc-client"]{implement a client library yourself}.
An example @tech{frontend} written using the Reach RPC Server is shown in the @seclink["tut-7-rpc"]{tutorial section on RPC-based frontends}.

The command @cmd{reach rpc-server} starts an instance of the Reach RPC Server.

@(hrule)

The Reach RPC Server supports the following @deftech{RPC methods}:

@itemlist[

@item{@tt{/health} returns @jsin{true} to indicate the server is running properly.}

@item{@tt{/stdlib/$METHOD} where @tt{$METHOD} is a function of the @seclink["ref-frontends-js"]{JavaScript standard library}.

All @tt{/stdlib} methods are @tech{synchronous value RPC methods} that accept and produce the same arguments and return values as the corresponding function, encoded as JSON objects, except those that that produce or consume @tech{account} representations.

Those methods instead accept and produce @tech{account} @tech{RPC handles}, which are random strings that represent the corresponding @tech{account} representations.
For example, @tt{/stdlib/newTestAccount} does not return an @tech{account} like @jsin{newTestAccount}, but instead returns an @tech{account} @tech{RPC handle}.

}

@item{@tt{/forget/acc} accepts an @tech{account} @tech{RPC handle} and deletes it from the Reach RPC Server's memory.}

@item{@tt{/acc/$METHOD} where @tt{$METHOD} is a method of an @tech{account} representation of the @seclink["ref-frontends-js"]{JavaScript standard library}.

All @tt{/acc} methods are @tech{synchronous value RPC methods} that accept and produce the same arguments and return values as the corresponding function, encoded as JSON objects, except they accept an additional first argument, which is the @tech{account} @tech{RPC handle} returned by a prior @tech{RPC method} invocation; and, a method that accepts a @tech{backend} (like @tt{/acc/attach} (i.e. @jsin{acc.attach}) or @tt{/acc/deploy} (i.e. @jsin{acc.deploy}) does not accept a @tech{backend} argument, but has it implicitly provided by the Reach RPC Server.

Furthermore, those that produce @tech{contract} representations, instead produce @tech{contract} @tech{RPC handles}.
For example, @tt{/acc/deploy} does not return a @tech{contract} representation like @jsin{acc.deploy}, but instead returns a @tech{contract} @tech{RPC handle}.

}

@item{@tt{/forget/ctc} accepts a @tech{contract} @tech{RPC handle} and deletes it from the Reach RPC Server's memory.}

@item{@tt{/ctc/$METHOD} where @tt{$METHOD} is a method of a @tech{contract} representation of the @seclink["ref-frontends-js"]{JavaScript standard library}.

All @tt{/ctc} methods are @tech{synchronous value RPC methods} that accept and produce the same arguments and return values as the corresponding function, encoded as JSON objects, except they accept an additional first argument, which is the @tech{contract} @tech{RPC handle} returned by a prior @tech{RPC method} invocation.

}

@item{@tt{/backend/$PARTICIPANT} where @tt{$PARTICIPANT} is a @tech{participant} of the @tech{backend} compiled by the @seclink["ref-backends-js"]{JavaScript backend}.

All @tt{/backend} methods are @tech{interactive RPC methods} that accept three arguments:
@itemlist[

@item{@tt{ctcId} --- A @tech{contract} @tech{RPC handle} to provide as the @tech{contract} to the @tech{backend}}

@item{@tt{values} --- An object containing the non-function components of the @tech{participant interact interface} of the @tech{backend}.}

@item{@tt{methods} --- An object whose keys correspond to the function components of the @tech{participant interact interface} of the @tech{backend}, but whose values are @jsin{true}.}

]

As a special case, if @tt{values} contains @litchar{stdlib.hasRandom} bound to @jsin{true}, then the @seclink["ref-frontends-js"]{JavaScript standard library}'s implementation of @jsin{hasRandom} is provided to the @tech{backend}.

As the @tech{backend} executes, any of the components of @tt{methods} invoked will be executed as @tech{interactive RPC callbacks} as described by the @seclink["ref-backends-rpc-proto"]{Reach RPC Protocol Specification}.
Reach RPC Client libraries @emph{should} expose a function that hides the details of the construction of the @tt{values} and @tt{methods} objects and implements @tech{interactive RPC callback} handlers automatically.

}

@item{@tt{/kont} handles @tech{interactive RPC continuation} completion during an @tech{interactive RPC method}.
It should not be invoked directly by @tech{frontends}.

}

@item{@tt{/stop} quits the server.}

]

@section[#:tag "ref-backends-rpc-opts"]{Reach RPC Client Standard Options}

Reach RPC client libraries must accept a dictionary data structure with the following keys for customizing their behavior:

@itemlist[

@item{@litchar{host} --- This value sets the hostname to contact for the Reach RPC Server instance.
If it is not present, the client library must default to the value of the environment variable @defenv{REACH_RPC_SERVER}.}

@item{@litchar{port} --- This value sets the TCP port to contact for the Reach RPC Server instance.
If it is not present, the client library must default to the value of the environment variable @defenv{REACH_RPC_PORT}.}

@item{@litchar{verify} --- This value determines whether to verify the TLS certificate of the Reach RPC Server instance.
If it is not present, the client library must default to the value of the environment variable @defenv{REACH_RPC_TLS_REJECT_UNVERIFIED}.
If that is not present, it must default to true.

To disable verification, set this value to the string: @litchar{"0"}; any other value will be considered to mean "enable".}

@item{@litchar{timeout} --- This value sets the number of seconds to wait for the Reach RPC Server instance to respond to its first request.
If it is not present, the client library must default to the value of the environment variable @defenv{REACH_RPC_TIMEOUT}.
If that is not present, it must default to 5 seconds.}

@item{@litchar{key} --- This value sets the @tech{API key} for the Reach RPC Server instance.
If it is not present, the client library must default to the value of the environment variable @defenv{REACH_RPC_KEY}.}

]

@include-section["ref-backends-rpc-proto.scrbl"]
@include-section["ref-backends-rpc-client.scrbl"]
