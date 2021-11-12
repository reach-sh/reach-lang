#lang scribble/manual
@(require "lib.rkt")

@title[#:version reach-vers #:tag "guide-rpc"]{Do I have to use JavaScript to write my frontend? What about Python, Go, or other languages?}

Most Reach examples, tutorials, and workshops use JavaScript as the language of choice for @tech{frontend} implementation, but Reach supports @tech{frontend} development in any language via the @seclink["ref-backends-rpc"]{Reach RPC Server}.
This server allows @tech{backends} compiled to @seclink["ref-backends-js"]{JavaScript} to be provided to another language via a simple @seclink["ref-backends-rpc-proto"]{RPC protocol}.

Presently, Reach provides RPC client implementations for:
@itemlist[

@item{@seclink["ref-frontends-rpc-js"]{JavaScript}}
@item{@seclink["ref-frontends-rpc-py"]{Python}}
@item{@seclink["ref-frontends-rpc-go"]{Go}}

]

The @seclink["tut-7-rpc"]{tutorial section on RPC-based frontends} provides a walkthrough of using these libraries.

If your language of choice isn't available yet, it is very simple to @seclink["ref-backends-rpc-proto"]{implement one yourself} if you've ever used a JSON-based RPC protocol before.
Most implementations are less than 100 lines of code!
Or, you could submit a request for Reach to build one on the Reach @link["https://github.com/reach-sh/reach-lang/issues"]{GitHub issue tracker} or on @(the-community-link).

