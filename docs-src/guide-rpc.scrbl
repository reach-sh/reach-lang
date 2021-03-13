#lang scribble/manual
@(require "lib.rkt")

@title[
  #:version reach-vers
  #:tag     "guide-rpc"
  #:style   'toc
  ]{The Reach RPC interface}

@(define link-client-go  @reachexlink["tut-6-rpc/client-go/index.go" "Go"])
@(define link-client-py  @reachexlink["tut-6-rpc/client-py/index.py" "Python"])
@(define link-react-code @reachexlink["overview-react" "overview-react example"])

@(define link-rpc
  @link["https://en.wikipedia.org/wiki/Remote_procedure_call"]{remote procedure call})

@(define link-react-tube
  @link["https://youtu.be/jHEKIMNvs-o"]{7-minute YouTube video})

@DApps implemented in Reach are comprised of multiple interrelated but
nonetheless fully-separate components. Of these, a given @DApp's
@tech{frontend} component is conceptually the furthest removed from its
underlying blockchain/@tech{consensus network} machinery, but perhaps the most
interesting for end-users and programmers focused on implementing rich
user-facing functionality.

Reach demands very little of its @tech{frontends}. Beyond exposing a simple
@tech[#:key "interact"]{interaction} interface with which @tech{participants}
may advance the state of the @DApp, there are no requirements for how such a
@tech{frontend} should look, operate, or be constructed.

@margin-note{Don't miss the @link-react-tube which demonstrates this
@DApp running live.}

So far you've seen the flexibility of this model leveraged to good effect in
the @link-react-code @|DApp|. This demo is a full-blown browser
application which was still written in JavaScript but required a very different
design than the simple terminal programs employed by most of the other
examples.

However, provided we still expose the necessary
@tech[#:key "interact"]{interaction} hooks, it's possible to extend the power
of this separation even further, namely by abstracting over the underlying
language used to write the client.

To this end, Reach offers a thin @deftech{RPC} (@link-rpc via HTTPS) layer
which sits between its JavaScript standard library and client
@tech[#:key "frontend"]{frontends} which are easily implemented in any general
purpose language (e.g.  @link-client-go, or @link-client-py).

@bold{In other words: @italic{the bulk of your Reach @DApp may be written in
whichever language you like best.}}

The following topics walk readers through how this is made possible:

@local-table-of-contents[#:style 'immediate-only]

@include-section["guide-rpc-server-api.scrbl"]
@include-section["guide-rpc-connect.scrbl"]
