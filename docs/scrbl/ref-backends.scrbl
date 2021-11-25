#lang scribble/manual
@(require "lib.rkt")

@title[#:version reach-vers #:tag "ref-backends" #:style 'toc]{Participant Backends}

This section describes the @tech{participant} @tech{backends}
supported by Reach version @|reach-vers|.

Backends are accessed via @seclink["ref-backends-js"]{JavaScript} or via @seclink["ref-backends-rpc"]{the RPC server}.

@include-section["ref-backends-js.scrbl"]
@include-section["ref-backends-rpc.scrbl"]
