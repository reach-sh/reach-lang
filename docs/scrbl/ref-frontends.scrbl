#lang scribble/manual
@(require "lib.rkt")

@title[#:version reach-vers #:tag "ref-frontends" #:style 'toc]{Frontend Support}

This section describes the libraries provided by Reach version @|reach-vers| to support developing @tech{frontends}.

Frontends are implemented in @seclink["ref-frontends-js"]{JavaScript}
or via the @seclink["ref-backends-rpc"]{the RPC server} and one of the languages with an RPC client:
@seclink["ref-frontends-rpc-cs"]{C Sharp},
@seclink["ref-frontends-rpc-js"]{JavaScript},
@seclink["ref-frontends-rpc-go"]{Go}, and
@seclink["ref-frontends-rpc-py"]{Python}.

@include-section["ref-frontends-js.scrbl"]
@include-section["ref-frontends-rpc-cs.scrbl"]
@include-section["ref-frontends-rpc-js.scrbl"]
@include-section["ref-frontends-rpc-go.scrbl"]
@include-section["ref-frontends-rpc-py.scrbl"]
