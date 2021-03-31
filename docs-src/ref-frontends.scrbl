#lang scribble/manual
@(require "lib.rkt")

@title[#:version reach-vers #:tag "ref-frontends" #:style 'toc]{Frontend Support}

This section describes the libraries provided by Reach version @|reach-vers| to support developing @tech{frontends}.

@local-table-of-contents[#:style 'immediate-only]

@include-section["ref-frontends-js.scrbl"]
@include-section["ref-frontends-rpc-js.scrbl"]
@include-section["ref-frontends-rpc-py.scrbl"]
@include-section["ref-frontends-rpc-go.scrbl"]
