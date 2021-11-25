#lang scribble/manual
@(require "lib.rkt"
          scribble/bnf)

@title[#:version reach-vers #:tag "ref" #:style 'toc]{Reference}

This document contains an exhaustive discussion of each of the parts of the Reach @DApp language and its standard library.

@itemlist[
@item{@secref["ref-install"]}
@item{@secref["ref-usage"]}
@item{@secref["ref-model"]}
@item{@secref["ref-programs"]}
@item{@secref["ref-networks"]}
@item{@secref["ref-backends"]}
@item{@secref["ref-frontends"]}
@item{@secref["ref-error-codes"]}
]

@include-section["ref-install.scrbl"]
@include-section["ref-usage.scrbl"]
@include-section["ref-model.scrbl"]
@include-section["ref-programs.scrbl"]
@include-section["ref-networks.scrbl"]
@include-section["ref-backends.scrbl"]
@include-section["ref-frontends.scrbl"]
@include-section["ref-error-codes.scrbl"]
