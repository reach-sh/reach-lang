#lang scribble/manual
@(require "lib.rkt")

@title[#:version reach-vers #:tag "guide" #:style 'toc]{Guide}

These guides discuss some key ideas and concepts used through Reach.
They aim to teach the "Why" of aspects of some Reach features and discuss the many alternatives available to @|DApp| developers.
Indeed, many of these points are relevant to all @|DApp| developers, because the problems appear in all applications.

The following guides are available:

@local-table-of-contents[#:style 'immediate-only]

@include-section["guide-windows.scrbl"]
@include-section["guide-versions.scrbl"]
@include-section["guide-solidity.scrbl"]
@include-section["guide-assert.scrbl"]
@include-section["guide-loop-invs.scrbl"]
@include-section["guide-deploymode.scrbl"]
@include-section["guide-timeout.scrbl"]
@include-section["guide-determ.scrbl"]
@include-section["guide-race.scrbl"]
@include-section["guide-abstract.scrbl"]
@include-section["guide-browser-testing.scrbl"]
@include-section["guide-limits.scrbl"]
@include-section["guide-reach.scrbl"]
@include-section["guide-editor-support.scrbl"]
