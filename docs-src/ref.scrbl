#lang scribble/manual
@(require "lib.rkt"
          scribble/bnf)

@title[#:version reach-vers #:tag "ref" #:style 'toc]{Reach Reference}

This document contains an exhaustive discussion of each of the parts of the Reach @DApp language and its standard library. In contrast, @secref{tut} contains a series of guiding tutorials to introduce the key concepts of the language and @DApp design.

@local-table-of-contents[#:style 'immediate-only]

@section[#:tag "ref-install"]{Installation & Usage}

Reach is a Dockerized program, so it has no dependencies and has a trivial installation process. You can install it by running:

@commandline{curl https://raw.githubusercontent.com/reach-sh/reach-lang/master/reachc -o reachc ; chmod +x reachc}

in your project repository.

After you install it, you can compile your Reach code by executing @exec{reachc SOURCE} where @exec{SOURCE} is your @tech{executable} @tech{source file}. @exec{reachc} supports the following options:

@itemlist[

  @item{@Flag{o}/@DFlag{output} @nonterm{OUTPUT} --- Writes the compiler output files to @nonterm{OUTPUT}, which is the current directory by default.}

]

@include-section["ref-model.scrbl"]
@include-section["ref-programs.scrbl"]
@include-section["ref-networks.scrbl"]
@include-section["ref-backends.scrbl"]

