#lang scribble/manual
@(require "lib.rkt"
          scribble/bnf)

@title[#:version reach-vers #:tag "ref" #:style 'toc]{Reference}

This document contains an exhaustive discussion of each of the parts of the Reach @DApp language and its standard library.

@local-table-of-contents[#:style 'immediate-only]

@section[#:tag "install"]{Installation}

Reach is a Dockerized program, so its only dependencies are @link["https://www.docker.com/get-started"]{Docker} and @link["https://docs.docker.com/compose/install/"]{Docker Compose}. You can install it by running:

@commandline{curl https://raw.githubusercontent.com/reach-sh/reach-lang/master/reachc -o reachc ; chmod +x reachc}

in your project repository.
You can copy this file to other repositories or move it to a directory in your @envvar{PATH}, like @exec{~/bin}.

@section[#:tag "ref-usage"]{Usage}

You compile your Reach code by executing

@commandline{reachc SOURCE EXPORT ...}

where @exec{SOURCE} is your @tech{source file},
and each @exec{EXPORT} is an @tech{export}ed @tech{Reach.App}.
If no @exec{EXPORT} is provided, then @litchar{main} is used.

@exec{reachc} supports the following options:

@itemlist[

  @item{@Flag{o}/@DFlag{output} @nonterm{OUTPUT} --- Writes the compiler output files to @nonterm{OUTPUT}, which is the current directory by default.}

  @item{@DFlag{intermediate-files} --- Write intermediate files, which may be interesting for debugging compilation failures or using in other contexts.}
]

@include-section["ref-model.scrbl"]
@include-section["ref-programs.scrbl"]
@include-section["ref-networks.scrbl"]
@include-section["ref-backends.scrbl"]

