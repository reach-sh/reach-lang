#lang scribble/manual
@(require "lib.rkt"
          scribble/bnf)

@title[#:version reach-vers #:tag "ref" #:style 'toc]{Reference}

This document contains an exhaustive discussion of each of the parts of the Reach @DApp language and its standard library.

@local-table-of-contents[#:style 'immediate-only]

@section[#:tag "install"]{Installation}

Reach is a Dockerized program, so its only dependencies are @link["https://www.docker.com/get-started"]{Docker} and @link["https://docs.docker.com/compose/install/"]{Docker Compose}. You can install it by running:

@cmd{curl https://raw.githubusercontent.com/reach-sh/reach-lang/master/reach -o reach ; chmod +x reach}

in your project repository.
You can copy this file to other repositories or move it to a directory in your @envvar{PATH}, like @exec{~/bin}.

@section[#:tag "ref-usage"]{Usage}

Reach has a few sub-commands, each with their own options.

@subsection[#:tag "ref-usage-compile"]{@tt{reach compile}}

You compile your Reach code by executing

@cmd{reach compile SOURCE EXPORT ...}

where @exec{SOURCE} is your @tech{source file},
and each @exec{EXPORT} is an @tech{export}ed @tech{Reach.App}.

If no @exec{SOURCE} is provided, then @exec{index.rsh} is used.

If no @exec{EXPORT} is provided, then @litchar{main} is used.

@exec{reach compile} supports the following options:

@itemlist[

  @item{@Flag{o}/@DFlag{output} @nonterm{OUTPUT} --- Writes compiler output files to @nonterm{OUTPUT}, which is @exec{$CWD/build} (the @exec{build} directory of the current directory) by default.}

  @item{@DFlag{intermediate-files} --- Write intermediate files, which may be interesting for debugging compilation failures or using in other contexts.}

  @item{The enviroment variable @envvar{REACH_VERSION} controls which version of the compiler is used.}
]

@subsection[#:tag "ref-usage-run"]{@tt{reach run}}

You can run a simple Reach application by executing

@cmd{reach run APP}

If no @exec{APP} is provided, @exec{index} is used.

If @exec{APP} is a directory, then @exec{APP/index} is used.

This assumes

@itemlist[

@item{Your Reach program is named @exec{APP.rsh}.}

@item{You are using the JavaScript backend, your interfacing code is named @exec{APP.mjs}, it assumes the backend is located at @exec{build/APP.main.mjs}, and only depends on the Reach standard library.}

]

It then

@itemlist[

@item{Compiles your program with Reach.}

@item{Builds a Docker image named @exec{reachsh/reach-app-APP:latest} that depends on the Reach JavaScript standard library.}

@item{Executes that image, connected with a private Ethereum devnet.}

]

@exec{reach run} supports the following options:

@itemlist[

  @item{The enviroment variable @envvar{REACH_VERSION} controls which version of the compiler and runtime is used.}

  @item{If the environment variable @envvar{REACH_ETH_MODE} is bound to @exec{ganache}, then Reach will use @link["https://www.trufflesuite.com/ganache"]{Ganache} instead of a private network.}

]


@subsection[#:tag "ref-usage-upgrade"]{@tt{reach upgrade}}

You can upgrade your Reach installation by executing

@cmd{reach upgrade}

@subsection[#:tag "ref-usage-update"]{@tt{reach update}}

You can update the Docker images used by your Reach installation by executing

@cmd{reach update}

@subsection[#:tag "ref-usage-version"]{@tt{reach version}}

You can see what version of Reach you have installed by running

@cmd{reach version}

@include-section["ref-model.scrbl"]
@include-section["ref-programs.scrbl"]
@include-section["ref-networks.scrbl"]
@include-section["ref-backends.scrbl"]

