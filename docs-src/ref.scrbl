#lang scribble/manual
@(require "lib.rkt"
          scribble/bnf)

@title[#:version reach-vers #:tag "ref" #:style 'toc]{Reference}

This document contains an exhaustive discussion of each of the parts of the Reach @DApp language and its standard library.

@local-table-of-contents[#:style 'immediate-only]

@section[#:tag "install"]{Installation}

Reach is designed to work on POSIX systems with @link["https://en.wikipedia.org/wiki/Make_(software)"]{make}, @link["https://www.docker.com/get-started"]{Docker}, and @link["https://docs.docker.com/compose/install/"]{Docker Compose} installed.
The best way to install Docker on Mac and Windows is with @link["https://www.docker.com/products/docker-desktop"]{Docker Desktop}.

@margin-note{You probably already have @exec{make} installed.
For example, OS X and many other POSIX systems come with @exec{make}, but some versions of Linux do not include it by default and will require you to install it.
If you're on Ubuntu, you can run @exec{sudo apt install make} to get it.}

You can install Reach by running:

@cmd{curl https://raw.githubusercontent.com/reach-sh/reach-lang/master/reach -o reach ; chmod +x reach}

in your project repository.
You can copy this file to other repositories or move it to a directory in your @envvar{PATH}, like @exec{~/bin}.

@margin-note{If you're using Windows, consult @seclink["guide-windows"]{the guide to using Reach on Windows}.}

@section[#:tag "ref-usage"]{Usage}

Reach has a few sub-commands, each with their own options.

However, all commands support the following options:

@itemlist[

@item{The environment variable @envvar{REACH_VERSION} signifies what @seclink["guide-versions"]{version of Reach} to use.}

]

@subsection[#:tag "ref-usage-compile"]{@tt{reach compile}}

You compile your Reach code by executing

@cmd{reach compile SOURCE EXPORT ...}

where @exec{SOURCE} is your @tech{source file},
and each @exec{EXPORT} is an @tech{export}ed @tech{Reach.App}.

If no @exec{SOURCE} is provided, then @exec{index.rsh} is used.

If no @exec{EXPORT} is provided, then @litchar{main} is used.

@exec{reach compile} supports the following options:

@itemlist[

  @item{@Flag{o}/@DFlag{output} @nonterm{OUTPUT} --- Writes compiler output files to @nonterm{OUTPUT}, which defaults to a directory named @exec{build} in the same directory as @exec{SOURCE}.}

  @item{@DFlag{intermediate-files} --- Write intermediate files, which may be interesting for debugging compilation failures or using in other contexts.}

]

@subsection[#:tag "ref-usage-init"]{@tt{reach init}}

You can create template @exec{index.rsh} and @exec{index.mjs} files for a simple Reach app by running

@cmd{reach init}

@subsection[#:tag "ref-usage-run"]{@tt{reach run}}

You can run a simple Reach application by executing

@cmd{reach run APP}

If no @exec{APP} is provided, @exec{index} is used.

If @exec{APP} is a directory, then @exec{APP/index} is used.

This assumes

@itemlist[

@item{Your Reach program is named @exec{APP.rsh}.}

@item{You are using the JavaScript backend and your @tech{frontend} is named @exec{APP.mjs}.
It also assumes the backend is located at @exec{build/APP.main.mjs}, and only depends on the Reach standard library.}

]

It then

@itemlist[

@item{Compiles your program with Reach.}

@item{Builds a Docker image named @exec{reachsh/reach-app-APP:latest} that depends on the Reach JavaScript standard library.}

@item{Executes that image, connected with a private Ethereum devnet.}

]

@exec{reach run} supports the following options:

@itemlist[

  @item{If the environment variable @envvar{REACH_ETH_MODE} is bound to @exec{ganache}, then Reach will use @link["https://www.trufflesuite.com/ganache"]{Ganache} instead of a private network.}

]

@subsection[#:tag "ref-usage-scaffold"]{@tt{reach scaffold}}

You can create template @exec{package.json}, @exec{Dockerfile}, @exec{docker-compose.yml}, and @exec{Makefile} files for a simple Reach app by running

@cmd{reach scaffold}

The files created are the same as those used temporarily by @exec{reach run}.

@subsection[#:tag "ref-usage-upgrade"]{@tt{reach upgrade}}

You can upgrade your Reach installation by executing

@cmd{reach upgrade}

This may change the default version used by @exec{reach} commands.

@subsection[#:tag "ref-usage-update"]{@tt{reach update}}

You can update the Docker images used by your Reach installation by executing

@cmd{reach update}

This may change the patch version used by @exec{reach} commands.

@subsection[#:tag "ref-usage-version"]{@tt{reach version}}

You can see what version of Reach you have installed by running

@cmd{reach version}

@include-section["ref-model.scrbl"]
@include-section["ref-programs.scrbl"]
@include-section["ref-networks.scrbl"]
@include-section["ref-backends.scrbl"]

