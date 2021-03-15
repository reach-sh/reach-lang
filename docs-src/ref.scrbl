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

@subsection[#:tag "ref-usage-hashes"]{@tt{reach hashes}}

You can see which exact versions of Reach Docker images you are using by running

@cmd{reach hashes}

This is more precise, but less readable, than @exec{reach version},
in that each hash refers to the git commit used to build the image.

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
  @item{
    The environment variable @deftech{REACH_CONNECTOR_MODE} specifies which context to run in.
    The default, if this variable is unset or empty, is @litchar{ETH-test-dockerized-geth}.
    The options are:

    @itemlist[
      @item{@litchar{ETH-live}, which uses a live Ethereum network node, specified by the environment variable @envvar{ETH_NODE_URI}.}
      @item{@litchar{ETH-test-dockerized-geth}, which uses a Dockerized private Ethereum network.}
      @item{@litchar{ALGO-test-dockerized-algod}, which uses a Dockerized private Algorand network.}
    ]
  }
  @item{
    The environment variable @deftech{REACH_DEBUG}, if set to any non-empty value, enables debug messages from the Reach standard library, which will appear in the console.
  }
]

@exec{reach run} can be configured via the presence of certain files.
In the absence of these files, @exec{reach run} assumes a default behavior based on @exec{reach scaffold}.

@itemlist[
  @item{
    If a @litchar{Makefile} is present,
    and if the @litchar{REACH_CONNECTOR_MODE} and @litchar{RUN_FROM_REACH} environment variables are unset or empty,
    then @litchar{make run} will be invoked, with the @litchar{RUN_FROM_REACH} environment variable set to true.}
  @item{
    If a @litchar{Makefile}, @litchar{Dockerfile}, @litchar{package.json}, and @litchar{docker-compose.yml} are all present,
    then these files will be used. You can call @exec{reach scaffold} to persist the default versions of these files.}
  @item{
    If only some of those files exist, but not all, then @litchar{reach run} will report an error.
    Please delete them, or add the missing ones.}
]

Furthermore, if all of the scaffolded files are present, then pay special attention to the @litchar{Makefile} that you write,
because @exec{reach} will use it in specific ways. Refer to the @litchar{Makefile} generated by @exec{reach scaffold} for hints on how to get it right.

@itemlist[
  @item{
    Various @exec{reach} commands may invoke @exec{make build}, which is assumed to build the Docker image for your app.}
  @item{
    @exec{reach run ...} will invoke @exec{make run-target ARGS="..."},
    where "..." is an escaped, space-separated representation of the command-line args to @exec{reach run}.
    See @link["https://github.com/reach-sh/reach-lang/blob/master/examples/argz/Makefile"]{examples/argz/Makefile}
    for an example invocation of @exec{reach run} with command-line arguments.
  }
]

@subsection[#:tag "ref-usage-down"]{@tt{reach down}}

You can halt the docker containers started by @exec{reach run} by running

@cmd{reach down}

@subsection[#:tag "ref-usage-scaffold"]{@tt{reach scaffold}}

You can create template @exec{package.json}, @exec{Dockerfile}, @exec{docker-compose.yml}, and @exec{Makefile} files for a simple Reach app by running

@cmd{reach scaffold}

The files created are the same as those used temporarily by @exec{reach run}.

@subsection[#:tag "ref-usage-react"]{@tt{reach react}}

You can run a simple react app by executing

@cmd{reach react}

This assumes

@itemlist[
  @item{Your Reach program is named @exec{index.rsh}}
  @item{Your frontend React program is named @exec{index.js}}
]

It then

@itemlist[
  @item{Compliles your program with Reach}
  @item{Runs the appropriate devnet based on @exec{REACH_CONNECTOR_MODE}}
  @item{Mounts the current directory into @exec{/app/} in the @exec{reachsh/react-runner} Docker image and runs it.}
  @item{Runs (via @exec{react-runner}) a proxy server used to avoid CORS issues on some devnets}
]

@exec{reach react} supports the following options:

@itemlist[
  @item{
    The environment variable @exec{REACH_CONNECTOR_MODE} specifies which context to run in. The default, if this variable is unset or empty, is @litchar{ETH}. The options are:

    @itemlist[
      @item{@litchar{ETH}, which runs a Dockerized private Ethereum network which may be used. The app can use any Ethereum network.}
      @item{@litchar{ALGO}, which runs a Dockerized private Algorand network which may be used. (Support for using any Algorand network is forthcoming with TEAL 3.)}
    ]
  }
  @item{
    The environment variable @exec{REACH_DEBUG}, if set to any non-empty value, enables debug messages from the Reach standard library, which will appear in the browser console.
  }
]

Note that @exec{reach react} does not respect the same scaffolded files as @exec{reach run}. If you would like a more customized browser-based project, we recommend that you simply use @exec{reach compile}, and use your own preferred setup for the project. The compiled @exec{build/index.main.mjs} JavaScript file and the @exec{'@"@"reach-sh/stdlib'} JavaScript library may be used in any JavaScript project like any other JavaScript file and library, respectively.

@subsection[#:tag "ref-usage-devnet"]{@tt{reach devnet}}

You can run a private Reach devnet by executing

@cmd{reach devnet}

@exec{reach devnet} supports the following options:

@itemlist[
  @item{
    The environment variable @exec{REACH_CONNECTOR_MODE} specifies which devnet to run. The default, if this variable is unset or empty, is @litchar{ETH}. The options are:

    @itemlist[
      @item{@litchar{ETH}, which runs an Ethereum devnet on localhost:8545}
      @item{@litchar{ALGO}, which runs an Algorand devnet on localhost:4180 and an Algorand indexer on localhost:8980}
    ]
  }
  @item{
    The environment variable @exec{REACH_DEBUG} enables some additional debugging information for the Algorand devnet, which is accessible via http://localhost:9392
  }
]

Note: the @jsin{'@"@"reach-sh/stdlib'} library, when working with Algorand,
sends requests to localhost:3000/algod when running in the browser,
to avoid CORS issues.
When using @exec{reach devnet} instead of @exec{reach react} to start your Algorand devnet,
you are expected to also run a proxy server from localhost:3000/algod to localhost:4180.
For an example of something similar to this,
see @link["https://github.com/reach-sh/reach-lang/blob/master/js/react-runner/craco.config.js"]{react-runner's use of craco}.

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

@subsection[#:tag "ref-usage-hashes"]{@tt{reach hashes}}

You can do a more specific version check of your Reach docker images by running

@cmd{reach hashes}

@include-section["ref-model.scrbl"]
@include-section["ref-programs.scrbl"]
@include-section["ref-networks.scrbl"]
@include-section["ref-backends.scrbl"]
