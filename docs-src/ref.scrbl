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

@cmd{curl https://docs.reach.sh/reach -o reach ; chmod +x reach}

in your project repository.
You can copy this file to other repositories or move it to a directory in your @envref{PATH}, like @exec{~/bin}.
(@defenv{PATH} is a UNIX environment variable listing each of the directories that contain programs you can run in a shell session.)

@margin-note{If you're using Windows, consult @seclink["guide-windows"]{the guide to using Reach on Windows}.}

@section[#:tag "ref-usage"]{Usage}

Reach has a few sub-commands, each with their own options.

However, all commands support the following options:

@itemlist[

@item{The environment variable @defenv{REACH_VERSION} signifies what @seclink["guide-versions"]{version of Reach} to use.}

]

@subsection[#:tag "ref-usage-compile"]{@tt{reach compile}}

You compile your Reach code by executing

@cmd{reach compile SOURCE EXPORT ...}

where @exec{SOURCE} is your @tech{source file},
and each @exec{EXPORT} is an @tech{export}ed @tech{Reach.App}.

If no @exec{SOURCE} is provided, then @exec{index.rsh} is used.

If no @exec{EXPORT} is provided, then all the exported @tech{Reach.App}s will be compiled. If there are no
@tech{Reach.App}s exported, then the program will be compiled as a library, where its exports are available
to other Reach programs and frontends. The output name of a library is the same as if it exported a @tech{Reach.App}
named @tt{default}.

@exec{reach compile} supports the following options:

@itemlist[

  @item{@Flag{o}/@DFlag{output} @nonterm{OUTPUT} --- Writes compiler output files to @nonterm{OUTPUT}, which defaults to a directory named @exec{build} in the same directory as @exec{SOURCE}.}

  @item{@DFlag{intermediate-files} --- Write intermediate files, which may be interesting for debugging compilation failures or using in other contexts.}

  @item{
  @DFlag{install-pkgs} --- Allows Reach to fetch remote @tech{package imports} and stop after doing so.

  Reach will fail with an error message if package imports have not yet been fetched and this flag is not activated.

  Using this flag will create a @tt{.reach} directory for your project.
  This directory stores your project's @deftech{lockfile}, which is how Reach pins remote packages to specific versions.
  It is recommended that you commit the @tt{.reach} directory to source control.
  Deleting this directory is also safe; it can easily be rebuilt by using the @DFlag{install-pkgs} flag again; in fact, this is the best way to upgrade your packages.
  }
]

@subsection[#:tag "ref-usage-init"]{@tt{reach init}}
@;TODO document [TEMPLATE] once more templates have been added

You can create template @exec{index.rsh} and @exec{index.mjs} files for a simple Reach app by running

@cmd{reach init}

@subsection[#:tag "ref-usage-run"]{@tt{reach run}}

You can run a simple Reach application by executing

@cmd{reach run [APP or DIR] [ARGS]}

@exec{APP} represents a Reach module name without its extension (e.g. "index" by default).

If no @exec{APP} or @exec{DIR} is provided then @exec{index} in the current working directory is assumed.

If @exec{DIR} matches an existing, relative subdirectory of the current working directory then @exec{DIR/index} is used.

@exec{ARGS} represents zero or more arguments to be passed into @exec{APP}'s @tech{frontend}.

This assumes

@itemlist[

@item{Your Reach program is named @exec{APP.rsh}.}

@item{You are using the JavaScript backend and your @tech{frontend} is named @exec{APP.mjs}.
It also assumes the backend is located at @exec{DIR/build/APP.main.mjs}, and only depends on the Reach standard library.}

]

It then

@itemlist[

@item{Compiles your program with Reach.}

@item{Builds a Docker image named @exec{reachsh/reach-app-APP:latest} that depends on the Reach JavaScript standard library.}

@item{Executes a container based upon that image while connected to the network determined by @envref{REACH_CONNECTOR_MODE}.}

]

@exec{reach run} supports the following options:

@itemlist[
  @item{
    The environment variable @defenv{REACH_CONNECTOR_MODE} specifies which context to run in.
    The default, if this variable is unset or empty, is @conmode{ETH-devnet}.
    The options are:

    @itemlist[
      @item{@defconmode{ETH-live}, which uses a live Ethereum network node, specified by the environment variable @envref{ETH_NODE_URI}.}
      @item{@defconmode{ETH-browser}, which uses Ethereum via a browser extension, like MetaMask.}
      @item{@defconmode{ETH-devnet}, which uses a Dockerized private Ethereum network.}
      @item{@defconmode{ALGO-live}, which uses a live Algorand network node, specified by the environment variables documented in @seclink["ref-network-algo"]{the Algorand connector section}.}
      @item{@defconmode{ALGO-browser}, which uses Algorand via a browser extension, like AlgoSigner.}
      @item{@defconmode{ALGO-devnet}, which uses a Dockerized private Algorand network.}
    ]
  }
  @item{
    The environment variable @defenv{REACH_DEBUG}, if set to any non-empty value, enables debug messages from the Reach standard library, which will appear in the console.
  }
]

@exec{reach run} can be further specialized via the presence of a @litchar{Dockerfile} and @litchar{package.json} file.
If either are absent, @exec{reach run} assumes a default behavior (which may be persisted with @exec{reach scaffold}).

The @litchar{Dockerfile} can be modified to introduce new dependencies, services, or filesystem prerequisites into your app's containerized environment, and the @litchar{package.json} file may likewise be extended to include additional libraries or make configuration changes to the resultant Node.js package.

@subsection[#:tag "ref-usage-down"]{@tt{reach down}}

You can halt all Dockerized Reach apps and devnets by running

@cmd{reach down}

@subsection[#:tag "ref-usage-scaffold"]{@tt{reach scaffold}}

You can create templated @exec{Dockerfile} and @exec{package.json} files for a simple Reach app by running

@cmd{reach scaffold}

The files created are the same as those used temporarily by @exec{reach run}.

@subsection[#:tag "ref-usage-react"]{@tt{reach react}}

You can run a simple React app by executing

@cmd{reach react}

This assumes

@itemlist[
  @item{Your Reach program is named @exec{index.rsh}}
  @item{Your frontend React program is named @exec{index.js}}
]

It then

@itemlist[
  @item{Compiles your program with Reach}
  @item{Runs the appropriate devnet based on @envref{REACH_CONNECTOR_MODE}}
  @item{Mounts the current directory into @exec{/app/src/} in the @exec{reachsh/react-runner} Docker image and runs it.}
]

@exec{reach react} supports the following options:

@itemlist[
  @item{
    The environment variable @envref{REACH_CONNECTOR_MODE} specifies which context to run in. The default, if this variable is unset or empty, is @litchar{ETH}. The options are:

    @itemlist[
      @item{@litchar{ETH}, which runs a Dockerized private Ethereum network which may be used. The app can use any Ethereum network.}
      @item{@litchar{ALGO}, which runs a Dockerized private Algorand network which may be used. (Support for using any Algorand network is forthcoming with TEAL 3.)}
    ]
  }
  @item{
    The environment variable @envref{REACH_DEBUG}, if set to any non-empty value, enables debug messages from the Reach standard library, which will appear in the browser console.
  }
]

When using @jsin{loadStdlib} in conjunction with @exec{reach react},
be sure to pass in @jsin{process.env} as its argument.
See @jsin{loadStdlib} for details.

@exec{reach react} does not respect the same scaffolded files as @exec{reach run}.
It is just a simplified tool to give you a taste of web programming with reach.
If you would like access to more customizations on a browser-based project,
such as custom environment variables,
custom JavaScript dependencies,
or using other JavaScript frameworks like Angular,
we recommend that you simply use @exec{reach compile},
and use your own preferred JavaScript setup for the project.
The compiled @exec{build/index.main.mjs} JavaScript file
and the @exec{'@"@"reach-sh/stdlib'} JavaScript library
may be used in any JavaScript project like any other JavaScript file and library, respectively.

@subsection[#:tag "ref-usage-devnet"]{@tt{reach devnet}}

You can run a private Reach devnet by executing

@cmd{reach devnet}

@exec{reach devnet} supports the following options:

@itemlist[
  @item{
    The environment variable @envref{REACH_CONNECTOR_MODE} specifies which devnet to run. The default, if this variable is unset or empty, is @litchar{ETH}. The options are:

    @itemlist[
      @item{@litchar{ETH}, which runs an Ethereum devnet on @tt{localhost:8545}}
      @item{@litchar{ALGO}, which runs an Algorand devnet on @tt{localhost:4180} and an Algorand indexer on @tt{localhost:8980}}
    ]
  }
  @item{
    The environment variable @envref{REACH_DEBUG} enables some additional debugging information for the Algorand devnet, which is accessible via http://localhost:9392
  }
]

@subsection[#:tag "ref-usage-rpc-server"]{@tt{reach rpc-server}}

The sub-command

@cmd{reach rpc-server}

starts an instance of the @seclink["ref-backends-rpc"]{Reach RPC Server} using all of the same options and defaults as @exec{reach run}.

@exec{reach rpc-server} supports the following options:

@itemlist[

  @item{The environment variable @envvar{REACH_RPC_KEY} is used to determine the RPC server key.
  If not defined, it defaults to @litchar{opensesame}, and a warning will
  appear in the console stating that the development key is being used.

  In a production context this key must be kept secret, and it should be
  randomly generated with a suitably strong method, such as:

  @cmd{head -c 24 /dev/urandom | base64}
  }

  @item{The environment variable @envvar{REACH_RPC_PORT} is used to determine which port to bind to.
  It defaults to @litchar{3000}.
  }

  @item{The environment variable @defenv{REACH_RPC_TLS_KEY} is used to determine the path to the TLS @tt{key} file, which must be in the @tt{./tls} directory.
  It defaults to @litchar{reach-server.key}.
  }

  @item{The environment variable @defenv{REACH_RPC_TLS_CRT} is used to determine the path to the TLS @tt{crt} file, which must be in the @tt{./tls} directory.
  It defaults to @litchar{reach-server.crt}.
  }

  @item{The environment variable @defenv{REACH_RPC_TLS_PASSPHRASE} is used to determine the TLS passphrase.
  It defaults to @litchar{rpc-demo}.
  }

]

@subsection[#:tag "ref-usage-rpc-run"]{@tt{reach rpc-run}}

The sub-command

@cmd{reach rpc-run CMD}

is a convenient means of launching a pre-configured RPC server and
@tech{frontend} which are suitable for development purposes.
It uses a @envvar{REACH_RPC_KEY} value of @litchar{opensesame} (the standard
development API key), and sets @envvar{REACH_RPC_TLS_REJECT_UNVERIFIED} to
@litchar{0}.

Consider this example from the @seclink{tut-7-rpc} tutorial:
@cmd{reach rpc-run python3 -u ./index.py}

@subsection[#:tag "ref-usage-docker-reset"]{@tt{reach docker-reset}}

You can easily kill and remove all Docker containers by executing

@cmd{reach docker-reset}

This can be a useful thing to try if your Docker containers stop responding to requests or otherwise misbehave, or if you have updated your Reach images (with @exec{reach update}) but those changes are not taking effect.
This command is a loose approximation of "turning Docker off and on again."
It will affect all Docker containers on your machine, not just those created by Reach.

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

You can see which exact versions of Reach Docker images you are using by running

@cmd{reach hashes}

This is more precise, but less readable, than @exec{reach version},
in that each hash refers to the git commit used to build the image.


@include-section["ref-model.scrbl"]
@include-section["ref-programs.scrbl"]
@include-section["ref-networks.scrbl"]
@include-section["ref-backends.scrbl"]
@include-section["ref-frontends.scrbl"]
@include-section["ref-error-codes.scrbl"]
