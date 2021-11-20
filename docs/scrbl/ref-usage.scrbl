#lang scribble/manual
@(require "lib.rkt"
          scribble/bnf)

@title[#:version reach-vers #:tag "ref-usage"]{Usage}

Reach has a few sub-commands, each with their own options.

However, all commands support the following options:

@itemlist[

@item{
  The environment variable @defenv{REACH_VERSION} signifies what version of Reach to use.

  Although normally expressed in a @seclink["guide-versions"]{semantic versioning}-friendly format, e.g. @exec{v0.1} or @exec{v0.1.6}, @envvar{REACH_VERSION} also supports:
  @itemlist[
  @item{
    Hashes such as @link["https://hub.docker.com/layers/reachsh/reach/639fa565/images/sha256-e72fbb183e559a6f531302843c1d4debb499c9286e0ca4839ae66023c7ba2296?context=explore"]{639fa565}.

    Valid hashes may be obtained by running:
    @verbatim{
      $ reach hashes
      reach: 8150e7e4
      reach-cli: 8150e7e4
      react-runner: fb449c94
      rpc-server: fb449c94
      runner: fb449c94
      devnet-algo: fb449c94
      devnet-cfx fb449c94
      devnet-eth: fb449c94
    }
  }

  @item{
    Date-stamps such as @link["https://hub.docker.com/layers/reachsh/reach/2021-11-04/images/sha256-e72fbb183e559a6f531302843c1d4debb499c9286e0ca4839ae66023c7ba2296?context=explore"]{2021-11-04}.

    Valid date-stamps may be obtained by browsing Reach's public Docker image registry on @link["https://hub.docker.com/r/reachsh/reach/tags"]{DockerHub}.

    Tip: try entering your desired year in the filter box to skip other tag types.
  }

  @item{
    The identifer "@exec{stable}".

    Reach will interpret this to mean the most recent stable @seclink["guide-versions"]{major} version.
  }
  ]

  When using the semantic versioning form of @envvar{REACH_VERSION} the preceding @exec{v} character is optional.
  In other words, @exec{v0.1.6} is equivalent to @exec{0.1.6}.
}
]

@section[#:tag "ref-usage-compile"]{@tt{reach compile}}

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

  @item{@DFlag{stop-after-eval} --- Stops the compilation process before verification and producing output files.
  This might be useful for performing syntax and type checking quickly.}

  @item{@DFlag{verify-timeout} @nonterm{TIMEOUT-MS} --- Sets the timeout of individual verification theorems, in milliseconds.
  The default value is 2 minutes.}

  @item{
    The environment variable @envvar{REACH_DEBUG}, if set to any non-empty value, enables debug messages from the Reach compiler, which will appear in the console.
    This debug information includes: the estimated cost of the contract on Algorand.
  }]

@section[#:tag "ref-usage-init"]{@tt{reach init}}
@;TODO document [TEMPLATE] once more templates have been added

You can create template @exec{index.rsh} and @exec{index.mjs} files for a simple Reach app by running

@cmd{reach init}

@section[#:tag "ref-usage-run"]{@tt{reach run}}

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
    The mandatory environment variable @defenv{REACH_CONNECTOR_MODE} specifies which context to run in.
    The options are:

    @itemlist[
      @item{@defconmode{ETH-live}, which uses a live Ethereum network node, specified by the environment variable @envref{ETH_NODE_URI}.}
      @item{@defconmode{ETH-browser}, which uses Ethereum via a browser extension, like MetaMask.}
      @item{@defconmode{ETH-devnet}, which uses a Dockerized private Ethereum network.}
      @item{@defconmode{ALGO-live}, which uses a live Algorand network node, specified by the environment variables documented in @seclink["ref-network-algo"]{the Algorand connector section}.}
      @item{@defconmode{ALGO-browser}, which uses Algorand via an ARC-0011 browser wallet.}
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

@section[#:tag "ref-usage-down"]{@tt{reach down}}

You can halt all Dockerized Reach apps and devnets by running

@cmd{reach down}

@section[#:tag "ref-usage-scaffold"]{@tt{reach scaffold}}

You can create templated @exec{Dockerfile} and @exec{package.json} files for a simple Reach app by running

@cmd{reach scaffold}

The files created are the same as those used temporarily by @exec{reach run}.

@section[#:tag "ref-usage-react"]{@tt{reach react}}

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
    The mandatory environment variable @envref{REACH_CONNECTOR_MODE} specifies which context to run in.
    The options are:

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

@section[#:tag "ref-usage-devnet"]{@tt{reach devnet}}

You can run a private Reach devnet by executing

@cmd{reach devnet}

@exec{reach devnet} supports the following options:

@itemlist[
  @item{@DFlag{await-background} --- Run in background and await availability.}

  @item{
    The mandatory environment variable @envref{REACH_CONNECTOR_MODE} specifies which devnet to run.
    The options are:

    @itemlist[
      @item{@litchar{ETH}, which runs an Ethereum devnet on @tt{localhost:8545}}
      @item{@litchar{ALGO}, which runs an Algorand devnet on @tt{localhost:4180} and an Algorand indexer on @tt{localhost:8980}}
    ]
  }
  @item{
    The environment variable @envref{REACH_DEBUG} enables some additional debugging information for the Algorand devnet, which is accessible via http://localhost:9392
  }
]

@section[#:tag "ref-usage-rpc-server"]{@tt{reach rpc-server}}

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

@section[#:tag "ref-usage-rpc-run"]{@tt{reach rpc-run}}

The sub-command

@cmd{reach rpc-run CMD}

is a convenient means of launching a pre-configured RPC server and
@tech{frontend} which are suitable for development purposes.
It uses a @envvar{REACH_RPC_KEY} value of @litchar{opensesame} (the standard
development API key), and sets @envvar{REACH_RPC_TLS_REJECT_UNVERIFIED} to
@litchar{0}.

Consider this example from the @secref{tut-7-rpc} tutorial:
@cmd{reach rpc-run python3 -u ./index.py}

@section[#:tag "ref-usage-docker-reset"]{@tt{reach docker-reset}}

You can easily kill and remove all Docker containers by executing

@cmd{reach docker-reset}

This can be a useful thing to try if your Docker containers stop responding to requests or otherwise misbehave, or if you have updated your Reach images (with @exec{reach update}) but those changes are not taking effect.
This command is a loose approximation of "turning Docker off and on again."
It will affect all Docker containers on your machine, not just those created by Reach.

@section[#:tag "ref-usage-upgrade"]{@tt{reach upgrade}}

You can upgrade your Reach installation by executing

@cmd{reach upgrade}

This may change the default version used by @exec{reach} commands.

@section[#:tag "ref-usage-update"]{@tt{reach update}}

You can update the Docker images used by your Reach installation by executing

@cmd{reach update}

This may change the patch version used by @exec{reach} commands.

@section[#:tag "ref-usage-version"]{@tt{reach version}}

You can see what version of Reach you have installed by running

@cmd{reach version}

@section[#:tag "ref-usage-hashes"]{@tt{reach hashes}}

You can see which exact versions of Reach Docker images you are using by running

@cmd{reach hashes}

This is more precise, but less readable, than @exec{reach version},
in that each hash refers to the git commit used to build the image.

@section[#:tag "ref-usage-config"]{@tt{reach config}}

Reach recommends tuning your default workflow settings by executing

@cmd{reach config}

Using @exec{reach config} is advisable when running Reach for the first time since it will set the @envref{REACH_CONNECTOR_MODE} environment variable, which is required when executing some other sub-commands (e.g. @exec{reach run}).

@exec{reach config} presents users with a guided menu which automatically creates an @exec{env} file and suggests subsequent steps to activate and make it permanent.
This @exec{env} file exports environment variable settings and is intended to be @exec{source}d by users' shells.

If an @exec{env} file already exists, @exec{reach config} will offer to back it up before proceeding.
