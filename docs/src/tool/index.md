# {#ref} Tool

This document describes the `reach` tool: how to install it and how to use it.

# {#ref-install} Installation

Reach is designed to work on POSIX systems with [make](https://en.wikipedia.org/wiki/Make_(software)), [Docker](https://www.docker.com/get-started), and [Docker Compose](https://docs.docker.com/compose/install/) installed.
The best way to install Docker on Mac and Windows is with [Docker Desktop](https://www.docker.com/products/docker-desktop).

:::note
You probably already have `make` installed.
For example, OS X and many other POSIX systems come with `make`, but some versions of Linux do not include it by default and will require you to install it.
If you are on Ubuntu, you can run `sudo apt install make` to get it.
:::

You can install Reach by running:

```cmd
$ curl https://docs.reach.sh/reach -o reach ; chmod +x reach
```

in your project repository.
You can copy this file to other repositories or move it to a directory in your `PATH`, like `~/bin`.
(`PATH` is a UNIX environment variable listing each of the directories that contain programs you can run in a shell session.)

:::note
If you're using Windows, consult [the guide to using Reach on Windows](##guide-windows).
:::

# {#ref-usage} Usage

# {#ref-usage-envvar-connector-mode} @{ref("cmd", "REACH_CONNECTOR_MODE")} `REACH_CONNECTOR_MODE`
`{!cmd} REACH_CONNECTOR_MODE` is how one targets their desired [network](##ref-networks).

The following are all valid options:
- `ALGO` (this is a shortcut for `ALGO-devnet`)
- `ALGO-browser`
- `ALGO-devnet`
- `ALGO-live`
- `ETH` (this is a shortcut for `ETH-devnet`)
- `ETH-browser`
- `ETH-devnet`
- `ETH-live`

Not all Reach commands require `{!cmd} REACH_CONNECTOR_MODE` to be set, and some commands only support a subset of the list above (e.g. `{!cmd} reach run` and `{!cmd} reach react`).

Commands for which `{!cmd} REACH_CONNECTOR_MODE` is mandatory will raise an exception at runtime if it isn't specified.

For the sake of convenience it's recommended to run `{!cmd} reach config` to select a permanent default when first setting up your development machine, but you may also supply an ad hoc network explicitly like so:

```cmd
$ REACH_CONNECTOR_MODE=ALGO reach run
```

Making an explicit selection in this way is how Reach developers can target multiple networks since an environment variable specified at the command-line takes precedence over one's `{!cmd} reach config` settings.

# {#ref-usage-envvar-version} @{ref("cmd", "REACH_VERSION")} `REACH_VERSION`
`{!cmd} REACH_VERSION` may be used to override Reach's default-to-latest behavior and instead select a specific, pinned release.

Although normally expressed in a [semantic versioning](##guide-versions)-friendly format, e.g. `v0.1` or `v0.1.6`, `{!cmd} REACH_VERSION` also supports:
+ Hashes such as [639fa565](https://hub.docker.com/layers/reachsh/reach/639fa565/images/sha256-e72fbb183e559a6f531302843c1d4debb499c9286e0ca4839ae66023c7ba2296?context=explore).

  Valid hashes may be obtained by running:
  ```cmd
  $ reach hashes
  reach: 8150e7e4
  reach-cli: 8150e7e4
  react-runner: fb449c94
  rpc-server: fb449c94
  runner: fb449c94
  devnet-algo: fb449c94
  devnet-eth: fb449c94
  ```

+ Date-stamps such as [2021-11-04](https://hub.docker.com/layers/reachsh/reach/2021-11-04/images/sha256-e72fbb183e559a6f531302843c1d4debb499c9286e0ca4839ae66023c7ba2296?context=explore).

  Valid date-stamps may be obtained by browsing Reach's public Docker image registry on [DockerHub](https://hub.docker.com/r/reachsh/reach/tags).

  Tip: try entering your desired year in the filter box to skip other tag types.

+ The identifer "`stable`".

  Reach will interpret this to mean the most recent stable [major](##guide-versions) version.

When using the semantic versioning form of `{!cmd} REACH_VERSION` the preceding `v` character is optional.
In other words, `v0.1.6` is equivalent to `0.1.6`.

# {#ref-usage-envvar-debug} @{ref("cmd", "REACH_DEBUG")} `REACH_DEBUG`
For commands that support it, setting `{!cmd} REACH_DEBUG` to any non-empty value produces verbose output which may be helpful during development or debugging.

See `{!cmd} reach compile`, `{!cmd} reach run`, or `{!cmd} reach react` for examples.

# {#ref-usage-compile} @{ref("cmd", "reach compile")} `reach compile`

Compile Reach code by executing

```cmd
$ reach compile SOURCE EXPORT ...
```

where `SOURCE` is your source file,
and each `EXPORT` is an exported `{!rsh} Reach.App`.

If no `SOURCE` is provided, then `index.rsh` is used.

If no `EXPORT` is provided, then all the exported `{!rsh} Reach.App`s will be compiled.
If there are no `{!rsh} Reach.App`s exported, then the program will be compiled as a library, where its exports are available to other Reach programs and frontends.
The output name of a library is the same as if it exported a `{!rsh} Reach.App` named `default`.

`{!cmd} reach compile` supports the following options:

+ `-o`/`--output` `OUTPUT` --- Writes compiler output files to `OUTPUT`, which defaults to a directory named `build` in the same directory as `SOURCE`.

+ `--intermediate-files` --- Write intermediate files, which may be interesting for debugging compilation failures or using in other contexts.

+ `--install-pkgs` --- Allows Reach to fetch remote package imports and stop after doing so.

  Reach will fail with an error message if package imports have not yet been fetched and this flag is not activated.

  Using this flag will create a `.reach` directory for your project.
  This directory stores your project's @{defn("lockfile")}, which is how Reach pins remote packages to specific versions.
  It is recommended that you commit the `.reach` directory to source control.
  Deleting this directory is also safe; it can easily be rebuilt by using the `--install-pkgs` flag again; in fact, this is the best way to upgrade your packages.

+ `--stop-after-eval` --- Stops the compilation process before verification and producing output files.
  This might be useful for performing syntax and type checking quickly.

+ `--verify-timeout` `TIMEOUT-MS` --- Sets the timeout of individual verification theorems, in milliseconds.
  The default value is 2 minutes.

+ `--verify-fail-once` --- Stops the compilation process after printing a single verification failure.
  This may help keep you organized while fixing verification failures in your program.

+ The environment variable `{!cmd} REACH_DEBUG`, if set to any non-empty value, enables debug messages from the Reach compiler, which will appear in the console.
  This debug information includes: the estimated cost of the contract on Algorand.
  This variable automatically enables `--intermediate-files`.

A `{!cmd} reach compile` usage example is available in [Overview](##over-compile).

# {#ref-usage-init} @{ref("cmd", "reach init")} `reach init`

This creates the `index.rsh` and `index.mjs` template files required for a basic Reach DApp.
It allows you to open the files and start writing code.
The `index.rsh` file is the DApp and is written in Reach, and the `index.mjs` file is the frontend of the DApp and is written in JavaScript.

```cmd
$ reach init
```

# {#ref-usage-run} @{ref("cmd", "reach run")} `reach run`

The `{!cmd} reach run` command with no arguments starts the `index` application in the current directory by default, but you can set a different directory, application name, or both.

The `{!cmd} reach run` command uses the following interface:

```cmd
$ reach run [APP or DIR] [ARGS]
```

`APP` represents a Reach module name without its extension (e.g. "index" by default).

If no `APP` or `DIR` is provided then `index` in the current working directory is assumed.

If `DIR` matches an existing, relative subdirectory of the current working directory then `DIR/index` is used.

`ARGS` represents zero or more arguments to be passed into `APP`'s frontend.

This assumes

+ Your Reach program is named `APP.rsh`.
+ You are using the JavaScript backend and your frontend is named `APP.mjs`.
It also assumes the backend is located at `DIR/build/APP.main.mjs`, and only depends on the Reach standard library.

It then

+ Compiles your program with Reach.
+ Builds a Docker image named `reachsh/reach-app-APP:latest` that depends on the Reach JavaScript standard library.
+ Executes a container based upon that image while connected to the network determined by `{!cmd} REACH_CONNECTOR_MODE`.

`{!cmd} reach run` supports the following options:

+ The mandatory environment variable `{!cmd} REACH_CONNECTOR_MODE` specifies which context to run in.
The options are:

  + `ETH-devnet` (or `ETH` for short), which uses a Dockerized private Ethereum network.
  + `ETH-live`, which uses a live Ethereum network node, specified by the environment variable `ETH_NODE_URI`.
  + `ALGO-devnet` (or `ALGO` for short), which uses a Dockerized private Algorand network.
  + `ALGO-live`, which uses a live Algorand network node, specified by the environment variables documented in [the Algorand connector section](##ref-network-algo).

+ The environment variable `{!cmd} REACH_DEBUG`, if set to any non-empty value, enables debug messages from the Reach standard library, which will appear in the console.

`{!cmd} reach run` can be further specialized via the presence of a `Dockerfile` and `package.json` file.
If either are absent, `{!cmd} reach run` assumes a default behavior (which may be persisted with `{!cmd} reach scaffold`).

The `Dockerfile` can be modified to introduce new dependencies, services, or filesystem prerequisites into your app's containerized environment, and the `package.json` file may likewise be extended to include additional libraries or make configuration changes to the resultant Node.js package.

# {#ref-usage-down} @{ref("cmd", "reach down")} `reach down`

You can halt all Dockerized Reach apps and devnets by running

```cmd
$ reach down
```

# {#ref-usage-clean} @{ref("cmd", "reach clean")} `reach clean`

You can delete the contents of `build/index.main.mjs` with

```cmd
$ reach clean
```

This command is useful when the backend's version no longer matches the Reach standard library you have installed and you need to recompile.

The standard usage is `{!cmd} reach clean [MODULE] [IDENT]` where `MODULE` is `index` by default and `IDENT` is `main`. 
Changing the default values will change the `mjs` file `{!cmd} reach clean` seeks to delete.

If `MODULE` is a directory then `{!cmd} reach clean` will `cd` into `MODULE` and remove `build/index.IDENT.mjs`.
If `MODULE` is a file name then `{!cmd} reach clean` will remove `build/MODULE.IDENT.mjs`.

# {#ref-usage-scaffold} @{ref("cmd", "reach scaffold")} `reach scaffold`

You can create templated `Dockerfile` and `package.json` files for a simple Reach app by running

```cmd
$ reach scaffold
```

The files created are the same as those used temporarily by `{!cmd} reach run`.

# {#ref-usage-support} @{ref("cmd", "reach support")} `reach support`

You can upload a gist to GitHub using

```cmd
$ reach support path/to/file1 path/to/file2 path/to/file3 ...
```

`{!cmd} reach support` takes a list of files that defaults to `index.rsh` and `index.mjs` if called without parameters.

It will fail at the first file it fails to find.

If all passed files are present, this command uploads those files as a gist to GitHub, and gives you a link to that gist that anyone with the link can view.

# {#ref-usage-react} @{ref("cmd", "reach react")} `reach react`

You can run a simple React app by executing

```cmd
$ reach react
```

This assumes

+ Your Reach program is named `index.rsh`
+ Your frontend React program is named `index.js`

It then

+ Compiles your program with Reach
+ Runs the appropriate devnet based on `{!cmd} REACH_CONNECTOR_MODE`
+ Mounts the current directory into `/app/src/` in the `reachsh/react-runner` Docker image and runs it.

`{!cmd} reach react` supports the following options:

+ The mandatory environment variable `{!cmd} REACH_CONNECTOR_MODE` specifies which context to run in.
The options are:

  + `ETH-browser` (or `ETH` for short), which targets a Dockerized private Ethereum network via a browser extension, like MetaMask.
  + `ALGO-browser` (or `ALGO` for short), which targets a Dockerized private Algorand network via an ARC-0011 browser wallet.

+ The environment variable `{!cmd} REACH_DEBUG`, if set to any non-empty value, enables debug messages from the Reach standard library, which will appear in the browser console.

When using `{!js} loadStdlib` in conjunction with `{!cmd} reach react`,
be sure to pass in `{!js} process.env` as its argument.
See `{!js} loadStdlib` for details.

`{!cmd} reach react` does not respect the same scaffolded files as `{!cmd} reach run`.
It is just a simplified tool to give you a taste of web programming with reach.
If you would like access to more customizations on a browser-based project,
such as custom environment variables,
custom JavaScript dependencies,
or using other JavaScript frameworks like Angular,
we recommend that you simply use `{!cmd} reach compile`,
and use your own preferred JavaScript setup for the project.
The compiled `build/index.main.mjs` JavaScript file
and the `'@reach-sh/stdlib'` JavaScript library
may be used in any JavaScript project like any other JavaScript file and library, respectively.

# {#ref-usage-devnet} @{ref("cmd", "reach devnet")} `reach devnet`

You can run a private Reach devnet by executing the following command:

```cmd
$ reach devnet
```

:::note
If running `{!cmd} reach devnet`, it is recommended to permanently set `{!cmd} REACH_CONNECTOR_MODE` to the desired consensus network.
If you did not set this when you originally ran `{!cmd} reach config`, then you can run `{!cmd} reach devnet` with the argument `{!cmd} REACH_CONNECTOR_MODE=[OPTION]` as in the following command.
:::

```cmd
$ REACH_CONNECTOR_MODE=ALGO ./reach devnet
```

`{!cmd} reach devnet` supports the following options:

+ `--await-background` --- Run in the background and await availability.

For more information on devnet options, refer to [Networks](##ref-networks).

# {#ref-usage-rpc-server} @{ref("cmd", "reach rpc-server")} `reach rpc-server`

The following command

```cmd
$ reach rpc-server
```

starts an instance of the [Reach RPC Server](##ref-backends-rpc), sharing all of the same options and defaults as `{!cmd} reach run`, but also including the following:

+ The environment variable `{!cmd} REACH_RPC_KEY` is used to determine the RPC server key.
  If not defined, it defaults to `opensesame`, and a warning will appear in the console stating that the development key is being used.

  In a production context this key must be kept secret, and it should be randomly generated with a suitably strong method, such as:

  ```cmd
  $ head -c 24 /dev/urandom | base64
  ```

+ The environment variable `{!cmd} REACH_RPC_PORT` is used to determine which port to bind to.
It defaults to `3000`.
+ The environment variable `{!cmd} REACH_RPC_TLS_KEY` is used to determine the path to the TLS `key` file, which must be in the `./tls` directory.
It defaults to `reach-server.key`.
+ The environment variable `{!cmd} REACH_RPC_TLS_CRT` is used to determine the path to the TLS `crt` file, which must be in the `./tls` directory.
It defaults to `reach-server.crt`.
+ The environment variable `{!cmd} REACH_RPC_TLS_PASSPHRASE` is used to determine the TLS passphrase.
It defaults to `rpc-demo`.

# {#ref-usage-rpc-run} @{ref("cmd", "reach rpc-run")} `reach rpc-run`

The following command

```cmd
$ reach rpc-run CMD
```

is a convenient means of launching a pre-configured RPC server and
frontend which are suitable for development purposes.
It uses a `REACH_RPC_KEY` value of `opensesame` (the standard
development API key), and sets `REACH_RPC_TLS_REJECT_UNVERIFIED` to
`0`.

Consider this example from the @{seclink("tut-7-rpc")} tutorial:

```cmd
$ reach rpc-run python3 -u ./index.py
```

# {#ref-usage-docker-reset} @{ref("cmd", "reach docker-reset")} `reach docker-reset`

You can easily kill and remove all Docker containers by executing

```cmd
$ reach docker-reset
```

This can be a useful thing to try if your Docker containers stop responding to requests or otherwise misbehave, or if you have updated your Reach images (with `{!cmd} reach update`) but those changes are not taking effect.

It will affect all Docker containers on your machine, not just those created by Reach.
For this reason it's recommended to prefer `{!cmd} reach down`.

# {#ref-usage-info} @{ref("cmd", "reach info")} `reach info`

To check whether new releases are available, run

```cmd
$ reach info
```

`{!cmd} reach info` may be paired with the `{!cmd} REACH_VERSION` environment variable in order to respect version-pinning (e.g. if you want the latest version on Reach's `0.1.6` branch but don't wish to upgrade to `0.1.7`).

An interactive menu, allowing you to selectively perform suggested updates, is also available:

```cmd
$ reach info --interactive
```

# {#ref-usage-update} @{ref("cmd", "reach update")} `reach update`

You can update Reach to a newer release by executing

```cmd
$ reach update
```

As with `{!cmd} reach info`, `{!cmd} reach update` respects the `{!cmd} REACH_VERSION` environment variable for the purpose of version-pinning.

# {#ref-usage-version} @{ref("cmd", "reach version")} `reach version`

Check which version of the Reach command-line tool is currently installed by running

```cmd
$ reach version
```

This is less precise than `{!cmd} reach hashes`, but gives you an idea of which features are, or are not, available in your build version.

# {#ref-usage-hashes} @{ref("cmd", "reach hashes")} `reach hashes`

Check which version of each Reach Docker image is installed.
This command returns the hash version of each image in an 8 digit alpha-numeric code (such as [639fa565](https://hub.docker.com/layers/reachsh/reach/639fa565/images/sha256-e72fbb183e559a6f531302843c1d4debb499c9286e0ca4839ae66023c7ba2296?context=explore)).

```cmd
$ reach hashes
```

This is more precise, but less readable, than `{!cmd} reach version`,
in that each hash refers to the git commit used to build the image.

# {#ref-usage-config} @{ref("cmd", "reach config")} `reach config`

Reach recommends tuning your default workflow settings by executing

```cmd
$ reach config
```

Using `{!cmd} reach config` is advisable when running Reach for the first time since it will set the `{!cmd} REACH_CONNECTOR_MODE` environment variable, which is required when executing some other sub-commands (e.g. `{!cmd} reach run`).

`{!cmd} reach config` presents users with a guided menu which automatically creates an `env` file and suggests subsequent steps to activate and make it permanent.
This `env` file exports environment variable settings and is intended to be `source`d by users' shells.

If an `env` file already exists, `{!cmd} reach config` offers to back it up before proceeding.

# {#ref-usage-arg-disable-reporting} @{ref("cmd", "reach --disable-reporting")} `reach --disable-reporting`
The Reach command-line tool collects anonymous usage data by default, but this can be skipped by prepending `--disable-reporting` before any given sub-command, e.g.:

```cmd
$ reach --disable-reporting compile index.rsh
```

Out of respect for users' privacy and the security of their intellectual property, Reach takes special care to avoid gathering personally-identifiable information and instead tallies only metrics which cannot easily be correlated back to specific individuals and which doesn't leak sensitive details about their code.

For instance, we discard IP addresses and strip timestamps to just UTC dates.

Only the following are tracked:
- From the Reach command-line tool or VS Code extension:
  - A random ID that's unique per-user and per-machine (but which doesn't reveal the user's identity);
  - The UTC date on which an event occurred;
  - The type of event, e.g. `{!cmd} reach run` or `{!cmd} reach devnet`;
  - Resultant [error code](##ref-error-codes) or indication of success associated with the event;
  - `{!cmd} REACH_VERSION`;
  - `{!cmd} REACH_CONNECTOR_MODE`;
  - Whether the event was triggered by the [Reach VSCode extension](##guide-install-VSCode).

- [Geolocation data](https://www.npmjs.com/package/fast-geoip) inferred from your IP address:
  - The user's country;
  - The user's state or region.

Invoking a sub-command with `--disable-reporting` instructs `reach` not to send any usage data at all.
