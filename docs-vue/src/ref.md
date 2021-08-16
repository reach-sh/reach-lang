


# {#ref} Reference

This document contains an exhaustive discussion of each of the parts of the Reach DApp language and its standard library.

[[toc]]

## {#install} Installation

Reach is designed to work on POSIX systems with [make](https://en.wikipedia.org/wiki/Make_(software)), [Docker](https://www.docker.com/get-started), and [Docker Compose](https://docs.docker.com/compose/install/) installed.
The best way to install Docker on Mac and Windows is with [Docker Desktop](https://www.docker.com/products/docker-desktop).

::: note
You probably already have `make` installed.
For example, OS X and many other POSIX systems come with `make`, but some versions of Linux do not include it by default and will require you to install it.
If you're on Ubuntu, you can run `sudo apt install make` to get it.
:::

You can install Reach by running:

```
$ curl https://docs.reach.sh/reach -o reach ; chmod +x reach
```


in your project repository.
You can copy this file to other repositories or move it to a directory in your `PATH`, like `~/bin`.
(`PATH` is UNIX environment variable listing each of the directories that contain programs you can run in a shell session.)

::: note
If you're using Windows, consult [the guide to using Reach on Windows](##guide-windows).
:::

## {#ref-usage} Usage

Reach has a few sub-commands, each with their own options.

However, all commands support the following options:

+ The environment variable `REACH_VERSION` signifies what [version of Reach](##guide-versions) to use.


### {#ref-usage-compile} `reach compile`

You compile your Reach code by executing

```
$ reach compile SOURCE EXPORT ...
```


where `SOURCE` is your source file,
and each `EXPORT` is an exported Reach.App.

If no `SOURCE` is provided, then `index.rsh` is used.

If no `EXPORT` is provided, then all the exported Reach.Apps will be compiled. If there are no
Reach.Apps exported, then the program will be compiled as a library, where its exports are available
to other Reach programs and frontends. The output name of a library is the same as if it exported a Reach.App
named `default`.

`reach compile` supports the following options:

+ `-o`/`--output` `OUTPUT` --- Writes compiler output files to `OUTPUT`, which defaults to a directory named `build` in the same directory as `SOURCE`.
+ `--intermediate-files` --- Write intermediate files, which may be interesting for debugging compilation failures or using in other contexts.
+ `--install-pkgs` --- Allows Reach to fetch remote package imports and stop after doing so.

Reach will fail with an error message if package imports have not yet been fetched and this flag is not activated.

Using this flag will create a `.reach` directory for your project.
This directory stores your project's <Defn :name="lockfile">lockfile</Defn>, which is how Reach pins remote packages to specific versions.
It is recommended that you commit the `.reach` directory to source control.
Deleting this directory is also safe; it can easily be rebuilt by using the `--install-pkgs` flag again; in fact, this is the best way to upgrade your packages.


### {#ref-usage-init} `reach init`

You can create template `index.rsh` and `index.mjs` files for a simple Reach app by running

```
$ reach init
```


### {#ref-usage-run} `reach run`

You can run a simple Reach application by executing

```
$ reach run APP
```


If no `APP` is provided, `index` is used.

If `APP` is a directory, then `APP/index` is used.

This assumes

+ Your Reach program is named `APP.rsh`.
+ You are using the JavaScript backend and your frontend is named `APP.mjs`.
It also assumes the backend is located at `build/APP.main.mjs`, and only depends on the Reach standard library.


It then

+ Compiles your program with Reach.
+ Builds a Docker image named `reachsh/reach-app-APP:latest` that depends on the Reach JavaScript standard library.
+ Executes that image, connected with a private Ethereum devnet.


`reach run` supports the following options:

+ The environment variable `REACH_CONNECTOR_MODE` specifies which context to run in.
The default, if this variable is unset or empty, is `ETH-devnet`.
The options are:

+ `ETH-live`, which uses a live Ethereum network node, specified by the environment variable `ETH_NODE_URI`.
+ `ETH-browser`, which uses Ethereum via a browser extension, like MetaMask.
+ `ETH-devnet`, which uses a Dockerized private Ethereum network.
+ `ALGO-live`, which uses a live Algorand network node, specified by the environment variables documented in [the Algorand connector section](##ref-network-algo).
+ `ALGO-browser`, which uses Algorand via a browser extension, like AlgoSigner.
+ `ALGO-devnet`, which uses a Dockerized private Algorand network.

+ The environment variable `REACH_DEBUG`, if set to any non-empty value, enables debug messages from the Reach standard library, which will appear in the console.


`reach run` can be configured via the presence of certain files.
In the absence of these files, `reach run` assumes a default behavior based on `reach scaffold`.

+ If a `Makefile` is present,
and if the `REACH_CONNECTOR_MODE` and `RUN_FROM_REACH` environment variables are unset or empty,
then `make run` will be invoked, with the `RUN_FROM_REACH` environment variable set to true.
+ If a `Makefile`, `Dockerfile`, `package.json`, and `docker-compose.yml` are all present,
then these files will be used. You can call `reach scaffold` to persist the default versions of these files.
+ If only some of those files exist, but not all, then `reach run` will report an error.
Please delete them, or add the missing ones.


Furthermore, if all of the scaffolded files are present, then pay special attention to the `Makefile` that you write,
because `reach` will use it in specific ways. Refer to the `Makefile` generated by `reach scaffold` for hints on how to get it right.

+ Various `reach` commands may invoke `make build`, which is assumed to build the Docker image for your app.
+ `reach run ...` will invoke `make run-target ARGS="..."`,
where "..." is an escaped, space-separated representation of the command-line args to `reach run`.
See [examples/argz/Makefile](https://github.com/reach-sh/reach-lang/blob/master/examples/argz/Makefile)
for an example invocation of `reach run` with command-line arguments.


### {#ref-usage-down} `reach down`

You can halt the docker containers started by `reach run` by running

```
$ reach down
```


### {#ref-usage-scaffold} `reach scaffold`

You can create template `package.json`, `Dockerfile`, `docker-compose.yml`, and `Makefile` files for a simple Reach app by running

```
$ reach scaffold
```


The files created are the same as those used temporarily by `reach run`.

### {#ref-usage-react} `reach react`

You can run a simple React app by executing

```
$ reach react
```


This assumes

+ Your Reach program is named `index.rsh`
+ Your frontend React program is named `index.js`


It then

+ Compiles your program with Reach
+ Runs the appropriate devnet based on `REACH_CONNECTOR_MODE`
+ Mounts the current directory into `/app/src/` in the `reachsh/react-runner` Docker image and runs it.


`reach react` supports the following options:

+ `--use-existing-devnet` --- Does not start a new devnet, but assumes that `reach devnet` is already running and connects to it.
+ The environment variable `REACH_CONNECTOR_MODE` specifies which context to run in. The default, if this variable is unset or empty, is `ETH`. The options are:

+ `ETH`, which runs a Dockerized private Ethereum network which may be used. The app can use any Ethereum network.
+ `ALGO`, which runs a Dockerized private Algorand network which may be used. (Support for using any Algorand network is forthcoming with TEAL 3.)

+ The environment variable `REACH_DEBUG`, if set to any non-empty value, enables debug messages from the Reach standard library, which will appear in the browser console.


When using `loadStdlib` in conjunction with `reach react`,
be sure to pass in `process.env` as its argument.
See `loadStdlib` for details.

`reach react` does not respect the same scaffolded files as `reach run`.
It is just a simplified tool to give you a taste of web programming with reach.
If you would like access to more customizations on a browser-based project,
such as custom environment variables,
custom JavaScript dependencies,
or using other JavaScript frameworks like Angular,
we recommend that you simply use `reach compile`,
and use your own preferred JavaScript setup for the project.
The compiled `build/index.main.mjs` JavaScript file
and the `'@reach-sh/stdlib'` JavaScript library
may be used in any JavaScript project like any other JavaScript file and library, respectively.

### {#ref-usage-devnet} `reach devnet`

You can run a private Reach devnet by executing

```
$ reach devnet
```


`reach devnet` supports the following options:

+ The environment variable `REACH_CONNECTOR_MODE` specifies which devnet to run. The default, if this variable is unset or empty, is `ETH`. The options are:

+ `ETH`, which runs an Ethereum devnet on `localhost:8545`
+ `ALGO`, which runs an Algorand devnet on `localhost:4180` and an Algorand indexer on `localhost:8980`

+ The environment variable `REACH_DEBUG` enables some additional debugging information for the Algorand devnet, which is accessible via http://localhost:9392


### {#ref-usage-rpc-server} `reach rpc-server`

The sub-command

```
$ reach rpc-server
```


starts an instance of the [Reach RPC Server](##ref-backends-rpc) using all of the same options and defaults as `reach run`.

`reach rpc-server` supports the following options:

+ `--use-existing-devnet` --- Does not start a new devnet, but assumes that `reach devnet` is already running and connects to it.
Ignored if `REACH_CONNECTOR_MODE` includes `live`.
+ The environment variable `REACH_RPC_KEY` is used to determine the RPC server key.
If not defined, it defaults to `opensesame`, and a warning will
appear in the console stating that the development key is being used.

In a production context this key must be kept secret, and it should be
randomly generated with a suitably strong method, such as:

```
$ head -c 24 /dev/urandom | base64
```

+ The environment variable `REACH_RPC_PORT` is used to determine which port to bind to.
It defaults to `3000`.
+ The environment variable `REACH_RPC_TLS_KEY` is used to determine the path to the TLS `key` file, which must be in the `./tls` directory.
It defaults to `reach-server.key`.
+ The environment variable `REACH_RPC_TLS_CRT` is used to determine the path to the TLS `crt` file, which must be in the `./tls` directory.
It defaults to `reach-server.crt`.
+ The environment variable `REACH_RPC_TLS_PASSPHRASE` is used to determine the TLS passphrase.
It defaults to `rpc-demo`.


### {#ref-usage-rpc-run} `reach rpc-run`

The sub-command

```
$ reach rpc-run CMD
```


is a convenient means of launching a pre-configured RPC server and
frontend which are suitable for development purposes.
It uses a `REACH_RPC_KEY` value of `opensesame` (the standard
development API key), and sets `REACH_RPC_TLS_REJECT_UNVERIFIED` to
`0`.

Consider this example from the XXX (seclink "tut-7-rpc") tutorial:
```
$ reach rpc-run python3 -u ./index.py
```


### {#ref-usage-docker-reset} `reach docker-reset`

You can easily kill and remove all Docker containers by executing

```
$ reach docker-reset
```


This can be a useful thing to try if your Docker containers stop responding to requests or otherwise misbehave, or if you have updated your Reach images (with `reach update`) but those changes are not taking effect.
This command is a loose approximation of "turning Docker off and on again."
It will affect all Docker containers on your machine, not just those created by Reach.

### {#ref-usage-upgrade} `reach upgrade`

You can upgrade your Reach installation by executing

```
$ reach upgrade
```


This may change the default version used by `reach` commands.

### {#ref-usage-update} `reach update`

You can update the Docker images used by your Reach installation by executing

```
$ reach update
```


This may change the patch version used by `reach` commands.

### {#ref-usage-version} `reach version`

You can see what version of Reach you have installed by running

```
$ reach version
```


### {#ref-usage-hashes} `reach hashes`

You can see which exact versions of Reach Docker images you are using by running

```
$ reach hashes
```


This is more precise, but less readable, than `reach version`,
in that each hash refers to the git commit used to build the image.








