


# {#ref-usage} Usage

Reach has a few sub-commands, each with their own options.

However, all commands support the following options:

+ The environment variable `REACH_VERSION` signifies what version of Reach to use.

Although normally expressed in a [semantic versioning](##guide-versions)-friendly format, e.g. `v0.1` or `v0.1.6`, `REACH_VERSION` also supports:
+ Hashes such as [639fa565](https://hub.docker.com/layers/reachsh/reach/639fa565/images/sha256-e72fbb183e559a6f531302843c1d4debb499c9286e0ca4839ae66023c7ba2296?context=explore).

Valid hashes may be obtained by running:
```
$ reach hashes
reach: 8150e7e4
reach-cli: 8150e7e4
react-runner: fb449c94
rpc-server: fb449c94
runner: fb449c94
devnet-algo: fb449c94
devnet-cfx fb449c94
devnet-eth: fb449c94
```

+ Date-stamps such as [2021-11-04](https://hub.docker.com/layers/reachsh/reach/2021-11-04/images/sha256-e72fbb183e559a6f531302843c1d4debb499c9286e0ca4839ae66023c7ba2296?context=explore).

Valid date-stamps may be obtained by browsing Reach's public Docker image registry on [DockerHub](https://hub.docker.com/r/reachsh/reach/tags).

Tip: try entering your desired year in the filter box to skip other tag types.
+ The identifer "`stable`".

Reach will interpret this to mean the most recent stable [major](##guide-versions) version.


When using the semantic versioning form of `REACH_VERSION` the preceding `v` character is optional.
In other words, `v0.1.6` is equivalent to `0.1.6`.


## {#ref-usage-compile} `reach compile`

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
This directory stores your project's @{defn("lockfile")}, which is how Reach pins remote packages to specific versions.
It is recommended that you commit the `.reach` directory to source control.
Deleting this directory is also safe; it can easily be rebuilt by using the `--install-pkgs` flag again; in fact, this is the best way to upgrade your packages.
+ `--stop-after-eval` --- Stops the compilation process before verification and producing output files.
This might be useful for performing syntax and type checking quickly.
+ `--verify-timeout` `TIMEOUT-MS` --- Sets the timeout of individual verification theorems, in milliseconds.
The default value is 2 minutes.
+ The environment variable `REACH_DEBUG`, if set to any non-empty value, enables debug messages from the Reach compiler, which will appear in the console.
This debug information includes: the estimated cost of the contract on Algorand.


## {#ref-usage-init} `reach init`

You can create template `index.rsh` and `index.mjs` files for a simple Reach app by running


```
$ reach init
```


## {#ref-usage-run} `reach run`

You can run a simple Reach application by executing


```
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
+ Executes a container based upon that image while connected to the network determined by `REACH_CONNECTOR_MODE`.


`reach run` supports the following options:

+ The mandatory environment variable `REACH_CONNECTOR_MODE` specifies which context to run in.
The options are:

+ `ETH-live`, which uses a live Ethereum network node, specified by the environment variable `ETH_NODE_URI`.
+ `ETH-browser`, which uses Ethereum via a browser extension, like MetaMask.
+ `ETH-devnet`, which uses a Dockerized private Ethereum network.
+ `ALGO-live`, which uses a live Algorand network node, specified by the environment variables documented in [the Algorand connector section](##ref-network-algo).
+ `ALGO-browser`, which uses Algorand via an ARC-0011 browser wallet.
+ `ALGO-devnet`, which uses a Dockerized private Algorand network.

+ The environment variable `REACH_DEBUG`, if set to any non-empty value, enables debug messages from the Reach standard library, which will appear in the console.


`reach run` can be further specialized via the presence of a `Dockerfile` and `package.json` file.
If either are absent, `reach run` assumes a default behavior (which may be persisted with `reach scaffold`).

The `Dockerfile` can be modified to introduce new dependencies, services, or filesystem prerequisites into your app's containerized environment, and the `package.json` file may likewise be extended to include additional libraries or make configuration changes to the resultant Node.js package.

## {#ref-usage-down} `reach down`

You can halt all Dockerized Reach apps and devnets by running


```
$ reach down
```


## {#ref-usage-scaffold} `reach scaffold`

You can create templated `Dockerfile` and `package.json` files for a simple Reach app by running


```
$ reach scaffold
```


The files created are the same as those used temporarily by `reach run`.

## {#ref-usage-react} `reach react`

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

+ The mandatory environment variable `REACH_CONNECTOR_MODE` specifies which context to run in.
The options are:

+ `ETH`, which runs a Dockerized private Ethereum network which may be used. The app can use any Ethereum network.
+ `ALGO`, which runs a Dockerized private Algorand network which may be used. (Support for using any Algorand network is forthcoming with TEAL 3.)

+ The environment variable `REACH_DEBUG`, if set to any non-empty value, enables debug messages from the Reach standard library, which will appear in the browser console.


When using `{!js} loadStdlib` in conjunction with `reach react`,
be sure to pass in `{!js} process.env` as its argument.
See `{!js} loadStdlib` for details.

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

## {#ref-usage-devnet} `reach devnet`

You can run a private Reach devnet by executing


```
$ reach devnet
```


`reach devnet` supports the following options:

+ `--await-background` --- Run in background and await availability.
+ The mandatory environment variable `REACH_CONNECTOR_MODE` specifies which devnet to run.
The options are:

+ `ETH`, which runs an Ethereum devnet on `localhost:8545`
+ `ALGO`, which runs an Algorand devnet on `localhost:4180` and an Algorand indexer on `localhost:8980`

+ The environment variable `REACH_DEBUG` enables some additional debugging information for the Algorand devnet, which is accessible via http://localhost:9392


## {#ref-usage-rpc-server} `reach rpc-server`

The sub-command


```
$ reach rpc-server
```


starts an instance of the [Reach RPC Server](##ref-backends-rpc) using all of the same options and defaults as `reach run`.

`reach rpc-server` supports the following options:

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


## {#ref-usage-rpc-run} `reach rpc-run`

The sub-command


```
$ reach rpc-run CMD
```


is a convenient means of launching a pre-configured RPC server and
frontend which are suitable for development purposes.
It uses a `REACH_RPC_KEY` value of `opensesame` (the standard
development API key), and sets `REACH_RPC_TLS_REJECT_UNVERIFIED` to
`0`.

Consider this example from the @{seclink("tut-7-rpc")} tutorial:

```
$ reach rpc-run python3 -u ./index.py
```


## {#ref-usage-docker-reset} `reach docker-reset`

You can easily kill and remove all Docker containers by executing


```
$ reach docker-reset
```


This can be a useful thing to try if your Docker containers stop responding to requests or otherwise misbehave, or if you have updated your Reach images (with `reach update`) but those changes are not taking effect.
This command is a loose approximation of "turning Docker off and on again."
It will affect all Docker containers on your machine, not just those created by Reach.

## {#ref-usage-upgrade} `reach upgrade`

You can upgrade your Reach installation by executing


```
$ reach upgrade
```


This may change the default version used by `reach` commands.

## {#ref-usage-update} `reach update`

You can update the Docker images used by your Reach installation by executing


```
$ reach update
```


This may change the patch version used by `reach` commands.

## {#ref-usage-version} `reach version`

You can see what version of Reach you have installed by running


```
$ reach version
```


## {#ref-usage-hashes} `reach hashes`

You can see which exact versions of Reach Docker images you are using by running


```
$ reach hashes
```


This is more precise, but less readable, than `reach version`,
in that each hash refers to the git commit used to build the image.

## {#ref-usage-config} `reach config`

Reach recommends tuning your default workflow settings by executing


```
$ reach config
```


Using `reach config` is advisable when running Reach for the first time since it will set the `REACH_CONNECTOR_MODE` environment variable, which is required when executing some other sub-commands (e.g. `reach run`).

`reach config` presents users with a guided menu which automatically creates an `env` file and suggests subsequent steps to activate and make it permanent.
This `env` file exports environment variable settings and is intended to be `source`d by users' shells.

If an `env` file already exists, `reach config` will offer to back it up before proceeding.
