---
menuItem: mi-docs
---

# Reach Tool

The [reach](https://github.com/reach-sh/reach-lang/blob/master/reach) command-line tool enables you to compile Reach DApps and manage your Reach development environment. To install the tool, see [Install and Run](/en/essentials/getting-started/install-and-run/). 

# Environment Variables

In addition to passing command-line arguments, you can influence the behavior of the [reach](https://github.com/reach-sh/reach-lang/blob/master/reach) command-line tool by setting environment variables.

## REACH_CONNECTOR_MODE

To target a specific [consensus network](/en/essentials/network-connectors/), set the `REACH_CONNECTOR_MODE` environment variable to the desired [connector mode](https://github.com/reach-sh/reach-lang/blob/master/js/stdlib/ts/ConnectorMode.ts):

|Network|Example|
|-|-|
|Algorand|`export REACH_CONNECTOR_MODE=ALGO-devnet` <br/> `export REACH_CONNECTOR_MODE=ALGO-live` <br/> `export REACH_CONNECTOR_MODE=ALGO-browser`|
|Conflux|`export REACH_CONNECTOR_MODE=CFX-devnet` <br/> `export REACH_CONNECTOR_MODE=CFX-live` <br/> `export REACH_CONNECTOR_MODE=CFX-browser`|
|Ethereum|`export REACH_CONNECTOR_MODE=ETH-devnet` <br/> `export REACH_CONNECTOR_MODE=ETH-live` <br/> `export REACH_CONNECTOR_MODE=ETH-browser`|

## REACH_DEBUG

To cause the Reach compiler to display additional debug information, set REACH_DEBUG to a non-empty value:

``` nonum
export REACH_DEBUG=1
```

## REACH_RPC

The [Reach RPC Server](/en/essentials/frontend-programming/rpc-frontends/) supports the environment variables listed below. See [reach rpc-server](/en/essentials/backend-programming/reach-tool/#reach-rpc-server) for details:

``` nonum
REACH_RPC_KEY
REACH_RPC_PORT
REACH_RPC_SERVER
REACH_RPC_TLS_CRT
REACH_RPC_TLS_KEY
REACH_RPC_TLS_PASSPHRASE
REACH_RPC_TLS_REJECT_UNVERIFIED
```

## REACH_VERSION

To target a specific Reach compiler version, find the version on [DockerHub](https://hub.docker.com/r/reachsh/reach/tags), and set the `REACH_VERSION` environment variable to the desired version, hash tag, date, or identifier:

|Technique|Example|
|-|-|
|Version|`export REACH_VERSION 0.1.6` <br/> `export REACH_VERSION v0.1.6` |
|Hash Tag|`export REACH_VERSION 639fa565`|
|Date|`export REACH_VERSION 2021-11-04`|
|Identifier|`export REACH_VERSION latest` <br/> `export REACH_VERSION stable`|

# Commands

Run `reach help` to view available commands.

## reach clean

By default, this command deletes `build/index.main.mjs`:

``` nonum
$ reach clean
```

To delete `build/myindex.mymain.mjs`, run the following:

``` nonum
$ reach clean myindex mymain
```

To delete `~/reach/hello-world/build/index.mymain.mjs`, run the following:

``` nonum
$ reach clean ~/reach/hello-world mymain
```

## reach compile

By default, this command compiles `index.rsh` into `build/index.main.mjs` and verifies it:

``` nonum
$ reach compile
```

To output debug information, set `REACH_DEBUG` to a non-zero value:

``` nonum
$ export REACH_DEBUG=1
$ reach compile
```

To compile `myindex.rsh` into `build/myindex.main.mjs`, run the following:

``` nonum
$ reach compile myindex.rsh 
```

To compile `myindex.rsh` into `mybuild/myindex.main.mjs`, run the following:

``` nonum
$ reach compile myindex.rsh --output mybuild
```

To retain intermediate build files in `/build`, use `--intermediate-files`:

``` nonum
$ reach compile --intermediate-files
```

To fetch remote package imports without compiling, use `--install-pkgs`:

``` nonum
$ reach compile --install-pkgs
```

## reach config

After soliciting information from you, this command creates a `~/.config/reach/env` script containing commands to export Reach environment variables:

``` nonum
$ reach config
```

Output from this command supplies you with customized directions to [source](https://en.wikipedia.org/wiki/Dot_(command)) the `env` script.

## reach devnet

With `REACH_CONNECTOR_MODE` set, this command launches a private devnet on which you can run/test your Reach DApps:

``` nonum
$ reach devnet
```

To run the devnet in the background, use `--await-background`:

``` nonum
$ reach devnet --await-background
```

## reach docker-reset

This command stops and deletes all Docker containers including Reach Docker containers. 

``` nonum
$ reach docker-reset
```

Use [reach down](#reach-down) to stop and delete only Reach Docker containers.

## reach down

This command stops and deletes all Reach Docker containers. 

``` nonum
$ reach down
```

Docker containers are instances of Docker images. So, after you run `reach update` to refresh you Reach Docker images, running `reach down` ensures that all Reach Docker containers based on old Reach Docker images are removed. 

## reach hashes

This command displays git hash values associated with your current Reach Docker images:

``` nonum
$ reach hashes
```

## reach init

This command creates minimal *index.rsh* and *index.mjs* files in the current directory:

``` nonum
$ reach init [TEMPLATE]
```

The command aborts if *index.rsh* or *index.mjs* already exists.

## reach react

This command runs a simple React app consisting of *index.rsh* and *index.js*:

``` nonum
$ reach react
```

The command compiles *index.rsh*, runs the devnet specified by `REACH_CONNECTOR_MODE`, mounts the current directory into `/app/src/` in the `reachsh/react-runner` Docker image, and runs the app.

To output debug information, set `REACH_DEBUG` to a non-zero value:

``` nonum
$ export REACH_DEBUG=1
$ reach react
```

## reach rpc-run

This command runs a Reach RPC server and an RPC frontend with a development configuration:

``` nonum
$ reach rpc-run
```

The command sets `REACH_RPC_KEY=opensesame` and `REACH_RPC_TLS_REJECT_UNVERIFIED=0`.

You can include on the `reach rpc-run` command line any arguments destined for your frontend executable like this:

``` nonum
$ reach rpc-run python3 -u ./index.py
```

## reach rpc-server

With `REACH_CONNECTOR_MODE` set, this command runs a Reach RPC server in support of [RPC frontends](/en/essentials/frontend-programming/rpc-frontends/):

``` nonum
$ reach rpc-server
```

The command respects the following environment variables:

|Variable|Default|
|-|-|
|`REACH_RPC_KEY`|`opensesame`|
|`REACH_RPC_PORT`|`3000`|
|`REACH_RPC_TLS_KEY`|`reach-server.key`|
|`REACH_RPC_TLS_CRT`|`reach-server.crt`|
|`REACH_RPC_TLS_PASSPHRASE`|`rpc-demo`|

In a production context, `REACH_RPC_KEY` should be secret, generated via a suitable method like this:

``` nonum
$ head -c 24 /dev/urandom | base64
```

`REACH_RPC_TLS_KEY` specifies the path to the TLS key file in the `./tls` directory.

`REACH_RPC_TLS_CRT` specifies the path to the TLS crt file in the `./tls` directory.

## reach run

With `REACH_CONNECTOR_MODE` set, this command runs an app in the current directory consisting of *index.rsh* and *index.js*: 

``` nonum
$ reach run
```

The command does the following:

1. Compiles `index.rsh` into `build/index.main.mjs`.
1. Builds a Docker image named `reachsh/reach-app-APP:latest` that depends on the [Reach JS standard library](/en/essentials/frontend-programming/javascript-frontends/).
1. Instantiates a Docker container based on the Docker image.
1. Connects to the consensus network determined by `REACH_CONNECTOR_MODE`.

To output debug information, set `REACH_DEBUG` to a non-zero value:

``` nonum
$ export REACH_DEBUG=1
$ reach run
```
To run an app in the current directory consisting of *myindex.rsh* and *myindex.js*, use the following:

``` nonum
$ reach run myindex
```

To pass arguments to `index.mjs` (e.g. `seller`), add the arguments to the `reach run index` command line:

``` nonum
$ reach run index seller
```

To extend `reach run` functionality, see [reach scaffold](#reach-scaffold).

## reach scaffold

This command lays the groundwork for adding NPM packages to your Reach DApp by creating two files, *package.json* and *Dockerfile*:

``` nonum
$ reach scaffold
```

After running `reach scaffold`, open *package.json*, and add desired dependencies (e.g. `yargs`):

``` nonum
"dependencies": {
  "yargs": "^17.2.1"
},
```

If you have an NPM environment installed on your computer, you can add dependencies by running `npm install`:

``` nonum
$ npm i yargs
```

Then, open `Dockerfile`, and uncomment the following lines:

``` nonum
COPY package.json /app/package.json
RUN npm install
RUN npm link @reach-sh/stdlib
```

These lines direct `reach run` to copy *package.json* to the Docker container where your app will run, and install your packages.

## reach update

This command downloads Reach Docker images to your computer.

``` nonum
$ reach update
```

By default, the command downloads the latest images. Set [REACH_VERSION](#reach_version) to specify a different version.

## reach upgrade

This command upgrades your copy of the [Reach Tool](https://github.com/reach-sh/reach-lang/blob/master/reach).

``` nonum
$ reach upgrade
```

## reach version

This command displays the version of the Reach tool, compiler, and other images installed on your computer:

``` nonum
$ reach version 
```
