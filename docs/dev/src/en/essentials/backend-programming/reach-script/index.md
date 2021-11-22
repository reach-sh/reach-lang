---
menuItem: mi-docs
---

# Reach Script

The [reach](https://github.com/reach-sh/reach-lang/blob/master/reach) command-line tool enables you to compile Reach DApps and manage your Reach development environment. To install the tool, see [Install and Run](/en/essentials/getting-started/install-and-run/). 

# Environment Variables

In addition to passing command-line arguments, you can influence the behavior of the [reach](https://github.com/reach-sh/reach-lang/blob/master/reach) command-line tool by setting environment variables.

## Reach Connector Mode

To target a specific [consensus network](/en/essentials/network-connectors/), set the `REACH_CONNECTOR_MODE` environment variable to the desired [connector mode](https://github.com/reach-sh/reach-lang/blob/master/js/stdlib/ts/ConnectorMode.ts):

|Network|Example|
|-|-|
|Algorand|`export REACH_CONNECTOR_MODE=ALGO-devnet` <br/> `export REACH_CONNECTOR_MODE=ALGO-live` <br/> `export REACH_CONNECTOR_MODE=ALGO-browser`|
|Conflux|`export REACH_CONNECTOR_MODE=CFX-devnet` <br/> `export REACH_CONNECTOR_MODE=CFX-live` <br/> `export REACH_CONNECTOR_MODE=CFX-browser`|
|Ethereum|`export REACH_CONNECTOR_MODE=ETH-devnet` <br/> `export REACH_CONNECTOR_MODE=ETH-live` <br/> `export REACH_CONNECTOR_MODE=ETH-browser`|

## Reach Debug

To cause the Reach compiler to display additional debug information, set REACH_DEBUG to a non-empty value:

``` nonum
export REACH_DEBUG=1
```

## Reach RPC

The [Reach RPC Server](/en/essentials/frontend-programming/rpc-frontends/) supports the environment variables listed below. See [reach rpc-server](/en/essentials/backend-programming/reach-script/#reach-rpc-server) for details:

``` nonum
REACH_RPC_KEY
REACH_RPC_PORT
REACH_RPC_SERVER
REACH_RPC_TLS_CRT
REACH_RPC_TLS_KEY
REACH_RPC_TLS_PASSPHRASE
REACH_RPC_TLS_REJECT_UNVERIFIED
```

## Reach Version

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

Output from this command supplies you with customized directions for sourcing the `env` script.

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

This command creates minimal index.rsh and index.mjs files in the current directory.

``` nonum
$ reach init [TEMPLATE]
```

## reach react

This command runs a simple React app.

``` nonum
$ reach react
```

## reach rpc-run

This command runs a Reach RPC server and an RPC frontend with a development configuration.

``` nonum
$ reach rpc-run
```

## reach rpc-server

This command runs a Reach RPC server.

``` nonum
$ reach rpc-server
```

## reach run

This command runs a simple Reach DApp.

``` nonum
$ reach run
```

## reach scaffold

This command creates Docker files for a simple app in the current directory.

``` nonum
$ reach scaffold
```

## reach update

This command updates the Reach Docker images.

``` nonum
$ reach update
```

## reach upgrade

This command upgrades the Reach CLI.

``` nonum
$ reach upgrade
```

## reach version

This command displays the Reach compiler version.

``` nonum
$ reach version 
```
