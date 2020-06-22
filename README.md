# Reach

We are building the next generation blockchain development platform
that will completely transform the paradigm of decentralized
application (DApp) development. DApps have enormous potential, but are
impractical today because of barriers that have kept blockchain
development risky and limited to specialists, like cryptographers. The
Reach platform is a comprehensive environment that enables junior
developers to build useful and safe DApps without years of experience
and easily deploy them on a variety of different blockchain networks.

This repository is for the Reach language, the domain-specific
language for trustworthy DApps used by the Reach platform.

# Documentation

Read the
[documentation](https://reach-sh.github.io/reach-lang/index.html) on GitHub. 

# Usage

The best way to use Reach is with Docker and docker-compose.

Run `reach` for a script to run the compiler using a local build if
one is available, or the Docker image if not. (If using Docker, the
only file in this repository you need is `reach`.)

See `examples/rps` for an example application.

If you just want to see some demos, run `make run-all` from this directory.

# Development

If you want to work on the Reach compiler, you can develop using the
docker build, but we recommend installing some packages locally,
you'll need:
- stack v2.1.3
- z3 v4.8.5
- solidity v0.5.11

Installation on macOS:
```
$ brew install haskell-stack z3 solidity
```

Building:
```
$ cd hs
$ stack build
```
