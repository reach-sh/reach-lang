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

# Dependencies

- stack v2.1.3
- z3 v4.8.5
- solidity v0.5.11

### Dependencies for macOS running homebrew

```
$ brew install haskell-stack z3 solidity
```

# Development Installation

If you want to develop from this checkout, you'll need to build the
compiler:

```
$ cd hs
$ stack build
```

# Running

From this directory, you can run the integration tests and demo
application with:

```
$ make test
```
