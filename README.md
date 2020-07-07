# Reach

We are building the next generation blockchain development platform that will completely transform the paradigm of decentralized application (DApp) development. DApps have enormous potential, but are impractical today because of barriers that have kept blockchain development risky and limited to specialists, like cryptographers. The Reach platform is a comprehensive environment that enables junior developers to build useful and safe DApps without years of experience and easily deploy them on a variety of different blockchain networks.

This repository is for the Reach language, the domain-specific language for trustworthy DApps used by the Reach platform.

# Documentation

Read the
[documentation](https://reach-sh.github.io/reach-lang/index.html) on GitHub. 

# Development

If you want to work on the Reach compiler, you'll need:
- stack v2.1.3
- z3 v4.8.5
- solidity v0.5.11
- algorand with application TEAL support

Installation on macOS:
```
$ brew tap ethereum/ethereum
$ brew install haskell-stack z3 solidity
```

Installation on Ubuntu:
```
$ sudo apt update
$ sudo apt install z3
$ sudo snap install solc
$ curl -sSL https://get.haskellstack.org/ | sh
```

The source code is in the `hs` directory.
