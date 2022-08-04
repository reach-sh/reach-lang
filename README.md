# Have a question?

Let's talk. **[Start a GitHub Discussion](https://github.com/reach-sh/reach-lang/discussions)**

# Reach
[![CircleCI Build Status](https://circleci.com/gh/reach-sh/reach-lang.svg?style=shield)](https://circleci.com/gh/reach-sh/reach-lang) [![GitHub License](https://img.shields.io/github/license/reach-sh/reach-lang)](https://raw.githubusercontent.com/reach-sh/reach-lang/master/LICENSE) [![Docs](https://img.shields.io/badge/docs-delicious-blue)](http://docs.reach.sh) [![Discord](https://img.shields.io/discord/628402598663290882)](https://discord.com/channels/628402598663290882)  [![Twitter Follow](https://img.shields.io/twitter/follow/reachlang?style=social)](https://twitter.com/reachlang) [![Subreddit subscribers](https://img.shields.io/reddit/subreddit-subscribers/reach_sh?style=social)](https://www.reddit.com/r/reach_sh)

We are building the next generation blockchain development platform that will completely transform the paradigm of decentralized application (DApp) development. DApps have enormous potential, but are impractical today because of barriers that have kept blockchain development risky and limited to specialists, like cryptographers. The Reach platform is a comprehensive environment that enables junior developers to build useful and safe DApps without years of experience and easily deploy them on a variety of different blockchain networks.

This repository is for the Reach language, the domain-specific language for trustworthy DApps used by the Reach platform.

Read the [documentation](https://docs.reach.sh).

# Development

If you want to work on the Reach compiler, you'll need:
- stack v2.7.5
- `z3`
- `solc`
- [`goal`](https://github.com/algorand/go-algorand) OR link [`goal-devnet`](https://github.com/reach-sh/reach-lang/blob/master/scripts/goal-devnet) to `goal` in your `PATH`
- [mo](https://github.com/tests-always-included/mo) v2.2.0

The versions of our dependencies are specified in `DEPS`.

Installation on macOS:
```
$ brew tap ethereum/ethereum
$ brew install haskell-stack z3 solidity
$ curl -sSL https://git.io/get-mo -o mo && chmod +x mo && sudo mv mo /usr/local/bin/
```

Installation on Ubuntu:
```
$ sudo apt update
$ sudo apt install z3
$ sudo snap install solc
$ curl -sSL https://get.haskellstack.org/ | sh
$ curl -sSL https://git.io/get-mo -o mo && chmod +x mo && sudo mv mo /usr/local/bin/
```

These instructions may not install the exactly correct versions, and that may
matter. If it does, consult `hs/Dockerfile.reachc` to learn how to get specific
versions.

The source code is in the `hs` directory.
