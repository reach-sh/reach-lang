


# {#guide-changelog} Reach's Changelog

Below is a list of changes to Reach.
Versions and changes-within-versions are listed in reverse-chronological order: newest things first.

## 0.1.6: 2021/10 - present

Version 0.1.6 is the current Reach release candidate version.

+ 2021/10/18: Added APIs via the `API` form.
+ 2021/10/15: The backend interface to deployed contracts was updated, so old contracts will not work with this version.
+ 2021/10/15: The backend interface to the compiled objects was updated, so you'll need to recompile for this release.
+ 2021/10/15: Algorand devnet updated to versions 3.0.1 and 2.6.4


## 0.1.5: 2021/09 - 2021/10

Version 0.1.5 is the current Reach release version.

+ 2021/10/11: The Solidity compiler has been upgraded to 0.8.9.
+ 2021/10/08: The backend interface to the compiled objects was updated, so you'll need to recompile for this release.
+ 2021/10/08: `parallelReduce` is more strict in checking that the `msg` argument is present in the parameter list of `case` components, even when it is is bound to a `null`.
+ 2021/10/08: Added `getContract` and `getAddress`.
+ 2021/10/08: Added `ctc.getContractAddress`.
+ 2021/10/05: Added `Contract`. Updated `ctc.getInfo` to return a `Contract`.
+ 2021/10/04: Added `unstrict`.
+ 2021/09/25: Reach clients will detect that they are attempting to publish in a race that they cannot win and switch to listening for the publication of another.
This has the impact of frontends not being asked to sign transactions that cannot possibly succeed.
+ 2021/09/25: Added `didPublish()`.
+ 2021/09/24: Contracts do not store the Merkleization of the state, but store the state itself; this changes the interface to contracts, so this release cannot communicate with DApps compiled by older versions of Reach.
+ 2021/09/16: Bare integers used as time arguments will throw a deprecation warning. Use `relativeTime` instead.
+ 2021/09/16: The concept of deployment modes has been removed and the only available behavior is what was previously the `firstMsg` deployment mode.

If you would like the old behavior, then you'll want to create a new participant, perhaps called `Constructor`, that exists simply to run `Constructor.publish(); commit();`, but we expect that almost no one actually wants the old behavior exactly.
Instead, you probably want to select one of your existing participants and assign the first publication to them.
+ 2021/09/16: The backend interface to deployed contracts was updated, so old contracts will not work with this version.
+ 2021/09/16: The backend interface to the compiled objects was updated, so you'll need to recompile for this release.


## 0.1.4: 2021/09 - 2021/09

Version 0.1.4 is is an old Reach release version.

+ 2021/09/15: `muldiv` added.
+ 2021/09/08: Add `--stop-after-eval` and `--verify-timeout` options to `reach compile`.
+ 2021/08/31: Removed `getSignStrategy` and `setSignStrategy` in favor of `setWalletFallBack` and `walletFallback`.
+ 2021/08/31: Algorand devnet updated to versions 2.9.1 and 2.6.1
+ 2021/08/31: The `reach` command-line has changed:
+ All subcommands now support `-h`/`--help` switches, e.g. `reach compile --help`.
+ The `APP` argument to ${seclink("ref-usage-init")} has been removed.
+ The `reach` script now permits the use of subdirectories as arguments to certain subcommands, e.g. `reach compile dir/index.rsh`, but disallows parent directories (`..`) for reasons pertaining to Docker.
+ Devnets have been consolidated into a single Dockerized network and container topology.
+ The `--use-existing-devnet` flag has been deprecated and no longer has any effect.
+ ${seclink("ref-usage-run")} will now automatically connect to a given connector's devnet when already present.
Devnets which are not yet running will be launched as needed.
+ ${seclink("ref-usage-down")} now halts _ALL_ Dockerized Reach containers and devnets (i.e. it's no longer specific to a single project).
Non-Reach Docker services are unaffected (see ${seclink("ref-usage-docker-reset")}).
+ ${seclink("ref-usage-docker-reset")} now prompts the user for confirmation before continuing since it kills and removes _ALL_ containers (not just those related to Reach).
The `-y` or `--even-non-reach` flags may be appended for non-interactive execution.
+ An `--await-background` flag has been introduced to the ${seclink("ref-usage-devnet")} subcommand.
+ The `reach` script has been simplified such that `Makefile` and `docker-compose.yml` files are no longer integral to its function.
Accordingly, these files have been removed from ${seclink("ref-usage-scaffold")}'s output.
Authors of existing projects which contain unmodified `Makefile` or `docker-compose.yml` files are encouraged to remove them.



## 0.1.3: 2021/07 - 2021/08

Version 0.1.3 is an old Reach release version.

+ 2021/08/31: Added `acc.setStorageLimit` to JavaScript standard library for Conflux.
+ 2021/08/16: Allow `continue` in step in some cases.
+ 2021/07/31: Added `newTestAccounts`, `waitUntilSecs`, and `getNetworkSecs` to JavaScript standard library.
+ 2021/07/31: Updated `onProgress` type in JavaScript standard library.
+ 2021/07/31: Added `relativeTime`, `absoluteTime`, `relativeSecs`, `absoluteSecs`, `baseWaitTime`, `baseWaitSecs`, and `lastConsensusSecs` to Reach, with support in `wait` and `.timeout`.
+ 2021/07/22: `numberToFixedPoint()` and `numberToInt()` added.
+ 2021/07/21: Renamed Ethereum devnet Docker image to `devnet-eth`.
+ 2021/07/21: Renamed connector modes to use naming convention `$NET-devnet`, rather than exposing implementation.
+ 2021/07/21: Ethereum contract bytecode verification changed to directly compare deployment data
+ 2021/07/20: `Array.slice()` added.
+ 2021/07/19: `Token.destroyed()` added.
+ 2021/07/15: Ethereum contract info (i.e. `ctc.getInfo()`) reduced to address only.
+ 2021/07/14: Algorand contract info (i.e. `ctc.getInfo()`) reduced to application id only.
+ 2021/07/14: Minted tokens must be destroyed by end of application.
+ 2021/07/14: Token minting support added to Algorand.
+ 2021/07/14: Token URL metadata increased to 96 bytes.
+ 2021/07/14: Algorand `digest` switched to SHA256 (to save compute cost).
+ 2021/07/14: Algorand connector updated to AVM 0.9 (TEAL version 4)
+ 2021/07/14: Algorand devnet version updated to 2.7.1, plus `DevMode` patch
+ 2021/07/14: Algorand devnet image renamed to `devnet-algo`
+ 2021/07/14: version tagged


## 0.1.2: 2020/09 - 2021/07

Version 0.1.2 is an old Reach release version.

It is the last version that supports Algorand using TEAL3; if you deployed a contract on Algorand using Reach version 0.1.2, you will need to continue accessing it via the 0.1.2 version of the Reach standard library.

+ 2021/07/09: `.define` component added to `parallelReduce`
+ 2021/07/08: [Error code referenec](##ref-error-codes) created
+ 2021/06/20: Token minting introduced, with implementation on ETH.
+ ... many interesting things ...
+ 2020/09/01: version tagged


## 0.1.1: 2019/09 - 2020/09

Version 0.1.1 was used prior to our documented release process.

## 0.1.0: 2019/09 - 2020/09

Version 0.1.0 was used prior to our documented release process.
