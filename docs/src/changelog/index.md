# {#guide-changelog} Changelog

Below is a list of changes to Reach.
Versions and changes-within-versions are listed in reverse-chronological order: newest things first.

## 0.1.12: 2022/09 - present

@{verRC("0.1.12")}
+ 2022/09/19: Added `{!js} ctc.appOptIn` and `{!js} account.appOptedIn`

@{rcHead("0.1.12-rc.5")}

@{rcHead("0.1.12-rc.4")}

+ 2022/10/12: Updated `{!js} test.chkErr` to use a regex for the expected error.
+ 2022/10/09 Added `{!rsh} opts.simNetRecv`, `{!rsh} opts.simTokensRecv`, and `{!rsh} opts.simReturnVal`.
+ 2022/10/07 Added `{!js} ctc.getEventTys`.

@{rcHead("0.1.12-rc.2")}

+ 2022/09/23: Added `{!js} ctc.getInternalState`.

@{rcHead("0.1.12-rc.1")}

+ 2022/09/19: Added `accounts` field to `{!rsh} remote.ALGO`.
+ 2022/09/15: Added `{!js} stdlib.contract`.
+ 2022/09/15: Added `{!js} new Reach(env?)`.

@{rcHead("0.1.12-rc.0")}

+ 2022/09/15: **Backwards Incompatible**:
  In 0.1.11-rc.6, we introduced the PeraWallet fallback for Algorand wallets.
  This introduced a dependency of the standard library on React, which was too onerous.
  We've removed it and thus, there is a slightly different way to enable PeraWallet as fallback.
  We apologize for this incompatibility.
+ 2022/09/14: Added `{!rsh} mixin`.
+ 2022/08/31: Upgraded to Algorand node v3.9.2, indexer 2.14.0, SDK 1.20.0.
+ 2022/08/31: Algorand-only: The backend interface to the compiled objects was updated, so you'll need to recompile for this release.

## 0.1.11: 2022/06 - 2022/09

@{verCur("0.1.11")}

@{rcHead("0.1.11-rc.10")}

+ 2022/09/08: Added `{!rsh} Contract.fromAddress`.

@{rcHead("0.1.11-rc.9")}

+ 2022/08/31: Added `rawCall` field to `{!rsh} remote.ALGO`.

@{rcHead("0.1.11-rc.8")}

+ 2022/08/29: The backend interface to the compiled objects was updated, so you'll need to recompile for this release.
+ 2022/08/29: Modified the type of `{!js} T_Bytes` in the JavaScript standard library to allow `{!js} Uint8Array`s and sometimes produce them when the bytes produced cannot be represented as a UTF-8 encoded string.
+ 2022/08/29: Added `{!rsh} Bytes.fromHex`.
+ 2022/08/25: Ethereum-like-only: The backend interface to deployed contracts was updated, so old contracts will not work with this version.
+ 2022/08/24: Added `{!rsh} StringDyn.concat` function and `{!rsh} StringDyn` cast application.
+ 2022/08/24: The backend interface to the compiled objects was updated, so you'll need to recompile for this release.
+ 2022/08/19: Added `{!rsh} StringDyn` type.
+ 2022/08/19: The backend interface to the compiled objects was updated, so you'll need to recompile for this release.
+ 2022/08/18: Added `{!rsh} BytesDyn` type.
+ 2022/08/16: Added `{!js} opts` parameter to `{!js} stdlib.transfer`.
+ 2022/08/16: Added `noVarOutput` option to `{!js} test.run`.
+ 2022/08/15: Added `{!rsh} autoTrackPublishedTokens` to `{!rsh} setOptions`.

@{rcHead("0.1.11-rc.7")}

+ 2022/08/10: Added support for exponential notation for numeric literals.
+ 2022/08/09: Added `strictPay` field to `{!rsh} remote.ALGO`.
+ 2022/08/07: The backend interface to the compiled objects was updated, so you'll need to recompile for this release.
+ 2022/08/07: Added `{!rsh} safe*` and `{!rsh} veri*` arithmetic functions, e.g. `{!rsh} safeAdd` and `{!rsh} veriAdd`.
+ 2022/08/05: Added `{!rsh} Struct.fields` and `{!rsh} Object.fields`.
+ 2022/08/04: Added `{!js} stdlib.tokensAccepted` and `{!js} acc.tokensAccepted`.
+ 2022/08/03: Added `{!rsh} isDataVariant`.

@{rcHead("0.1.11-rc.6")}

+ 2022/07/30: **Backwards Incompatible**: The `{!rsh} this` variable is removed from the scope inside `{!rsh} while` and `{!rsh} parallelReduce` syntax, except for the initialization expressions.

  We consider backwards incompatible changes to be very serious and do not introduce them lightly.
  In this case, we have observed too many programs containing major errors because they confuse `{!rsh} this`, which would refer to the _previous_ actor in these contexts with the _current_ actor.
  This confusion creates scenarios where applications are incorrectly protected: either they are overly protected, by enforcing that an actor do two things in a row; or they are under protected, by allowing anyone to perform any action after the desired actor does something else first.
  Given this danger, we are introducing this backwards incompatible change.
=======
+ 2022/07/30: Added support for [PeraWallet Connect](https://github.com/perawallet/connect) as wallet fallback for Algorand.
+ 2022/07/30: Upgraded React to 18.2.0 in `{!cmd} reach react`.
+ 2022/07/30: Many more customization options were added to `{!js} stdlib.setWalletFallback`.
+ 2022/07/30: Added support for customize `ALGO_INDEXER_TOKEN_HEADER` in Algorand provider configuration.
+ 2022/07/30: Added support for customize `ALGO_TOKEN_HEADER` in Algorand provider configuration.
+ 2022/07/30: Added `{!rsh} Token.accepted`.
+ 2022/07/29: Allow `{!js} loadStdlib` to be called multiple times to get separate standard library instances.

@{rcHead("0.1.11-rc.5")}

+ 2022/07/22: Added support for `{!rsh} View` aliases.
+ 2022/07/22: Added `freeze`, `reserve`, and `defaultFrozen` options to `{!js} stdlib.launchToken`.
+ 2022/07/13: Added `{!rsh} Token.track`.

@{rcHead("0.1.11-rc.4")}

+ 2022/06/18: Added `{!rsh} enforce`.
+ 2022/06/18: Added `{!rsh} currentMode`.
+ 2022/06/01: Added `onCompletion` field to `{!rsh} remote.ALGO`.
+ 2022/06/01: Added @{seclink("ref-programs-consensus-new-contract")} support.
+ 2022/06/01: Added @{seclink("ref-programs-appinit-contractcode")} support.
+ 2022/06/01: The backend interface to the compiled objects was updated, so you'll need to recompile for this release.

## 0.1.10: 2022/04 - 2022/05

@{verOld("0.1.10")}

+ 2022/05/14: Added `{!cmd} reach support` sub-command.
+ 2022/05/13: Support custom violation messages for `{!rsh} invariant`s.
+ 2022/05/13: Support multiple `{!rsh} invariant`s for `{!rsh} while` loops and `{!rsh} parallelReduce`.
+ 2022/05/13: The backend interface to the compiled objects was updated, so you'll need to recompile for this release.
+ 2022/05/13: Added `{!rsh} getCompanion`.
+ 2022/05/13: Added `apps` field to `{!rsh} remote.ALGO`.
+ 2022/05/07: Added `addressToAccount` field to `{!rsh} remote.ALGO`.
+ 2022/05/05: Added support for multiple `{!rsh} .define` branches in a `{!rsh} parallelReduce`.
+ 2022/05/04: Added `.api_` branch to `{!rsh} fork` and `{!rsh} parallelReduce`.
+ 2022/05/04: Added `CHECK_EXPR` to `{!rsh} .case` in `{!rsh} fork` and `{!rsh} parallelReduce`.
+ 2022/05/04: Added `.check` to `{!rsh} publish` and API `{!rsh} call`s.
+ 2022/04/30: Added `assets` field to `{!rsh} remote.ALGO`.
+ 2022/04/28: Added `{!rsh} sqrt` and renamed old version to `{!rsh} sqrtApprox`.
  This change is backwards incompatible, but we believe that most users will prefer the new version and will want to be warned, via a compile error, that it is available.
+ 2022/04/28: The backend interface to the compiled objects was updated, so you'll need to recompile for this release.
+ 2022/04/28: Added `{!cmd} REACH_NO_WARN` environment variable.
+ 2022/04/28: Added `{!js} test` to the JavaScript standard library.
+ 2022/04/26: Add support for calling `{!rsh} API`s multiple times in a program.
+ 2022/04/26: The backend interface to the compiled objects was updated, so you'll need to recompile for this release.
+ 2022/04/22: Added `--verify-fail-once` flag to @{seclink("ref-usage-compile")} to only print the first verification failure produced by a program.
+ 2022/04/19: Added `{!js} withDisconnect` and `{!js} disconnect` to help participant frontends disconnect early from a contract.
+ 2022/04/18: Added `{!rsh} Contract.addressEq`.
+ 2022/04/18: Added `{!rsh} remote.ALGO`.
+ 2022/04/18: Ethereum-like-only: The backend interface to deployed contracts was updated, so old contracts will not work with this version.
+ 2022/04/18: The backend interface to the compiled objects was updated, so you'll need to recompile for this release.
+ 2022/04/12: Added `{!js} getTimeSecs` for converting network time to network seconds.
+ 2022/04/09: Added the `note` field to `{!js} launchToken`.
+ 2022/04/07: The backend interface to the compiled objects was updated, so you'll need to recompile for this release.
+ 2022/04/07: Added `{!rsh} UInt256` type and associated operations.
+ 2022/04/07: Algorand indexer upgraded to `2.10.0`.
+ 2022/04/07: Algorand node upgraded to `3.5.1`.

## 0.1.9: 2022/03 - 2022/04

@{verOld("0.1.9")}

+ 2022/04/06: Added support for arbitrary `{!rsh} Map` keys on Ethereum/Conflux.
+ 2022/04/05: Added support for creating overloaded `{!rsh} API` functions and calling overloaded `{!rsh} remote` functions.
+ 2022/03/30: Added `{!rsh} thisConsensusTime` and `{!rsh} thisConsensusSecs`.
+ 2022/03/30: Added `{!rsh} polyMod`, which supports `{!rsh} Bytes` and `{!rsh} Digest`s.
+ 2022/03/22: Added `{!rsh} Set.Map` field.
+ 2022/03/22: Added support for `{!js} _` in JavaScript number literals, like
  `{!js} 1_337`.
+ 2022/03/17: Removed `{!rsh} polyXor` and made `{!rsh} xor` polymorphic.
+ 2022/03/16: On Algorand, Reach will use a companion application and inner transaction calls to it to provide up to 179,200 units of computation budget.
  Each 700 costs an extra transaction fee.
  The compiler will produce a fee and resource allocation report when run with debugging enabled.
+ 2022/03/14: Algorand-only: The backend interface to the compiled objects was updated, so you'll need to recompile for this release.
+ 2022/03/14: Added `{!rsh} polyXor` and update `{!rsh} ^` to use it.
+ 2022/03/12: The backend interface to the compiled objects was updated, so you'll need to recompile for this release.
+ 2022/03/09: Added `{!rsh} _local` to `{!rsh} parallelReduce` and `{!rsh} fork`.
+ 2022/03/08: Added `{!rsh} distinct`.
+ 2022/03/07: `{!rsh} remote` objects supported on Algorand.
+ 2022/03/02: Algorand-only: Drastically reduced the cost of `{!rsh} muldiv`.
+ 2022/03/02: Algorand-only: Contract bytecode upgraded to version `6`.
  (This enables `{!rsh} API` and `{!rsh} View` functions to be called on-chain.)
+ 2022/03/02: Algorand-only: Updated the default faucet.
  If you upgrade the standard library, but do not restart your devnet, tests will fail.
+ 2022/03/02: Algorand indexer upgraded to `2.9.0`.
+ 2022/03/02: Algorand node upgraded to `3.4.2`.
+ 2022/03/02: Ethers upgraded to `5.5.4`.
+ 2022/03/02: Ethereum ERC-20 implementation upgraded to OpenZeppelin `4.5.0`.
+ 2022/03/02: Ethereum (`geth`) upgraded to `1.10.16`.
+ 2022/03/02: Solidity upgraded to `0.8.12`.
+ 2022/03/02: Z3 upgraded to `4.8.14`.

## 0.1.8: 2022/01 - 2022/03

@{verOld("0.1.8")}

+ 2022/02/28: Added `{!js} launchToken` for token minting in frontends.
+ 2022/02/25: Added support for tracking `{!rsh} Token` information—such as balance, supply, and whether its destroyed—dynamically.
+ 2022/02/25: Added `{!cmd} reach info`.
+ 2022/02/25: Removed `{!cmd} reach upgrade` (subsumed by `{!cmd} reach update`).
+ 2022/02/08: Added `{!js} balancesOf`.
+ 2022/02/06: The backend interface to the compiled objects was updated, so you'll need to recompile for this release.
+ 2022/01/29: Added `{!cmd} ALGO_NODE_WRITE_ONLY`.
+ 2022/01/28: Added `{!js} setSigningMonitor`.
+ 2022/01/27: Added `{!js} setMinMillisBetweenRequests`.
+ 2022/01/27: Added `{!js} setCustomHttpEventHandler`.
+ 2022/01/27: Added `{!js} minimumBalanceOf`.
+ 2022/01/26: Added `{!rsh} untrustworthyMaps` option to `{!rsh} setOptions`.
+ 2022/01/25: The backend interface to the compiled objects was updated, so you'll need to recompile for this release.
+ 2022/01/24: Added `{!rsh} Array.forEachWithIndex`.
+ 2022/01/20: Added `{!rsh} check`.
+ 2022/01/13: Algorand-only: The backend interface to deployed contracts was updated, so old contracts will not work with this version.
+ 2022/01/13: The backend interface to the compiled objects was updated, so you'll need to recompile for this release.
+ 2022/01/13: Added `{!js} ctc.getABI`.
+ 2022/01/08: The Algorand connector does not rely on an Algorand Node for any
  information available through an Algorand Indexer.
  This is compatible with some Algorand network providers that restrict the API
  on Algorand Nodes.
+ 2022/01/06: The analysis of cost on Algorand is more precise in the presence
  of array operations.
+ 2022/01/06: Verification of only-one-honest participant is disabled.
  This proves nothing that wasn't verified already by the other two cases and
  just takes more time.
+ 2022/01/05: Conflux devnet has been upgraded to 0.1.7.

## 0.1.7: 2021/11 - 2021/12

@{verOld("0.1.7")}

+ 2022/01/06: Added `{!rsh} getUntrackedFunds`.
+ 2021/12/31: `{!js} setQueryLowerBound` is deprecated.
+ 2021/12/28: Algorand-only: The backend interface to compiled contract objects was updated, so you'll need to recompile for this release.
+ 2021/12/23: Algorand-only: The backend interface to deployed contracts was updated, so old contracts will not work with this version.
+ 2021/12/23: Added `{!rsh} verifyMuldiv`.
+ 2021/12/23: `{!rsh} deploy` was renamed to `{!rsh} init`.
+ 2021/12/21: The backend interface to deployed contracts was updated, so old contracts will not work with this version.
+ 2021/12/21: EVM-only: The backend interface to the compiled objects was updated, so you'll need to recompile for this release.
+ 2021/12/20: Added support for `{!rsh} PAY_REQUIRE_EXPR`, which allows `{!rsh} require` claims to be made about payments.
+ 2021/12/16: Added `{!rsh} Events`.
+ 2021/12/16: The backend interface to the compiled objects was updated, so you'll need to recompile for this release.
+ 2021/12/16: The backend interface to deployed contracts was updated, so old contracts will not work with this version.
+ 2021/12/10: Added support for using Reach on Apple Silicon.
+ 2021/11/25: The backend interface to the compiled objects was updated, so you'll need to recompile for this release.
+ 2021/11/19: The backend interface to deployed contracts was updated, so old contracts will not work with this version.
+ 2021/11/19: The backend interface to the compiled objects was updated, so you'll need to recompile for this release.
+ 2021/11/17: Added `{!js} ctc.unsafeViews` and `{!js} ctc.safeApis`.
+ 2021/11/15: Algorand connector now uses the application-controlled account for the escrow account.
This means that some programs will no longer work, if they do certain things, like trying to transfer non-network tokens in the same step that they share them with the consensus, because the contract needs to opt-in.
On the other hand, some things that used to not work, now do; for example, there used to be a limit of 15 extra transactions per consensus transfer, but now the limit is 31, with 15 from the client and 16 generated by the contract.
+ 2021/11/15: Algorand-only: The backend interface to deployed contracts was updated, so old contracts will not work with this version.

## 0.1.6: 2021/10 - 2021/11

@{verOld("0.1.6")}

+ 2021/11/02: Allow `{!rsh} API`s and `{!rsh} View`s to be specified without names.
+ 2021/10/28: `REACH_CONNECTOR_MODE` no longer defaults to `ETH` when unset.

Users are instead encouraged to run [`reach config`](##ref-usage-config) to set a persistent default in their shell or supply `REACH_CONNECTOR_MODE` explicitly at the command-line and in scripts.

Attempting to `reach run|react|rpc-server|rpc-run|devnet` without setting `REACH_CONNECTOR_MODE` in one's terminal will result in an error message being shown and execution aborted.
+ 2021/10/28: Added [`reach config` subcommand](##ref-usage-config).
+ 2021/10/28: The backend interface to deployed contracts was updated, so old contracts will not work with this version.
+ 2021/10/28: The backend interface to the compiled objects was updated, so you'll need to recompile for this release.
+ 2021/10/20: Added `{!rsh} decimals` field for token minting.
+ 2021/10/18: Added APIs via the `{!rsh} API` form.
+ 2021/10/15: The backend interface to deployed contracts was updated, so old contracts will not work with this version.
+ 2021/10/15: The backend interface to the compiled objects was updated, so you'll need to recompile for this release.
+ 2021/10/15: Algorand devnet updated to versions 3.0.1 and 2.6.4

## 0.1.5: 2021/09 - 2021/10

@{verOld("0.1.5")}

+ 2021/10/15: Added `{!js} setValidQueryWindow`.
+ 2021/10/11: The Solidity compiler has been upgraded to 0.8.9.
+ 2021/10/08: The backend interface to the compiled objects was updated, so you'll need to recompile for this release.
+ 2021/10/08: `{!rsh} parallelReduce` is more strict in checking that the `msg` argument is present in the parameter list of `{!rsh} case` components, even when it is is bound to a `{!rsh} null`.
+ 2021/10/08: Added `{!rsh} getContract` and `{!rsh} getAddress`.
+ 2021/10/08: Added `{!js} ctc.getContractAddress`.
+ 2021/10/05: Added `{!rsh} Contract`. Updated `{!js} ctc.getInfo` to return a `{!rsh} Contract`.
+ 2021/10/04: Added `{!rsh} unstrict`.
+ 2021/09/25: Reach clients will detect that they are attempting to publish in a race that they cannot win and switch to listening for the publication of another.
This has the impact of frontends not being asked to sign transactions that cannot possibly succeed.
+ 2021/09/25: Added `{!rsh} didPublish()`.
+ 2021/09/24: Contracts do not store the Merkleization of the state, but store the state itself; this changes the interface to contracts, so this release cannot communicate with DApps compiled by older versions of Reach.
+ 2021/09/16: Bare integers used as time arguments will throw a deprecation warning. Use `{!rsh} relativeTime` instead.
+ 2021/09/16: The concept of deployment modes has been removed and the only available behavior is what was previously the `firstMsg` deployment mode.

If you would like the old behavior, then you'll want to create a new participant, perhaps called `Constructor`, that exists simply to run `{!rsh} Constructor.publish(); commit();`, but we expect that almost no one actually wants the old behavior exactly.
Instead, you probably want to select one of your existing participants and assign the first publication to them.
+ 2021/09/16: The backend interface to deployed contracts was updated, so old contracts will not work with this version.
+ 2021/09/16: The backend interface to the compiled objects was updated, so you'll need to recompile for this release.

## 0.1.4: 2021/09 - 2021/09

@{verOld("0.1.4")}

+ 2021/09/15: `{!rsh} muldiv` added.
+ 2021/09/08: Add `--stop-after-eval` and `--verify-timeout` options to `reach compile`.
+ 2021/08/31: Removed `{!js} getSignStrategy` and `{!js} setSignStrategy` in favor of `{!js} setWalletFallBack` and `{!js} walletFallback`.
+ 2021/08/31: Algorand devnet updated to versions 2.9.1 and 2.6.1
+ 2021/08/31: The `reach` command-line has changed:
+ All subcommands now support `-h`/`--help` switches, e.g. `reach compile --help`.
+ The `APP` argument to @{seclink("ref-usage-init")} has been removed.
+ The `reach` script now permits the use of subdirectories as arguments to certain subcommands, e.g. `reach compile dir/index.rsh`, but disallows parent directories (`..`) for reasons pertaining to Docker.
+ Devnets have been consolidated into a single Dockerized network and container topology.
+ The `--use-existing-devnet` flag has been deprecated and no longer has any effect.
+ @{seclink("ref-usage-run")} will now automatically connect to a given connector's devnet when already present.
Devnets which are not yet running will be launched as needed.
+ @{seclink("ref-usage-down")} now halts _ALL_ Dockerized Reach containers and devnets (i.e. it's no longer specific to a single project).
Non-Reach Docker services are unaffected (see @{seclink("ref-usage-docker-reset")}).
+ @{seclink("ref-usage-docker-reset")} now prompts the user for confirmation before continuing since it kills and removes _ALL_ containers (not just those related to Reach).
The `-y` or `--even-non-reach` flags may be appended for non-interactive execution.
+ An `--await-background` flag has been introduced to the @{seclink("ref-usage-devnet")} subcommand.
+ The `reach` script has been simplified such that `Makefile` and `docker-compose.yml` files are no longer integral to its function.
Accordingly, these files have been removed from @{seclink("ref-usage-scaffold")}'s output.
Authors of existing projects which contain unmodified `Makefile` or `docker-compose.yml` files are encouraged to remove them.

## 0.1.3: 2021/07 - 2021/08

@{verOld("0.1.3")}

+ 2021/08/31: Added `{!js} acc.setStorageLimit` to JavaScript standard library for Conflux.
+ 2021/08/16: Allow `{!rsh} continue` in step in some cases.
+ 2021/07/31: Added `{!js} newTestAccounts`, `{!js} waitUntilSecs`, and `{!js} getNetworkSecs` to JavaScript standard library.
+ 2021/07/31: Updated `{!js} onProgress` type in JavaScript standard library.
+ 2021/07/31: Added `{!rsh} relativeTime`, `{!rsh} absoluteTime`, `{!rsh} relativeSecs`, `{!rsh} absoluteSecs`, `{!rsh} baseWaitTime`, `{!rsh} baseWaitSecs`, and `{!rsh} lastConsensusSecs` to Reach, with support in `{!rsh} wait` and `{!rsh} .timeout`.
+ 2021/07/22: `{!js} numberToFixedPoint()` and `{!js} numberToInt()` added.
+ 2021/07/21: Renamed Ethereum devnet Docker image to `devnet-eth`.
+ 2021/07/21: Renamed connector modes to use naming convention `$NET-devnet`, rather than exposing implementation.
+ 2021/07/21: Ethereum contract bytecode verification changed to directly compare deployment data
+ 2021/07/20: `{!rsh} Array.slice()` added.
+ 2021/07/19: `{!rsh} Token.destroyed()` added.
+ 2021/07/15: Ethereum contract info (i.e. `{!js} ctc.getInfo()`) reduced to address only.
+ 2021/07/14: Algorand contract info (i.e. `{!js} ctc.getInfo()`) reduced to application id only.
+ 2021/07/14: Minted tokens must be destroyed by end of application.
+ 2021/07/14: Token minting support added to Algorand.
+ 2021/07/14: Token URL metadata increased to 96 bytes.
+ 2021/07/14: Algorand `{!rsh} digest` switched to SHA256 (to save compute cost).
+ 2021/07/14: Algorand connector updated to AVM 0.9 (TEAL version 4)
+ 2021/07/14: Algorand devnet version updated to 2.7.1, plus `DevMode` patch
+ 2021/07/14: Algorand devnet image renamed to `devnet-algo`
+ 2021/07/14: version tagged

## 0.1.2: 2020/09 - 2021/07

@{verOld("0.1.2")}

It is the last version that supports Algorand using TEAL3; if you deployed a contract on Algorand using Reach version 0.1.2, you will need to continue accessing it via the 0.1.2 version of the Reach standard library.

+ 2021/07/09: `{!rsh} .define` component added to `{!rsh} parallelReduce`
+ 2021/07/08: [Error code reference](##ref-error-codes) created
+ 2021/06/20: Token minting introduced, with implementation on ETH.
+ ... many interesting things ...
+ 2020/09/01: version tagged

## 0.1.1: 2019/09 - 2020/09

Version 0.1.1 was used prior to our documented release process.

## 0.1.0: 2019/09 - 2020/09

Version 0.1.0 was used prior to our documented release process.
