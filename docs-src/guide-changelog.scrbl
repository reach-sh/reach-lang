#lang scribble/manual
@(require "lib.rkt")

@title[#:version reach-vers #:tag "guide-changelog"]{Reach's Changelog}

Below is a list of changes to Reach.
Versions and changes-within-versions are listed in reverse-chronological order: newest things first.

@section[#:style 'hidden-number]{0.1.5: 2021/09 - present}

Version 0.1.5 is the current Reach release candidate version.

@itemlist[
@item{2021/10/08: The backend interface to the compiled objects was updated, so you'll need to recompile for this release.}
@item{2021/10/08: @reachin{parallelReduce} is more strict in checking that the @litchar{msg} argument is present in the parameter list of @reachin{case} components, even when it is is bound to a @reachin{null}.}
@item{2021/10/08: Added @jsin{ctc.getContractAddress}.}
@item{2021/10/05: Added @reachin{Contract}. Updated @jsin{ctc.getInfo} to return a @reachin{Contract}.}
@item{2021/10/04: Added @reachin{unstrict}.}
@item{2021/09/25: Reach clients will detect that they are attempting to publish in a race that they cannot win and switch to listening for the publication of another.
This has the impact of frontends not being asked to sign transactions that cannot possibly succeed.}
@item{2021/09/25: Added @reachin{didPublish()}.}
@item{2021/09/24: Contracts do not store the Merkleization of the state, but store the state itself; this changes the interface to contracts, so this release cannot communicate with DApps compiled by older versions of Reach.}
@item{2021/09/16: Bare integers used as time arguments will throw a deprecation warning. Use @reachin{relativeTime} instead.}
@item{2021/09/16: The concept of deployment modes has been removed and the only available behavior is what was previously the @litchar{firstMsg} deployment mode.

If you would like the old behavior, then you'll want to create a new participant, perhaps called @litchar{Constructor}, that exists simply to run @reachin{Constructor.publish(); commit();}, but we expect that almost no one actually wants the old behavior exactly.
Instead, you probably want to select one of your existing participants and assign the first publication to them.
}
@item{2021/09/16: The backend interface to deployed contracts was updated, so old contracts will not work with this version.}
@item{2021/09/16: The backend interface to the compiled objects was updated, so you'll need to recompile for this release.}
]

@section[#:style 'hidden-number]{0.1.4: 2021/09 - 2021/09}

Version 0.1.4 is the current Reach release version.

@itemlist[
@item{2021/09/15: @reachin{muldiv} added.}
@item{2021/09/08: Add @DFlag{stop-after-eval} and @DFlag{verify-timeout} options to @exec{reach compile}.}
@item{2021/08/31: Removed @jsin{getSignStrategy} and @jsin{setSignStrategy} in favor of @jsin{setWalletFallBack} and @jsin{walletFallback}.}
@item{2021/08/31: Algorand devnet updated to versions 2.9.1 and 2.6.1}
@item{2021/08/31: The @exec{reach} command-line has changed:
  @itemlist[
    @item{All subcommands now support @Flag{h}/@DFlag{help} switches, e.g. @exec{reach compile --help}.}
    @item{The @tt{APP} argument to @secref["ref-usage-init"] has been removed.}
    @item{The @exec{reach} script now permits the use of subdirectories as arguments to certain subcommands, e.g. @exec{reach compile dir/index.rsh}, but disallows parent directories (@tt{..}) for reasons pertaining to Docker.}
    @item{Devnets have been consolidated into a single Dockerized network and container topology.}
    @item{The @DFlag{use-existing-devnet} flag has been deprecated and no longer has any effect.}
    @item{@secref["ref-usage-run"] will now automatically connect to a given connector's devnet when already present.
      Devnets which are not yet running will be launched as needed.}
    @item{@secref["ref-usage-down"] now halts @italic{ALL} Dockerized Reach containers and devnets (i.e. it's no longer specific to a single project).
      Non-Reach Docker services are unaffected (see @secref["ref-usage-docker-reset"]).}
    @item{@secref["ref-usage-docker-reset"] now prompts the user for confirmation before continuing since it kills and removes @italic{ALL} containers (not just those related to Reach).
      The @Flag{y} or @DFlag{even-non-reach} flags may be appended for non-interactive execution.}
    @item{An @DFlag{await-background} flag has been introduced to the @secref["ref-usage-devnet"] subcommand.}
    @item{The @tt{reach} script has been simplified such that @tt{Makefile} and @tt{docker-compose.yml} files are no longer integral to its function.
      Accordingly, these files have been removed from @secref["ref-usage-scaffold"]'s output.
      Authors of existing projects which contain unmodified @tt{Makefile} or @tt{docker-compose.yml} files are encouraged to remove them.}
  ]}
]

@section[#:style 'hidden-number]{0.1.3: 2021/07 - 2021/08}

Version 0.1.3 is an old Reach release version.

@itemlist[
@item{2021/08/31: Added @jsin{acc.setStorageLimit} to JavaScript standard library for Conflux.}
@item{2021/08/16: Allow @reachin{continue} in @tech{step} in some cases.}
@item{2021/07/31: Added @jsin{newTestAccounts}, @jsin{waitUntilSecs}, and @jsin{getNetworkSecs} to JavaScript standard library.}
@item{2021/07/31: Updated @jsin{onProgress} type in JavaScript standard library.}
@item{2021/07/31: Added @reachin{relativeTime}, @reachin{absoluteTime}, @reachin{relativeSecs}, @reachin{absoluteSecs}, @reachin{baseWaitTime}, @reachin{baseWaitSecs}, and @reachin{lastConsensusSecs} to Reach, with support in @reachin{wait} and @reachin{.timeout}.}
@item{2021/07/22: @jsin{numberToFixedPoint()} and @jsin{numberToInt()} added.}
@item{2021/07/21: Renamed Ethereum devnet Docker image to @litchar{devnet-eth}.}
@item{2021/07/21: Renamed connector modes to use naming convention @litchar{$NET-devnet}, rather than exposing implementation.}
@item{2021/07/21: Ethereum contract bytecode verification changed to directly compare deployment data}
@item{2021/07/20: @reachin{Array.slice()} added.}
@item{2021/07/19: @reachin{Token.destroyed()} added.}
@item{2021/07/15: Ethereum contract info (i.e. @jsin{ctc.getInfo()}) reduced to address only.}
@item{2021/07/14: Algorand contract info (i.e. @jsin{ctc.getInfo()}) reduced to application id only.}
@item{2021/07/14: Minted tokens must be destroyed by end of application.}
@item{2021/07/14: Token minting support added to Algorand.}
@item{2021/07/14: Token URL metadata increased to 96 bytes.}
@item{2021/07/14: Algorand @reachin{digest} switched to SHA256 (to save compute cost).}
@item{2021/07/14: Algorand connector updated to AVM 0.9 (TEAL version 4)}
@item{2021/07/14: Algorand devnet version updated to 2.7.1, plus @litchar{DevMode} patch}
@item{2021/07/14: Algorand devnet image renamed to @litchar{devnet-algo}}
@item{2021/07/14: version tagged}
]

@section[#:style 'hidden-number]{0.1.2: 2020/09 - 2021/07}

Version 0.1.2 is an old Reach release version.

It is the last version that supports Algorand using TEAL3; if you deployed a contract on Algorand using Reach version 0.1.2, you will need to continue accessing it via the 0.1.2 version of the Reach standard library.

@itemlist[
@item{2021/07/09: @reachin{.define} component added to @reachin{parallelReduce}}
@item{2021/07/08: @seclink["ref-error-codes"]{Error code referenec} created}
@item{2021/06/20: @tech{Token minting} introduced, with implementation on ETH.}
@item{... many interesting things ...}
@item{2020/09/01: version tagged}
]

@section[#:style 'hidden-number]{0.1.1: 2019/09 - 2020/09}

Version 0.1.1 was used prior to our documented release process.

@section[#:style 'hidden-number]{0.1.0: 2019/09 - 2020/09}

Version 0.1.0 was used prior to our documented release process.
