#lang scribble/manual
@(require "lib.rkt")

@title[#:version reach-vers #:tag "ref-backends" #:style 'toc]{Participant Backends}

This section describes the @tech{participant} @tech{backends}
supported by Reach version @|reach-vers|.

@local-table-of-contents[#:style 'immediate-only]

@section[#:tag "ref-backend-js"]{JavaScript}

The Reach JavaScript @tech{backend} produces a compilation output named @filepath{input.mjs} which exports an asynchronous function for each @tech{participant}. Each function accepts @jsin{3+n} arguments where the first three arguments are @jsin{stdlib}, @jsin{ctc}, and @jsin{interact}, while the remaining arguments are the initial @tech{local state} of the @tech{participant}. These functions should be called by the @tech{frontend}.

The @jsin{stdlib} argument is provided by either
@itemlist[
 @item{the module @litchar{@"@"reach-sh/stdlib/ETH.mjs}; or,}
 @item{the modules @litchar{@"@"reach-sh/stdlib/ALGO.mjs}.} ]

The @jsin{ctc} argument is the result of a call to @jsin{acc.deploy} or @jsin{acc.attach}.

The @jsin{interact} argument is an object that has a method for each @tech{interact}ion in the corresponding @tech{participant}'s @tech{local computation}.

The @jsin{stdlib} modules export the following functions that might be used in this @tech{frontend}.

@(hrule)
@js{
 newTestAccount(balance) => acc }

@index{newTestAccount} Returns a Reach @tech{account} abstraction for a new @tech{account} on the @tech{consensus network} with a given balance of @tech{network tokens}. This can only be used in private testing scenarios, as it uses a private faucet to issue @tech{network tokens}.

@(hrule)
@js{
 connectAccount(networkAccount) => acc }

@index{connectAccount} Returns a Reach @tech{account} abstraction for an existing @tech{account} for the @tech{consensus network} based on the @tech{connector}-specific @tech{account} specification provided by the @jsin{networkAccount} argument.

@(hrule)
@js{
 acc.address => networkAccount }

@index{acc.address} Returns the @tech{connector}-specific @tech{account} specification of a Reach @tech{account} abstraction.

@(hrule)
@js{
 acc.deploy(bin) => ctc }

@index{acc.deploy} Returns a Reach @tech{contract} abstraction after deploying a Reach @DApp @tech{contract} based on the @jsin{bin} argument provided. This @jsin{bin} argument is the @filepath{input.mjs} module produced by the JavaScript @tech{backend}.

@(hrule)
@js{
 acc.attach(bin, ctc) => ctc }

@index{acc.attach} Returns a Reach @tech{contract} abstraction based on a deployed Reach @DApp @tech{contract} provided in the @jsin{ctc} argument and the @jsin{bin} argument. This @jsin{bin} argument is the @filepath{input.mjs} module produced by the JavaScript @tech{backend}.

@(hrule)
@js{
 balanceOf(acc) => amount }

@index{balanceOf} Returns the balance of @tech{network tokens} held by the @tech{account} given by a Reach @tech{account} abstraction provided by the @jsin{acc} argument.

@(hrule)
@js{
 transfer(from:networkAccount, to:networkAccount, amount) => void }

@index{transfer} Transfers @jsin{amount} @tech{network tokens} from @jsin{from} to @jsin{to}, which are provided by @tech{connector}-specific @tech{account} specifications.

@section[#:tag "ref-backend-go"]{Go} @(experimental)

An experimental, and undocumented, Go @tech{backend} is in progress in the
repository. When we complete it, this section will describe the use of
the @tech{backend}.
