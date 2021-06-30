#lang scribble/manual
@(require "lib.rkt")

@title[#:version reach-vers #:tag "guide-roadmap"]{Reach's Roadmap}

This section describes in a vague way some of the bigger future plans for Reach's development.
These are vague for expediency, but we're willing to elaborate if you ask on @(the-community-link).
We welcome your contributions on @link["https://github.com/reach-sh/reach-lang"]{GitHub} and on @(the-community-link) to helping make these plans come to fruition.

Last updated: 2021/06/30

@bold{Short term}:
@itemlist[
@item{General - Apple Silicon support}
@item{Language - @reachin{race}-winner @reachin{only} blocks}
@item{General - Error code index}
@item{Language - @reachin{fork}/@reachin{parallelReduce} local pass-through values}
@item{Language - Stateless participants}
@item{Language - @reachin{interact} continuations}
@item{Networks - Explicit state compilation option}
@item{Networks - Participant fast catch-up}
@item{Networks - @link["https://confluxnetwork.org/"]{Conflux}}
@item{Networks - Algorand - @link["https://forum.algorand.org/t/testnet-and-mainnet-update-2-7-1/3647"]{AVM 0.9} support}
@item{Networks - Algorand - Post-AVM limitation removal - remote objects, token minting, arbitrary contract length, etc}
@item{Optimization - Unify @tech{view} functions with identical bodies}
@item{Language - State linear in the number of transactions, rather than only participants}
@item{Testing - Unified devnet rather than per-application devnet}
@item{Frontends - Expose TypeScript types}
@item{Frontends - Session resumption}
@item{Language - Output streams}
@item{Networks - Interface constraints}
@item{General - Reach debugger}
]

@bold{Medium term}:
@itemlist[
@item{IDE - Language Server Protocol implementation}
@item{Language - threading / futures}
@item{Language - @reachin{for} to @reachin{while} syntactic sugar}
@item{Language - tail-recursive function to @reachin{while}}
@item{Language - fixed range integer types}
@item{Optimization - data-type coallescing}
@item{Language - non-communicating guaranteed termination loops}
@item{Verification - Constrain eventual use of values}
@item{Verification - @reachin{exit()} reachability}
@item{Verification - network analysis}
@item{Language - Dynamic token tracking}
]

@bold{Long term}:
@itemlist[
@item{Language - @link["https://en.wikipedia.org/wiki/Substructural_type_system"]{substructural types} to allow mutation}
@item{Language - pay-as-you-go closures}
@item{Language - general recursion through closure conversion of non-contifiable continuations}
@item{Verification - game-theoretic property verification}
@item{Verification - verified compiler}
]

