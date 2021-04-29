#lang scribble/manual
@(require "lib.rkt")

@title[#:version reach-vers #:tag "guide-limits"]{What are Reach's limitations and its future roadmap}

Today, Reach is a powerful language for building decentralized applications, as demonstrated in @seclink["overview"]{the overview}, @seclink["tut"]{the tutorial}, and the @seclink["workshop"]{workshop series}.
However, it has a lot of potential for growth.
This section describes a few of these areas and gives brief sketches of our roadmap for directing this growth.
We welcome your contributions on @link["https://github.com/reach-sh/reach-lang"]{GitHub} and on @(the-community-link) to helping make these improvement plans come to fruition.

@(hrule)
@bold{Connectors.} Foremost, Reach is a @tech{consensus network}-agnostic language, so one of our highest priorities is supporting a wide variety of platforms, including layer-2 abstractions over other layer-1 networks.
Presently, we have a robust @seclink["ref-network-eth"]{Ethereum backend} and @seclink["ref-network-algo"]{Algorand backend}.

@(hrule)
@bold{Backends.} Presently, Reach has a robust @tech{backend} for JavaScript that is well-suited for client-facing applications and JavaScript servers.
However, we believe that many decentralized application developers would like to make use of languages like Go and Rust for their participants.
Presently, this can be accomplished via the @secref["ref-backends-rpc"], but we'd like to build a dedicated backend for languages like these.

@(hrule)
@bold{Computation.} Reach's computational language is based on JavaScript and contains many of JavaScript's most desirable features, like @tech{arrow expressions}, free-form objects, destructuring bindings, robust @reachin{import} and @reachin{export} specificiers, and so on.
We are working on making the transition for JavaScript developers as seamless as possible by integrating more compatibility through features like syntactic sugar for @reachin{while} patterns, like @jsin{for} in JavaScript, and recursive functions (when they are in a tail-recursive set.)
We have plans to extend Reach's type system to be able to track more specific features of values, such as @link["https://en.wikipedia.org/wiki/Substructural_type_system"]{substructural types} to allow mutation and arbitrary range integer types, which will be coalesced during compilation.
Finally, we have plans to allow more exotic features, like non-communicating loops with guaranteed termination, statically computable exceptions, and pay-as-you-go closures, including non-tail-recursion through closure conversion of non-contifiable continuations.

@(hrule)
@bold{Verification.} Reach's verifier is robust in the face of many complex and interesting theorems about decentralized application behavior.
In the longer term, we intend to introduce verification promises that constrain the eventual use of values, refine the knowledge checker to reduce false positives, verify core compiler algorithms, and introduce a model-checking-based assertion mechanism for specifying game theoretic properties of an application, such as that all state changes are Pareto improvements.

@(hrule)
@bold{Infrastructure.} We intend to build a package system for Reach to allow for sharing composable decentralized applications.

@(hrule)
@bold{Network Integration.} Since Reach is @tech{consensus network}-agnostic, it is not possible for Reach programs to directly integrate with network-specific features, such as observing the blockhash on Ethereum.
Reach programs can instead interact with these low-level details of their chosen consensus network via @tech{remote object} interaction.

@(hrule)
@bold{Communication.}
Reach's communication language has some limitations that we have plans to remove.
For example, Reach @tech{linear state} must be linear in the number of participants to the application, rather than in the number of @tech{publications}, because some consensus networks do not support the latter.
We plan to allow such programs and simply limit which networks they may compile to.

Reach's communication language has some limitations for which we do not have plans for removing.
For example, we do not intend to support co-inductive or cyclic state, nor expose an arbitrary consensus heap to programmers.
