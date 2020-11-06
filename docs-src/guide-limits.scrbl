#lang scribble/manual
@(require "lib.rkt")

@title[#:version reach-vers #:tag "guide-limits"]{What are Reach's limitations and its future roadmap}

Today, Reach is a powerful language for build decentralized applications, as demonstrated in @seclink["overview"]{the overview}, @seclink["tut"]{the tutorial}, and the @seclink["workshop"]{workshop series}.
However, it has a lot of potential for growth.
This section describes a few of these areas and gives brief sketches of our roadmap for directing this growth.
We welcome your contributions on @link["https://github.com/reach-sh/reach-lang"]{GitHub} and on @(the-community-link) to helping make these improvement plans come to fruition.

@(hrule)
@bold{Connectors.} Foremost, Reach is a @tech{consensus network}-agnostic language, so one of our highest priorities is supporting a wide variety of platforms, including layer-2 abstractions over other layer-1 networks.
Presently, we have a robust @seclink["ref-network-eth"]{Ethereum backend} and are working on an @seclink["ref-network-algo"]{Algorand backend}.

@(hrule)
@bold{Backends.} Presently, Reach has a robust @tech{backend} for JavaScript that is well-suited for client-facing applications and JavaScript servers.
However, we believe that many decentralized application developers would like to make use of languages like Go and Rust for their participants.
We are building a Go backend as a prototype of how to build a backend for a statically typed language.

@(hrule)
@bold{Computation.} Reach's computational language is based on JavaScript and contains many of JavaScript's most desirable features, like @tech{arrow expressions} and free-form objects.
We are working on making the transition for JavaScript developers as seamless as possible by integrating more compatibility through features like binding patterns in function arguments, more specific @reachin{import} and @reachin{export} specifiers, syntactic sugar for @reachin{while} patterns, like @jsin{for} in JavaScript, and recursive functions (when they are in a tail-recursive set.)
We also plan to add features from typed languages, like abstract data types and pattern matching.
Similarly, we have plans to extend Reach's type system to be able to track more specific features of values, such as @link["https://en.wikipedia.org/wiki/Refinement_type"]{refinement types}, @link["https://en.wikipedia.org/wiki/Substructural_type_system"]{substructural types} to allow mutation, and arbitrary range integer types.
Finally, we have plans to allow more exotic features, like non-communicating loops with guaranteed termination, statically computable exceptions, and pay-as-you-go closures, including non-tail-recursion through closure conversion of non-contifiable continuations.

@(hrule)
@bold{Verification.} Reach's verifier is robust in the face of many complex and interesting theorems about decentralized application behavior.
However, it does not soundly represent integer widths and not sensitive to numeric operation overflow, which causes traps on platforms like Algorand.
This is short-term work that is required before Reach should be consider safe for production.
In the longer term, we intend to introduce verification promises that constrain the eventual use of values, refine the knowledge checker to reduce false positives, verify core compiler algorithms, and introduce a model-checking-based assertion mechanism for specifying game theoretic properties of an application, such as that all state changes are Pareto improvements.

@(hrule)
@bold{Infrastructure.} We intend to build a package system for Reach to allow for sharing composable decentralized applications.

@(hrule)
@bold{Network Integration.} Since Reach is @tech{consensus network}-agnostic, it is not possible for Reach programs to directly integrate with network-specific features, such as other contracts on Ethereum or @link["https://developer.algorand.org/docs/features/asa/"]{standard assets} on Algorand.
We intend to support these through network selection options with Reach programs and a @link["https://en.wikipedia.org/wiki/Gradual_typing"]{gradual typing}-style protection mechanism to characterize the verifiable properties of the foreign resources.

@(hrule)
@bold{Communication.} Reach's communication language is limited to finite sets of a predetermined number of participants with deterministic choice and finite @tech{consensus state}.
This rules out many popular decentralized application designs, like auctions (which have an arbitrary number of participants communicating non-deterministically) and tokens (which have state linear in the number of participants.)
We intend to address these issues in three phases:

@itemize[

@item{The introduction of inductive state through bounded induction, to ensure that arbitrary data is not communicated or stored in @tech{consensus state}.
Bounded inductive state can be represented via @link["https://en.wikipedia.org/wiki/Merkle_tree"]{Merkle trees} to ensure that @tech{consensus state} remains finite.}

@item{The introduction of "participant classes", which are categories of participants that act identically.
After which, Reach will be limited to finite sets of a predetermined number of participant @emph{classes}.}

@item{The introduction of safe non-determinism through @link["https://en.wikipedia.org/wiki/Monoid#Commutative_monoid"]{commutative monoid} reduction, wherein a set of participants, @litchar{P}, may all contribute a value, @litchar{v(P)}, from a @link["https://en.wikipedia.org/wiki/Monoid"]{monoid} @litchar{M}.
Since @litchar{S} is a commutative monoid, it has a combining operation @litchar{+ : S x S -> S} such that @litchar{(a + b) + c = a + (b + c)} and @litchar{a + b = b + a}, so it is possible to combine the values in any order (i.e. non-deterministically) and arrive at the same value.
In practice, each participant will actually contribute a value, @litchar{u(P)} from an arbitrary set @litchar{U(P)} such that an operation @litchar{m(P) : U(P) -> M} exists, so that the value computed will be @litchar{𝛴 { m(P)(u(P)) | P ∈ participants }}.

This is analogous to using the so-called @link["https://en.wikipedia.org/wiki/MapReduce"]{MapReduce} programming model, with the additional constraint of commutativity on the reduce method.

This solution indicates why non-determinism is difficult: participants must be able to submit their values without knowing the other values in flight, yet must also still reach consensus on the outcome of the computation.
Non-commutativity would be acceptable, but would be lower-performance as participants that "missed" and were not the first sender would be required to review the first value to ensure that they compute the same answer as the @tech{consensus state}.
This "reviewing" would be observable by @tech{frontend}s if they were involved in computing the initial value.
}

]

Reach's communication language has other limitations for which we do not have short-term plans for removing.
For example, we do not intend to support co-inductive or cyclic state, nor expose an arbitrary consensus heap to programmers.
