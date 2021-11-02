#lang scribble/manual
@(require "lib.rkt")

@title[#:version reach-vers #:tag "guide-abstract"]{Building decentralized abstractions}

Many decentralized applications have the same structure, similar to how there are many games that can be categorized as @link["https://en.wikipedia.org/wiki/Combinatorial_game_theory"]{combinatorial games} or @link["https://en.wikipedia.org/wiki/Simultaneous_game"]{simultaneous games}.

These applications can either be programmed individually, or you can build an abstraction that captures the common structure of a game.
Reach supports typical programming language abstractions, like first-class functions (via @tech{arrow expressions}) and objects, that can be used to build these abstractions.

When building such abstractions, the most difficult part is correctly capturing @seclink["guide-loop-invs"]{loop invariants} of the user of the abstraction on the inside of the abstraction.
Often, this means the abstraction must set up a protocol to communicate with its user, such as by receiving an invariant captured by an @tech{arrow expression}.

@margin-note{See @seclink["workshop-abstract-simul"]{the abstraction workshop} for a walkthrough of building such an abstraction.}
