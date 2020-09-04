#lang scribble/manual
@(require "lib.rkt")

@title[#:version reach-vers #:tag "guide-reach"]{How does Reach work?}

It is not necessary to understand how Reach works to use it effectively, but many users are curious about how it works.
The Reach compiler uses the following strategy for analysis and compiling programs:

@itemize[
#:style 'ordered

@item{A partial evaluation of the source program that removes all function calls & compile-time values.}

@item{A linearization of the residual program that removes the need for a runtime stack to track any consensus state.}

@item{A conservative (sound) analysis of the knowledge of each participant.}

@item{A reduction of the program to an instance of a SMT (@link["http://en.wikipedia.org/wiki/Satisfiability_Modulo_Theories"]{satisfiability modulo theories}) theory of decentralized applications.}

@item{An end-point projection of the linearization to produce a perspective for each participant, as well as the consensus.}

@item{A single-pass top-down construction of backend and consensus programs.}

]

Reach is proud to: be implemented in @link["https://en.wikipedia.org/wiki/Haskell_(programming_language)"]{Haskell} using the @link["https://en.wikipedia.org/wiki/Glasgow_Haskell_Compiler"]{Glorious Haskell Compiler}; use the @link["https://en.wikipedia.org/wiki/Z3_Theorem_Prover"]{Z3 theorem prover} for verification; use  @link["https://www.racket-lang.org/"]{Racket}'s @link["https://docs.racket-lang.org/scribble/"]{Scribble} tool for documentation; and use @link["https://www.docker.com/"]{Docker} for containerization.
