---
author: Jay McCarthy
hasOtp: false
menuItem: mi-articles
publishedDate: 2020-08-29T14:00:00
---

# How does Reach work?

It is not necessary to understand how Reach works to use it effectively, but many users are curious about how it works. The Reach compiler uses the following strategy for analysis and compiling programs:

1. A partial evaluation of the source program that removes all function calls & compile-time values.

1. A linearization of the residual program that removes the need for a runtime stack to track any consensus state.

1. A conservative (sound) analysis of the knowledge of each participant.

1. A reduction of the program to an instance of an SMT ([satisfiability modulo theories](http://en.wikipedia.org/wiki/Satisfiability_Modulo_Theories)) theory of decentralized applications.

1. An end-point projection of the linearization to produce a perspective for each participant, as well as the consensus.

1. A single-pass top-down construction of backend and consensus programs.

Reach is proud to be implemented in [Haskell](https://en.wikipedia.org/wiki/Haskell_(programming_language)) using the [Glasgow Haskell Compiler](https://en.wikipedia.org/wiki/Glasgow_Haskell_Compiler); use the [Z3 theorem prover](https://en.wikipedia.org/wiki/Z3_Theorem_Prover) for verification; and use [Docker](https://www.docker.com/) for containerization.