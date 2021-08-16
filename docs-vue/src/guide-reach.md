


# {#guide-reach} How does Reach work?

It is not necessary to understand how Reach works to use it effectively, but many users are curious about how it works.
The Reach compiler uses the following strategy for analysis and compiling programs:

1. A partial evaluation of the source program that removes all function calls & compile-time values.
2. A linearization of the residual program that removes the need for a runtime stack to track any consensus state.
3. A conservative (sound) analysis of the knowledge of each participant.
4. A reduction of the program to an instance of a SMT ([satisfiability modulo theories](http://en.wikipedia.org/wiki/Satisfiability_Modulo_Theories)) theory of decentralized applications.
5. An end-point projection of the linearization to produce a perspective for each participant, as well as the consensus.
6. A single-pass top-down construction of backend and consensus programs.


Reach is proud to: be implemented in [Haskell](https://en.wikipedia.org/wiki/Haskell_(programming_language)) using the [Glorious Haskell Compiler](https://en.wikipedia.org/wiki/Glasgow_Haskell_Compiler); use the [Z3 theorem prover](https://en.wikipedia.org/wiki/Z3_Theorem_Prover) for verification; use  [Racket](https://www.racket-lang.org/)'s [Scribble](https://docs.racket-lang.org/scribble/) tool for documentation; and use [Docker](https://www.docker.com/) for containerization.
