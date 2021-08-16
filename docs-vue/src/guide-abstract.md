


# {#guide-abstract} Building decentralized abstractions

Many decentralized applications have the same structure, similar to how there are many games that can be categorized as [combinatorial games](https://en.wikipedia.org/wiki/Combinatorial_game_theory) or [simultaneous games](https://en.wikipedia.org/wiki/Simultaneous_game).

These applications can either be programmed individually, or you can build an abstraction that captures the common structure of a game.
Reach supports typical programming language abstractions, like first-class functions (via arrow expressions) and objects, that can be used to build these abstractions.

When building such abstractions, the most difficult part is correctly capturing [loop invariants](##guide-loop-invs) of the user of the abstraction on the inside of the abstraction.
Often, this means the abstraction must set up a protocol to communicate with its user, such as by receiving an invariant captured by an arrow expression.

::: note
See [the abstraction workshop](##workshop-abstract-simul) for a walthrough of building such an abstraction.
:::

