---
author: Jay McCarthy
hasOtp: false
menuItem: mi-articles
publishedDate: 2020-12-09T14:00:00
---

# Racing non-determinism in decentralized applications

As discussed earlier in the guide, Reach computations have a deterministic structure, but non-deterministic values. This means that a program will always execute steps A, B, and then C, but the values manipulated by those steps may be different on every execution.

The most common form of value non-determinism is through the interact expression and frontend-provided values. A Reach program merely specifies that a frontend must provide an unsigned integer that it will name bid, but not what value is actually provided.

However, a more subtle form of value non-determinism occurs with the race expression. This expression allows multiple participants to all attempt to provide the same value for a publication. For example, consider a turn-based game, like Nim, where there is no a priori way to determine who goes first. We could write a Reach program where a race determines the first player:

``` js
{
  // Alice publishes the wager

  // Bob accepts the wager

  Alice.only(() => { const aliceGoesFirst = true; });
  Bob.only(() => { const aliceGoesFirst = false; });
  race(Alice, Bob).publish(aliceGoesFirst);

  // They play the game, taking turns
}
```

This use-case demonstrates a major problem with races though. In the case of Nim, there is an advantage to whoever goes first: they can win if they choose the correct moves! Since Bob sent the previous publication, he will know about the opportunity to determine who goes first before Alice, so he can send both publications in back-to-back and be guaranteed to win. One strategy to avoid this would be to ensure that Alice and Bob both wait a pre-determined amount of time, after which they would each have a fair chance to race:

``` js
{
  // Alice publishes the wager

  // Bob accepts the wager

  Alice.only(() => { const aliceGoesFirst = true; });
  Bob.only(() => { const aliceGoesFirst = false; });

  wait(duration);

  race(Alice, Bob).publish(aliceGoesFirst);

  // They play the game, taking turns
}
```

However, even this strategy is dangerous, because it just creates an arms race between Alice and Bob to acquire more computational and network resources to guarantee that they are the first one, because whoever is first is the actual winner of the game, whatever happens next. A classic example of a situation like this was the Fomo3D winner, who used their capital to acquire millions in ETH in 2018.

A better strategy in this application would be to have each participant provide randomness using a commitment pattern (see makeCommitment and checkCommitment) then reveal that randomness to determine the winner. Or, to play a different game that is actually skill-based.

This example demonstrates the crucial problem with the participant non-determinism enabled by race: it will always produce an arms race for resources if winning the race results in winning funds. It is only safe and acceptable if who the winner is has no bearing on the ultimate outcome of the computation.

We can express this condition formally by saying that if A and B compete to provide value a and b respectively, then the computation should provide an opportunity for the first loser to provide their value later, such that it doesn’t matter what order they are provided. Mathematically, we could say that the program should not be a one-parameter function ‘f‘, where the computation is either ‘f(a)‘ or ‘f(b)‘. Instead, it should be a two-parameter function ‘g‘, such that ‘g(a, b) = g(b, a)‘ (i.e. a commutative function).