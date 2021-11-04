---
author: Jay McCarthy
hasOtp: false
menuItem: mi-docs
publishedDate: 2020-08-29T14:00:00
---

# Finding and using loop invariants

Reach requires that `while` loops are annotated with [loop invariants](https://en.wikipedia.org/wiki/Loop_invariant).
A loop invariant is a property `INV` which is true before the loop starts and is true after the loop ends.

Consider the following program fragment:

```js
... before ...
var V = INIT;
invariant( INV );
while ( COND ) {
 ... body ...
 V = NEXT;
 continue; }
... after ...
assert(P); 
```

We can summarize the properties that must be true about this code as follows:

* `before` and `V = INIT` implies `INV` --- The earlier part of the program must establish `INV`.
* If `COND` and `INV`, then `body` and `V = NEXT` implies `INV` --- The loop body can make use of the truth of the condition and the invariant to re-establish the invariant after `V` is mutated to `NEXT`.
* `! COND` and `INV` and `after` implies `P` --- The later part of the program can make use of the negation of the condition and the invariant to establish any future assertions.

Loop invariants only need to mention values that can vary because of the execution of the loop. In Reach, all bindings are immutable, except for those bound by `while`, so they never need to be mentioned. However, Reach has two kinds of mutable bindings: loop variables and the contract balance (which is imperatively modified by `pay` and `transfer`). As such, both of these are typically mentioned in loop invariants.

Loop variables are mentioned if they occur in subsequent assertions, or if they are used to perform potentially unsafe actions, like an array dereference. But, since every Reach program terminates with the token linearity property, loop invariants always reference the contract balance.

When designing a loop invariant, first write down an equation for the contract balance before the loop. If the loop contains any transfers to the contract, then you must be able to track the amount and number of these. In the best case, you should be able to express the balance as an equation over the existing loop variables. In the worst case, you will have to add more loop variables to track some quantity, like the number of rounds of a game, that the balance is derived from.

After you've tracked the balance, you will need to add additional clauses that track whatever properties you rely on in the tail of the loop. The most complex circumstance is when you have nested loops. In this situation, the inner loop's invariant will have to include clauses related to the outer loop's invariant.