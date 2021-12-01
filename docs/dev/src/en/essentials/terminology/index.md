---
menuItem: mi-docs
---

# Terminology

# Assertion

An assertion is a statement that a given expression must be `true` or `false`. Consider the following:

``` js nonum
const a = Array.iota(5); // array(UInt, [0, 1, 2, 3, 4])
assert(a.sum() == 10);
```

In the example above, `assert` *asserts* that the sum of the elements in the array is 10. If the sum is not 10, the compiler stops.

Some assertions hinge on the assumption of honesty. An honest participant executes the steps specified by the DApp, and an honest frontend only returns values that ensure assumptions evaluate to `true`.

Types of assertions include the following:

* A *static assertion* is a must-be-true formula. 

* A *knowledge assertion* is a claim that one honest participant does not know something that another honest participant does know. 

* A *possibility assertion* is a formula for which there exists some values that honest participants and frontends could submit which result in the truth of the formula.

# Linear State

A smart contract may include [Maps](/en/essentials/backend-programming/reach-types/#map) that associate participant account addresses with values. The length of each `Map` equals the number of participants in the contract. This equality is called *linear state*:

<div><img src="linear-state.png" class="img-fluid my-4 d-block" width=200 height=198 loading="lazy"></div>

# Loop Invariant

A [Loop Invariant](https://en.wikipedia.org/wiki/Loop_invariant) is a property of a loop that is true before and after each iteration. Contract balance is a common example. Because any tokens paid to a contract account must be transferred out before a DApp exits -- a rule called the *Token Linearity Property* -- one invariant of many Reach `while` loops is contract balance. Consider the following:

``` js
const ctMap = new Map(UInt);
const [sum, stop] = parallelReduce([0, false])
  .invariant(balance() == sum && balance() == ctMap.sum())
  .while(!stop && balance() < p.goal)
  .case(C, () => {
    const amt = declassify(interact.getContribution());
    return { when: amt > 0, msg: amt };
  },
    (amt) => amt,
    (amt) => {
      const winner = this;
      ctMap[winner] = myFromMaybe(ctMap[winner]) + amt;
      return [sum + amt, false];
    }
  )
  .timeout(p.duration, () => {
    Anybody.publish();
    return [sum, true];
  });
```

* Line 2: Sets `sum` equal to 0.
* Line 3: Asserts that `balance()` will always be equal to `sum`.
* Line 9: Pays `amt` to the contract balance.
* Line 13: Adds `amt` to `sum`.

# Pay Amount

A pay amount is either:

* An integer, denoting an amount of network tokens; or,
* A tuple of token amounts.

A token amount is either:

* An integer, denoting an amount of network tokens; or,
* A tuple with two elements, where the first is an integer, denoting an amount of non-network tokens, and the second is Token, specifying a particular non-network token.

For example, these are all pay amounts:

``` nonum
0
5
[ 5 ]
[ 5, [ 2, gil ] ]
[ [ 2, gil ], 5 ]
[ 5, [ 2, gil ], [ 8, zorkmids ] ]
```

It is invalid for a pay amount to specify an amount of tokens multiple times. For examples, these are invalid pay amounts:

``` nonum
[ 1, 2 ]
[ [2, gil], [1, gil] ]
```

The ordering of a pay amount is only significant when used within a fork statement or parallel reduce statement that specifies a paySpec. In 
this case, payments are expected to be a tuple where the first element is an integer pay amount, and the rest of the elements are token amount tuples. The ordering of the token amount elements should match the ordering in paySpec. For example,

``` nonum
.paySpec([tokA, tokB])
```

will indicate that fork payments should be of the format:

``` nonum
[ NETWORK_TOKEN_AMT, [ amtA, tokA ], [ amtB, tokB ] ]
```

# Time Argument

A time argument represents consensus-network time. The argument is of type `Either(UInt, UInt)`, where the left variant refers to absolute network time and the right variant refers to absolute network seconds.
