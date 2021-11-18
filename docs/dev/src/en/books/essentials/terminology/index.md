---
menuItem: mi-docs
---

# Terminology

# Assertion

An assertion is either: a knowledge assertion, which is a claim that one honest participant cannot know something that another honest participant does know; a static assertion, which is an always-true formula; an assumption, which is a true formula if frontends behave honestly; a requirement, which is a true formula if participants behave honestly; or, a possibility assertion, which is a formula for which there exists some values that honest participants and frontends could submit which results in the truth of the formula. An honest participant is one that executes the steps specified by the DApp, while an honest frontend is one that only returns values which ensure that all assumptions evaluate to the boolean true.

# Linear State

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
