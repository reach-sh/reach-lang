---
menuItem: mi-docs
---

# Terminology

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
