# {#ref-programs-local} Local Steps

A Reach local step occurs in the body of `{!rsh} only` or `{!rsh} each` statements.
It represents the actions taken by a single participant in an application.

## {#ref-programs-local-stmts} Statements

Any statements valid for a [computation](##ref-programs-compute-stmts) are valid for a local step.

## {#ref-programs-local-exprs} Expressions

Any expressions valid for a [computation](##ref-programs-compute-exprs) are valid for a local step.
However, some additional expressions are allowed.

### {#ref-programs-local-this} `this`

Inside of a local step, `{!rsh} this` refers to the participant performing the step.
This is useful when the local step was initiated by an `{!rsh} each` expression.

### `interact`

@{ref("rsh", "interact")}
```reach
interact.amount
interact.notify(handA, handB)
interact.chooseAmount(heap1, heap2)
```

An @{defn("interaction expression")}, written `{!rsh} interact.METHOD(EXPR_0, ..., EXPR_n)`, where `{!rsh} METHOD` is an identifier bound in the participant interact interface to a function type, and `{!rsh} EXPR_0` through `{!rsh} EXPR_n` are expressions that evaluate to the result of an interaction with a frontend that receives the evaluation of the `{!rsh} n` expressions and sends a value.

An interaction expression may also be written `{!rsh} interact.KEY`, where `{!rsh} KEY` is bound in the participant interact interface to a non-function type.

An interaction expression may only occur in a local step.

``` reach
load: /examples/rps-7-loops/index.rsh
md5: ee287e712cdfe8d91bbb038c383d25d3
range: 45-48
```

In this example from the [Rock, Paper Scissors](##tut) tutorial, the program is in the local step of Alice.
It has Alice `{!rsh} declassify` the `{!rsh} interact` object for both the `wager` and the `deadline` for Bob, so that Bob can accept them.

### `assume`

@{ref("rsh", "assume")}
```reach
assume( claim, [msg] )
```

An assumption where `{!rsh} claim` evaluates to `{!rsh} true` with honest frontends.
This may only appear in a local step.
It accepts an optional bytes argument, which is included in any reported violation.

If the claim dynamically evaluates to `false`, the frontend will raise an exception.

``` reach
load: /examples/atomic-swap/index.rsh
md5: 03b8d3534337fd397adb4120ff90a103
range: 20-22
```

In this example, the claim is that the two tokens, tokenA and tokenB, are not the same, and there is no message that displays.

### `fail`

@{ref("rsh", "fail")}
```reach
fail()
```

is a convenience method equivalent to `{!rsh} assume(false)`. This may only appear in a local step.

Example:

```reach
load: /hs/t/y/fail.rsh
md5: 3be6dfe7a8cce9030cc121e0572ccdee
range: 14 - 19
```

This code calls `{!rsh} fail` inside of `A`'s `{!rsh} only` block on line 16.
It evaluates to `{!rsh} assume(false)`.

### `declassify`

@{ref("rsh", "declassify")}
```reach
declassify( arg )
```

The @{defn("declassify")} primitive performs a declassification of the given argument.

``` reach
load: /examples/rps-7-array/index.rsh
md5: d38eb05c7dc15d65d60114e7784da358
range: 67-71
```

In this statement, Alice declassifies the wager and then publishes it so that Bob can know the wager.

### `makeCommitment`

@{ref("rsh", "makeCommitment")}
```reach
makeCommitment( interact, x )
```

Returns two values, `{!rsh} [ commitment, salt ]`, where `{!rsh} salt` is the result of calling `{!rsh} interact.random()`, and
`{!rsh} commitment` is the digest of `{!rsh} salt` and `{!rsh} x`.
This is used in a local step before `{!rsh} checkCommitment` is used in a consensus step.

This is demonstrated in the example below.
`{!rsh} makeCommitment` is used on line 18 before `{!rsh} checkCommitment` on line 21:

```reach
load: /examples/object-digest/index.rsh
md5: 8d7e9f0180b6f53f80b36dcf172f5e73
range: 16-22
```

### `didPublish`

@{ref("rsh", "didPublish")}
```reach
didPublish()
```

Returns a boolean that indicates whether the last publication was made by this principal.
A `{!rsh} didPublish` call must be inside an `{!rsh} only` block of code and be after a `{!rsh} publish` call.

For example, in the code below, a `{!rsh} didPublish` call is made on line 62 in an `{!rsh} only` block of code after a `{!rsh} publish` call was made on line 35:

```reach
load: /examples/raffle/index.rsh
md5: 86dfc59fc496f3be68da7f97ff4abfac
range: 35-35
```

```reach
load: /examples/raffle/index.rsh
md5: 86dfc59fc496f3be68da7f97ff4abfac
range: 62-62
```
