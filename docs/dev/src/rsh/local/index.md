



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

### `assume`

@{ref("rsh", "assume")}
```reach
assume( claim, [msg] ) 
```


 An assumption where `{!rsh} claim` evaluates to `{!rsh} true` with honest frontends.
This may only appear in a local step.
It accepts an optional bytes argument, which is included in any reported violation.

### `fail`

@{ref("rsh", "fail")}
```reach
fail() 
```


 is a convenience method equivalent to `{!rsh} assume(false)`. This may only appear in a local step.

### `declassify`

@{ref("rsh", "declassify")}
```reach
declassify( arg ) 
```


The @{defn("declassify")} primitive performs a declassification of the given argument.

### `makeCommitment`

@{ref("rsh", "makeCommitment")}
```reach
makeCommitment( interact, x ) 
```


 Returns two values, `{!rsh} [ commitment, salt ]`, where `{!rsh} salt` is the result of calling `{!rsh} interact.random()`, and
`{!rsh} commitment` is the digest of `{!rsh} salt` and `{!rsh} x`.
This is used in a local step before `{!rsh} checkCommitment` is used in a consensus step.

### `didPublish`

@{ref("rsh", "didPublish")}
```reach
didPublish() 
```


 Returns a boolean that indicates whether the last publication was made by this principal.
